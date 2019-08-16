(in-package :filecoin)

(in-suite filecoin-suite)

(defun filecoin-json-path () (merge-pathnames (filecoin-json-directory) "filecoin.json"))

(defvar *benchmarks* nil)

(defun benchmarks-pathname ()
  (let* ((candidates (directory (merge-pathnames (make-pathname :directory '(:relative "input") :name :wild :type "json") (filecoin-json-directory))))
	 (narrowed (remove-if-not (lambda (pathname)
				    (let* ((name (pathname-name pathname))					  
					   (suffix "-benchmarks")
					   (name-length (length name))
					   (suffix-length (length suffix))
					   (start1 (- name-length suffix-length)))
				      (and (>= start1 0)
					   (string= name suffix :start1 (- name-length suffix-length)))))
				  candidates))
	 (sorted (sort narrowed #'> :key #'file-write-date)))    
    ;; Return the most recently written json file with suffix, -benchmarks.
    (car sorted)))

(defun read-benchmarks (&optional (pathname (benchmarks-pathname)))
  (awhen pathname
    (with-open-file (in it :direction :input)
      (let ((json:*json-symbols-package* 'keyword))
	(json:decode-json in)))))

(defun extract-data (key data) (cdr (assoc key data)))

(defun to-tuple (pairs)
  (make-tuple
   (loop for pair in pairs
      collect (list (intern (string (car pair)) (find-package :filecoin)) (cdr pair)))))

(defun extract-tuples (key data)
  (loop for pairs in (cdr (assoc key data))
     collect (to-tuple pairs)))

(defun extract-labeled-tuples (key data)
  (loop for (k . pairs) in (extract-data key data)
     collect (list k (to-tuple pairs))))

(defun micro-bench-desc-pairs (desc)
  (destructuring-bind (&optional name op size) (string-split #\/ desc)
    `((:name . ,name)
      (:op . ,op)
      (:size . ,(and size (parse-integer size))))))

(defun raw-benchmarks (data)
  (extract-data :benchmarks data))

(defun make-micro-benchmarks (benchmarks-data)
  (loop for ((ignore . desc) . pairs) in (car (extract-data :micro-benchmarks (raw-benchmarks benchmarks-data)))
     collect (to-tuple (append (micro-bench-desc-pairs desc) pairs))))

(defun make-zigzag-benchmarks (benchmarks-data)
  (extract-labeled-tuples :zigzag-benchmarks (raw-benchmarks benchmarks-data)))

(defun make-hash-constraints (benchmarks-data)
  (extract-tuples :hash-constraints (raw-benchmarks benchmarks-data)))

(defun get-hash-constraints (hash-fn bytes)
  (with-attributes (hash-constraints)
      (benchmarks)
    (tref 'constraints (extract (join (tuple (bytes bytes) (hash-fn hash-fn))
				      hash-constraints)))))

(defun benchmarks (&optional (pathname (benchmarks-pathname)))
  (or *benchmarks*
      (let* ((data (read-benchmarks pathname))
	     (benchmarks-tuple (tuple
				(git (to-tuple (extract-data :git data)))
				(system (to-tuple (extract-data :system data)))
				(micro-benchmarks (make-relation (make-micro-benchmarks data)))
				(zigzag-benchmarks (make-zigzag-benchmarks data))
				(hash-constraints (make-relation (make-hash-constraints data))))))
	(setq *benchmarks* benchmarks-tuple))))

(defun lookup-micro-benchmark (name &key op size (benchmarks (benchmarks)))
  (join (make-tuple (remove nil `((name ,name)
				  (op ,op)
				  (size ,size))
			    :key #'cadr))
	(tref 'micro-benchmarks benchmarks)))
