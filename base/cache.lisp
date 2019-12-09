(in-package orient.cache)

(defvar *cache-dir* "/tmp/orient-cache/")

(defclass cache () ())

(defclass mem-cache (cache)
  ;; TODO: limit size.
  ((hash-table :initarg :hash-table :initform (make-hash-table :test #'equal) :accessor cache-hash-table)))

(defclass disk-cache (cache)
  ((root :initarg :root :initform *cache-dir* :reader disk-cache-root :accessor cache-root)))

(defclass disk-backed-mem-cache (cache)
  ((mem-cache :initarg :mem-cache :initform (make-instance 'mem-cache) :accessor cache-mem-cache)
   (disk-cache :initarg :disk-cache :initform (make-instance 'disk-cache) :accessor cache-disk-cache)))

(defgeneric cache-lookup (key cache)
  (:method ((key t) (cache mem-cache))
    (gethash key (cache-hash-table cache))))

(defgeneric cache-store (key cache value)
  (:method ((key t) (cache mem-cache) (value t))
    (setf (gethash key (cache-hash-table cache)) value)))

;; Collisions are not impossible. SXHASH output is 62 bits. (But this is fast.)
;; TODO: handle collisions with fallback to collision-resistant hash. (And only then pay that cost.)
(defun key-for (args)
  (sxhash args))

(defun call-with-cache (cache function key-args all-args)
  (let* ((key (key-for key-args)))
    (multiple-value-bind (cached-values presentp)
        (cache-lookup key cache)
      (if presentp
          (values-list cached-values)
          (let ((values (multiple-value-list (apply function all-args))))
            (cache-store key cache values)
            (values-list values))))))
