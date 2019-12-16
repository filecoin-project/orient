(in-package orient.cache)

(defvar *cache-dir* "/tmp/orient-cache/")

(defclass cache () ())

(defclass mem-cache (cache)
  ;; TODO: limit size.
  ((lock :initform (bt:make-lock) :reader cache-lock)
   (hash-table :initarg :hash-table :initform (make-hash-table :test #'equal) :accessor cache-hash-table)))

(defclass disk-cache (cache)
  ((root :initarg :root :initform *cache-dir* :reader disk-cache-root :accessor cache-root)))

(defclass disk-backed-mem-cache (cache)
  ((mem-cache :initarg :mem-cache :initform (make-instance 'mem-cache) :accessor cache-mem-cache)
   (disk-cache :initarg :disk-cache :initform (make-instance 'disk-cache) :accessor cache-disk-cache)))

(defun make-disk-backed-mem-cache (&key (root *cache-dir*))
  (make-instance 'disk-backed-mem-cache :disk-cache (make-instance 'disk-cache :root root)))

(defgeneric cache-lookup (key cache &key payload-deserializer)
  (:method ((key t) (cache mem-cache)  &key &allow-other-keys)
    (bt:with-lock-held ((cache-lock cache))
      (gethash key (cache-hash-table cache))))
  (:method ((key t) (cache disk-cache)  &key payload-deserializer &allow-other-keys)
    (let ((pathname (cache-path-for-key cache key)))
      (if (probe-file pathname)
          (let ((raw-values (load-json :data pathname)))
            (aif payload-deserializer
                 (values (funcall it raw-values) t)
                 (values raw-values t)))
          (values nil nil))))
  (:method ((key t) (cache disk-backed-mem-cache) &key payload-deserializer)
    (multiple-value-bind (values mem-presentp)
        (cache-lookup key (cache-mem-cache cache))
      (if mem-presentp
          (values values t)
          (multiple-value-bind (values disk-presentp)
              (cache-lookup key (cache-disk-cache cache)
                            :payload-deserializer payload-deserializer)
            (if disk-presentp
                (progn
                  (cache-store key (cache-mem-cache cache) values)
                  (values values t))
                (values nil nil)))))))

;; Returns VALUES as values.
(defgeneric cache-store (key cache values &key payload-serializer)
  (:method ((key t) (cache mem-cache) (values list) &key &allow-other-keys)
    (bt:with-lock-held ((cache-lock cache))
      (setf (gethash key (cache-hash-table cache)) values)))
  (:method ((key t) (cache disk-cache) (values list) &key payload-serializer)
    (let ((pathname (cache-path-for-key cache key)))
      (ensure-directories-exist pathname)
      (with-open-file (out pathname
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (let ((interface:*schema-package* (find-package 'orient.lang))
              (json:*lisp-identifier-name-to-json* #'string-downcase))
          (json:with-array (out)
            (dolist (value (aif payload-serializer
                                (funcall it values)
                                values))
              (cl-json:encode-array-member value out)))
          (values-list values)))))
  (:method ((key t) (cache disk-backed-mem-cache) (values list) &key payload-serializer)
    (cache-store key (cache-disk-cache cache) values :payload-serializer payload-serializer)
    (cache-store key (cache-mem-cache cache) values :payload-serializer payload-serializer)))

(defgeneric cache-path-for-key (cache key)
  (:method ((cache disk-cache) (key t))
    (make-pathname :defaults (cache-root cache)
                   :name (princ-to-string key))))

(defun key-for (args)
  (let ((args-string (write-to-string args)))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence
      :sha256
      (ironclad:ascii-string-to-byte-array args-string)))))

(defun call-with-cache (cache function key-args all-args &key values-serializer values-deserializer)
  (let* ((key (key-for key-args)))
    (multiple-value-bind (cached-values presentp)
        (cache-lookup key cache :payload-deserializer values-deserializer)
      (if presentp
          (values-list cached-values)
          (let ((values (multiple-value-list (apply function all-args))))
            (cache-store key cache values :payload-serializer values-serializer)
            (values-list values))))))
