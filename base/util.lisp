(defpackage :orient.base.util
  (:use :common-lisp)
  (:nicknames :util)
  (:export :comma-list))

(in-package :orient.base.util)

(defun comma-list (things &key (separator ", ") before after)
  (with-output-to-string (out)
    (when before (princ separator out))
    (loop for (thing . rest) on things
          do (princ thing out)
          when rest do (princ separator out))
    (when after (princ separator out))))
