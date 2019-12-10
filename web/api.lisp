(defpackage orient.web.api
    (:use :common-lisp :orient :filecoin :orient.web.html :orient.base.util :it.bese.FiveAm :orient.web :orient.interface)
    (:shadowing-import-from :fset :reduce  :union)
    (:export :register-primary-solver-endpoints)
    (:nicknames :api))

(in-package :orient.web.api)
(def-suite api-suite)
(in-suite api-suite)

(defvar *system-endpoints* '())

(defvar *system-endpoint-base-uri* "/system/")

(defun system-endpoint (name)
  (concatenate 'string *system-endpoint-base-uri* name))

(defun random-endpoint ()
  (let ((rand (hunchentoot::create-random-string 10)))
    (system-endpoint rand)))


;; TODO: Refactor this.
(defun register-primary-solver-endpoints (solve-callback solve-many-callback dump-vars-callback)
  (hunchentoot:define-easy-handler (solve-primary-system :uri "/solve") ()
    (when (boundp '*acceptor*)
      (setf (hunchentoot:header-out :Access-Control-Allow-Origin hunchentoot:*reply*) "*")
      (setf (hunchentoot:content-type*) "text/html"))
    (let* ((raw-data (hunchentoot:raw-post-data :force-text t)))
      (funcall solve-callback raw-data)))

  (hunchentoot:define-easy-handler (solve-many-primary-system :uri "/solve-many") ()
    (when (boundp '*acceptor*)
      (setf (hunchentoot:header-out :Access-Control-Allow-Origin hunchentoot:*reply*) "*")
      (setf (hunchentoot:content-type*) "text/html"))
    (let* ((raw-data (hunchentoot:raw-post-data :force-text t)))
      (funcall solve-many-callback raw-data)))

  (hunchentoot:define-easy-handler (dump-vars-primary-system :uri "/dump-vars") ()
    (when (boundp '*acceptor*)
      (setf (hunchentoot:header-out :Access-Control-Allow-Origin hunchentoot:*reply*) "*")
      (setf (hunchentoot:content-type*) "text/html")
      (let* ((raw-data (hunchentoot:raw-post-data :force-text t)))
        (funcall dump-vars-callback raw-data)))))
