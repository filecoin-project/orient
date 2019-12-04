(defpackage orient.web.api
    (:use :common-lisp :orient :filecoin :orient.web.html :orient.base.util :it.bese.FiveAm :orient.web :orient.interface)
    (:shadowing-import-from :fset :reduce  :union)
    (:export :register-primary-solver-endpoint)
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

(defvar *solution-endpoint* "/solve")

(defun register-primary-solver-endpoint (callback &key merge)
  (hunchentoot:define-easy-handler (solve-primary-system :uri *solution-endpoint*) ()
    (when (boundp '*acceptor*)
      (setf (hunchentoot:header-out :Access-Control-Allow-Origin hunchentoot:*reply*) "*")
      (setf (hunchentoot:content-type*) "text/html"))
    (let* ((raw-data (hunchentoot:raw-post-data :force-text t)))
      (funcall callback raw-data))))
