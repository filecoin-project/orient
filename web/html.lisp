;;; Imported, with light modifications from porcuquine historical code base.

(defpackage :orient.web.html
  (:use :common-lisp :orient.base.util)
  (:export
   #:*html-output-stream*
   #:*html-print-escape*
   #:*html-eval-function*
   #:html
   #:html-print
   #:html-print-to-string
   #:html-print-list
   #:def-html-method
   #:+null-attribute+
   #:css-print #:css-declarations
   )
  (:nicknames :html))

(defpackage :orient.web.html.pi
  ;; Processing Instructions
  (:nicknames :pi))

(in-package :orient.web.html)

;;;; FIXME: Need some way to control security wrt EVAL in expressions to be printed.

;;;; HTML-PRINT, actually this wants to be a general *ML printer, with compile-time macros.
;;;; Inspired by Franz's HTMLGEN, but not quite the same.

(defparameter *html-output-stream* *standard-output*
  "Stream to which html-print will print output.")

(defparameter *html-print-escape* nil
  "If T then escape strings in output so they cannot be interpreted as tags.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *emitting* nil))

(defmacro html-format+ (&rest format-args)
  `(format *html-output-stream* ,@format-args))

(defconstant +null-attribute+ '+null-attribute+)

(defgeneric expand-html (thing))

;;; Use FORMAT in all its glory.  --- No.
(defun html-attribute-print+ (plist)
  (loop for (attribute value . tail) on plist by #'cddr
        ;; ---Need to escape quotes.
        do (cond ((eq value +null-attribute+)
                  (html-format+ (string attribute)))
                 (t (html-format+ "~a=\""  attribute)
                    (let ((*html-print-escape* t))
;                      (html-print value)
                      (html-print-escape+ value '((#\" . "&quot;"))
                                          ))
                    (write-string "\"" *html-output-stream*)))
        (when tail
          (html-format+ " "))))

#+(or)
(defun html-attribute-print+ (plist)
  (loop for (attribute value . tail) on plist by #'cddr
        ;; ---Need to escape quotes.
        do (progn 
             (html-format+ "~a=\"" attribute)
             (let ((*html-print-escape* t))
               (html-print value))
             (write-string "\"" *html-output-stream*))
        (when tail
          (html-format+ " "))))

(defun html-print-escape+ (string escape-alist)
  (unless string
    (return-from html-print-escape+))
  (unless (stringp string)
    (setq string (princ-to-string string)))
  (loop for char across string
        as match-pair = (assoc char escape-alist)
        if match-pair do (write-string (cdr match-pair) *html-output-stream*)
        else do (write-char char *html-output-stream*)))

(defmethod html-print-list ((form list))
  (mapc #'html-print form))

(defparameter *html-escape-alist* '((#\< . "&lt;")
                                    (#\& . "&amp;")))

(defparameter *html-eval-function* 'eval)

(defmacro def-html-method ((var type) &body body)
  `(progn
     (macrolet ((bind-print-escape ((value) &body forms)
                  `(let ((*html-print-escape* ,value))
                     ,@forms))
                (html-print-escape (string)
                  `(html-print-escape+ ,string *html-escape-alist*))
                (html-attribute-print (plist)
                  `(html-attribute-print+ ,plist))
                (html-print-block (&body forms)
                  `(progn ,@forms))
                (html-recurse (elements)
                  `(mapc #'html-print ,elements))
                (html-format (format-string &rest format-args)
                  `(format *html-output-stream* ,format-string ,@format-args)) ;)
                (html-emit (thing) 
                  `(cond
                    (*emitting* 
                     (format *html-output-stream* "~w" ,thing))
                    ;; Basically, only evaluate to a depth of 1.
                    (t (let ((*emitting* t))
                         (html-print ,thing)))))
                (html-eval (form)
                  `(identity (funcall *html-eval-function* ,form))))
       (defmethod html-print ((,var ,type))
         (prog1 nil ,@body)))
     (macrolet ((bind-print-escape ((value) &body forms)
                  `(let ((*html-print-escape* ,value))
                     `(let ((*html-print-escape* ,,value))
                        ,,@forms)))
                (html-print-escape (thing)
                  `(let ((evaled ,thing))
                     (typecase evaled
                       (string
                        `(format *html-output-stream*
                                 ,(with-output-to-string (*html-output-stream*)
                                    (html-print-escape+ evaled *html-escape-alist*))))
                       (t `(html-print-escape+ ,,thing *html-escape-alist*)))))
                (html-attribute-print (plist)
                  `(identity
                    `(princ ,(with-output-to-string (*html-output-stream*)
                                      (html-attribute-print+ ,plist))
                                   *html-output-stream*)))
                (html-print-block (&body forms)
                  `(list 'progn ,@forms))
                (html-recurse (elements)
                  `(list* 'progn 
                          (mapcar #'expand-html ,elements)))                
                (html-format (format-string &rest format-args)                  
                  `(identity
                    `(princ ,(format nil ,format-string ,@format-args)
                            *html-output-stream*))) ;)
                (html-emit (thing)
                  (cond
                   (*emitting*
                    `(let ((*emitting* t))
                       (identity (html-print ,thing))))
                   ;; If emitting at macroexpansion time, then don't evaluate again at runtime.
                    (t `(identity
                         `(let ((*emitting* t)) 
                            (html-print ,,thing)))))
                  #+(or)
                  (let ((*emitting* t))
                    `(identity `(html-print ,,thing))
;                  ;; This change seems to fix an inconsistency with the functional HTML-PRINT
                  ;; Example: (html-print '(list 'list :x))
                  ;;          (html (list 'list :x)) should => (X), not <X></X>.
                  ;; Argh, leaving it 'broken/inconsistent for now since we depend on this behavior.
                  ;; Epilogue? I believe the above changes fix this.  Leaving code and comments in
                  ;; just in case it's not quite there yet.
;                  `(identity (html-print ,thing))

                    )
                  )
                (html-eval (form)
                  `(identity ,form)))
       (defmethod expand-html ((,var ,type))
         ,@body))))

(def-html-method (form null) ())

(def-html-method (form t)
  (if *html-print-escape*
      (html-print-escape (format nil "~a" form))
    (html-format "~a" form)))

(def-html-method (form symbol)
  (typecase form
    (keyword
     (case form
       (:newline (html-format "~%"))
       (t 
        (let* ((name (symbol-name form)))
          (if (char= (char name 0) #\&)
              (html-format "~(~a;~)" name)
            (html-format "<~a>" form))))))
    (t
     ;; We don't *want* variables to be repeatedly evaluated.  As an example, this causes
     ;; an evil infinite loop when presented with the symbol T!
     ;; NOTE: I think this is fixed.
;     (break)
;     (if *emitting*
 ;        (html-format "~a" form);(html-eval form))
       (html-emit (html-eval form))
       )))

(def-html-method (form list)
  (let ((tag (car form))
        (body (cdr form)))
    (typecase tag
      ((eql quote)
       (html-format "~a" (cadr form))
       )
      (keyword
       (case tag
         ((:html-escape :escape)
          (bind-print-escape (t)
           (html-recurse (cdr form))))
         ((:html-literal :literal)
          (bind-print-escape (nil)
           (html-recurse (cdr form))))
         ((:%css :css) (html-print-escape (css body)))
         (:%comment
          ;; Note: Doesn't attempt to escape comment close tags ("-->").
          (html-format "<!--")
          (html-recurse (cdr form))
          (html-format "-->"))
         (:|@|
          (html-recurse (cdr form)))
         (t
          (html-print-block
           (html-format "<~a>" tag)
           (html-recurse body)
           (html-format "</~a>" tag)))))      
      (list
       (cond ((and (symbolp (car tag))
                   (eq (symbol-package (car tag))
                       (load-time-value (find-package :net.html.pi))))
              (assert (null (cdr form)))
              (html-format "<?~a " (car tag))
              (html-attribute-print (cdr tag))
              (html-format "?>"))
             (t
              (html-print-block
               (html-format "<~a " (car tag))
               (html-attribute-print (cdr tag))
               (html-format ">")
               (html-recurse (cdr form))
               (html-format "</~a>" (car tag))))))
      (t 
       (html-emit (html-eval form))))))
    
(defmacro html (form)
  ;; Don't return anything, so that nested macro forms will do the right thing.
  ;; That is, they won't print the result twice - once as side effect, and once
  ;; as interpolation.
  `(prog1 nil
     ,(expand-html form)))

(defun expand-html-attribute (form)
  (declare (ignore form))
  (error "EXPAND-HTML-ATTRIBUTE not defined yet."))

(defun expand-html-print-escape (form)
  (declare (ignore form))
  (error "EXPAND-HTML-PRINT-ESCAPE not defined yet."))

(defun html-print-to-string (form)
  (with-output-to-string (*html-output-stream*)
    (html-print form)))


;;;; CSS
#|
TODO:

Selectors

h1#idstuffhere
h1:pseudo-class
h1.class




/*Comments*/

|#






(defun css-attribute-selector (attribute)
  (when attribute
    (let ((inside (typecase attribute
                    (cons (destructuring-bind (operator attr val)
                              attribute
                            (ecase operator
                              (= (format nil "~a=~w" attr val))
                              (~= (format nil "~a~~=~w" attr val))
                              (-= (format nil "~a|=~w" attr val)))))
                    (t attribute))))
      (format nil "[~a]" inside))))

(defun css-simple-selector (element &key class id pseudo attribute)
  (let ((formatted-attribute (css-attribute-selector attribute)))
    (format nil "~@[~a~]~@[.~a~]~@[#~a~]~@[:~a~]~@[~a~]"
            element class id pseudo formatted-attribute)))

(deftype compound-selector () '(cons (member :& :_ :> :+)))

(defun css-compound-selector (selector-list)
  (let* ((separator (case (car selector-list)
                      (:& ", ")
                      (:_ " ")
                      (:> " > ")
                      (:+ " + ")))
         (expanded (loop for elt in (cdr selector-list)
                         collect (typecase elt 
                                   (atom (css-simple-selector elt))
                                   (compound-selector (css-compound-selector elt))
                                   ((cons keyword) (apply #'css-simple-selector nil elt))
                                   (t (apply #'css-simple-selector elt))))))
    (comma-list expanded :separator separator)))

(defun css-selector (spec)
  (typecase spec
    (atom (css-simple-selector spec))
    (compound-selector (css-compound-selector spec))
    ((cons keyword) (apply #'css-simple-selector nil spec))
    (t (apply #'css-simple-selector spec))))

(defun css-declaration (declaration)
  (destructuring-bind (property value) declaration
    (format nil "~a: ~a" property value)))

(defun css-declarations (declarations)
  (let ((formatted-declarations (mapcar #'css-declaration declarations)))
    (comma-list formatted-declarations :separator "; ")))

#+(or)
(defun css-ruleset (selector declarations)
  (let ((formatted-declarations (mapcar #'css-declaration declarations)))
    (format nil "~a { ~a } " 
            (css-selector selector)
            (comma-list formatted-declarations :separator "; "))))

(defun css-ruleset (selector declarations)
  (let ((formatted-declarations (css-declarations declarations)))
    (format nil "~a { ~a } " 
            (css-selector selector)
            formatted-declarations)))

(defun css-expression (expression)
 (check-type expression list)
 (case (car expression)
   (:@ (error "At-rules not yet implemented."))
   (t (css-ruleset (first expression) (rest expression)))))

(defun css (expressions)
  (apply #'concatenate 'string (mapcar #'css-expression expressions)))


#|

(defun css-print (expressions &optional (stream *html-output-stream*))
  (dolist (expression expressions)
    (css-print-expression expression stream)))

(defun css-print-expression (expression stream)
 (check-type expression list)
 (case (car expression)
   (:@ (error "At-rules not yet implemented."))
   (t (css-print-ruleset (first expression) (rest expression) stream))))

(defun css-print-selector (spec stream)
  (typecase spec
    (list (loop for (item . rest) on spec
                do (css-print-selector-item item stream)
                when rest do (princ ", " stream)))
    ;; FIXME: Need to figure out escaping, etc. generally for CSS.
    (t (css-print-selector-item spec stream))))

(defun %css-print-selector-item (item stream &key class id pseudo attribute)
  (let ((formatted-item (typecase item
                          ((sexp :_) (comma-list (cdr item) :separator " "))
                          ((sexp :>) (comma-list (cdr item) :separator " > "))
                          ((sexp :+) (comma-list (cdr item) :separator " + "))
                          (t item))))
    (format stream "~@[~a~]~@[.~a~]~@[#~a~]~@[:~a~]" formatted-item class id pseudo)))

(defun css-print-selector-item (spec stream)
  (typecase spec
    (list 
     (if (keywordp (car spec))
         (apply #'%css-print-selector-item nil stream spec)
       (apply #'%css-print-selector-item  (car spec) stream (cdr spec))))
    (t (%css-print-selector-item spec stream))))


(defun css-print-declaration (declaration stream)
  (destructuring-bind (property value) declaration
    (format stream "~a: ~a" property value)))

(defun css-print-ruleset (selector declarations stream)
  (css-print-selector selector stream)
  (format stream " { ")
  (loop for (declaration . rest) on  declarations
        do (css-print-declaration declaration stream)
        when rest do (princ "; " stream))
  (format stream " } "))

|#
