(in-package :orient.web)
(in-suite web-suite)

(hunchentoot:define-easy-handler (zigzag-theory :uri "/zigzag-theory") ()
  (with-page ("ZigZag Theory")
    `(:div "Intentionally Blank")))
