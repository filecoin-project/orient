(in-package :orient.solver)
(defconstant +huge+ 100000000000000.d0)
(defconstant +epsilon+ 0.0001)

(def-suite orient-solver-suite)
(in-suite orient-solver-suite)

(defun squared-error (expected actual)
  (let ((err (- expected actual)))
    (* err err)))

(defun ~= (a b) (< (abs (- a b)) +epsilon+))

(test simple-solve
  (let* ((target 2)
         (e (lambda (v)
              (let ((x (aref v 0)))
                (squared-error target x))))
         (found (grnm-optimize e #(123.0d0))))
    (is (~= target (aref found 0)))))

;; Not optimized, just finding interface.
(defun make-objective-function (error-function)
  (lambda (vector)
    (apply error-function (coerce vector 'list))))

(defun squared-error-from (expected)
  (lambda (actual) (squared-error expected actual)))

(test better-solve
  (let* ((target 2)
         (e (make-objective-function (squared-error-from target)))
         (found (grnm-optimize e #(666.6d0))))
    (is (~= target (aref found 0)))))

(defconstraint-system rectangle
    ((area (* height width))
     (perimeter (* 2 (+ height width)))
     (perimeter-error (- perimeter target-perimeter)) ;; Set to target
     (area-error (/ 1 area)) ;; Maximize
     (o (+ (* perimeter-error perimeter-error)
           (* area-error area-error)))
     (objective (* o o))))

(defun make-rectangle-objective (target-perimeter)
  (let ((system (find-system 'rectangle)))
    (lambda (vector)
      (let* ((height (aref vector 0))
             (width (aref vector 1))
             (solution (solve-for system '(objective) (tuple (target-perimeter target-perimeter)
                                                             (height height)
                                                             (width width)))))
        (if solution (tref 'objective solution) +huge+)))))

(test system-solver
  (let* ((target-perimeter 20)
         (objective (make-rectangle-objective target-perimeter))
         (solution (grnm-optimize objective #(1.0d0 1.0d0) :max-function-calls 500))
         (solution-height (aref solution 0))
         (solution-width (aref solution 1)))
    (is (~= 5 solution-height))
    (is (~= 5 solution-width))))

(defun make-input-tuple (input-tuple vars-to-optimize values-vector)
  (let ((input-tuple (or input-tuple (tuple))))
    (loop for attr in vars-to-optimize
       for i from 0
       do (setf (tref attr input-tuple) (aref values-vector i)))
    input-tuple))

(defun make-solver-objective-function (system input-tuple objective-var vars-to-optimize)
  (lambda (values-vector)
    (let* ((merged-input (make-input-tuple input-tuple vars-to-optimize values-vector))
           ;; FIXME: could be relation.
           (solution-tuple (solve-for system (list objective-var) merged-input)))
      (if solution-tuple
          (tref objective-var solution-tuple)
          +huge+))))

(defun solver-optimize (system input-tuple objective-var vars-to-optimize &key max-function-calls guess)
  (let* ((objective-function (make-solver-objective-function system input-tuple objective-var vars-to-optimize))
         ;; TODO: Allow any provided GUESS to be NIL and supply a default if so.
         (guess (or (and guess (map 'vector (lambda (x) (float x 1.0d0)) guess))
                    (make-array (list (length vars-to-optimize)) :initial-element 1.0d0)))
         (solution-vector (grnm-optimize objective-function guess :max-function-calls max-function-calls))
         (final-input-tuple (make-input-tuple input-tuple vars-to-optimize solution-vector)))
    ;; TODO: cache and get this somehow. It must have been called with these values by the objective-function.
    (solve-for system () final-input-tuple)))

(test wrapped-solver
  (let* ((input (tuple (target-perimeter 20)))
         (system (find-system 'rectangle))
         (solution (solver-optimize system input 'objective '(height width))))
    (is (~= 5 (tref 'height solution)))
    (is (~= 5 (tref 'width solution)))))
