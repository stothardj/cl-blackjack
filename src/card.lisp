(in-package :blackjack)

(defclass card ()
  ((value
    :initarg :value
    :accessor card-value)
   (suite
    :initarg :suite
    :accessor card-suite)))

(defun make-card (value suite)
  (make-instance 'card :value value :suite suite))

(defparameter *suites* (list :spades "♠"
                             :hearts "♥"
                             :diamonds "♦"
                             :clubs "♣"))

(defmethod print-object ((obj card) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((value card-value)
                     (suite card-suite))
        obj
      (format stream "~a~a" value (getf *suites* suite)))))
