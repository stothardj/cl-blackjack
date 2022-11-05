(defpackage blackjack/tests/card
  (:use :cl
        :blackjack
        :rove))
(in-package :blackjack/tests/card)

;; NOTE: To run this test file, execute `(asdf:test-system :blackjack)' in your Lisp.

(deftest test-make-card
  (testing "should store the value"
    (ok (= (blackjack:card-value (blackjack:make-card 3 :hearts)) 3)))
  (testing "should store the suite"
    (ok (eql (blackjack:card-suite (blackjack:make-card 3 :hearts)) :hearts))))
