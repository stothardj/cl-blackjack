(defpackage blackjack/tests/main
  (:use :cl
        :blackjack
        :rove))
(in-package :blackjack/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :blackjack)' in your Lisp.

(deftest test-make-deck
  (testing "should have 52 cards"
    (ok (= (length (blackjack:make-deck)) 52))))

(deftest test-hand-value
  (testing "uses single card value"
    (ok (= (blackjack:hand-value (list (blackjack:make-card 3 :hearts))) 3)))
  (testing "adds card values"
    (ok (= (blackjack:hand-value (list (blackjack:make-card 3 :hearts)
                                          (blackjack:make-card 5 :spades))) 8)))
  (testing "ace defaults to high"
    (ok (= (blackjack:hand-value (list (blackjack:make-card 3 :hearts)
                                          (blackjack:make-card :ace :spades))) 14)))
  (testing "ace drops to low when needed"
    (ok (= (blackjack:hand-value (list (blackjack:make-card 3 :hearts)
                                          (blackjack:make-card 9 :clubs)
                                          (blackjack:make-card :ace :spades))) 13))))
