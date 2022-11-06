(defpackage blackjack/tests/main
  (:use :cl
        :blackjack
        :rove))
(in-package :blackjack/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :blackjack)' in your Lisp.

(defun fake-query-io-stream (pattern)
  (make-two-way-stream (make-string-input-stream (format nil pattern))
                       (make-broadcast-stream)))

(deftest test-make-deck
  (testing "has 52 cards"
           (ok (= (length (blackjack:make-deck)) 52)))
  (testing "has 4 instances of a number"
           (ok (= (count-if (lambda (c) (eql (card-value c) 3)) (make-deck)) 4)))
  (testing "has 13 cards in a suite"
           (ok (= (count-if (lambda (c) (eql (card-suite c) :hearts)) (make-deck)) 13))))

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

;; This relys on the player getting both their cards first.
;; TODO: Make this kind of test less awkward to write.
;; TODO: Suppress stdout in test.
(deftest test-play-hand
  (testing "both stay player wins"
    (ok (eql :player (getf (let ((*query-io* (fake-query-io-stream "s~%")))
                             (blackjack:play-hand (list (blackjack:make-card 10 :hearts)
                                                        (blackjack:make-card :jack :clubs)
                                                        (blackjack:make-card 10 :spades)
                                                        (blackjack:make-card 8 :spades))))
                           :winner)))))
