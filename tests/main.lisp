(defpackage blackjack/tests/main
  (:use :cl
   :blackjack
        :rove))
(in-package :blackjack/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :blackjack)' in your Lisp.

(defun fake-query-io-stream (pattern)
  (make-two-way-stream (make-string-input-stream (format nil pattern))
                       (make-broadcast-stream)))

(defmacro with-fake-query-io (input &rest body)
  `(let ((*query-io* (fake-query-io-stream ,input))
         (*standard-output* (make-broadcast-stream)))
     ,@body))

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

(deftest test-determine-winner
  (testing "player busts"
    (ok (eql :dealer (blackjack:determine-winner
                      (list (blackjack:make-card 10 :hearts)
                            (blackjack:make-card :king :hearts)
                            (blackjack:make-card 5 :hearts))
                      (list (blackjack:make-card 5 :spades)
                            (blackjack:make-card 8 :spades))))))
  (testing "dealer busts"
    (ok (eql :player (blackjack:determine-winner
                      (list (blackjack:make-card 5 :spades)
                            (blackjack:make-card 8 :spades))
                      (list (blackjack:make-card 10 :hearts)
                            (blackjack:make-card :king :hearts)
                            (blackjack:make-card 5 :hearts))))))
  (testing "player has blackjack"
    (ok (eql :player (blackjack:determine-winner
                      (list (blackjack:make-card :ace :hearts)
                            (blackjack:make-card :jack :clubs))
                      (list (blackjack:make-card 7 :hearts)
                            (blackjack:make-card 4 :spades)
                            (blackjack:make-card 10 :hearts))))))
  (testing "dealer has blackjack"
    (ok (eql :dealer (blackjack:determine-winner
                      (list (blackjack:make-card 7 :hearts)
                            (blackjack:make-card 4 :spades)
                            (blackjack:make-card 10 :hearts))
                      (list (blackjack:make-card :ace :hearts)
                            (blackjack:make-card :jack :clubs))))))
  (testing "dealer has greater amount"
    (ok (eql :dealer (blackjack:determine-winner
                      (list (blackjack:make-card 7 :spades)
                            (blackjack:make-card 5 :clubs))
                      (list (blackjack:make-card 10 :spades)
                            (blackjack:make-card 5 :diamonds))))))
  (testing "player has greater amount"
    (ok (eql :player (blackjack:determine-winner
                      (list (blackjack:make-card 10 :spades)
                            (blackjack:make-card 5 :diamonds))
                      (list (blackjack:make-card 7 :spades)
                            (blackjack:make-card 5 :clubs))))))
  (testing "equal amounts"
    (ok (eql :tie (blackjack:determine-winner
                   (list (blackjack:make-card 10 :clubs)
                         (blackjack:make-card 7 :clubs))
                   (list (blackjack:make-card 10 :diamonds)
                         (blackjack:make-card 7 :diamonds)))))))

;; This relies on the player getting both their cards first.
(deftest test-play-hand
  (testing "both stay player wins"
    (ok (eql :player (getf (with-fake-query-io "s~%"
                         (blackjack:play-hand (list (blackjack:make-card 10 :hearts)
                                                    (blackjack:make-card :jack :clubs)
                                                    (blackjack:make-card 10 :spades)
                                                    (blackjack:make-card 8 :spades))))
                           :winner))))
  (testing "player hits to get win"
    (ok (eql :player (getf (with-fake-query-io "h~%s~%"
                             (blackjack:play-hand (list (blackjack:make-card 5 :hearts)
                                                        (blackjack:make-card 4 :hearts)
                                                        (blackjack:make-card 10 :spades)
                                                        (blackjack:make-card 8 :spades)
                                                        (blackjack:make-card :king :hearts))))
                           :winner)))))
