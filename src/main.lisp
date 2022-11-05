(in-package :blackjack)

(defun make-deck ()
  (iter outer
    (with numbers = (iter (for i from 2 to 10) (collect i)))
    (for i in (append numbers '(:jack :queen :king :ace)))
    (iter
      (for s in '(:hearts :spades :diamonds :clubs))
      (in outer (collect (make-card i s))))))

(defun face-value (card-number)
  (cond ((eql :ace card-number) 11)
        ((find card-number '(:jack :queen :king)) 10)
        (t card-number)))

(defun hand-value (cards)
  (iter
    (for card in cards)
    (for n = (card-value card))
    (summing (face-value n) into value)
    (count (eql :ace n) into aces)
    (finally (return (iter
                       (while (and (> value 21) (> aces 0)))
                       (decf value 10)
                       (decf aces)
                       (finally (return value)))))))
    
(defun parse-action (action)
  (let ((s (string-downcase action)))
    (cond ((or (equal s "hit") (equal s "h")) :hit)
          ((or (equal s "stay") (equal s "s")) :stay))))

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-action ()
  (iter
    (with action = nil)
    (until action)
    (setq action (parse-action (prompt-read "[h]it or [s]tay?")))
    (finally (return action))))

(defun display-hand (hand &optional hide-first)
  (if hide-first
      (format t "#<face-down>, ~{~a~^, ~}~%" (cdr hand))
      (format t "~{~a~^, ~}~%" hand)))

(defun play-hand-player (deck player dealer)
  (iter
    (format t "Dealer: ")
    (display-hand dealer t)
    (format t "Player: ")
    (display-hand player)
    (while (< (hand-value player) 21))
    (when (eql (read-action) :stay)
      (finish))
    (push (pop deck) player)
    (finally (let ((player-value (hand-value player)))
               (if (> player-value 21)
                   (format t "Player busts.~%")
                   (format t "~%"))
               (return player-value)))))

(defun play-hand-dealer (deck player dealer)
  (iter
    (format t "Dealer: ")
    (display-hand dealer)
    (format t "Player: ")
    (display-hand player)
    (while (< (hand-value dealer) 17))
    (format t "Dealer hits.~%~%")
    (push (pop deck) dealer)
    (finally (if (> (hand-value dealer) 21)
                 (format t "Dealer busts.~%")
                 (format t "Dealer stays.~%"))
             (return (hand-value dealer)))))

(defun display-winner (winner)
  (format t "~a~%"
          (case winner
            (:player "Player wins!")
            (:dealer "Dealer wins.")
            (:tie "It's a tie."))))

;; TODO: Changes to "player" are lost when play-hand-player returns.
(defun play-hand (deck)
  (let* ((player (list (pop deck) (pop deck)))
         (dealer (list (pop deck) (pop deck)))
         (player-value (play-hand-player deck player dealer)))
    (if (> player-value 21)
        (list :winner :dealer)
        (let ((dealer-value (play-hand-dealer deck player dealer)))
          (cond ((> dealer-value 21) (list :winner :player))
                ((and (= dealer-value 21) (= player-value 21)
                      (= 2 (length player)) (> 2 (length dealer)))
                 (list :winner :player))
                ((and (= dealer-value 21) (= player-value 21)
                      (= 2 (length dealer)) (> 2 (length player)))
                 (list :winner :dealer))
                ((> dealer-value player-value) (list :winner :dealer))
                ((< dealer-value player-value) (list :winner :player))
                (t (list :winner :tie)))))))

(defun play-game ()
  (let ((results (play-hand (alexandria:shuffle (make-deck)))))
    (display-winner (getf results :winner))))
