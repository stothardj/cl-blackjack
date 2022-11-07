(defpackage blackjack
  (:use :cl :iterate)
  (:import-from #:alexandria
                #:shuffle)
  (:export "MAKE-CARD" "CARD-VALUE" "CARD-SUITE" "MAKE-DECK" "HAND-VALUE" "DETERMINE-WINNER" "PLAY-HAND" "PLAY-GAME"))
