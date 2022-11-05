(defsystem "blackjack"
  :version "0.1.0"
  :author "Jake Stothard"
  :license "MIT"
  :depends-on ("alexandria" "iterate")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "card" :depends-on ("package"))
                 (:file "main" :depends-on ("package" "card")))))
  :description ""
  :in-order-to ((test-op (test-op "blackjack/tests"))))

(defsystem "blackjack/executable"
  :build-operation program-op
  :build-pathname blackjack
  :entry-point "blackjack::play-game"
  :depends-on ("blackjack"))

(defsystem "blackjack/tests"
  :author "Jake Stothard"
  :license "MIT"
  :depends-on ("blackjack"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "card"))))
  :description "Test system for blackjack"
  :perform (test-op (op c) (symbol-call :rove :run c)))
