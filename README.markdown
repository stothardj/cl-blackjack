# Cl-Blackjack

## Usage

This allows playing a very basic blackjack game from the terminal.
A typical game will look like:

```
Dealer: #<face-down>, #<5♥>
Player: #<QUEEN♣>, #<5♠>
[h]it or [s]tay? s

Dealer: #<7♥>, #<5♥>
Player: #<QUEEN♣>, #<5♠>
Dealer hits.

Dealer: #<6♦>, #<7♥>, #<5♥>
Player: #<QUEEN♣>, #<5♠>
Dealer stays.
Dealer wins.
```

It was made to learn Common Lisp and not expected to be that amusing.

## Installation

Install a Common Lisp compiler (ex. SBCL) and quicklisp.
Clone this to ~/quicklisp/local-projects/ and load it with (ql:quickload "blackjack")

### Running inside Lisp

Inside a lisp interpreter run:

```
(ql:quickload "blackjack")
(blackjack:play-game)
```

### Compiling a binary

Inside a lisp interpreter run:

```
(ql:quickload "blackjack")
(asdf:make "blackjack/executable")
```

This will create an executable called "blackjack".

## Author

* Jake Stothard (stothardj@gmail.com)

## Copyright

Copyright (c) 2022 Jake Stothard (stothardj@gmail.com)
