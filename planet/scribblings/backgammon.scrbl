#lang scribble/doc
@(require scribble/manual
          planet/util
          (planet cce/scheme:7:1/planet)
          (planet cce/scheme:7:1/scribble)
          (for-label (for-label (this-package-in main)))
          (for-label 2htdp/universe)
          (for-label racket))



@title{@bold{Rackgammon}: Backgammon for Racket}

@author+email["David Van Horn" "dvanhorn@ccs.neu.edu"]

This package provides a distributed Backgammon game.

@link[(format 
       "http://planet.racket-lang.org/trac/newticket?component=~a%2F~a&planetversion=(~a+~a)"
       (this-package-version-owner)
       (this-package-version-name)
       (this-package-version-maj)
       (this-package-version-min))]{Report a bug}.

@section{How to play}

@defmodule[(planet dvanhorn/backgammon)]

These instructions assume you know the rules of Backgammon.

You can either play in singles or couples mode.  In either case, 
you need to run a "Backgammon Server".

@defproc[(serve-singles) thread?]{
Serve single (self-playing) games.}

@defproc[(serve-couples) thread?]{
Serve coupled games, pairing together players on a first-come, first-served basis.}

@defproc[(play [name string?] [#:IP ip LOCALHOST string?]) any/c]{
Play a game under the given player name using the Backagammon
server located at the given IP address.}

To play a game between two players, call @racket[(serve-couples)] to start the 
backgammon server.  Then call @racket[(play "Player 1")] and @racket[(play "Player 2")].
If the backgammon server is on a different machine than the players, use
the @racket[#:IP] keyword argument to specify the IP address of the server.
The players do not need to be on the same computer, but if they are, the calls to 
@racket[play] should be in different tabs of DrRacket, or use @racket[launch-many-worlds].

To play a game using one player who controls the entire game, call @racket[(serve-singles)],
then call @racket[play] as before.

When it is not your turn, you will see "Passive" displayed across the board.  You
are not able to modify the board, but you will see all changes as the opponent makes
them.  When the opponent defers to you, it will be your turn.

When it is your turn, you may roll the dice and move checkers around the board.
To roll the dice, press @racket["R"].
To select a checker, click on it.  Once selected, it is attached to your cursor. 
Click again on an area of the board to place it.

When you're done with your turn, press @racket["D"].  This defers to the opponent
and makes your board passive.

In a singles game, the opponent always and immediately defers back to you.

Just as on a real backgammon board, there is no checking of the validity of moves.
It is up to the players, all of whom can see each move made, to enforce the rules
of play.
