module Exercises13_1

import Vending

-----------------------------
-- Exercise 13.1.1
-----------------------------

namespace Ex_1
  data DoorState = DoorOpen | DoorClosed

  data DoorCmd : Type -> 
                 DoorState ->
                 DoorState ->
                 Type where
    Open : DoorCmd ()     DoorClosed DoorOpen
    Close : DoorCmd ()    DoorOpen   DoorClosed
    RingBell : DoorCmd () state state

    Pure : ty -> DoorCmd ty state state --why not state1 and state2 ?!?
    (>>=) : DoorCmd a state1 state2 -> (a -> DoorCmd b state2 state3) -> DoorCmd b state1 state3

  doorProg : DoorCmd () DoorClosed DoorClosed
  doorProg = do RingBell
                Open
                RingBell
                Close

-----------------------------
-- Exercise 13.1.2
-----------------------------

data GuessCmd : Type -> Nat -> Nat -> Type where
  Try : Integer -> GuessCmd Ordering (S k) k

  Pure : ty -> GuessCmd ty state state
  (>>=) : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3

threeGuesses : GuessCmd () 3 0
threeGuesses = do Try 10
                  Try 20
                  Try 15
                  Pure ()

-- Doesn't type check
--noGuesses : GuessCmd () 0 0
--noGuesses = do Try 10
--               Pure ()

-----------------------------
-- Exercise 13.1.3
-----------------------------

namespace Ex_3
  data Matter = Solid | Liquid | Gas

  data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid
    Sublimate : MatterCmd () Solid Gas
    Deposit : MatterCmd () Gas Solid

    Pure : ty -> MatterCmd ty state state
    (>>=) : MatterCmd a state1 state2 -> (a -> MatterCmd b state2 state3) -> MatterCmd b state1 state3

  iceSteam : MatterCmd () Solid Gas
  iceSteam = do Melt
                Boil

  steamIce : MatterCmd () Gas Solid
  steamIce = do Condense
                Freeze


  -- Doesn't type check
  --overMelt : MatterCmd () Solid Gas
  --overMelt = do Melt
  --              Melt
