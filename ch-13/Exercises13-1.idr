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


