module Exercises12_1

import Control.Monad.State
import TreeLabel

-----------------------------
-- Exercise 12.1.1
-----------------------------

update : (stateType -> stateType) -> State stateType ()
update f = do curState <- get
              put (f curState)

increase : Nat -> State Nat ()
increase inc = update (+inc)

-----------------------------
-- Exercise 12.1.2
-----------------------------

countEmpty : Tree a -> State Nat ()
countEmpty Empty = put (S Z)
countEmpty (Node left val right) = do countEmpty left
                                      leftCount <- get
                                      countEmpty right
                                      rightCount <- get
                                      -- This doesn't work, for some reason (TODO: Figure out why)
                                      -- put (leftCount + rightCount)
                                      -- But this works
                                      put (plus leftCount rightCount)

-----------------------------
-- Exercise 12.1.3
-----------------------------

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = put ((S Z), Z)
countEmptyNode (Node left val right) = do countEmptyNode left
                                          (leftEmptyCount, leftNodeCount) <- get
                                          countEmptyNode right
                                          (rightEmptyCount, rightNodeCount) <- get
                                          put (plus leftEmptyCount rightEmptyCount, S (plus leftNodeCount rightNodeCount))
