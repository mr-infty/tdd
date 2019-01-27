module TreeLabelType

import TreeLabel

%default total

public export
data State : (stateType : Type) -> Type -> Type where
  Get : State stateType stateType
  Put : stateType -> State stateType ()

  Pure : ty -> State stateType ty
  Bind : State stateType a -> (a -> State stateType b) -> State stateType b

public export
runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put x) st = ((), x)
runState (Pure x) st = (x, st)
runState (Bind x f) st = let (x', st') = runState x st in
                             runState (f x') st'

public export
get : State stateType stateType
get = Get

public export
put : stateType -> State stateType ()
put = Put

public export
pure : ty -> State stateType ty
pure = Pure

public export
Functor (State stateType) where
  map f x_a = Bind x_a (\x => Pure (f x))

public export
Applicative (State stateType) where
  pure = Pure
  (<*>) st_f st_a = Bind st_a (\x_a => map (\x_f => x_f x_a) st_f)

public export
Monad (State stateType) where
  (>>=) = Bind
--  join x = Bind x (\y => y)

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = Pure Empty
treeLabelWith (Node left val right) = do leftLabelled <- treeLabelWith left
                                         (this :: rest) <- get
                                         put rest
                                         rightLabelled <- treeLabelWith right
                                         pure (Node leftLabelled (this, val) rightLabelled)

treeLabel : Tree a -> Stream labelType -> Tree (labelType, a)
treeLabel tree labels = fst (runState (treeLabelWith tree) labels)
