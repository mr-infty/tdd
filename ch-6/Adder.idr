AdderType : (numArgs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = numType -> AdderType k numType

adder : Num numType => (numArgs : Nat) -> (acc : numType) -> AdderType numArgs numType
adder Z acc = acc
adder (S k) acc = \x => adder k (acc+x)
