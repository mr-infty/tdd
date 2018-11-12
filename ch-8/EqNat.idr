module EqNat

public export
data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

public export
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just (sameS _ _ eq) 

checkEqNatWithCaseSplit : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNatWithCaseSplit Z Z = Just (Same Z)
checkEqNatWithCaseSplit Z (S k) = Nothing
checkEqNatWithCaseSplit (S k) Z = Nothing
checkEqNatWithCaseSplit (S k) (S j) = case checkEqNatWithCaseSplit k j of
                                           Nothing => Nothing
                                           Just (Same j) => Just (Same (S j))

checkEqNatWithDo : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNatWithDo Z Z = Just (Same Z)
checkEqNatWithDo Z (S k) = Nothing
checkEqNatWithDo (S k) Z = Nothing
checkEqNatWithDo (S k) (S j) = do eq <- checkEqNatWithDo k j
                                  Just (sameS _ _ eq)
