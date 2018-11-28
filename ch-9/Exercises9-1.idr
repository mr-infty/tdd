data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

Uninhabited (Last [] x) where
  uninhabited LastOne impossible
  uninhabited (LastCons _) impossible

notLast : (contra : (x = value) -> Void) -> Last [x] value -> Void
notLast contra LastOne = contra Refl
notLast _ (LastCons LastOne) impossible
notLast _ (LastCons (LastCons _)) impossible

firstDoesntMatter : Last (y :: xs) value -> Last (x :: (y :: xs)) value
firstDoesntMatter LastOne = LastCons LastOne
firstDoesntMatter (LastCons prf) = LastCons (LastCons prf)

firstDoesntMatter2 : Last (x :: (y :: xs)) value -> Last (y :: xs) value
firstDoesntMatter2 (LastCons prf) = prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No uninhabited
isLast (x :: []) value = case decEq x value of
                              Yes Refl => Yes LastOne
                              No contra => No (notLast contra)
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                                     Yes prf => Yes (firstDoesntMatter prf)
                                     No contra => No (contra . firstDoesntMatter2)
