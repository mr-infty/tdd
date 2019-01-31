# Chapter 8

1. (p. 233): Listing 8.11 is incorrect. It should read

    exactLengthWith : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
    exactLengthWith {m} len input = case decEq m len of
                                          Yes prf => Just rewrite sym prf in input
                                          No contra => Nothing

# Chapter 9

1. (p. 256): There is no function called `guess`. Does this refer to `processGuess` (or `game`) maybe?

# Chapter 11

1. The definition of `getPrefix` with an explicit `Force` no longer works in Idris 1.3.1; indeed, `xs` is now reported to be of type `InfList typ` instead of `Inf (InfList ty)`.

# Chapter 12

1. The type of `Nat` in listing 12.33 is possibly erroneous; in any case, this listing doesn't type check as there is a mismatch between `Nat` (type of `newDiff`) and `Int` (the type that's in the record declaration).

2. Listing 12.32 is a bit idiosyncratic: `score` is actually of type `GameState`, so it should be renamed to `state` and the variable `state` should be replaced by an underscore.

# Chapter 13

1. In listing 13.1, the declaration of `DoorCmd` is incorrect: it should be `DoorCmd : Type -> Type`.

2. In listing 13.13, it should be `StkInput` instead of `RPNInput`.
