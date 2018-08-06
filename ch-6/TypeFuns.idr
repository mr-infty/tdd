StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x

valToStringUsingCases : (isInt : Bool) -> (case isInt of
                                                False => String
                                                True => Int) -> String
valToStringUsingCases False x = trim x
valToStringUsingCases True x = cast x