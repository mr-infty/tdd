module Exercises14_2

-----------------------------
-- Exercise 14.2.1
-----------------------------

namespace Ex_1
  data Access = LoggedOut | LoggedIn
  data IsLoggedIn : Access -> Type where
    IsLI : IsLoggedIn LoggedIn
  data PwdCheck = Correct | Incorrect

  data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
    Password : String -> ShellCmd PwdCheck state (\res => (case res of
                                                                Correct => LoggedIn
                                                                Incorrect => LoggedOut))
    Logout : ShellCmd () LoggedIn (const LoggedOut)
    -- It would be simpler to write this as `ShellCmd String LoggedIn (const LoggedIn)`,
    -- but conceptually this is better because the type `Access` might be enlarged by further
    -- constructors in the future.
    GetSecret : {auto prf : IsLoggedIn state} -> ShellCmd String state (const state)

    PutStr : String -> ShellCmd () state (const state)
    Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn
    (>>=) : ShellCmd a state1 state2_fn ->
            ((res : a) -> ShellCmd b (state2_fn res) state3_fn) ->
            ShellCmd b state1 state3_fn

  session : ShellCmd () LoggedOut (const LoggedOut)
  session = do Correct <- Password "wurzel"
               | Incorrect => PutStr "Wrong Password"
               msg <- GetSecret
               PutStr ("Secret code: " ++ show msg ++ "\n")
               Logout
  -- Doesn't type check
  --sessionBad : ShellCmd () LoggedOut (const LoggedOut)
  --sessionBad = do Password "wurzel"
  --                msg <- GetSecret
  --                PutStr ("Secret code: " ++ show msg ++ "\n")
  --                Logout

  -- Doesn't type check
  --noLogout : ShellCmd () LoggedOut (const LoggedOut)
  --noLogout = do Correct <- Password "wurzel"
  --              | Incorrect => PutStr "Wrong password"
  --              msg <- GetSecret
  --              PutStr ("Secret code: " ++ show msg ++ "\n")

-----------------------------
-- Exercise 14.2.2
-----------------------------

-- See Vending.idr for the solution of this exercise.
