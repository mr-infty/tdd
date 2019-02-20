module ProcessState

import System.Concurrency.Channels

%default total

data ProcState = NoRequest | Sent | Complete
data MessagePID = MkMessage PID
data Message = Add Nat Nat

data Process : Type ->
               (in_state : ProcState) ->
               (out_state : ProcState) ->
               Type where
     Request : MessagePID -> Message -> Process Nat st st
     Respond : ((msg : Message) -> Process Nat NoRequest NoRequest) ->
               Process (Maybe Message) st Sent
     Spawn : Process () NoRequest Complete -> Process (Maybe MessagePID) st st
     Loop : Inf (Process a NoRequest Complete) -> Process a Sent Complete
     Action : IO a -> Process a st st

     Pure : a -> Process a st st
     (>>=) : Process a st1 st2 -> (a -> Process b st2 st3) -> Process b st1 st3

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Process t in_state out_state -> IO (Maybe t)
run Dry _ = do putStrLn "Out of fuel"
               pure Nothing
run fuel (Request (MkMessage pid) msg) = do Just chan <- connect pid
                                            | Nothing => pure Nothing
                                            ok <- unsafeSend chan msg
                                            if ok
                                               then do Just val <- unsafeRecv Nat chan
                                                       | Nothing => pure Nothing
                                                       pure (Just val)
                                               else pure Nothing
run fuel (Respond calc) = do Just sender <- listen 1
                             | Nothing => pure Nothing
                             Just msg <- unsafeRecv Message sender
                             | Nothing => pure Nothing
                             Just res <- run fuel (calc msg)
                             | Nothing => pure Nothing
                             unsafeSend sender res
                             pure (Just (Just msg))
run fuel (Spawn proc) = do Just pid <- spawn (do run fuel proc
                                                 pure ())
                           | Nothing => pure Nothing
                           pure (Just (Just (MkMessage pid)))
run (More fuel) (Loop proc) = run fuel proc
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure val) = pure (Just val)
run fuel (act >>= next) = do Just res <- run fuel act
                             | Nothing => pure Nothing
                             run fuel (next res)

Service : Type -> Type
Service a = Process a NoRequest Complete

Client : Type -> Type
Client a = Process a NoRequest NoRequest

procAdder : Service ()
procAdder = do Respond (\msg => case msg of
                                     Add x y => Pure (x+y))
               Loop procAdder

procMain : Client ()
procMain = do Just msg_pid <- Spawn procAdder
              | Nothing => Action (putStrLn "Spawn failed")
              res <- Request msg_pid (Add 2 3)
              Action (printLn res)

partial
runProc : Process () in_state out_state -> IO ()
runProc proc = do run forever proc
                  pure ()
