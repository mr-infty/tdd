module Process

import System.Concurrency.Channels

%default total

data MessagePID = MkMessage PID
data Message = Add Nat Nat

data Process : Type -> Type where
  Action : IO a -> Process a
  Pure : a -> Process a
  (>>=) : Process a -> (a -> Process b) -> Process b
  
  Spawn : Process () -> Process (Maybe MessagePID)
  Request : MessagePID -> Message -> Process (Maybe Nat)
  Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
  Loop : Inf (Process a) -> Process a

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Process a -> IO (Maybe a)
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure val) = pure (Just val)
run fuel (act >>= next) = do Just val <- run fuel act
                             | Nothing => pure Nothing
                             run fuel (next val)
run fuel (Spawn proc) = do Just pid <- spawn (do run fuel proc
                                                 pure ())
                           | Nothing => pure Nothing
                           pure (Just (Just (MkMessage pid)))
run fuel (Request (MkMessage pid) msg) = do Just chan <- connect pid
                                            | Nothing => pure Nothing
                                            ok <- unsafeSend chan msg
                                            if ok
                                               then do Just val <- unsafeRecv Nat chan
                                                       | Nothing => pure Nothing
                                                       pure (Just (Just val))
                                               else pure Nothing
run fuel (Respond calc) = do Just sender <- listen 1
                             | Nothing => pure Nothing
                             Just msg <- unsafeRecv Message sender
                             | Nothing => pure Nothing
                             Just res <- run fuel (calc msg)
                             | Nothing => pure Nothing
                             unsafeSend sender res
                             pure (Just (Just msg))
run (More fuel) (Loop (Delay proc)) = run fuel proc
run Dry _ = do putStrLn "Out of fuel"
               pure Nothing

procAdder : Process ()
procAdder = do Respond (\msg => case msg of
                                     Add x y => Pure (x+y))
               Loop procAdder

procMain : Process ()
procMain = do Just msg_pid <- Spawn procAdder
              | Nothing => Action (putStrLn "Spawn failed")
              Just res <- Request msg_pid (Add 2 3)
              | Nothing => Action (putStrLn "Got no response")
              Action (printLn res)

partial
runProc : Process () -> IO ()
runProc proc = do run forever proc
                  pure ()
