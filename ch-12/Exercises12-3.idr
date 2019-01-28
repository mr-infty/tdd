module Exercises12_3

import ArithState

-----------------------------
-- Exercise 12.3.1
-----------------------------

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do st <- GetGameState
                       PutGameState (f st)

-----------------------------
-- Exercise 12.3.2
-----------------------------

mutual
  Functor Command where
    map f cmd = do x <- cmd
                   pure (f x)

  Applicative Command where
    pure = Pure
    (<*>) cmd_f cmd_x = do f <- cmd_f
                           x <- cmd_x
                           pure (f x)
  
  Monad Command where
    (>>=) = Bind

-----------------------------
-- Exercise 12.3.3
-----------------------------

record Votes where
  constructor MkVotes
  upvotes : Integer
  downvotes : Integer

record Article where
  constructor MkArticle
  title : String
  url : String
  score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle url url (MkVotes 0 0)

getScore : Article -> Integer
getScore art = let score = (score art) in
                   (upvotes score) - (downvotes score)

badSite : Article
badSite = MkArticle "Bad Page" "https://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "https://example.com/good" (MkVotes 101 7)

-----------------------------
-- Exercise 12.3.3
-----------------------------

addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+1) }
