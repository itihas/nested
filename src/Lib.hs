module Lib where

import qualified Data.Set as Set

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype Player = Player {player::Int} deriving (Show, Eq)

newtype State = State Int deriving (Show, Eq)
  
newtype Outcome = Outcome Int deriving (Show, Eq)


data Game = Game {players :: Int
                 , states :: [State]
                 , outcomes :: [Outcome]
                 , label:: State -> Outcome -- s in states, o in outcomes
                 , playerMoves :: Player -> State -> Int -- player(p) < players,s in states, i represents number of actions that can be taken
                 , transition :: State -> [Int] -> State    -- s in states, length([i]) == players, [i]!!n < playerMoves(n, s)
                  } 

          -- TODO: lots of checks to obey the rules in the comments above
          
instance Show Game where
  show g = (show $ players test) ++ ", " ++ (show $ states test) ++ ", " ++ (show $ outcomes test)

test = Game { players = 3
            , states = [State 1, State 2, State 3]
            , outcomes =  [Outcome 1, Outcome 2, Outcome 3]
            , label = \(State i) -> Outcome i
            , playerMoves = \(Player p) (State s) -> p*s
            , transition = \(State s) as -> State (((product as) * s) `mod` 3)
            }

