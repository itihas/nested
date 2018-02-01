module Lib where

import qualified Data.Set as Set
import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)

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


fptpVote k a = Game { players = k
                     , states = [State i | i <- [0..a]]
                     , outcomes = [Outcome i | i <- [1..a]]
                     , label = \(State i) -> Outcome i
                     , playerMoves = \(Player p) (State s) -> if s==0
                                                              then 3
                                                              else 0
                     , transition = \(State s) js -> if s==0
                                                     then State ((maximumBy (comparing length) . group $ sort js)!!0) -- find the mode of player actions
                                                     else State s
                 }



alternativeVote k a = Game { players = k
                     , states = [State i | i <- [0..a]]
                     , outcomes = [Outcome i | i <- [1..a]]
                     , label = \(State i) -> Outcome i
                     , playerMoves = \(Player p) (State s) -> if s==0
                                                              then 3
                                                              else 0
                     , transition = \(State s) js -> undefined -- TODO
                 }

