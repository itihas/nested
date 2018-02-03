module Lib where

import qualified Data.Set as Set
import Data.List (sort, group, maximumBy, minimumBy)
import Data.Ord (comparing)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype Player = Player {player::Int} deriving (Show, Eq)

newtype State = State String deriving (Show, Eq)
  
data Game = Outcome String
          | Game {players :: Int
                 , states :: [State]
                 , outcomes :: [Game]
                 , label:: State -> Game -- s in states, o in outcomes
                 , playerMoves :: Player -> State -> Int -- player(p) < players,s in states, i represents number of actions that can be taken
                 , transition :: State -> [Int] -> State    -- s in states, length([i]) == players, [i]!!n < playerMoves(n, s)
                 }

          -- TODO: lots of checks to obey the rules in the comments above
          
instance Show Game where
  show (Outcome s) = show s
  show g@(Game _ _ _ _ _ _) = (show $ players g) ++ ", " ++ (show $ states g) ++ ", " ++ (show $ outcomes g)



-- test = Game { players = 3
--             , states = [State 1, State 2, State 3]
--             , outcomes =  [Outcome 1, Outcome 2, Outcome 3]
--             , label = \(State i) -> Outcome i
--             , playerMoves = \(Player p) (State s) -> p*s
--             , transition = \(State s) as -> State (((product as) * s) `mod` 3)
--             }

fptpVote :: Int -> Int -> Game
fptpVote k a = Game { players = k
                     , states = [State $ show i | i <- [0..a]]
                     , outcomes = [Outcome $ show i | i <- [1..a]]
                     , label = \(State i) -> Outcome i
                     , playerMoves = \(Player p) (State s) -> if s=="0"
                                                              then 3
                                                              else 0
                     , transition = \(State s) js -> if s=="0"
                                                     then State $ show ((maximumBy (comparing length) . group $ sort js)!!0) -- find the mode of player actions
                                                     else State s
                 }


                  -- alternative vote has n levels of m intermediate states each where players express their preferences among the m remaining candidates. It's like iterated last-past-the-post-elimination.
                  -- I wonder if it can be expressed as the composition of last-past-the post eliminations?

lptpElimination :: Int -> Int -> Game
lptpElimination k a = Game {players = k
                           , states = [State $ show i | i <- [0..a]]
                           , outcomes = [Outcome $ show i | i <- [1..a]]
                           , label = \(State i) -> Outcome i
                           , playerMoves = \(Player p) (State s) -> if s=="0"
                                                                    then 3
                                                                    else 0
                           , transition = \(State s) js -> if s=="0"
                                                           then State $ show ((minimumBy (comparing length) . group $ sort js)!!0) -- find the anti-mode of player actions
                                                           else State s
                           }

alternativeVote :: Int -> Int -> Game
alternativeVote k a = Game { players = k
                     , states = [State $  show i | i <- [0..a]]
                     , outcomes = [Outcome $ show i | i <- [1..a]]
                     , label = \(State i) -> Outcome i
                     , playerMoves = \(Player p) (State s) -> if s=="0"
                                                              then 3
                                                              else 0
                     , transition = \(State s) js -> undefined -- TODO
                 }

