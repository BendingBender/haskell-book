module Ch23_state.RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Ch23_state.RandomExample

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
   intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
    liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty gen = go 0 0 gen
    where
        go :: Int -> Int -> StdGen -> Int
        go sum' count gen'
            | sum' >= 20 = count
            | otherwise =
                let
                    (die, nextGen) = randomR (1, 6) gen'
                in
                    go (sum' + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit gen = go 0 0 gen
    where
        go :: Int -> Int -> StdGen -> Int
        go sum' count gen'
            | sum' >= limit = count
            | otherwise =
                let
                    (die, nextGen) = randomR (1, 6) gen'
                in
                    go (sum' + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit gen = go 0 0 [] gen
    where
        go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum' count dice gen'
            | sum' >= limit = (count, dice)
            | otherwise =
                let
                    (die, nextGen) = randomR (1, 6) gen'
                    nextDice = dice ++ [intToDie die]
                in
                    go (sum' + die) (count + 1) nextDice nextGen