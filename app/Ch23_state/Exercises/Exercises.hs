module Ch23_state.Exercises.Exercises where

import Ch23_state.State

get :: State s s
get = State (\s -> (s,s))

put :: s -> State s ()
put s = State (const ((), s))

exec :: State s a -> s -> s
exec (State sa) =
    snd . sa

eval :: State s a -> s -> a
eval (State sa) =
    fst . sa

modify :: (s -> s) -> State s ()
modify fn =
    State (\s -> ((), fn s))