module ChapterExercises where

import Moi

-- Construct a State where the state is also the value you return
get :: State s s
get = State $ \x -> (x, x)

-- Construct a State where the resulting state is the argument
-- provided and the value is defaulted to unit
put :: s -> State s ()
put s = State $ const ((), s)

-- Run the State with s and get the state that results
exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)

-- Run the State with s and get the value that results
eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)

-- Write a function which applies a function to create a new State
modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)
