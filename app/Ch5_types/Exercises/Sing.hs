module Ch5_types.Exercises.Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if (x < y) then fstString x else sndString y
    where
        x = "Singin"
        y = "Somewhere"