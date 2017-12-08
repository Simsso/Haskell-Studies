module ExerciseSing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing y = if (x > y) then fstString x else sndString y 
  where x = "Singin"

x = "Somewhere"