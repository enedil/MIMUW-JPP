module Common where

posShow :: Pos -> String
posShow Nothing = "(unknown)"
posShow (Just (line, col)) = show line ++ ":" ++ show col

