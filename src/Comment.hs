module Comment (deleteComment) where

removeTillNewLineOrComment :: String -> String
removeTillNewLineOrComment [] = []
removeTillNewLineOrComment a@[_] = []
removeTillNewLineOrComment (a:b:c)
        | a == '\n' = a : deleteComment (b:c)
        | a == '-' && b == '-' = deleteComment c
        | otherwise = removeTillNewLineOrComment (b:c)

deleteComment :: String -> String
deleteComment [] = []
deleteComment (a:b:c)
        | a == '-' && b == '-' = removeTillNewLineOrComment c
        | otherwise = a : deleteComment (b:c)
deleteComment (x:xs) = x : deleteComment xs
