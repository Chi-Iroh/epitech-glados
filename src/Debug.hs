module Debug (debug, debug2) where

import Debug.Trace (trace)

-- debug "Hello " 4 prints "Hello 4" and returns 4
debug :: Show a => String -> a -> a
debug msg a = trace (msg ++ (show a)) a

-- debug "Hello " 4 5 prints "Hello 4" and returns 5
debug2 :: Show a => String -> a -> b -> b
debug2 msg a b = trace (msg ++ (show a)) b
