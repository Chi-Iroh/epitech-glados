module Parser.Parser (splitByParanthese) where

collectUntilClosing :: String -> (Maybe String, String)
collectUntilClosing [] = (Nothing, [])
collectUntilClosing (a:as)
        | a == '(' =
            let (inner, rest) = collectUntilClosing as
            in case inner of
                Just content    ->
                    let (nested, finalRest) = collectUntilClosing rest
                    in case nested of
                        Just nestedContent  -> (Just ('(':content ++ nestedContent), finalRest)
                        Nothing             -> (Just ('(':content), rest)
                Nothing         -> (Nothing, rest)
        | a == ')' = (Just [a], as)
        | otherwise =
            let (inner, rest) = collectUntilClosing as
            in case inner of
                Just content    -> (Just (a: content), rest)
                Nothing         -> (Nothing, rest)


splitByParanthese :: String -> (Maybe String, String)
splitByParanthese [] = (Nothing, [])
splitByParanthese (a:as)
        | a == '(' =
            let (inner, rest) = collectUntilClosing as
            in case inner of
                Just content    -> (Just ('(' : content), rest)
                Nothing         -> (Nothing, rest)
        | otherwise = error "unwanted text before first open paranthese"