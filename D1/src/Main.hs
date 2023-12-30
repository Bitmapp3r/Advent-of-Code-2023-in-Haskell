module Main where

    import Data.Char
    
    main :: IO ()
    main = do
        putStrLn "Run from file [f] or user input [i]?:"
        input <- getLine
        case input of
            "f" -> do
                doc <- readFile "test.txt"
                calSum doc
            "i" -> do
                doc <- readLn :: IO String
                calSum doc

    calSum :: String -> IO ()
    calSum doc = putStrLn (show (sum calVals))
        where
            calVals = calibList lines
            lines = splitIntoLines doc

    calibList :: [String] -> [Int]
    calibList [] = []
    calibList (line : lines) = 
        (read ((firstNuminLine line) 
        ++ (firstNuminLine (reverse line))) :: Int) : (calibList lines)   

    splitIntoLines :: String -> [String]
    splitIntoLines str 
        | str == "" = []
        -- better to do a before (1st) newline and after (1st) newline function 
        -- and then cons the former with the recursion of the latter
        | otherwise = lines str
        -- | otherwise = line : (splitIntoLines (tail (dropWhile (/= '\n') str)))
        where
            line = takeWhile (/= '\n') str  

    firstNuminLine :: String -> String
    firstNuminLine (c : cs)
        | isDigit c = [c]
        | otherwise = firstNuminLine cs

    {- Equivalent to: main = readFile "test.txt" >>= (\fileContent -> print 
    fileContent) -}
    -- main = do
    --     fileContent <- readFile "test.txt"
    --     print fileContent
