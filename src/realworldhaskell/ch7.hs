module Main where
import           System.IO
import           Data.Char                      ( toUpper )

name2reply :: String -> String
name2reply name =
    "Pleased to meet you, "
        ++ name
        ++ ".\n"
        ++ "Your name contains "
        ++ charcount
        ++ " characters."
    where charcount = show (length name)

main :: IO ()
main = do
    -- example 1
    -- putStrLn "Greetings! What is your name?"
    -- inpStr <- getLine
    -- putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

    -- example 2
    -- putStrLn "Greetings once again.  What is your name?"
    -- inpStr <- getLine
    -- let outStr = name2reply inpStr
    -- putStrLn outStr

    -- example 3
    inh  <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    mainloop inh outh
    hClose inh
    hClose outh


mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
    ineof <- hIsEOF inh
    if ineof
        then return ()
        else do
            inpStr <- hGetLine inh
            hPutStrLn outh (map toUpper inpStr)
            mainloop inh outh
