import System.IO

-- top-down manner
hangman :: IO()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

-- echo each character as a dash symbol 
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

-- 
getCh :: IO Char 
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!!"
               else
                  do putStrLn (match word guess)
                     play word

--use a list comprehension to indicate which letters occur anywhere in the guess
match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <-xs]
