module Console (showInstructions, askWordLength, askMaxGuesses, askGuess, showClue) where

import Text.Read (readMaybe)

showInstructions :: IO ()
showInstructions = do
  putStrLn ""
  putStrLn "Welcome to Wordly!"
  putStrLn "------------------"
  putStrLn "You have to guess a word."
  putStrLn "You can guess a word by typing it in."
  putStrLn "Each turn your previous guesses are printed."
  putStrLn "Markers are used to indicate if the letter correctly guessed."
  putStrLn "A '*' indicates a correct letter in the correct position."
  putStrLn "A '?' indicates a correct letter in the wrong position."
  putStrLn "A '-' indicates an incorrect letter."
  putStrLn "You win if you guess the word correctly."
  putStrLn "You lose if you run out of guesses."
  putStrLn "Good luck!"

showClue :: String -> [String] -> IO ()
showClue word guesses = do
    putStrLn ""
    mapM_ (showClueForGuess word) $ reverse (guesses)

askGuess :: Int -> IO String
askGuess wordLength = do
  putStrLn $ "\nGuess a " ++ show (wordLength) ++ " letter word: "
  guess <- getLine
  if length guess /= wordLength
    then do
      askGuess wordLength
    else return guess

askWordLength :: IO Int
askWordLength = do
  putStrLn "\nHow long do you want the word to be?"
  askInt

askMaxGuesses :: IO Int
askMaxGuesses = do
  putStrLn "\nHow many guesses do you want?"
  askInt

askInt :: IO Int
askInt = do
  input <- getLine
  case readMaybe input of
    Just n -> return n
    Nothing -> do
      putStrLn "\nPlease enter a number!"
      askInt

showClueForGuess :: String -> String -> IO ()
showClueForGuess word guess = do
  putStrLn guess
  putStrLn $ clueForGuess word guess

clueForGuess :: String -> String -> String
clueForGuess word guess = zipWith (curry (clueForChar word)) [0 ..] guess

clueForChar :: String -> (Int, Char) -> Char
clueForChar word (i, c)
  | nthCharIs i c word = '*'
  | c `elem` word = '?'
  | otherwise = '-'

nthChar :: Int -> String -> Maybe Char
nthChar n s = if n < length s then Just $ s !! n else Nothing

nthCharIs :: Int -> Char -> String -> Bool
nthCharIs n c s = nthChar n s == Just c