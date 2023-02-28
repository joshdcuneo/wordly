{-# LANGUAGE OverloadedRecordDot #-}

module Console (showInstructions, showResult, askWordLength, askMaxGuesses, askGuess, showClue) where

import Game (Game (..), GameResult (..))
import System.Console.ANSI (Color (Green, Yellow), ColorIntensity (Vivid), ConsoleLayer (Background, Foreground), SGR (SetColor), clearFromCursorToScreenEnd, clearLine, clearScreen, cursorDownLine, cursorUp, cursorUpLine, setSGR)
import Text.Read (readMaybe)

askGuess :: Int -> IO String
askGuess wordLength = do
  putStrLn ""
  putStrLn $ "Guess a " ++ show wordLength ++ " letter word: "
  guess <- getLine
  if length guess /= wordLength
    then do
      clearPrompt
      askGuess wordLength
    else do
      clearPrompt
      return guess

askWordLength :: IO Int
askWordLength = do
  putStrLn ""
  putStrLn "How long do you want the word to be?"
  askInt

askMaxGuesses :: IO Int
askMaxGuesses = do
  putStrLn ""
  putStrLn "How many guesses do you want?"
  askInt

askInt :: IO Int
askInt = do
  input <- getLine
  case readMaybe input of
    Just n -> do
      clearPrompt
      return n
    Nothing -> do
      putStrLn "Please enter a number!"
      askInt

showInstructions :: IO ()
showInstructions = do
  clearScreen
  putStrLn ""
  setTextGreen
  putStrLn "  Welcome to Wordly!"
  clearTextColor
  putStrLn "  ------------------"
  putStrLn "  You have to guess a word."
  putStrLn "  You can guess a word by typing it in."
  putStrLn "  Each turn your previous guesses are printed."
  putStrLn "  Colors are used to indicate if the letter correctly guessed."
  putStrLn "  Green indicates a correct letter in the correct position."
  putStrLn "  Yellow indicates a correct letter in the wrong position."
  putStrLn "  You win if you guess the word correctly."
  putStrLn "  You lose if you run out of guesses."
  putStrLn "  Good luck!"
  putStrLn ""

showResult :: GameResult -> IO ()
showResult (Win game) = do
  showClue game.word game.guesses
  putStrLn ""
  putStrLn "  You win!"
showResult (Loss game) = do
  showClue game.word game.guesses
  putStrLn ""
  putStrLn $ "  You lose! The word was '" ++ game.word ++ "'."

showClue :: String -> [String] -> IO ()
showClue word []  = return ()
showClue word guesses = do
  mapM_ (showClueForGuess word) $ reverse guesses
  putStrLn ""
  putStrLn ""

showClueForGuess :: String -> String -> IO ()
showClueForGuess word guess = do
  putStrLn ""
  putStr "  "
  mapM_ (putClueChar word) (zip [0 ..] guess)

putClueChar :: String -> (Int, Char) -> IO ()
putClueChar word (i, c)
  | nthCharIs i c word = putColorChar Green c
  | c `elem` word = putColorChar Yellow c
  | otherwise = putChar c

putGreenChar :: Char -> IO ()
putGreenChar = putColorChar Green

putYellowChar :: Char -> IO ()
putYellowChar = putColorChar Yellow

putColorChar :: Color -> Char -> IO ()
putColorChar color c = do
  setSGR [SetColor Background Vivid color]
  putChar c
  setSGR []

setTextYellow :: IO ()
setTextYellow = setTextColor Yellow

setTextGreen :: IO ()
setTextGreen = setTextColor Green

clearTextColor :: IO ()
clearTextColor = setSGR []

setTextColor :: Color -> IO ()
setTextColor color = setSGR [SetColor Foreground Vivid color]

nthChar :: Int -> String -> Maybe Char
nthChar n s = if n < length s then Just $ s !! n else Nothing

nthCharIs :: Int -> Char -> String -> Bool
nthCharIs n c s = nthChar n s == Just c

clearPrompt :: IO ()
clearPrompt = do
  clearScreen
  showInstructions