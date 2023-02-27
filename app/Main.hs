{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.IO.Class ()
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, runReaderT)
import Control.Monad.Trans.State (StateT (..), evalStateT, get, put)
import Data.Foldable (Foldable (fold))
import Data.List ()
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import System.Random (Random (random), newStdGen, randomR)
import Text.Read (readMaybe)

type WordListReaderT m a = ReaderT [String] m a

data Game = Game
  { word :: String,
    maxGuesses :: Int,
    wordLength :: Int,
    guesses :: [String]
  }

type GameStateT m = StateT Game m

data GameResult = Win Game | Loss Game

type App a = WordListReaderT (GameStateT IO) a

performState :: StateT Game IO a -> App a
performState = lift

performIO :: IO a -> App a
performIO = lift . lift

main :: IO ()
main = do
  showInstructions
  wordLength <- askWordLength
  maxGuesses <- askMaxGuesses
  wordList <- loadWords wordLength
  word <- randomWord wordList
 
  gameResult <- evalStateT (runReaderT playGame wordList) Game {word, maxGuesses,wordLength, guesses = []}
  case gameResult of
    Win game -> putStrLn "\nYou win!"
    Loss game -> putStrLn $ "\nYou lose! The word was '" ++ game.word ++ "'."


playGame :: App GameResult
playGame = do
  gameResult <- playRound
  maybe playGame return gameResult

playRound :: App (Maybe GameResult)
playRound = do
  game <- performState get
  if isWin game
    then return $ Just $ Win game
    else
      if isLoss game
        then return $ Just $ Loss game
        else do
          showClue
          guess <- askGuess
          performState $ put game {guesses = guess : game.guesses}
          playRound

isLoss :: Game -> Bool
isLoss game@Game {guesses, maxGuesses} = length guesses >= maxGuesses

isWin :: Game -> Bool
isWin Game {guesses = []} = False
isWin Game {word, guesses = (lastGuess : _)} = word == lastGuess


askGuess :: App String
askGuess = do
  game <- performState get
  performIO $ putStrLn $ "\nGuess a " ++ show (game.wordLength) ++ " letter word: "
  guess <- performIO getLine
  if length guess /= game.wordLength
    then do
      askGuess
    else return guess

showClue :: App ()
showClue = do
  game <- performState get
  performIO $ putStrLn ""
  mapM_ showClueForGuess $ reverse (game.guesses)

showClueForGuess :: String -> App ()
showClueForGuess guess = do
  game <- performState get
  performIO $ do
    putStrLn guess
    putStrLn $ clueForGuess guess game.word

clueForGuess :: String -> String -> String
clueForGuess guess word = zipWith (curry (clueForChar word)) [0 ..] guess

clueForChar :: String  -> (Int, Char) -> Char
clueForChar word (i, c)
  | nthCharIs i c word = '*'
  | c `elem` word = '?'
  | otherwise = '-'

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
      askMaxGuesses

loadWords :: Int -> IO [String]
loadWords wordLength = do
  handle <- openFile "words.txt" ReadMode
  contents <- hGetContents handle
  return $ filter ((== wordLength) . length) (words contents)

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

nthChar :: Int -> String -> Maybe Char
nthChar n s = if n < length s then Just $ s !! n else Nothing

nthCharIs :: Int -> Char -> String -> Bool
nthCharIs n c s = nthChar n s == Just c

randomWord :: [String] -> IO String
randomWord words = do
  g <- newStdGen
  let i = fst $ randomR (0, length words - 1) g
  return $ words !! i
