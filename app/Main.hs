{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Console (askGuess, askMaxGuesses, askWordLength, showClue, showInstructions, showResult)
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
import Game (Game(..), GameResult (..), isLoss, isWin)
import Numeric (showInt)

type WordListReaderT m a = ReaderT [String] m a

type GameStateT m = StateT Game m

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
  gameResult <- evalStateT (runReaderT playGame wordList) Game {word, maxGuesses, wordLength, guesses = []}
  showResult gameResult

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
          performIO showInstructions
          performIO $ showClue game.word game.guesses
          guess <- performIO $ askGuess game.wordLength
          performState $ put game {guesses = guess : game.guesses}
          playRound

loadWords :: Int -> IO [String]
loadWords wordLength = do
  handle <- openFile "words.txt" ReadMode
  contents <- hGetContents handle
  return $ filter ((== wordLength) . length) (words contents)

randomWord :: [String] -> IO String
randomWord words = do
  g <- newStdGen
  let i = fst $ randomR (0, length words - 1) g
  return $ words !! i
