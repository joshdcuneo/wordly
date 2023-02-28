{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game (Game(..), GameResult(..), isLoss, isWin) where

data Game = Game
  { word :: String,
    maxGuesses :: Int,
    wordLength :: Int,
    guesses :: [String]
  }

data GameResult = Win Game | Loss Game

isLoss :: Game -> Bool
isLoss game = length game.guesses >= game.maxGuesses

isWin :: Game -> Bool
isWin Game {guesses = []} = False
isWin Game {word, guesses = (lastGuess : _)} = word == lastGuess