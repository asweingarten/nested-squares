{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Linear.V2
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Colour.Palette.Types
import Data.Colour.Palette.RandomColor

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Random

import Control.Applicative
import Data.Coerce

import Data.Time.Clock.POSIX
import Data.Random.Source.StdGen
import Data.List.Split

import Lib

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed
  renderCairo "out.png" (dims $ V2 1200 800) $ squareGrid rsrc

squareGrid :: StdGen -> Diagram B
squareGrid src =
  vsep 0
    . fmap (hsep 0)
    . chunksOf 12
    . fmap stackedSquares
    . fst
    $ randomColorSets 96 3 src

randomColors :: Int -> StdGen -> ([Kolor], StdGen)
randomColors n src = flip runRand src $ replicateM n $ randomCIELab

randomColorSets :: Int -> Int -> StdGen -> ([[Kolor]], StdGen)
randomColorSets nSet nCol src =
  flip runRand src
  . replicateM nSet
  . replicateM nCol
  $ randomCIELab

stackedSquares :: [Kolor] -> Diagram B
stackedSquares colors =
  foldr atop mempty
  . fmap (\(c,d) -> fc c d)
  . zip colors
  . fmap (lw none)
  . fmap square
  $ [1,2,3]

-- (brightColors, src') = flip runRand src   $ replicateM numWoobles $ randomColor bright LumBright
-- foldr (\d acc -> center d `atop` acc) mempty woobles
