{-  menu-selection, a Haskell example
    Copyright (C) 2025  Nikolaos Chatzikonstantinou

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{- |
   Module      : Main
   Copyright   : Copyright (C) 2025 Nikolaos Chatzikonstantinou
   License     : GPL-3.0-or-later

   Maintainer  : Nikolaos Chatzikonstantinou <nchatz314@gmail.com>
   Stability   : alpha
   Portability : portable

A program that gives you a menu similar to a phone tree. It remembers
your final choice but then loops back to the beginning. It only quits
on EOF.
-}
module Main where

import Data.Tree (Tree(Node, rootLabel))
import Data.Sequence (Seq(Empty, (:<|), (:|>)))
import Text.Read (readMaybe)
import Control.Exception (catch)
import Control.Monad (void)
import System.IO(hFlush, stdout)

-- | The menu tree.
--
-- Choose your favorite animal!
--
-- Note that each label is unique; associated data could therefore be
-- attached to the nodes by using a 'Data.Map.Map'.
--
-- We opted to use a 'Data.Tree.Tree' instead of a 'Data.Graph.Graph'
-- because the latter is more complicated, although the latter can
-- also hold metadata without the use of a map.
menuTree :: Tree String
menuTree =
  n "Animal" [
    n "Arthropod" [
        n "Crab" []
      , n "Millipede" []
      , n "SeaSpider" []
      , n "Springtail" []
      ]
  , n "Chordate" [
        n "Lancelet" []
      , n "SeaSquirt" []
      , n "Shark" []
      , n "Tiger" []
      ]
  , n "Mollusc" [
        n "Clam" []
      , n "Dentalium" []
      , n "Periwinkle" []
      ]
  , n "Porifera" [
        n "CallyspongiaVaginalis" []
      , n "NegombataMagnifica" []
      , n "SpongillaLacustris" []
      ]
  ]
  where n = Node

-- | Beginning from a node, navigate the user until a leaf is met.
--
-- Returns the sequence of choices the user made.
navigateFromNode :: IO () -> Seq String -> Tree String -> IO (Either (Seq String) String)
navigateFromNode _ currentPath (Node label []) = pure (Left $ currentPath :|> label)
navigateFromNode printBanner currentPath (Node label children) = do
  putStrLn ""
  printBanner
  putStrLn $ "You are at: " <> (position newPath) <> "."
  putStrLn "Choose one of:"
  sequence_ (printItem <$> numberedLabels)
  putStr " > "
  hFlush stdout
  choice <- getLine
  case (readMaybe choice :: Maybe Int) of
    Nothing -> do pure (Right $ "Invalid choice `" <> choice <> "'.")
    Just number -> if 0 < number && number <= length children
                   then navigateFromNode printBanner newPath (children !! (number - 1))
                   else pure (Right $ "The choice `" <> choice <> "' is out of range.")
  where newPath = currentPath :|> label
        position Empty = ""
        position (x :<| Empty) = x
        position (x :<| xs) = x <> " > " <> position xs
        printItem (n, s) = putStrLn $ (show n) <> ". " <> s
        numberedLabels = zip [1..] (rootLabel <$> children)

-- | Ignore any IO exception raised by an action.
--
-- Does not re-raise the exception.
ignoreException :: IO a -> IO ()
ignoreException f = (void f) `catch` (pure . const () :: IOError -> IO ())

main :: IO ()
main = do
  let printBanner Empty = pure ()
      printBanner (_ :|> last) = putStrLn $ "Your current choice is: " <> last
  let loop currentChoice = do
        result <- navigateFromNode (printBanner currentChoice) Empty menuTree
        case result of
          Left choice -> loop choice
          Right error -> putStrLn error >> loop currentChoice
  ignoreException $ loop Empty
