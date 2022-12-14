{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ( Arrow((&&&)) )
import Data.Char (isNumber, isAlpha)
import qualified Data.Text as T
import qualified Data.Text.Read as Tread
import qualified Data.Text.IO as Tio
import Debug.Trace ( trace )
import Text.Read (Lexeme(String))

data Order = Order {from :: Int, amount :: Int, destiny :: Int} deriving (Show)

donothing = Order 1 0 1

putCrate :: Char -> Int -> [[Char]] -> [[Char]]
putCrate _ _ [] = []
putCrate c 1 (s : ss) = (c : s) : ss
putCrate c n (s : ss) = s : putCrate c (n -1) ss

putSeveral :: [Char] -> Int -> [[Char]] -> [[Char]]
putSeveral _ _ [] = []
putSeveral [] _ s = s
putSeveral cs 1 (s : ss) = (cs ++ s) : ss
putSeveral cs n (s : ss) = s : putSeveral cs (n -1) ss

removeCrate :: Int -> [[Char]] -> [[Char]]
removeCrate _ [] = []
removeCrate 0 s = s
removeCrate 1 (s : ss) = tail s : ss
removeCrate n (s : ss) = s : removeCrate (n -1) ss

removeSeveral :: Int -> Int -> [[Char]] -> ([Char], [[Char]])
removeSeveral _ _ [] = ([], [])
removeSeveral 0 _ s = ([], s)
removeSeveral n 1 (s : ss) = (take n s, drop n s : ss)
removeSeveral n m (s : ss) = let (taken, state) = removeSeveral n (m -1) ss in (taken, s : state)

moveCrates :: [[Char]] -> Order -> [[Char]]
moveCrates state (Order frm amnt dst) = let (taken, result) = removeSeveral amnt frm state in putSeveral (reverse taken) dst result

operateCrates :: [[Char]] -> [Order] -> [[Char]]
operateCrates state [] = state
operateCrates state os = foldl moveCrates state os

part1 :: [[Char]] -> [Order] -> [Char]
part1 state os = map head $ operateCrates state os

takeNConsecutive' :: Int -> String -> [String] -> [String]
takeNConsecutive' n c t | n == 0 = t
                        | otherwise = rolit t n c True
                          where
                            rolit :: [String] -> Int -> String -> Bool -> [String]
                            rolit [] _ _ _ = []
                            rolit s 0 _ _ =  {-trace ("WTF " ++ show s)-} s
                            rolit (sc:ss) n cm b | b && sc == cm = sc : rolit ss (n-1) cm False
                                                 | sc /= cm && not b = sc : rolit ss n cm True
                                                 | sc == cm && not b = rolit ss n cm False
                                                 | otherwise = rolit ss n cm b


takeAtMost :: Int -> Char -> T.Text -> T.Text
takeAtMost n c t | n == 0 = t
                 | otherwise = T.pack $ rolit (T.unpack t) n c 
                 where
                  rolit :: String -> Int -> Char -> String
                  rolit [] _ _ = []
                  rolit s 0 _ =  {-trace ("WTF " ++ show s)-} s
                  rolit (sc:ss) n cm | sc == cm = rolit ss (n-1) cm
                                     | otherwise = sc : rolit ss n cm




-- parseBoxes:: [[Text]] -> Integer -> [[Char]]
--parseBoxes t n =   replicate n []

main = do
  input <- Tio.readFile "day05\\input.txt"
  --- this leaves with a tuple where the boxes are on the left and the numbers and the rest is on the right
  let (boxes, stacks : rest) = break (all (T.foldl (\bacc c -> bacc && isNumber c) True) . Prelude.filter (not . T.null) . T.splitOn " ") $ T.lines input
      Right (totalStacks, _) = Tread.decimal $ last . filter (not.T.null) $ T.splitOn " " stacks 
      -- 3+1 comes from you have to delete spaces+1 blankspaces per each empty column because they are 3 spaces each
      pene = map (  takeNConsecutive' (totalStacks*(3+1)) "" .  (length . filter (""==) &&& id) . map T.unpack . T.splitOn " " . takeAtMost (totalStacks-1) ' ') boxes
  print boxes
  print $ totalStacks - 1
  print pene