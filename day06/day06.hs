{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as DT
import qualified Data.Set as DS
import Debug.Trace

areAllDifferent :: String -> Int -> Bool
areAllDifferent s l = ((== l). DS.size . DS.fromList) s

mymeme :: String -> Int ->Maybe Int
mymeme "" _ = Nothing
mymeme t l | (length . take l) t /= l = Nothing
           | otherwise = run t 1
            where
              run [] _ = Nothing
              run tt _ | (length . take l) tt /= l = Nothing
              run tt n | areAllDifferent (take l tt) l = Just n
                       | otherwise = run (tail tt) (n+1)


part1:: String -> Int
part1 t = let Just answer = mymeme t 4 in answer +3

part2 t = let Just answer = mymeme t 14 in answer +13

main = do
  input <- getContents
  let answers = map part1 $ lines input
  let answers2 = map part2 $ lines input
  print answers
  print answers2
