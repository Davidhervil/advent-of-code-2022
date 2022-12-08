{-# LANGUAGE OverloadedStrings #-}
import System.IO (isEOF)
import Data.Char ( toUpper )
import qualified Data.Text as T (Text, lines)
import Data.Text.Read (decimal)
import qualified Data.Text.IO as TIO
import Data.List (sortBy)

readInventory :: T.Text -> [[Integer]]
readInventory s = aux (T.lines s) [] []
                  where
                    aux [] curr acc = curr:acc
                    aux (t:ts) curr acc = if t /= "" then let Right(number, _) = decimal t in
                                            aux ts (number:curr) acc
                                          else
                                            aux ts [] (curr:acc)

sumCalories :: [[Integer]] -> [Integer]
sumCalories = map sum


leloop :: IO ()
leloop = do
          input <- TIO.getContents
          let inventory = readInventory input
              caloriesPerElf = sumCalories inventory
          print (maximum caloriesPerElf)

leloop' :: IO ()
leloop' = do
          input <- TIO.getContents
          print $ (maximum . sumCalories . readInventory) input


leloop2 :: IO ()
leloop2 = do
          input <- TIO.getContents
          -- reverse . sortBy compare . sumCalories . readInventory el HLS detecta que el reverse no hace falta si hacemos flip del compare :O
          print $ ( sum . take 3 . sortBy (flip compare) . sumCalories . readInventory) input

main :: IO ()
main = leloop2