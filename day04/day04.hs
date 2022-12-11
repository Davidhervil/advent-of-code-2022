{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.Text (splitOn)
import Data.Text.Read (decimal)
import Debug.Trace ( trace )


type Range = (Integer, Integer)

parseRange :: T.Text -> Range
parseRange s = let [Right (l,_), Right (r,_)] = map decimal $ T.splitOn "-" s in (l, r)

parsePartners :: T.Text -> (Range, Range)
parsePartners s = let [l,r] = map parseRange $ T.splitOn "," s in (l,r)

isContained :: Range -> Range -> Bool
isContained l r = fst l <= fst r && snd l >= snd r || fst r <= fst l && snd r >= snd l

overlaps :: Range -> Range -> Bool
overlaps l r = snd l >= fst r || snd r >= fst l -- pending finish

part1 :: [(Range,Range)] -> Integer
part1 = fromIntegral . length . filter (uncurry isContained) 

part2 :: [(Range,Range)] -> Integer
part2 = fromIntegral . length . filter (uncurry overlaps) 

main = do
        input <- Tio.getContents
        let pairs = T.lines input
            result = part1 $ map parsePartners pairs
            result2 = part2 $ map parsePartners pairs
        print result