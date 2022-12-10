
import qualified Data.Text.IO as Tio
import Control.Arrow
import Data.Char     (ord)
import qualified Data.Set as S
import qualified Data.Bifunctor as Bf
import Data.Maybe (fromJust)
import Debug.Trace as Trc ( trace )

-- While Sets is an aparently intuitive way of solving this problem, Data.List already provides
-- with a library of function to treat lists as sets XD

priority :: Char -> Int
priority c | ord c >= ord 'a' = ord c - ord 'a' +1
priority c | ord c >= ord 'A' = ord c - ord 'A' + 27

parseCompartments :: String -> (String, String)
parseCompartments rapsack = let amount = length rapsack `div` 2 in (take amount &&& drop amount) rapsack

findRepeated :: S.Set Char -> String -> Maybe Char
findRepeated s "" = Nothing
findRepeated s (c:cs)  = if S.member c s then Just c else findRepeated s cs

part1 :: IO ()
part1 = do
        input <- getContents
        let rapsacks = lines input
            total =  sum $ map ( sum . (<$>) priority . uncurry findRepeated . Bf.first S.fromList . parseCompartments ) rapsacks
        print total


intersecta3 :: S.Set Char -> S.Set Char -> S.Set Char -> S.Set Char
intersecta3 s1 s2 s3 = S.intersection (S.intersection s1 s2) s3

comprimirConjunto :: [S.Set Char] -> S.Set Char
comprimirConjunto [] = S.empty
comprimirConjunto (s:ss) = foldl S.intersection s ss


part2 :: IO ()
part2 = do
        input <- getContents
        let rapsacks = lines input
            result = sum $ map ( priority . S.findMax . comprimirConjunto . map S.fromList ) (group 3 rapsacks)
        print result

group :: Int -> [String] -> [[String]]
group _ [] = []
group 0 _ = []
group n l = take 3 l : group n (drop 3 l)

main :: IO ()
main = part2
