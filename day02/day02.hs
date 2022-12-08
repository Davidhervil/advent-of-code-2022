{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Debug.Trace as Trc ( trace )

data Jankenpon =  Piedra
                | Papel
                | Tijeras
                deriving(Show, Enum, Read)
data MatchResult = Draw
                  | YouWon
                  | TheyWon
                  deriving(Show)

-- I know again this fucking sucks but I don't know how to haskell it better :(
legana = [Tijeras, Piedra, Papel]
pierde = [Papel, Tijeras, Piedra]


-- I know this fucking sucks but I don't know how to haskell it better :(
undostres :: Jankenpon -> Jankenpon -> MatchResult
undostres Piedra Tijeras = Trc.trace (show YouWon) YouWon
undostres Piedra Papel = Trc.trace (show TheyWon) TheyWon
undostres Piedra Piedra = Trc.trace (show Draw) Draw
undostres Tijeras Papel = Trc.trace (show YouWon) YouWon
undostres Tijeras Piedra = Trc.trace (show TheyWon) TheyWon
undostres Tijeras Tijeras = Trc.trace (show Draw) Draw
undostres Papel Piedra = Trc.trace (show YouWon) YouWon
undostres Papel Tijeras = Trc.trace (show TheyWon) TheyWon
undostres Papel Papel = Trc.trace (show Draw) Draw


points:: Jankenpon -> Int
points j = Trc.trace (show j) fromEnum j + 1

strategize :: [T.Text] -> ([Jankenpon], [Jankenpon])
strategize ps = aux ps [] []
                where
                    aux [] them you = let tthem = reverse them 
                                          yyou = reverse you 
                                      in trace ( "They " ++ show tthem ++ " You " ++ show yyou) (tthem, yyou)
                    aux (p:pps) them you = case p of
                        "A" -> aux pps (Piedra:them) you
                        "B" -> aux pps (Papel:them) you
                        "C" -> aux pps (Tijeras:them) you
                        "X" -> aux pps them (Piedra:you)
                        "Y" -> aux pps them (Papel:you)
                        "Z" -> aux pps them (Tijeras:you)

parseText :: [T.Text] -> ([Jankenpon], [MatchResult])
parseText ps = aux ps [] []
                where
                    aux [] them you = let tthem = reverse them 
                                          yyou = reverse you 
                                      in trace ( "They " ++ show tthem ++ " You " ++ show yyou) (tthem, yyou)
                    aux (p:pps) them you = case p of
                        "A" -> aux pps (Piedra:them) you
                        "B" -> aux pps (Papel:them) you
                        "C" -> aux pps (Tijeras:them) you
                        "X" -> aux pps them (TheyWon:you)
                        "Y" -> aux pps them (Draw:you)
                        "Z" -> aux pps them (YouWon:you)

strategy :: [Jankenpon] -> [MatchResult] -> [Jankenpon]
strategy them expected = let pepe = zipWith setup them expected in Trc.trace ("Tus jugadas " ++ show pepe) pepe 
                            where
                                setup a Draw = a
                                setup a TheyWon = legana !! fromEnum a
                                setup a YouWon = pierde !! fromEnum a

calculatePoints :: [Jankenpon] -> [Jankenpon] -> [Int]
calculatePoints them yours = zipWith play them yours
                                where
                                    play they you = case undostres you they of
                                        Draw -> 3 + points you
                                        YouWon -> 6 + points you
                                        TheyWon -> 0 + points you


part1 :: IO ()
part1 = do
        input <- Tio.getContents
        let letrillas = T.words input
            (them, yours) = strategize letrillas
        print ( sum (calculatePoints them yours))

part2 :: IO ()
part2 = do
        input <- Tio.getContents
        let letrillas = T.words input
            (them, results) = parseText letrillas
            yours = strategy them results
        print ( sum (calculatePoints them yours))



main :: IO ()
main = part2