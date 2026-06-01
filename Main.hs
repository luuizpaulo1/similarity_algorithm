module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

tokenize :: String -> String -> [String]
tokenize seps str = words [ if c `elem` seps then ' ' else c | c <- str ]

computeFrequencies :: [String] -> [String] -> M.Map String Double
computeFrequencies resWords tokens = M.fromListWith (+) [ (t, weight t) | t <- tokens ]
  where
    resSet = S.fromList resWords
    weight w = if w `S.member` resSet then 2.0 else 1.0

analyzeFiles :: String -> String -> String -> String -> ([(String, Double)], Double, Double, Double)
analyzeFiles resContent sepContent c1Content c2Content =
  let resWords = words resContent
      seps     = filter (`notElem` " \t\n\r") sepContent

      tokens1  = tokenize seps c1Content
      tokens2  = tokenize seps c2Content

      f1Map    = computeFrequencies resWords tokens1
      f2Map    = computeFrequencies resWords tokens2

      totalF1  = sum (M.elems f1Map)

      calculateM w f1 =
        let f2 = M.findWithDefault 0.0 w f2Map
            diff = abs (f1 - f2)
            maxF = max f1 f2
        in if maxF > 0 && (diff / maxF <= 0.10) then f1 else 0.0

      mValue     = sum [calculateM w f1 | (w, f1) <- M.toList f1Map]
      similarity = if totalF1 == 0 then 0.0 else mValue / totalF1
      c1Report   = sortOn (\(w, f) -> (Down f, w)) (M.toList f1Map)
  in (c1Report, totalF1, mValue, similarity)


printReport :: ([(String, Double)], Double, Double, Double) -> IO ()
printReport (c1Report, totalF1, mValue, similarity) = do
  putStrLn "\n========================================================"
  putStrLn "       RELATÓRIO DE FREQUÊNCIAS (CÓDIGO 1 - c1)         "
  putStrLn "========================================================"
  printf "%-25s | %-20s\n" "Palavra" "Frequência Ponderada"
  putStrLn "--------------------------------------------------------"
  mapM_ (\(w, f) -> printf "%-25s | %-20.1f\n" w f) c1Report
  putStrLn "--------------------------------------------------------"

  putStrLn "\n========================================================"
  putStrLn "                 MÉTRICAS DE SIMILARIDADE               "
  putStrLn "========================================================"
  printf "Soma total de f1 (denominador): %.1f\n" totalF1
  printf "Valor acumulado de m:           %.1f\n" mValue
  printf "Índice de Similaridade:         %.4f (%.2f%%)\n" similarity (similarity * 100)
  putStrLn "========================================================\n"

main :: IO ()
main = do
  args <- getArgs
  if length args /= 4
    then do
      putStrLn "Erro: Número incorreto de argumentos."
      putStrLn "Uso: ./simil_prog <res_file> <sep_file> <c1_file> <c2_file>"
      exitFailure
    else do
      let [resPath, sepPath, c1Path, c2Path] = args

      resContent <- readFile resPath
      sepContent <- readFile sepPath
      c1Content  <- readFile c1Path
      c2Content  <- readFile c2Path

      let results = analyzeFiles resContent sepContent c1Content c2Content

      printReport results