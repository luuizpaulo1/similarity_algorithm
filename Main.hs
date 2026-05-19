{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

-- | Divide uma string em tokens baseando-se em uma lista de caracteres separadores
-- e nos caracteres de espaço em branco padrão (\s, \t, \n, \r).
tokenize :: String -> String -> [String]
tokenize seps str = filter (not . null) $ wordsWhen (`elem` (seps ++ " \t\n\r")) str
  where
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s = case dropWhile p s of
      "" -> []
      s' -> let (w, s'') = break p s' in w : wordsWhen p s''

-- | Computa as frequências ponderadas das palavras.
-- Se a palavra for reservada, seu peso é multiplicado por 2.
computeFrequencies :: [String] -> [String] -> M.Map String Double
computeFrequencies resWords tokens =
  let rawCounts = M.fromListWith (+) [(t, 1.0) | t <- tokens]
      resSet = M.fromList [(w, True) | w <- resWords]
      weight w = if w `M.member` resSet then 2.0 else 1.0
  in M.mapWithKey (\w count -> count * weight w) rawCounts

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

      -- Leitura dos arquivos de entrada
      resContent <- readFile resPath
      sepContent <- readFile sepPath
      c1Content  <- readFile c1Path
      c2Content  <- readFile c2Path

      -- Processamento preliminar das palavras reservadas e separadores
      -- Nota: Linhas/espaços dentro do arquivo de separadores são ignorados como conteúdo útil.
      let resWords = words resContent
          seps     = filter (`notElem` " \t\n\r") sepContent

      -- Tokenização dos códigos fonte
      let tokens1 = tokenize seps c1Content
          tokens2 = tokenize seps c2Content

      -- Cálculo das frequências fi (f1 e f2)
      let f1Map = computeFrequencies resWords tokens1
          f2Map = computeFrequencies resWords tokens2

      -- Soma total de f1 para o denominador da similaridade
      let totalF1 = sum (M.elems f1Map)

      -- Regra de decisão para o cálculo acumulado de m
      -- Considera-se aceitável uma diferença de ATÉ 10% em relação ao maior valor absoluto entre f1 e f2.
      let calculateM w f1 =
            let f2 = M.findWithDefault 0.0 w f2Map
                diff = abs (f1 - f2)
                maxF = max f1 f2
            in if maxF > 0 && (diff / maxF <= 0.10)
               then f1
               else 0.0

      -- Somatório de m baseado nas chaves existentes em c1
      let mValue = sum [calculateM w f1 | (w, f1) <- M.toList f1Map]
          similarity = if totalF1 == 0 then 0.0 else mValue / totalF1

      -- Ordenação do relatório de c1: Frequência decrescente, desempate por ordem lexicográfica
      let c1Report = sortOn (\(w, f) -> (Down f, w)) (M.toList f1Map)

      -- Impressão do Relatório de Frequências de c1
      putStrLn "\n========================================================"
      putStrLn "       RELATÓRIO DE FREQUÊNCIAS (CÓDIGO 1 - c1)         "
      putStrLn "========================================================"
      printf "%-25s | %-20s\n" "Palavra" "Frequência Ponderada"
      putStrLn "--------------------------------------------------------"
      mapM_ (\(w, f) -> printf "%-25s | %-20.1f\n" w f) c1Report
      putStrLn "--------------------------------------------------------"

      -- Impressão dos Resultados Métricos
      putStrLn "\n========================================================"
      putStrLn "                 MÉTRICAS DE SIMILARIDADE               "
      putStrLn "========================================================"
      printf "Soma total de f1 (denominador): %.1f\n" totalF1
      printf "Valor acumulado de m:           %.1f\n" mValue
      printf "Índice de Similaridade:         %.4f (%.2f%%)\n" similarity (similarity * 100)
      putStrLn "========================================================\n"
