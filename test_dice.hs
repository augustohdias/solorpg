#!/usr/bin/env runghc
import qualified System.Dice as Dice
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "=== Teste do módulo Dice ==="
  
  
  putStrLn "\n1. Testando roll('3d6'):"
  results <- Dice.roll (T.pack "3d6")
  print results
  
  
  putStrLn "\n2. Testando roll('2d10,1d6'):"
  results2 <- Dice.roll (T.pack "2d10,1d6")
  print results2
  
  
  putStrLn "\n3. Testando challengeRoll():"
  eitherResult <- Dice.challengeRoll
  case eitherResult of
    Left err -> putStrLn $ "Erro: " ++ T.unpack err
    Right result -> print result
  
  putStrLn "\n=== Fim dos testes ==="

