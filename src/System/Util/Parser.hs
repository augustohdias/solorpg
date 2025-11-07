{-# LANGUAGE OverloadedStrings #-}
{- | Módulo de funções utilitárias para parsing de texto.

     Este módulo contém funções auxiliares para parsear diferentes formatos
     de entrada de texto, como atributos, recursos, valores numéricos, etc.
-}
module System.Util.Parser
  ( -- * Parsing de atributos
    parseAttributes
  , parseAttributeUpdate
  , parseAttributeAdd
  
    -- * Parsing de recursos
  , parseResourceUpdate
  , parseResourceAdd
  
    -- * Parsing genérico
  , parseKeyValue
  , parseDecimal
  , parseSignedDecimal
  , parseQuotedString
  , parseOracleQuery
  
    -- * Funções auxiliares
  , clamp
  ) where

import qualified System.GameContextContract as GameContext
import qualified System.Constants as C
import qualified Data.Text as T
import qualified Data.Text.Read as TR

-- | Parse atributos de uma lista de textos (ex: ["iron:3", "edge:2"])
parseAttributes :: [T.Text] -> GameContext.Attributes
parseAttributes parts =
  let defVal = C.configDefaultAttributeValue C.defaultConfig
      defaultAttrs = GameContext.Attributes defVal defVal defVal defVal defVal
      updateAttr attr key val =
        case key of
          "iron" -> attr { GameContext.iron = val }
          "edge" -> attr { GameContext.edge = val }
          "heart" -> attr { GameContext.heart = val }
          "shadow" -> attr { GameContext.shadow = val }
          "wits" -> attr { GameContext.wits = val }
          _ -> attr
  in foldl (\acc part ->
        case T.splitOn ":" part of
          [k, v] -> case TR.decimal v of
            Right (n, _) -> updateAttr acc k n
            _ -> acc
          _ -> acc
      ) defaultAttrs parts

-- | Parse atualização de atributo
parseAttributeUpdate :: T.Text -> GameContext.Attributes -> Maybe GameContext.Attributes
parseAttributeUpdate input attrs = do
  (key, valStr) <- parseKeyValue input
  n <- parseDecimal valStr
  case key of
    "iron"   -> Just $ attrs { GameContext.iron = n }
    "edge"   -> Just $ attrs { GameContext.edge = n }
    "heart"  -> Just $ attrs { GameContext.heart = n }
    "shadow" -> Just $ attrs { GameContext.shadow = n }
    "wits"   -> Just $ attrs { GameContext.wits = n }
    _        -> Nothing

-- | Parse adição/remoção de atributo (delta com valores negativos)
parseAttributeAdd :: T.Text -> GameContext.Attributes -> Maybe GameContext.Attributes
parseAttributeAdd input attrs = do
  (key, valStr) <- parseKeyValue input
  delta <- parseSignedDecimal valStr
  case key of
    "iron"   -> Just $ attrs { GameContext.iron = GameContext.iron attrs + delta }
    "edge"   -> Just $ attrs { GameContext.edge = GameContext.edge attrs + delta }
    "heart"  -> Just $ attrs { GameContext.heart = GameContext.heart attrs + delta }
    "shadow" -> Just $ attrs { GameContext.shadow = GameContext.shadow attrs + delta }
    "wits"   -> Just $ attrs { GameContext.wits = GameContext.wits attrs + delta }
    _        -> Nothing

-- | Parse definição de recurso (apenas define valor absoluto)
parseResourceUpdate :: T.Text -> GameContext.Resources -> Maybe GameContext.Resources
parseResourceUpdate input res = do
  (key, valStr) <- parseKeyValue input
  n <- parseDecimal valStr
  case key of
    "spirit"     -> Just $ res { GameContext.spirit = n }
    "health"     -> Just $ res { GameContext.health = n }
    "supply"     -> Just $ res { GameContext.supply = n }
    "momentum"   -> Just $ res { GameContext.momentum = n }
    "experience" -> Just $ res { GameContext.experience = n }
    _            -> Nothing

-- | Parse adição/remoção de recurso (delta com valores negativos)
parseResourceAdd :: T.Text -> GameContext.Resources -> Maybe GameContext.Resources
parseResourceAdd input res = do
  (key, valStr) <- parseKeyValue input
  delta <- parseSignedDecimal valStr
  case key of
    "spirit"     -> Just $ res { GameContext.spirit     = clamp 0 5 (GameContext.spirit res + delta) }
    "health"     -> Just $ res { GameContext.health     = clamp 0 5 (GameContext.health res + delta) }
    "supply"     -> Just $ res { GameContext.supply     = clamp 0 5 (GameContext.supply res + delta) }
    "momentum"   -> Just $ res { GameContext.momentum   = GameContext.momentum res + delta }
    "experience" -> Just $ res { GameContext.experience = max 0 (GameContext.experience res + delta) }
    _            -> Nothing

-- | Funções auxiliares de parsing
parseKeyValue :: T.Text -> Maybe (T.Text, T.Text)
parseKeyValue input =
  case T.splitOn ":" input of
    [k, v] -> Just (k, v)
    _      -> Nothing

parseDecimal :: T.Text -> Maybe Int
parseDecimal valStr =
  case TR.decimal valStr of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

parseSignedDecimal :: T.Text -> Maybe Int
parseSignedDecimal valStr =
  case TR.signed TR.decimal valStr of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

-- | Limita um valor a um intervalo [low, high].
clamp :: Int -> Int -> Int -> Int
clamp low high x = max low (min high x)

-- | Parse string entre aspas duplas
-- Retorna (conteúdo, resto após as aspas)
parseQuotedString :: T.Text -> Maybe (T.Text, T.Text)
parseQuotedString input =
  case T.stripPrefix "\"" (T.stripStart input) of
    Nothing -> Nothing
    Just afterFirstQuote ->
      case T.breakOn "\"" afterFirstQuote of
        (content, rest) | not (T.null rest) ->
          Just (content, T.drop 1 rest)  -- Remove a segunda aspa
        _ -> Nothing

-- | Parse query de oráculo
-- Separa o nome do oráculo (que pode estar entre aspas) e um valor opcional
parseOracleQuery :: T.Text -> (T.Text, T.Text)
parseOracleQuery input =
  case parseQuotedString input of
    Just (name, rest) -> (name, T.strip rest)
    Nothing ->
      let parts = T.words input
      in if null parts
         then ("", "")
         else (head parts, T.unwords (tail parts))

