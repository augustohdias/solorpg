{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


--


module System.Util.Parser
  ( 
    parseAttributes,
    parseAttributeUpdate,
    parseAttributeAdd,

    
    parseResourceUpdate,
    parseResourceAdd,

    
    parseRank,
    rankToText,

    
    parseKeyValue,
    parseDecimal,
    parseSignedDecimal,
    parseQuotedString,
    parseOracleQuery,
    formatProgressTrack,
    buildChoicePromptPayload,
    encodeConsequencesToText,

    
    parseBondCommand,

    
    parseBonusCommand,

    
    clamp,
  )
where

import qualified Data.Aeson as Aeson
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified System.ConsequenceContract as Consequence
import qualified System.Constants as C
import qualified System.GameContext as GameContext
import qualified System.Progress as Progress
import System.Tui.Comm
  ( ChoiceOptionPayload (..),
    ChoicePromptPayload (..),
  )

formatProgressTrack :: Progress.ProgressTrack -> T.Text
formatProgressTrack track =
  let name = Progress.trackName track
      pType = case Progress.trackType track of
        Progress.Vow -> "Vow"
        Progress.Combat -> "Combat"
        Progress.Journey -> "Journey"
        Progress.Bond -> "Bond"
      rank = rankToText (Progress.trackRank track)
      boxes = Progress.getProgressScore track
      ticks = Progress.trackTicks track
      percentage = Progress.progressPercentage track
      completed = Progress.trackCompleted track
   in T.pack $ C.formatProgressTrack C.moveMessages name pType (T.unpack rank) boxes ticks percentage completed

buildChoicePromptPayload :: Int -> [Consequence.Choice] -> [Consequence.Consequence] -> ChoicePromptPayload
buildChoicePromptPayload promptIdSeed choices remaining =
  ChoicePromptPayload
    { choicePromptId = promptId,
      choicePromptTitle = "Escolha uma opção",
      choicePromptMessage = "Use ↑/↓ ou números para navegar. Enter confirma, ESC cancela.",
      choicePromptOptions = options
    }
  where
    promptId = T.pack $ "choice-" <> show (abs promptIdSeed)
    options =
      zipWith
        ( \idx Consequence.Choice {Consequence.choiceDescription, Consequence.choiceConsequences} ->
            ChoiceOptionPayload
              { choiceOptionIndex = idx,
                choiceOptionLabel = choiceDescription,
                choiceOptionConsequences = encodeConsequencesToText (choiceConsequences ++ remaining)
              }
        )
        [1 ..]
        choices

encodeConsequencesToText :: [Consequence.Consequence] -> T.Text
encodeConsequencesToText =
  TL.toStrict . TLE.decodeUtf8 . Aeson.encode


parseAttributes :: [T.Text] -> GameContext.Attributes
parseAttributes parts =
  let defVal = C.configDefaultAttributeValue C.defaultConfig
      defaultAttrs = GameContext.Attributes defVal defVal defVal defVal defVal
      updateAttr attr key val =
        case key of
          "iron" -> attr {GameContext.iron = val}
          "edge" -> attr {GameContext.edge = val}
          "heart" -> attr {GameContext.heart = val}
          "shadow" -> attr {GameContext.shadow = val}
          "wits" -> attr {GameContext.wits = val}
          _ -> attr
   in foldl
        ( \acc part ->
            case T.splitOn ":" part of
              [k, v] -> case TR.decimal v of
                Right (n, _) -> updateAttr acc k n
                _ -> acc
              _ -> acc
        )
        defaultAttrs
        parts


parseAttributeUpdate :: T.Text -> GameContext.Attributes -> Maybe GameContext.Attributes
parseAttributeUpdate input attrs = do
  (key, valStr) <- parseKeyValue input
  n <- parseDecimal valStr
  case key of
    "iron" -> Just $ attrs {GameContext.iron = n}
    "edge" -> Just $ attrs {GameContext.edge = n}
    "heart" -> Just $ attrs {GameContext.heart = n}
    "shadow" -> Just $ attrs {GameContext.shadow = n}
    "wits" -> Just $ attrs {GameContext.wits = n}
    _ -> Nothing


parseAttributeAdd :: T.Text -> GameContext.Attributes -> Maybe GameContext.Attributes
parseAttributeAdd input attrs = do
  (key, valStr) <- parseKeyValue input
  delta <- parseSignedDecimal valStr
  case key of
    "iron" -> Just $ attrs {GameContext.iron = GameContext.iron attrs + delta}
    "edge" -> Just $ attrs {GameContext.edge = GameContext.edge attrs + delta}
    "heart" -> Just $ attrs {GameContext.heart = GameContext.heart attrs + delta}
    "shadow" -> Just $ attrs {GameContext.shadow = GameContext.shadow attrs + delta}
    "wits" -> Just $ attrs {GameContext.wits = GameContext.wits attrs + delta}
    _ -> Nothing


parseResourceUpdate :: T.Text -> GameContext.Resources -> Maybe GameContext.Resources
parseResourceUpdate input res = do
  (key, valStr) <- parseKeyValue input
  n <- parseDecimal valStr
  case key of
    "spirit" -> Just $ res {GameContext.spirit = n}
    "health" -> Just $ res {GameContext.health = n}
    "supply" -> Just $ res {GameContext.supply = n}
    "momentum" -> Just $ res {GameContext.momentum = n}
    "experience" -> Just $ res {GameContext.experience = n}
    _ -> Nothing


parseResourceAdd :: T.Text -> GameContext.Resources -> Maybe GameContext.Resources
parseResourceAdd input res = do
  (key, valStr) <- parseKeyValue input
  delta <- parseSignedDecimal valStr
  case key of
    "spirit" -> Just $ res {GameContext.spirit = clamp 0 5 (GameContext.spirit res + delta)}
    "health" -> Just $ res {GameContext.health = clamp 0 5 (GameContext.health res + delta)}
    "supply" -> Just $ res {GameContext.supply = clamp 0 5 (GameContext.supply res + delta)}
    "momentum" -> Just $ res {GameContext.momentum = GameContext.momentum res + delta}
    "experience" -> Just $ res {GameContext.experience = max 0 (GameContext.experience res + delta)}
    _ -> Nothing

parseBondCommand :: T.Text -> Maybe GameContext.BondCommand
parseBondCommand input =
  case tokens of
    [] ->
      Just $ GameContext.BondCommand GameContext.ListBonds emptyBond
    [single] ->
      let name = stripQuotes single
          bond = emptyBond {GameContext.bondName = name}
       in Just $ GameContext.BondCommand (GameContext.ListBondNotes name) bond
    (token : _) ->
      let lowerToken = T.toLower token
       in case lowerToken of
            "add" -> buildAddCommand GameContext.AddBond GameContext.PersonBond argsText
            "addc" -> buildAddCommand GameContext.AddCommunityBond GameContext.CommunityBond argsText
            "remove" -> buildRemoveCommand argsText
            _ -> buildUpdateCommand trimmedInput
  where
    trimmedInput = T.strip input
    tokens = T.words trimmedInput
    argsText = dropFirstToken trimmedInput

    emptyBond =
      GameContext.Bond
        { GameContext.bondName = "",
          GameContext.bondType = GameContext.Undefined,
          GameContext.bondNotes = ""
        }

    buildAddCommand cmd bondType text = do
      (name, notes) <- parseNameAndNotes text
      let bond =
            GameContext.Bond
              { GameContext.bondName = name,
                GameContext.bondType = bondType,
                GameContext.bondNotes = notes
              }
      Just $ GameContext.BondCommand cmd bond

    buildRemoveCommand text = do
      (name, _) <- parseNameAndNotes text
      let bond = emptyBond {GameContext.bondName = name}
      Just $ GameContext.BondCommand GameContext.RemoveBond bond

    buildUpdateCommand text = do
      (name, notes) <- parseNameAndNotes text
      let bond =
            GameContext.Bond
              { GameContext.bondName = name,
                GameContext.bondType = GameContext.Undefined,
                GameContext.bondNotes = notes
              }
      Just $ GameContext.BondCommand (GameContext.UpdateBondNotes name) bond

    parseNameAndNotes raw
      | T.null trimmed = Nothing
      | T.isPrefixOf "\"" trimmed =
          case separateStrings trimmed of
            [] -> Nothing
            (nameSegment : rest) ->
              let notes = cleanNote (T.unwords rest)
               in Just (nameSegment, notes)
      | otherwise =
          case T.words trimmed of
            [] -> Nothing
            (nameToken : restTokens) ->
              let name = stripQuotes nameToken
                  notes = cleanNote (T.unwords restTokens)
               in Just (name, notes)
      where
        trimmed = T.strip raw

    dropFirstToken txt =
      let stripped = T.stripStart txt
          (_, rest) = T.break isSpace stripped
       in T.stripStart rest

    stripQuotes txt =
      fromMaybe stripped (T.stripPrefix "\"" stripped >>= T.stripSuffix "\"")
      where
        stripped = T.strip txt

    cleanNote = stripQuotes . T.strip


separateStrings :: T.Text -> [T.Text]
separateStrings texto = case T.uncons texto of
  Nothing -> []
  Just ('"', resto) ->
    let (str, resto') = T.break (== '"') resto
     in if T.null resto'
          then [str] 
          else str : separateStrings (T.tail resto')
  Just (_, resto) -> separateStrings resto


parseKeyValue :: T.Text -> Maybe (T.Text, T.Text)
parseKeyValue input =
  case T.splitOn ":" input of
    [k, v] -> Just (k, v)
    _ -> Nothing

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


clamp :: Int -> Int -> Int -> Int
clamp low high x = max low (min high x)



parseQuotedString :: T.Text -> Maybe (T.Text, T.Text)
parseQuotedString input =
  case T.stripPrefix "\"" (T.stripStart input) of
    Nothing -> Nothing
    Just afterFirstQuote ->
      case T.breakOn "\"" afterFirstQuote of
        (content, rest)
          | not (T.null rest) ->
              Just (content, T.drop 1 rest) 
        _ -> Nothing



parseOracleQuery :: T.Text -> (T.Text, T.Text)
parseOracleQuery input =
  case parseQuotedString input of
    Just (name, rest) -> (name, T.strip rest)
    Nothing ->
      let parts = T.words input
       in if null parts
            then ("", "")
            else (head parts, T.unwords (tail parts))


parseRank :: T.Text -> Maybe Progress.ChallengeRank
parseRank txt = case T.toLower txt of
  "troublesome" -> Just Progress.Troublesome
  "dangerous" -> Just Progress.Dangerous
  "formidable" -> Just Progress.Formidable
  "extreme" -> Just Progress.Extreme
  "epic" -> Just Progress.Epic
  _ -> case reads (T.unpack txt) of
    [(rank, "")] -> Just rank
    _ -> Nothing


rankToText :: Progress.ChallengeRank -> T.Text
rankToText Progress.Troublesome = "troublesome"
rankToText Progress.Dangerous = "dangerous"
rankToText Progress.Formidable = "formidable"
rankToText Progress.Extreme = "extreme"
rankToText Progress.Epic = "epic"







parseBonusCommand :: T.Text -> Maybe GameContext.ActiveBonus
parseBonusCommand input = do
  let parts = T.words input
  if null parts
    then Nothing
    else do
      let bonusTypeStr = T.toLower (head parts)
      bonusType <- parseBonusType bonusTypeStr
      if length parts < 2
        then Nothing
        else do
          let valueStr = parts !! 1
          value <- parseBonusValue valueStr
          let description = if length parts > 2
                            then T.unwords (drop 2 parts)
                            else T.pack $ case bonusType of
                                  GameContext.NextRoll -> "Bônus na próxima rolagem"
                                  GameContext.NextMove _ -> "Bônus no próximo move"
                                  GameContext.Persistent -> "Bônus permanente"
          Just $ GameContext.ActiveBonus bonusType value description
  where
    parseBonusType :: T.Text -> Maybe GameContext.BonusType
    parseBonusType txt
      | txt == "nextroll" = Just GameContext.NextRoll
      | txt == "persistent" = Just GameContext.Persistent
      | T.isPrefixOf "nextmove:" txt = 
          let moveName = T.drop (T.length "nextmove:") txt
          in Just (GameContext.NextMove moveName)
      | otherwise = Nothing
    
    parseBonusValue :: T.Text -> Maybe Int
    parseBonusValue txt = case TR.signed TR.decimal txt of
      Right (val, _) -> Just val
      Left _ -> Nothing
