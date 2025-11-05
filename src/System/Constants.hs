{-# LANGUAGE OverloadedStrings #-}
{- | Módulo de constantes do SoloRPG
     
     Centraliza todas as strings e constantes do projeto para facilitar:
     - Manutenção
     - Tradução futura
     - Consistência nas mensagens
     - Evitar duplicação de código
-}
module System.Constants
  ( -- * Language Support
    Language(..)
  , getMessages
  , getMoveMessages
  , getHelpMessages
  , getErrorMessages
  , getProgressInterpretation
  , getChallengeInterpretation
  , currentLanguage
  
    -- * Mensagens do Sistema
  , Messages(..)
  , messages
  , messagesEN
  
    -- * Labels de Atributos e Recursos
  , AttributeLabels(..)
  , attributeLabels
  , ResourceLabels(..)
  , resourceLabels

    -- * Configurações
  , Config(..)
  , defaultConfig

    -- * Constantes de Jogo
  , GameConstants(..)
  , gameConstants

    -- * Mensagens de Moves
  , MoveMessages(..)
  , moveMessages

    -- * Help Messages
  , HelpMessages(..)
  , helpMessages

    -- * Error Messages
  , ErrorMessages(..)
  , errorMessages

    -- * Progress Interpretation
  , ProgressInterpretation(..)
  , progressInterpretation

    -- * Challenge Interpretation
  , ChallengeInterpretation(..)
  , challengeInterpretation

    -- * Character Display
  , CharacterDisplay(..)
  , characterDisplay
  ) where

import qualified Data.Text as T

-- | Idioma do sistema
data Language = PT | EN deriving (Eq, Show)

-- | Mensagens do sistema
data Messages = Messages
  { -- Mensagens de sucesso
    msgCharacterCreated :: T.Text
  , msgCharacterLoaded :: T.Text
  , msgSessionResumed :: T.Text
  , msgAttributeUpdated :: T.Text
  , msgResourceUpdated :: T.Text
  , msgSessionStarted :: T.Text
  , msgSessionEnded :: T.Text

    -- Mensagens de erro
  , msgNoCharacterLoaded :: T.Text
  , msgCharacterNameRequired :: T.Text
  , msgInvalidFormat :: T.Text
  , msgInvalidAttributeFormat :: T.Text
  , msgInvalidResourceFormat :: T.Text
  , msgErrorCreating :: T.Text
  , msgErrorLoading :: T.Text

    -- Mensagens informativas
  , msgDiceRolled :: T.Text
  , msgLogsLoaded :: T.Text
  , msgLogsHeader :: T.Text

    -- Cabeçalhos
  , msgGameHeader :: T.Text
  , msgGameWelcome :: T.Text
  , msgGameSeparator :: T.Text

    -- Seções da ficha
  , msgAttributesSection :: T.Text
  , msgResourcesSection :: T.Text
  , msgEventsSection :: T.Text
  }

-- | Mensagens em Português (padrão)
messages :: Messages
messages = Messages
  { -- Sucesso
    msgCharacterCreated = "Personagem criado: "
  , msgCharacterLoaded = "Personagem carregado: "
  , msgSessionResumed = "Sessão retomada com "
  , msgAttributeUpdated = "Atributo atualizado com sucesso"
  , msgResourceUpdated = "Recurso atualizado com sucesso"
  , msgSessionStarted = "Nova sessão iniciada"
  , msgSessionEnded = "Sessão encerrada"
  
    -- Erros
  , msgNoCharacterLoaded = "Nenhum personagem carregado"
  , msgCharacterNameRequired = "Erro: Nome do personagem não fornecido"
  , msgInvalidFormat = "Formato inválido"
  , msgInvalidAttributeFormat = "Formato inválido. Use: atributo:valor (ex: iron:3)"
  , msgInvalidResourceFormat = "Formato inválido. Use: recurso:valor (ex: health:4)"
  , msgErrorCreating = "Erro ao criar personagem: "
  , msgErrorLoading = "Erro ao carregar personagem: "
  
    -- Informativas
  , msgDiceRolled = "Dados rolados: "
  , msgLogsLoaded = "Carregados "
  , msgLogsHeader = "Logs:"
  
    -- Cabeçalhos
  , msgGameHeader = "================ SoloRPG ================"
  , msgGameWelcome = "Bem-vindo ao SoloRPG! Um RPG solo baseado em Ironsworn."
  , msgGameSeparator = "========================================="
  
    -- Seções
  , msgAttributesSection = "\nATRIBUTOS:"
  , msgResourcesSection = "\nRECURSOS:"
  , msgEventsSection = "\nÚLTIMOS EVENTOS:"
  }

-- | Mensagens em Inglês
messagesEN :: Messages
messagesEN = Messages
  { -- Success
    msgCharacterCreated = "Character created: "
  , msgCharacterLoaded = "Character loaded: "
  , msgSessionResumed = "Session resumed with "
  , msgAttributeUpdated = "Attribute updated successfully"
  , msgResourceUpdated = "Resource updated successfully"
  , msgSessionStarted = "New session started"
  , msgSessionEnded = "Session ended"
  
    -- Errors
  , msgNoCharacterLoaded = "No character loaded"
  , msgCharacterNameRequired = "Error: Character name not provided"
  , msgInvalidFormat = "Invalid format"
  , msgInvalidAttributeFormat = "Invalid format. Use: attribute:value (ex: iron:3)"
  , msgInvalidResourceFormat = "Invalid format. Use: resource:value (ex: health:4)"
  , msgErrorCreating = "Error creating character: "
  , msgErrorLoading = "Error loading character: "
  
    -- Informative
  , msgDiceRolled = "Dice rolled: "
  , msgLogsLoaded = "Loaded "
  , msgLogsHeader = "Logs:"
  
    -- Headers
  , msgGameHeader = "================ SoloRPG ================"
  , msgGameWelcome = "Welcome to SoloRPG! A solo RPG based on Ironsworn."
  , msgGameSeparator = "========================================="
  
    -- Sections
  , msgAttributesSection = "\nATTRIBUTES:"
  , msgResourcesSection = "\nRESOURCES:"
  , msgEventsSection = "\nRECENT EVENTS:"
  }

-- | Labels dos atributos
data AttributeLabels = AttributeLabels
  { labelIron :: T.Text
  , labelEdge :: T.Text
  , labelHeart :: T.Text
  , labelShadow :: T.Text
  , labelWits :: T.Text
  }

-- | Labels padrão dos atributos
attributeLabels :: AttributeLabels
attributeLabels = AttributeLabels
  { labelIron = "Iron (Ferro)"
  , labelEdge = "Edge (Gume)"
  , labelHeart = "Heart (Coração)"
  , labelShadow = "Shadow (Sombra)"
  , labelWits = "Wits (Astúcia)"
  }

-- | Labels dos recursos
data ResourceLabels = ResourceLabels
  { labelSpirit :: T.Text
  , labelHealth :: T.Text
  , labelSupply :: T.Text
  , labelMomentum :: T.Text
  , labelExperience :: T.Text
  }

-- | Labels padrão dos recursos
resourceLabels :: ResourceLabels
resourceLabels = ResourceLabels
  { labelSpirit = "Spirit (Espírito)"
  , labelHealth = "Health (Saúde)"
  , labelSupply = "Supply (Suprimento)"
  , labelMomentum = "Momentum (Impulso)"
  , labelExperience = "Experience (XP)"
  }

-- | Configurações do jogo
data Config = Config
  { -- Arquivos
    configSessionLogFile :: FilePath
  , configContextFileExtension :: String

    -- Limites
  , configMaxCharacterNameLength :: Int
  , configMaxEventsToShow :: Int

    -- Valores padrão
  , configDefaultAttributeValue :: Int
  , configDefaultResourceSpirit :: Int
  , configDefaultResourceHealth :: Int
  , configDefaultResourceSupply :: Int
  , configDefaultResourceMomentum :: Int
  , configDefaultResourceExperience :: Int
  }

-- | Configuração padrão do jogo
defaultConfig :: Config
defaultConfig = Config
  { -- Arquivos
    configSessionLogFile = "session.log"
  , configContextFileExtension = ".slg"

    -- Limites
  , configMaxCharacterNameLength = 100
  , configMaxEventsToShow = 5

    -- Valores padrão
  , configDefaultAttributeValue = 2
  , configDefaultResourceSpirit = 5
  , configDefaultResourceHealth = 5
  , configDefaultResourceSupply = 5
  , configDefaultResourceMomentum = 2
  , configDefaultResourceExperience = 0
  }

-- | Constantes do jogo
data GameConstants = GameConstants
  { -- Prefixos de log
    logPrefixCharacterCreated :: T.Text
  , logPrefixSessionResumed :: T.Text
  , logPrefixDiceRolled :: T.Text
  , logPrefixAttributeUpdated :: T.Text
  , logPrefixResourceUpdated :: T.Text

    -- Formato
  , formatTimestamp :: String
  , formatLogEntry :: T.Text -> T.Text -> T.Text  -- timestamp -> message -> formatted
  }

-- | Mensagens de moves
data MoveMessages = MoveMessages
  { -- Progress Moves
    msgVowSworn :: T.Text
  , msgProgressMarked :: T.Text
  , msgTrackRemoved :: T.Text
  , msgTrackNotFound :: T.Text
  , msgVowUsage :: String
  , msgProgressUsage :: String
  , msgNoTracksActive :: String
  , msgTracksHeader :: String
  , msgExecutingPayThePrice :: String

    -- Oracle
  , msgOracleResult :: T.Text
  , msgNoOraclesLoaded :: T.Text
  , msgOracleNotFound :: T.Text
  , msgOracleUsage :: String
  , msgOraclesHeader :: String

    -- Formatters
  , formatOracleRoll :: T.Text -> Int -> T.Text -> String
  , formatOracleIndex :: T.Text -> Int -> T.Text -> String
  , formatVowCreated :: T.Text -> String -> Int -> String
  , formatProgressTrack :: T.Text -> String -> String -> Int -> Int -> Double -> Bool -> String
  , formatOracleComplete :: T.Text -> T.Text -> T.Text -> [(Int, Int, T.Text)] -> String
  , msgUndertakeJourney :: String
  }

-- | Character display messages
data CharacterDisplay = CharacterDisplay
  { formatCharacterSheet :: T.Text -> [(String, Int)] -> [(String, Int)] -> [T.Text] -> Int -> String
  , formatActionRoll :: Int -> Int -> Int -> String -> String
  }

-- | Mensagens padrão de moves
moveMessages :: MoveMessages
moveMessages = MoveMessages
  { msgVowSworn = "✓ Voto criado: "
  , msgProgressMarked = "Marco alcançado! Progresso marcado."
  , msgTrackRemoved = "Track removido: "
  , msgTrackNotFound = "Track não encontrado: "
  , msgVowUsage = "Uso: :vow \"<nome>\" <rank>\n\
                  \Ranks: troublesome, dangerous, formidable, extreme, epic\n\
                  \Exemplo: :vow \"Vingar meu pai\" dangerous"
  , msgProgressUsage = "Use :tracks para ver tracks ativos"
  , msgNoTracksActive = "\nNenhum progress track ativo."
  , msgTracksHeader = "\n=== Progress Tracks Ativos ==="
  , msgExecutingPayThePrice = "\n>>> Executando Pay the Price..."

  , msgOracleResult = "🔮 Oráculo: "
  , msgNoOraclesLoaded = "Nenhum oráculo carregado."
  , msgOracleNotFound = "Oráculo não encontrado: "
  , msgOracleUsage = "Uso: :oracle \"Nome do Oráculo\" [valor]\n\
                     \:oracle - Lista oráculos disponíveis\n\
                     \:oracle \"Nome\" - Rola automaticamente\n\
                     \:oracle \"Nome\" 42 - Consulta valor específico"
  , msgOraclesHeader = "\n=== Oráculos Disponíveis ==="

  -- Formatters
  , formatOracleRoll = \name roll text -> unlines
      [ ""
      , "🔮 Oráculo: " ++ T.unpack name
      , "Rolagem: " ++ show roll
      , "→ " ++ T.unpack text
      ]
  , formatOracleIndex = \name idx text -> unlines
      [ ""
      , "🔮 Oráculo: " ++ T.unpack name
      , "Índice: " ++ show idx
      , "Resultado: " ++ T.unpack text
      ]
  , formatVowCreated = \name rank ticks -> unlines
      [ ""
      , "✓ Voto criado: " ++ T.unpack name ++ " (" ++ rank ++ ")"
      , "  Progresso por mark: " ++ show ticks ++ " ticks"
      ]
  , formatProgressTrack = \name pType rank boxes ticks percentage completed ->
      let status = if completed then " [COMPLETO]" else ""
          bar = replicate boxes '█' ++ replicate (10 - boxes) '░'
      in unlines
      [ ""
      , "• " ++ T.unpack name ++ status
      , "  Tipo: " ++ pType
      , "  Rank: " ++ rank
      , "  Progresso: " ++ show boxes ++ "/10 boxes (" ++ show ticks ++ "/40 ticks)"
      , "  " ++ bar ++ " " ++ show (round percentage :: Int) ++ "%"
      ]
  , msgUndertakeJourney = "Undertake a Journey requer journey track ativo\n\
                          \Use :vow para criar journey primeiro ou especifique nome"
  , formatOracleComplete = \name desc dice entries ->
      let entryLines = map (\(low, high, text) ->
            "  " ++ show low ++ "-" ++ show high ++ ": " ++ T.unpack text) entries
      in unlines ([ ""
            , "=== " ++ T.unpack name ++ " ==="
            , T.unpack desc
            , "Dado: " ++ T.unpack dice
            , ""
            , "Entradas:"
            ] ++ entryLines)
  }

characterDisplay :: CharacterDisplay
characterDisplay = CharacterDisplay
  { formatCharacterSheet = \name attrs resources logs maxEvents ->
      let attrLines = map (\(label, val) -> "  " ++ label ++ ": " ++ show val) attrs
          resLines = map (\(label, val) -> "  " ++ label ++ ": " ++ show val) resources
          eventLines = if null logs then [] else ["", "ÚLTIMOS EVENTOS:"] ++ map (("  " ++) . T.unpack) (take maxEvents logs)
      in unlines $ concat
          [ ["", "=== " ++ T.unpack name ++ " ===", "", "ATRIBUTOS:"]
          , attrLines
          , ["", "RECURSOS:"]
          , resLines
          , eventLines
          , [""]
          ]
  , formatActionRoll = \actionDie ch1 ch2 resultMsg ->
      unlines
        [ ""
        , "=== Action Roll ==="
        , "Action Die: " ++ show actionDie
        , "Challenge Dice: " ++ show ch1 ++ ", " ++ show ch2
        , ""
        , "Resultado: " ++ resultMsg
        ]
  }

-- | Help messages
data HelpMessages = HelpMessages
  { msgMoveUsage :: String
  , msgRanksAvailable :: String
  , msgMovesAvailable :: String
  }

helpMessages :: HelpMessages
helpMessages = HelpMessages
  { msgMoveUsage = "Uso: :move <nome> [stat]\n\
                   \Exemplo: :move Face Danger edge"
  , msgRanksAvailable = "Ranks válidos: troublesome, dangerous, formidable, extreme, epic"
  , msgMovesAvailable = unlines
      [ "Moves disponíveis:"
      , "  Adventure: Face Danger, Gather Information, Secure Advantage, Heal, Resupply, Make Camp"
      , "  Combat: Enter the Fray, Strike, Clash, Turn the Tide, End the Fight"
      , "  Quest: Swear Vow, Fulfill Vow, Reach Milestone, Forsake Vow"
      , "  Suffer: Endure Harm, Face Death, Endure Stress, Face Desolation"
      , "  Fate: Pay the Price, Ask the Oracle"
      ]
  }

-- | Error messages
data ErrorMessages = ErrorMessages
  { errChallengeRequiresChaining :: String
  , errMoveNameRequired :: String
  , errMoveUnknown :: String
  , errVowNotFound :: String
  , errJourneyNotFound :: String
  , errCombatNotFound :: String
  , errInvalidRank :: String
  , errInvalidValue :: String
  , errOracleError :: String
  }

errorMessages :: ErrorMessages
errorMessages = ErrorMessages
  { errChallengeRequiresChaining = "Erro: :challenge só funciona via chaining.\n\
                                   \Use: :roll 1d6,2d10 :over :challenge"
  , errMoveNameRequired = "Erro: Nome do move não fornecido"
  , errMoveUnknown = "Move desconhecido: "
  , errVowNotFound = "Voto não encontrado: "
  , errJourneyNotFound = "Jornada não encontrada: "
  , errCombatNotFound = "Combate não encontrado: "
  , errInvalidRank = "Rank inválido: "
  , errInvalidValue = "Valor inválido. Use número inteiro."
  , errOracleError = "Erro: "
  }

-- | Progress roll interpretation messages
data ProgressInterpretation = ProgressInterpretation
  { -- Fulfill Your Vow
    vowStrongHit :: String
  , vowWeakHit :: String
  , vowMiss :: String

    -- End the Fight
  , combatStrongHit :: String
  , combatWeakHit :: String
  , combatMiss :: String

    -- Reach Your Destination
  , journeyStrongHit :: String
  , journeyWeakHit :: String
  , journeyMiss :: String

    -- General
  , rollError :: String
  }

progressInterpretation :: ProgressInterpretation
progressInterpretation = ProgressInterpretation
  { vowStrongHit = unlines
      [ "✓ STRONG HIT"
      , "Seu voto está cumprido. Marque experiência (rank do voto)."
      , "Você pode Forge a Bond (se apropriado)."
      ]
  , vowWeakHit = unlines
      [ "~ WEAK HIT"
      , "Seu voto está cumprido, mas há uma complicação."
      , "Marque experiência (rank -1, mínimo 1)."
      , "Escolha um:"
      , "  • Faça um voto relacionado (formidable+)"
      , "  • Adicione complicação à narrativa"
      ]
  , vowMiss = unlines
      [ "✗ MISS"
      , "Seu voto ainda não está cumprido."
      , "Limpe todo o progresso e escolha:"
      , "  • Reafirme o voto: sofra -2 spirit"
      , "  • Forsake Your Vow"
      ]

  , combatStrongHit = unlines
      [ "✓ STRONG HIT"
      , "O combate termina. Você vence decisivamente."
      ]
  , combatWeakHit = unlines
      [ "~ WEAK HIT"
      , "O combate termina, mas escolha um:"
      , "  • Sofra harm mas vence"
      , "  • Vitória Pyrrhica (complicação narrativa)"
      ]
  , combatMiss = unlines
      [ "✗ MISS"
      , "Você falha em encerrar o combate."
      , "Pay the Price - situação piorou!"
      ]

  , journeyStrongHit = unlines
      [ "✓ STRONG HIT"
      , "Você completa sua jornada."
      , "+1 momentum (se tiver bonds no destino)"
      ]
  , journeyWeakHit = unlines
      [ "~ WEAK HIT"
      , "Você chega, mas há uma complicação ou custo."
      ]
  , journeyMiss = unlines
      [ "✗ MISS"
      , "Você se perde, sofre um revés, ou a situação piora."
      , "Pay the Price"
      ]

  , rollError = "Erro na rolagem"
  }

-- | Challenge interpretation messages
data ChallengeInterpretation = ChallengeInterpretation
  { challengeHeader :: String
  , challengeStrongHit :: String
  , challengeWeakHit :: String
  , challengeMiss :: String
  , challengeMatch :: String
  , challengeExpects3Dice :: String
  }

challengeInterpretation :: ChallengeInterpretation
challengeInterpretation = ChallengeInterpretation
  { challengeHeader = "\n=== Action Roll ==="
  , challengeStrongHit = "✓ SUCESSO TOTAL! Você consegue o que quer."
  , challengeWeakHit = "~ SUCESSO PARCIAL. Você consegue, mas há um custo."
  , challengeMiss = "✗ FALHA. As coisas pioram."
  , challengeMatch = "\n⚠ MATCH! Algo inesperado acontece!"
  , challengeExpects3Dice = "Erro: :challenge espera exatamente 3 dados (1d6,2d10)"
  }

-- | Idioma atual (configurável)
currentLanguage :: Language
currentLanguage = PT  -- Mude para EN para inglês

-- | Obtém mensagens baseado no idioma
getMessages :: Language -> Messages
getMessages PT = messages
getMessages EN = messagesEN

getMoveMessages :: Language -> MoveMessages
getMoveMessages PT = moveMessages
getMoveMessages EN = moveMessagesEN

getHelpMessages :: Language -> HelpMessages
getHelpMessages PT = helpMessages
getHelpMessages EN = helpMessagesEN

getErrorMessages :: Language -> ErrorMessages
getErrorMessages PT = errorMessages
getErrorMessages EN = errorMessagesEN

getProgressInterpretation :: Language -> ProgressInterpretation
getProgressInterpretation PT = progressInterpretation
getProgressInterpretation EN = progressInterpretationEN

getChallengeInterpretation :: Language -> ChallengeInterpretation
getChallengeInterpretation PT = challengeInterpretation
getChallengeInterpretation EN = challengeInterpretationEN

-- | Constantes padrão do jogo
gameConstants :: GameConstants
gameConstants = GameConstants
  { -- Prefixos
    logPrefixCharacterCreated = "Personagem criado: "
  , logPrefixSessionResumed = "Sessão retomada com "
  , logPrefixDiceRolled = "Dados rolados: "
  , logPrefixAttributeUpdated = "Atributo atualizado: "
  , logPrefixResourceUpdated = "Recurso atualizado: "
  
    -- Formato
  , formatTimestamp = "%Y-%m-%d %H:%M:%S"
  , formatLogEntry = \timestamp msg -> T.concat ["[", timestamp, "] ", msg]
  }

-- Versões EN dos outros tipos serão adicionadas conforme necessário
moveMessagesEN :: MoveMessages
moveMessagesEN = moveMessages  -- TODO: Traduzir

helpMessagesEN :: HelpMessages
helpMessagesEN = helpMessages  -- TODO: Traduzir

errorMessagesEN :: ErrorMessages
errorMessagesEN = errorMessages  -- TODO: Traduzir

progressInterpretationEN :: ProgressInterpretation
progressInterpretationEN = progressInterpretation  -- TODO: Traduzir

challengeInterpretationEN :: ChallengeInterpretation
challengeInterpretationEN = challengeInterpretation  -- TODO: Traduzir