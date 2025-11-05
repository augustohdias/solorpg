{-# LANGUAGE OverloadedStrings #-}
{- | MoveContract - Especificação do serviço de Moves do Ironsworn
     
     Este serviço gerencia a execução de Moves (movimentos) do Ironsworn,
     incluindo rolagens automáticas, avaliação de resultados e execução
     de consequências.
     
     Este módulo deve ser importado qualificado:
     > import qualified System.MoveContract as Move
-}
module System.MoveContract
  ( -- * Abstract handle
    Handle (..)
    
    -- * Pure types
  , MoveType (..)
  , Stat (..)
  , MoveResult (..)
  , Consequence (..)
  , Choice (..)
  
    -- * Derived functions
  , statToText
  , moveTypeToText
  ) where

import qualified Data.Text as T
import qualified System.GameContextContract as GameContext
import System.ActionContract (ActionRollResult)

-- | Tipos de Moves implementados
data MoveType
  -- Fate Moves
  = PayThePrice         -- ^ Sofra as consequências
  | AskTheOracle        -- ^ Consulte o oráculo
  
  -- Adventure Moves
  | FaceDanger          -- ^ Enfrente um perigo
  | SecureAdvantage     -- ^ Ganhe vantagem
  | GatherInformation   -- ^ Colete informações
  | Heal                -- ^ Recupere health
  | Resupply            -- ^ Recupere supply
  | MakeCamp            -- ^ Acampe e descanse
  | UndertakeJourney    -- ^ Viaje através de terras perigosas
  | ReachYourDestination  -- ^ Complete sua jornada (progress move)
  
  -- Relationship Moves
  | CompelAction        -- ^ Force alguém a agir
  | Sojourn             -- ^ Descanse em comunidade
  | DrawTheCircle       -- ^ Desafio ritual
  | ForgeABond          -- ^ Crie vínculo
  | TestYourBond        -- ^ Teste relacionamento
  | AidYourAlly         -- ^ Ajude aliado
  | WriteYourEpilogue   -- ^ Conclua sua história (progress move)
  
  -- Combat Moves
  | EnterTheFray        -- ^ Inicie combate
  | Strike              -- ^ Ataque em combate
  | Clash               -- ^ Combate defensivo
  | TurnTheTide         -- ^ Vire o jogo (baixo momentum)
  | EndTheFight         -- ^ Finalize o combate (progress move)
  | Battle              -- ^ Combate em larga escala
  
  -- Suffer Moves
  | EndurHarm           -- ^ Resista a dano
  | FaceDeath           -- ^ À beira da morte
  | EndurStress         -- ^ Resista a stress
  | FaceDesolation      -- ^ À beira do colapso
  | OutOfSupply         -- ^ Sem suprimentos
  | FaceSetback         -- ^ Revés em progress
  
  -- Quest Moves
  | SwearIronVow        -- ^ Jure um voto de ferro
  | ReachMilestone      -- ^ Alcance um marco
  | FulfillYourVow      -- ^ Complete seu voto (progress move)
  | ForsakeYourVow      -- ^ Abandone seu voto
  | Advance             -- ^ Ganhe XP e melhorias
  deriving (Eq, Show)

-- | Atributos que podem ser usados em Moves
data Stat
  = Iron
  | Edge
  | Heart
  | Shadow
  | Wits
  deriving (Eq, Show, Read)

-- | Resultado de execução de um Move
data MoveResult = MoveResult
  { moveExecuted :: MoveType
  , rollResult :: ActionRollResult
  , actionDie :: Int
  , challengeDice :: (Int, Int)
  , modifier :: Int
  , matchOccurred :: Bool
  , consequencesApplied :: [Consequence]
  } deriving (Show)

-- | Consequência de um Move
data Consequence
  = LoseHealth Int           -- ^ Perder saúde
  | LoseSpirit Int           -- ^ Perder spirit
  | LoseSupply Int           -- ^ Perder suprimento
  | LoseMomentum Int         -- ^ Perder momentum
  | GainHealth Int           -- ^ Ganhar saúde
  | GainSpirit Int           -- ^ Ganhar spirit
  | GainSupply Int           -- ^ Ganhar suprimento
  | GainMomentum Int         -- ^ Ganhar momentum
  | TriggerMove MoveType     -- ^ Executa outro move
  | TriggerOracle T.Text     -- ^ Executa oráculo e aplica consequência
  | PlayerChoice [Choice]    -- ^ Jogador deve escolher
  | Narrative T.Text         -- ^ Apenas narrativa
  | AddBonus GameContext.ActiveBonus  -- ^ Adiciona bônus ativo
  deriving (Eq, Show)

-- | Opção de escolha para o jogador
data Choice = Choice
  { choiceDescription :: T.Text
  , choiceConsequences :: [Consequence]
  } deriving (Eq, Show)

-- | Handle abstrato para o serviço de Moves
data Handle = Handle
  { -- | Executa um move com stat opcional e contexto
    executeMove :: !(MoveType -> Maybe Stat -> GameContext.Context -> GameContext.Handle -> IO (MoveResult, GameContext.Context))
    
    -- | Executa um move a partir de rolagem já feita
  , executeMoveWithRoll :: !(MoveType -> Maybe Stat -> Int -> (Int, Int) -> GameContext.Context -> GameContext.Handle -> IO (MoveResult, GameContext.Context))
  
    -- | Processa uma escolha do jogador
  , processChoice :: !(Choice -> GameContext.Context -> GameContext.Handle -> IO GameContext.Context)
  
    -- | Aplica consequências ao contexto (precisa do Handle para executar moves)
  , applyConsequences :: !(Handle -> [Consequence] -> GameContext.Context -> GameContext.Handle -> IO GameContext.Context)
  
    -- | Mostra menu de escolhas e obtém resposta
  , showChoices :: !([Choice] -> IO (Maybe Choice))
  
    -- | Parse nome de move a partir de texto
  , parseMoveType :: !(T.Text -> Maybe MoveType)
  
    -- | Parse stat a partir de texto
  , parseStat :: !(T.Text -> Maybe Stat)
  }

-- | Converte Stat para texto
statToText :: Stat -> T.Text
statToText Iron = "iron"
statToText Edge = "edge"
statToText Heart = "heart"
statToText Shadow = "shadow"
statToText Wits = "wits"

-- | Converte MoveType para texto legível
moveTypeToText :: MoveType -> T.Text
moveTypeToText PayThePrice = "Pay the Price"
moveTypeToText AskTheOracle = "Ask the Oracle"
moveTypeToText FaceDanger = "Face Danger"
moveTypeToText SecureAdvantage = "Secure an Advantage"
moveTypeToText GatherInformation = "Gather Information"
moveTypeToText Heal = "Heal"
moveTypeToText Resupply = "Resupply"
moveTypeToText MakeCamp = "Make Camp"
moveTypeToText UndertakeJourney = "Undertake a Journey"
moveTypeToText ReachYourDestination = "Reach Your Destination"
moveTypeToText CompelAction = "Compel"
moveTypeToText Sojourn = "Sojourn"
moveTypeToText DrawTheCircle = "Draw the Circle"
moveTypeToText ForgeABond = "Forge a Bond"
moveTypeToText TestYourBond = "Test Your Bond"
moveTypeToText AidYourAlly = "Aid Your Ally"
moveTypeToText WriteYourEpilogue = "Write Your Epilogue"
moveTypeToText EnterTheFray = "Enter the Fray"
moveTypeToText Strike = "Strike"
moveTypeToText Clash = "Clash"
moveTypeToText TurnTheTide = "Turn the Tide"
moveTypeToText EndTheFight = "End the Fight"
moveTypeToText Battle = "Battle"
moveTypeToText EndurHarm = "Endure Harm"
moveTypeToText FaceDeath = "Face Death"
moveTypeToText EndurStress = "Endure Stress"
moveTypeToText FaceDesolation = "Face Desolation"
moveTypeToText OutOfSupply = "Out of Supply"
moveTypeToText FaceSetback = "Face a Setback"
moveTypeToText SwearIronVow = "Swear an Iron Vow"
moveTypeToText ReachMilestone = "Reach a Milestone"
moveTypeToText FulfillYourVow = "Fulfill Your Vow"
moveTypeToText ForsakeYourVow = "Forsake Your Vow"
moveTypeToText Advance = "Advance"

