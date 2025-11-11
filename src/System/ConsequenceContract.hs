{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{- | ConsequenceContract - Sistema unificado de consequências para Moves e Oráculos
     
     Este módulo define as consequências que podem ser aplicadas tanto por
     Moves quanto por Oráculos, unificando o sistema e evitando dependências circulares.
     
     Este módulo deve ser importado qualificado:
     > import qualified System.ConsequenceContract as Consequence
-}
module System.ConsequenceContract
  ( -- * Pure types
    Consequence (..)
  , Choice (..)
  , MoveType (..)
    -- * Derived functions
  , moveTypeToText
  ) where

import qualified Data.Text as T
import qualified System.GameContext as GameContext
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Tipos de Moves (copiado de MoveContract para evitar dependência circular)
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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Consequência de um Move ou Oráculo
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
  | MarkBondProgress         -- ^ Marca 1 tick na progress track de bonds
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Opção de escolha para o jogador
data Choice = Choice
  { choiceDescription :: T.Text
  , choiceConsequences :: [Consequence]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Converte MoveType para texto legível
moveTypeToText :: MoveType -> T.Text
moveTypeToText PayThePrice = T.pack "PayThePrice"
moveTypeToText AskTheOracle = T.pack "AskTheOracle"
moveTypeToText FaceDanger = T.pack "FaceDanger"
moveTypeToText SecureAdvantage = T.pack "SecureAdvantage"
moveTypeToText GatherInformation = T.pack "GatherInformation"
moveTypeToText Heal = T.pack "Heal"
moveTypeToText Resupply = T.pack "Resupply"
moveTypeToText MakeCamp = T.pack "MakeCamp"
moveTypeToText UndertakeJourney = T.pack "UndertakeJourney"
moveTypeToText ReachYourDestination = T.pack "ReachYourDestination"
moveTypeToText CompelAction = T.pack "Compel"
moveTypeToText Sojourn = T.pack "Sojourn"
moveTypeToText DrawTheCircle = T.pack "DrawTheCircle"
moveTypeToText ForgeABond = T.pack "ForgeaBond"
moveTypeToText TestYourBond = T.pack "TestYourBond"
moveTypeToText AidYourAlly = T.pack "AidYourAlly"
moveTypeToText WriteYourEpilogue = T.pack "WriteYourEpilogue"
moveTypeToText EnterTheFray = T.pack "EnterTheFray"
moveTypeToText Strike = T.pack "Strike"
moveTypeToText Clash = T.pack "Clash"
moveTypeToText TurnTheTide = T.pack "TurnTheTide"
moveTypeToText EndTheFight = T.pack "EndTheFight"
moveTypeToText Battle = T.pack "Battle"
moveTypeToText EndurHarm = T.pack "EndureHarm"
moveTypeToText FaceDeath = T.pack "FaceDeath"
moveTypeToText EndurStress = T.pack "EndurStress"
moveTypeToText FaceDesolation = T.pack "FaceDesolation"
moveTypeToText OutOfSupply = T.pack "OutOfSupply"
moveTypeToText FaceSetback = T.pack "FaceSetback"
moveTypeToText SwearIronVow = T.pack "SwearIronVow"
moveTypeToText ReachMilestone = T.pack "ReachMilestone"
moveTypeToText FulfillYourVow = T.pack "FulfillYourVow"
moveTypeToText ForsakeYourVow = T.pack "ForsakeYourVow"
moveTypeToText Advance = T.pack "Advance"