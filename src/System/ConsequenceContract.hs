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
import qualified System.GameContextContract as GameContext
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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Opção de escolha para o jogador
data Choice = Choice
  { choiceDescription :: T.Text
  , choiceConsequences :: [Consequence]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Converte MoveType para texto legível
moveTypeToText :: MoveType -> T.Text
moveTypeToText PayThePrice = T.pack "Pay the Price"
moveTypeToText AskTheOracle = T.pack "Ask the Oracle"
moveTypeToText FaceDanger = T.pack "Face Danger"
moveTypeToText SecureAdvantage = T.pack "Secure an Advantage"
moveTypeToText GatherInformation = T.pack "Gather Information"
moveTypeToText Heal = T.pack "Heal"
moveTypeToText Resupply = T.pack "Resupply"
moveTypeToText MakeCamp = T.pack "Make Camp"
moveTypeToText UndertakeJourney = T.pack "Undertake a Journey"
moveTypeToText ReachYourDestination = T.pack "Reach Your Destination"
moveTypeToText CompelAction = T.pack "Compel"
moveTypeToText Sojourn = T.pack "Sojourn"
moveTypeToText DrawTheCircle = T.pack "Draw the Circle"
moveTypeToText ForgeABond = T.pack "Forge a Bond"
moveTypeToText TestYourBond = T.pack "Test Your Bond"
moveTypeToText AidYourAlly = T.pack "Aid Your Ally"
moveTypeToText WriteYourEpilogue = T.pack "Write Your Epilogue"
moveTypeToText EnterTheFray = T.pack "Enter the Fray"
moveTypeToText Strike = T.pack "Strike"
moveTypeToText Clash = T.pack "Clash"
moveTypeToText TurnTheTide = T.pack "Turn the Tide"
moveTypeToText EndTheFight = T.pack "End the Fight"
moveTypeToText Battle = T.pack "Battle"
moveTypeToText EndurHarm = T.pack "Endure Harm"
moveTypeToText FaceDeath = T.pack "Face Death"
moveTypeToText EndurStress = T.pack "Endure Stress"
moveTypeToText FaceDesolation = T.pack "Face Desolation"
moveTypeToText OutOfSupply = T.pack "Out of Supply"
moveTypeToText FaceSetback = T.pack "Face a Setback"
moveTypeToText SwearIronVow = T.pack "Swear an Iron Vow"
moveTypeToText ReachMilestone = T.pack "Reach a Milestone"
moveTypeToText FulfillYourVow = T.pack "Fulfill Your Vow"
moveTypeToText ForsakeYourVow = T.pack "Forsake Your Vow"
moveTypeToText Advance = T.pack "Advance"
