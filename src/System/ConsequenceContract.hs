{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | ConsequenceContract - Sistema unificado de consequências para Moves e Oráculos
--
--     Este módulo define as consequências que podem ser aplicadas tanto por
--     Moves quanto por Oráculos, unificando o sistema e evitando dependências circulares.
--
--     Este módulo deve ser importado qualificado:
--     > import qualified System.ConsequenceContract as Consequence
module System.ConsequenceContract
  ( Consequence (..),
    Choice (..),
    MoveType (..),
    moveTypeToText,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified System.GameContext as GameContext

data MoveType
  = PayThePrice
  | AskTheOracle
  | FaceDanger
  | SecureAdvantage
  | GatherInformation
  | Heal
  | Resupply
  | MakeCamp
  | UndertakeJourney
  | ReachYourDestination
  | CompelAction
  | Sojourn
  | DrawTheCircle
  | ForgeABond
  | TestYourBond
  | AidYourAlly
  | WriteYourEpilogue
  | EnterTheFray
  | Strike
  | Clash
  | TurnTheTide
  | EndTheFight
  | Battle
  | EndurHarm
  | FaceDeath
  | EndurStress
  | FaceDesolation
  | OutOfSupply
  | FaceSetback
  | SwearIronVow
  | ReachMilestone
  | FulfillYourVow
  | ForsakeYourVow
  | Advance
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Consequence
  = LoseHealth Int
  | LoseSpirit Int
  | LoseSupply Int
  | LoseMomentum Int
  | GainHealth Int
  | GainSpirit Int
  | GainSupply Int
  | GainMomentum Int
  | TriggerMove MoveType
  | TriggerOracle T.Text
  | PlayerChoice [Choice]
  | Narrative T.Text
  | AddBonus GameContext.ActiveBonus
  | MarkBondProgress
  | MarkJourneyProgress
  | ImproveAsset T.Text
  | ChooseAssetImprovement T.Text
  | ImproveSpecificAssetSkill T.Text Int
  | PickAsset T.Text
  | AddAssetToPlayer T.Text
  | DeductExperience Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Choice = Choice
  { choiceDescription :: T.Text,
    choiceConsequences :: [Consequence]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

moveTypeToText :: MoveType -> T.Text
moveTypeToText PayThePrice = T.pack "PagarOPreco"
moveTypeToText AskTheOracle = T.pack "Oraculo"
moveTypeToText FaceDanger = T.pack "EnfrentarPerigo"
moveTypeToText SecureAdvantage = T.pack "SegurarVantagem"
moveTypeToText GatherInformation = T.pack "ColetarInformacao"
moveTypeToText Heal = T.pack "Curar"
moveTypeToText Resupply = T.pack "Reabastecer"
moveTypeToText MakeCamp = T.pack "FazerCampo"
moveTypeToText UndertakeJourney = T.pack "EmpreenderJornada"
moveTypeToText ReachYourDestination = T.pack "AlcancarDestino"
moveTypeToText CompelAction = T.pack "Convencer"
moveTypeToText Sojourn = T.pack "Sojourn"
moveTypeToText DrawTheCircle = T.pack "DesenharCirculo"
moveTypeToText ForgeABond = T.pack "ForjarVinculo"
moveTypeToText TestYourBond = T.pack "TestarVinculo"
moveTypeToText AidYourAlly = T.pack "AjudarAliado"
moveTypeToText WriteYourEpilogue = T.pack "EscreverEpilogo"
moveTypeToText EnterTheFray = T.pack "EntrarNoFronte"
moveTypeToText Strike = T.pack "Atacar"
moveTypeToText Clash = T.pack "Confrontar"
moveTypeToText TurnTheTide = T.pack "VirarMesa"
moveTypeToText EndTheFight = T.pack "TerminarBatalha"
moveTypeToText Battle = T.pack "Batalhar"
moveTypeToText EndurHarm = T.pack "EnfrentarDano"
moveTypeToText FaceDeath = T.pack "EnfrentarMorte"
moveTypeToText EndurStress = T.pack "EnfrentarEstresse"
moveTypeToText FaceDesolation = T.pack "EnfrentarDesolacao"
moveTypeToText OutOfSupply = T.pack "SemSuprimento"
moveTypeToText FaceSetback = T.pack "EnfrentarReves"
moveTypeToText SwearIronVow = T.pack "JurarVoto"
moveTypeToText ReachMilestone = T.pack "AlcancarMarco"
moveTypeToText FulfillYourVow = T.pack "CumprirVoto"
moveTypeToText ForsakeYourVow = T.pack "AbanarVoto"
moveTypeToText Advance = T.pack "Avancar"