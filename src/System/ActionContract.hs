module System.ActionContract
  ( Handle (..),
    ActionType (..),
    ActionRollResult (..),
  )
where

import qualified Data.Text as T

data ActionType
  = RollDice            -- ^ Rola dados (ex: "3d6,2d10")
  | AddStoryLog         -- ^ Adiciona entrada narrativa ao log
  | Show                -- ^ Mostra logs da sessão
  | Save                -- ^ Salva contexto atual
  | Exit                -- ^ Encerra sessão
  | CreateCharacter     -- ^ Cria novo personagem (ex: "NomePersonagem")
  | LoadCharacter       -- ^ Carrega personagem existente (ex: "NomePersonagem")
  | ShowCharacter       -- ^ Mostra informações do personagem atual
  | UpdateAttribute     -- ^ Define atributo (ex: "iron:3")
  | UpdateResource      -- ^ Define recurso (ex: "health:4")
  | AddAttribute        -- ^ Adiciona/remove atributo (ex: "iron:-1")
  | AddResource         -- ^ Adiciona/remove recurso (ex: "supply:-2")
  | Challenge           -- ^ Rola 1d6,2d10 e avalia resultado
  | Move                -- ^ Executa um Move de Ironsworn
  | SwearVow            -- ^ Jura um voto (cria progress track)
  | MarkProgress        -- ^ Marca progresso em track
  | RollProgress        -- ^ Faz progress roll
  | ShowTracks          -- ^ Mostra todos os tracks ativos
  | AbandonTrack        -- ^ Abandona/remove um track
  | Oracle              -- ^ Consulta oráculo
  | Help                -- ^ Mostra ajuda
  | Bond                -- ^ Gerencia bonds (vínculos)
  | Unknown             -- ^ Ação desconhecida
  deriving (Show, Eq)

data Handle = Handle
  { process :: ActionType -> T.Text -> IO Bool
  }

-- | Resultado de um Action Roll (Ironsworn)
data ActionRollResult
  = StrongHit    -- ^ Action > ambos challenge dice
  | WeakHit      -- ^ Action > apenas 1 challenge die
  | Miss         -- ^ Action <= ambos challenge dice
  | InvalidRoll  -- ^ Rolagem inválida
  deriving (Eq, Show)