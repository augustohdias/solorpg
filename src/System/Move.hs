{-# LANGUAGE OverloadedStrings #-}

module System.Move
  ( Stat (..),
    executeMove,
    executeMoveWithRoll,
    parseMoveType,
    parseStat,
    statToText,
  )
where

import qualified Data.Text as T
import System.ConsequenceContract (Choice (..), Consequence (..), MoveType (..))
import qualified System.ConsequenceContract as C
import qualified System.Dice as Dice
import qualified System.GameContext as GameContext
import qualified System.Progress as Progress

data Stat
  = Iron
  | Edge
  | Heart
  | Shadow
  | Wits
  deriving (Eq, Show, Read)

statToText :: Stat -> T.Text
statToText Iron = "iron"
statToText Edge = "edge"
statToText Heart = "heart"
statToText Shadow = "shadow"
statToText Wits = "wits"

executeMove :: MoveType -> Maybe Stat -> GameContext.Attributes -> GameContext.Resources -> IO [Consequence]
executeMove moveType maybeStat attrs resources = do
  rolls <- Dice.roll (T.pack "1d6,2d10")

  case rolls of
    [(_, actionDie), (_, ch1), (_, ch2)] ->
      executeMoveWithRoll moveType maybeStat actionDie (ch1, ch2) attrs resources
    _ -> do
      let errorMsg = "Erro na rolagem de dados. Dados rolados: " <> T.pack (show rolls)
      return [Narrative errorMsg]

executeMoveWithRoll :: MoveType -> Maybe Stat -> Int -> (Int, Int) -> GameContext.Attributes -> GameContext.Resources -> IO [Consequence]
executeMoveWithRoll Advance _ _ (_, _) _ _ = do
  consequences <- getMoveConsequences Advance Dice.StrongHit True
  let rollInfo = Narrative (T.pack $ "\n>>> " ++ T.unpack (C.moveTypeToText Advance) ++ " <<<\n")
  return $ rollInfo : consequences
executeMoveWithRoll EndTheFight _ actionDie (ch1, ch2) _ _ = do
  -- Para EndTheFight, o actionDie na verdade representa o progress score (boxes preenchidas)
  let progressScore = actionDie  -- Número de boxes preenchidas na combat track
  let rollResult = Progress.evaluateProgressRoll progressScore ch1 ch2
  let isMatch = ch1 == ch2

  consequences <- getMoveConsequences EndTheFight rollResult isMatch

  let rollInfo =
        Narrative $
          T.pack $
            "\n>>> "
              ++ T.unpack (C.moveTypeToText EndTheFight)
              ++ " <<<\n"
              ++ "Progress Score: "
              ++ show progressScore
              ++ " boxes\n"
              ++ "Challenge Dice: "
              ++ show ch1
              ++ ", "
              ++ show ch2
              ++ "\n"
              ++ "Resultado: "
              ++ showRollResult rollResult
              ++ (if isMatch then "\n[!]" else "")

  return $ rollInfo : consequences
executeMoveWithRoll ReachYourDestination _ actionDie (ch1, ch2) _ _ = do
  -- Para ReachYourDestination, o actionDie representa o progress score (boxes preenchidas)
  let progressScore = actionDie  -- Número de boxes preenchidas na journey track
  let rollResult = Progress.evaluateProgressRoll progressScore ch1 ch2
  let isMatch = ch1 == ch2

  consequences <- getMoveConsequences ReachYourDestination rollResult isMatch

  let rollInfo =
        Narrative $
          T.pack $
            "\n>>> "
              ++ T.unpack (C.moveTypeToText ReachYourDestination)
              ++ " <<<\n"
              ++ "Progress Score: "
              ++ show progressScore
              ++ " boxes\n"
              ++ "Challenge Dice: "
              ++ show ch1
              ++ ", "
              ++ show ch2
              ++ "\n"
              ++ "Resultado: "
              ++ showRollResult rollResult
              ++ (if isMatch then "\n[!]" else "")

  return $ rollInfo : consequences
executeMoveWithRoll moveType maybeStat actionDie (ch1, ch2) attrs _resources = do
  let statModifier = case maybeStat of
        Just stat -> getStatValue stat attrs
        Nothing -> 0

  -- Obter contexto atual para calcular bônus aplicáveis
  maybeCtx <- GameContext.getCurrentContext
  let bonusModifier = case maybeCtx of
        Nothing -> 0
        Just ctx -> 
          let moveName = C.moveTypeToText moveType
              applicableBonuses = GameContext.getApplicableBonuses ctx (Just moveName)
              totalBonus = sum $ map GameContext.bonusValue applicableBonuses
          in totalBonus

  let totalModifier = statModifier + bonusModifier
  let actionTotal = actionDie + totalModifier
  let rollResult = evaluateRoll actionTotal ch1 ch2
  let isMatch = ch1 == ch2

  consequences <- getMoveConsequences moveType rollResult isMatch

  -- Consumir bônus aplicáveis após o movimento
  case maybeCtx of
    Nothing -> return ()
    Just ctx -> do
      let moveName = C.moveTypeToText moveType
      _ <- GameContext.consumeBonuses ctx (Just moveName)
      return ()

  let bonusInfo = if bonusModifier > 0 
        then " (stat: +" ++ show statModifier ++ ", bonus: +" ++ show bonusModifier ++ ")"
        else " (stat: +" ++ show statModifier ++ ")"
  
  let rollInfo =
        Narrative $
          T.pack $
            "\n>>> "
              ++ T.unpack (C.moveTypeToText moveType)
              ++ " <<<\n"
              ++ "Action Die: "
              ++ show actionDie
              ++ " + "
              ++ show totalModifier
              ++ bonusInfo
              ++ " = "
              ++ show actionTotal
              ++ "\n"
              ++ "Challenge Dice: "
              ++ show ch1
              ++ ", "
              ++ show ch2
              ++ "\n"
              ++ "Resultado: "
              ++ showRollResult rollResult
              ++ (if isMatch then "\n[!]" else "")

  return $ rollInfo : consequences

getStatValue :: Stat -> GameContext.Attributes -> Int
getStatValue stat attrs = case stat of
  Iron -> GameContext.iron attrs
  Edge -> GameContext.edge attrs
  Heart -> GameContext.heart attrs
  Shadow -> GameContext.shadow attrs
  Wits -> GameContext.wits attrs

evaluateRoll :: Int -> Int -> Int -> Dice.RollResult
evaluateRoll action ch1 ch2
  | action > ch1 && action > ch2 = Dice.StrongHit
  | action > ch1 || action > ch2 = Dice.WeakHit
  | otherwise = Dice.Miss

showRollResult :: Dice.RollResult -> String
showRollResult Dice.StrongHit = "[+] Sucesso Total!"
showRollResult Dice.WeakHit = "[~] Sucesso Parcial!"
showRollResult Dice.Miss = "[X] Falha!"
showRollResult Dice.InvalidRoll = "INVALID"

getMoveConsequences :: MoveType -> Dice.RollResult -> Bool -> IO [Consequence]
getMoveConsequences moveType result isMatch = do
  let baseConsequences = case moveType of
        PayThePrice -> getPayThePriceConsequences result isMatch
        AskTheOracle -> getAskOracleConsequences result
        FaceDanger -> getFaceDangerConsequences result isMatch
        GatherInformation -> getGatherInformationConsequences result isMatch
        SecureAdvantage -> getSecureAdvantageConsequences result
        Heal -> getHealConsequences result
        Resupply -> getResupplyConsequences result
        MakeCamp -> getMakeCampConsequences result
        UndertakeJourney -> getUndertakeJourneyConsequences result
        CompelAction -> getCompelConsequences result
        Sojourn -> getSojournConsequences result
        ForgeABond -> getForgeABondConsequences result
        TestYourBond -> getTestYourBondConsequences result
        AidYourAlly -> getAidYourAllyConsequences result
        EnterTheFray -> getEnterTheFrayConsequences result
        Strike -> getStrikeConsequences result
        Clash -> getClashConsequences result
        TurnTheTide -> getTurnTheTideConsequences result
        EndTheFight -> getEndTheFightConsequences result
        EndurHarm -> getEndurHarmConsequences result
        FaceDeath -> getFaceDeathConsequences result
        EndurStress -> getEndurStressConsequences result
        FaceDesolation -> getFaceDesolationConsequences result
        OutOfSupply -> getOutOfSupplyConsequences result
        FaceSetback -> getFaceSetbackConsequences result
        SwearIronVow -> getSwearIronVowConsequences result
        ReachMilestone -> getReachMilestoneConsequences result
        ForsakeYourVow -> getForsakeVowConsequences result
        Advance -> getAdvanceConsequences result
        ReachYourDestination -> getReachDestinationConsequences result
        _ -> getDefaultConsequences result

  return baseConsequences

getPayThePriceConsequences :: Dice.RollResult -> Bool -> [Consequence]
getPayThePriceConsequences _result _isMatch =
  [ PlayerChoice
      [ Choice
          "Fazer o resultado negativo mais óbvio acontecer (escolha narrativa)"
          [Narrative "[Acontece o que eu temia...]"],
        Choice
          "Visualizar dois resultados e usar Ask the Oracle (sim/não)"
          [ Narrative "[Role 1d2: 1=primeiro resultado, 2=segundo resultado]",
            TriggerOracle "Default"
          ],
        Choice
          "Rolar na tabela Pagar o Preço"
          [TriggerOracle "\"Pagar o Preço\""]
      ]
  ]

getFaceDangerConsequences :: Dice.RollResult -> Bool -> [Consequence]
getFaceDangerConsequences result isMatch =
  let matchPenalty = ([LoseMomentum 1 | isMatch])
   in matchPenalty ++ case result of
        Dice.StrongHit ->
          [ GainMomentum 1,
            Narrative "[Você é bem-sucedido.]"
          ]
        Dice.WeakHit ->
          [ PlayerChoice
              [ Choice
                  "Delayed, lose advantage, or face new danger (-1 momentum)"
                  [LoseMomentum 1],
                Choice
                  "You are tired or hurt (Endure Harm - 1 harm)"
                  [TriggerMove EndurHarm],
                Choice
                  "You are dispirited or afraid (Endure Stress - 1 stress)"
                  [TriggerMove EndurStress],
                Choice
                  "You sacrifice resources (-1 supply)"
                  [LoseSupply 1]
              ],
            Narrative "[Você teve sucesso, mas enfrenta um custo problemático.]"
          ]
        Dice.Miss ->
          [ TriggerMove PayThePrice,
            Narrative "[Você falha ou seu progresso é prejudicado.]"
          ]
        Dice.InvalidRoll ->
          [Narrative "[Rolagem inválida.]"]

getGatherInformationConsequences :: Dice.RollResult -> Bool -> [Consequence]
getGatherInformationConsequences result isMatch =
  let matchBonus = if isMatch then [GainMomentum 1, Narrative "[Você descobre algo excepcional!]"] else []
   in case result of
        Dice.StrongHit ->
          [ GainMomentum 2,
            Narrative "[Você descobre algo útil e específico. O caminho é claro.]"
          ]
            ++ matchBonus
        Dice.WeakHit ->
          [ GainMomentum 1,
            Narrative "[A informação complica sua missão ou introduz um novo perigo.]"
          ]
            ++ matchBonus
        Dice.Miss ->
          [ TriggerMove PayThePrice,
            Narrative "[Sua investigação revela uma ameaça terrível ou verdade indesejada.]"
          ]
        Dice.InvalidRoll ->
          [Narrative "[Rolagem inválida.]"]

getSwearIronVowConsequences :: Dice.RollResult -> [Consequence]
getSwearIronVowConsequences result = case result of
  Dice.StrongHit ->
    [ GainMomentum 2,
      Narrative "[Você está determinado. O próximo passo é claro.]"
    ]
  Dice.WeakHit ->
    [ GainMomentum 1,
      Narrative "[Você está determinado, mas enfrenta incerteza ou dúvida.]"
    ]
  Dice.Miss ->
    [ PlayerChoice
        [ Choice "Aceitar o desafio (-2 momentum)" [LoseMomentum 2],
          Choice "Prove-se primeiro (Enfrentar Perigo)" [TriggerMove FaceDanger]
        ],
      Narrative "[Seu voto é posto em questão...]"
    ]
  Dice.InvalidRoll ->
    [Narrative "[Rolagem inválida.]"]

getUndertakeJourneyConsequences :: Dice.RollResult -> [Consequence]
getUndertakeJourneyConsequences result = case result of
  Dice.StrongHit ->
    [ PlayerChoice
        [ Choice "Recursos sábios: marque progresso" 
            [MarkJourneyProgress, Narrative "[Você alcança um waypoint de forma eficiente.]"],
          Choice
            "Velocidade: marque progresso, +1 momentum, -1 supply"
            [MarkJourneyProgress, GainMomentum 1, LoseSupply 1, 
             Narrative "[Você alcança um waypoint rapidamente, mas consome recursos.]"]
        ]
    ]
  Dice.WeakHit ->
    [ MarkJourneyProgress,
      LoseSupply 1,
      Narrative "[Você alcança um waypoint, mas a jornada exige seu preço.]"
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Você é impedido por um evento perigoso ou complicação.]"
    ]
  Dice.InvalidRoll ->
    [Narrative "[Rolagem inválida.]"]

getReachMilestoneConsequences :: Dice.RollResult -> [Consequence]
getReachMilestoneConsequences result = case result of
  Dice.StrongHit ->
    [ Narrative "[Marco alcançado! Marque progresso no seu voto.]"
    ]
  Dice.WeakHit ->
    [ Narrative "[Marco alcançado com complicação.]"
    ]
  Dice.Miss ->
    [ Narrative "[Você não completa o marco ainda.]"
    ]
  Dice.InvalidRoll ->
    [Narrative "[Rolagem inválida.]"]

getSecureAdvantageConsequences :: Dice.RollResult -> [Consequence]
getSecureAdvantageConsequences result = case result of
  Dice.StrongHit ->
    [ PlayerChoice
        [ Choice
            "Take control: Faça outro move agora com +1"
            [AddBonus (GameContext.ActiveBonus GameContext.NextRoll 1 "Take Control")],
          Choice
            "Prepare to act: +2 momentum"
            [GainMomentum 2]
        ],
      Narrative "[Você ganha vantagem.]"
    ]
  Dice.WeakHit ->
    [ GainMomentum 1,
      Narrative "[Sua vantagem é de curta duração.]"
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Você falha ou suas suposições te traem.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getHealConsequences :: Dice.RollResult -> [Consequence]
getHealConsequences result = case result of
  Dice.StrongHit ->
    [ GainHealth 2,
      Narrative "[Seu cuidado é útil. Pode limpar 'wounded' e ganhar até +2 health.]"
    ]
  Dice.WeakHit ->
    [ PlayerChoice
        [ Choice
            "Ganhe +2 health, sofra -1 supply"
            [GainHealth 2, LoseSupply 1],
          Choice
            "Ganhe +2 health, sofra -1 momentum"
            [GainHealth 2, LoseMomentum 1]
        ],
      Narrative "[Como Strong Hit, mas sofra -1 supply ou -1 momentum.]"
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Sua ajuda é ineficaz.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getResupplyConsequences :: Dice.RollResult -> [Consequence]
getResupplyConsequences result = case result of
  Dice.StrongHit ->
    [ GainSupply 2,
      Narrative "[Você reforça seus recursos.]"
    ]
  Dice.WeakHit ->
    [ PlayerChoice
        [ Choice
            "Ganhe +1 supply, sofra -1 momentum"
            [GainSupply 1, LoseMomentum 1],
          Choice
            "Ganhe +2 supply, sofra -2 momentum"
            [GainSupply 2, LoseMomentum 2]
        ],
      Narrative "[Você pode ganhar até +2 supply, mas sofre -1 momentum para cada.]"
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Você não encontra nada útil.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getMakeCampConsequences :: Dice.RollResult -> [Consequence]
getMakeCampConsequences result = case result of
  Dice.StrongHit ->
    [ Narrative "[Escolha uma combinação de duas opções:]",
      PlayerChoice
        [ Choice "+2 health, -1 supply" [GainHealth 1, LoseSupply 1, GainHealth 1],
          Choice "+1 health, +1 spirit" [GainHealth 1, GainSpirit 1],
          Choice "+1 health, +1 momentum" [GainHealth 1, GainMomentum 1],
          Choice "+1 health, +1 próxima jornada" [GainHealth 1, AddBonus (GameContext.ActiveBonus (GameContext.NextMove "EmpreenderJornada") 1 "Prepared")],
          Choice "+1 health, +1 spirit, -1 supply" [LoseSupply 1, GainHealth 1, GainSpirit 1],
          Choice "+1 health, +1 momentum, -1 supply" [LoseSupply 1, GainHealth 1, GainMomentum 1],
          Choice "+1 health, +1 próxima jornada, -1 supply" [LoseSupply 1, GainHealth 1, AddBonus (GameContext.ActiveBonus (GameContext.NextMove "EmpreenderJornada") 1 "Prepared")],
          Choice "+1 spirit, +1 momentum" [GainSpirit 1, GainMomentum 1],
          Choice "+1 spirit, +1 próxima jornada" [GainSpirit 1, AddBonus (GameContext.ActiveBonus (GameContext.NextMove "EmpreenderJornada") 1 "Prepared")],
          Choice "+1 momentum, +1 próxima jornada" [GainMomentum 1, AddBonus (GameContext.ActiveBonus (GameContext.NextMove "EmpreenderJornada") 1 "Prepared")]
        ]
    ]
  Dice.WeakHit ->
    [ Narrative "[Escolha uma opção]",
      PlayerChoice
        [ Choice "+1 health" [GainHealth 1],
          Choice "-1 supply, +1 health" [LoseSupply 1, GainHealth 1],
          Choice "+1 spirit" [GainSpirit 1],
          Choice "+1 momentum" [GainMomentum 1],
          Choice
            "+1 quando Empreender Jornada"
            [AddBonus (GameContext.ActiveBonus (GameContext.NextMove "EmpreenderJornada") 1 "Prepared")]
        ]
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Você não encontra conforto.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getCompelConsequences :: Dice.RollResult -> [Consequence]
getCompelConsequences result = case result of
  Dice.StrongHit ->
    [ GainMomentum 1,
      Narrative "[Eles farão o que você quer ou compartilharão o que sabem.]",
      Narrative "[Se usar para Gather Information, faça esse move agora com +1.]"
    ]
  Dice.WeakHit ->
    [ Narrative "[Como Strong Hit, mas eles pedem algo em retorno.]",
      PlayerChoice
        [ Choice "Decida sem consultar o oráculo" [],
          Choice
            "Pergunte ao oráculo (sim/não) para saber o que eles exigem"
            [TriggerOracle "Default"]
        ]
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Eles recusam ou fazem demanda que custa caro.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getSojournConsequences :: Dice.RollResult -> [Consequence]
getSojournConsequences result = case result of
  Dice.StrongHit ->
    [ Narrative "[Escolha uma combinação de duas opções]",
      PlayerChoice
        [ Choice "+1 health, +1 spirit" [GainHealth 1, GainSpirit 1],
          Choice "+1 health, +1 supply" [GainHealth 1, GainSupply 1],
          Choice "+3 health" [GainHealth 1, GainHealth 2],
          Choice "+1 health, +2 spirit" [GainHealth 1, GainSpirit 2],
          Choice "+1 health, +2 supply" [GainHealth 1, GainSupply 2],
          Choice "+1 health, +2 momentum" [GainHealth 1, GainMomentum 2],
          Choice "+1 spirit, +1 supply" [GainSpirit 1, GainSupply 1],
          Choice "+1 spirit, +2 health" [GainSpirit 1, GainHealth 2],
          Choice "+3 spirit" [GainSpirit 1, GainSpirit 2],
          Choice "+1 spirit, +2 supply" [GainSpirit 1, GainSupply 2],
          Choice "+1 spirit, +2 momentum" [GainSpirit 1, GainMomentum 2],
          Choice "+1 supply, +2 health" [GainSupply 1, GainHealth 2],
          Choice "+1 supply, +2 spirit" [GainSupply 1, GainSpirit 2],
          Choice "+3 supply" [GainSupply 1, GainSupply 2],
          Choice "+1 supply, +2 momentum" [GainSupply 1, GainMomentum 2],
          Choice "+2 health, +2 spirit" [GainHealth 2, GainSpirit 2],
          Choice "+2 health, +2 supply" [GainHealth 2, GainSupply 2],
          Choice "+2 health, +2 momentum" [GainHealth 2, GainMomentum 2],
          Choice "+2 spirit, +2 supply" [GainSpirit 2, GainSupply 2],
          Choice "+2 spirit, +2 momentum" [GainSpirit 2, GainMomentum 2],
          Choice "+2 supply, +2 momentum" [GainSupply 2, GainMomentum 2]
        ]
    ]
  Dice.WeakHit ->
    [ Narrative "[Escolha uma opção]",
      PlayerChoice
        [ Choice "Mend: Clear wounded, +1 health" [GainHealth 1],
          Choice "Hearten: Clear shaken, +1 spirit" [GainSpirit 1],
          Choice "Equip: Clear unprepared, +1 supply" [GainSupply 1],
          Choice "Recuperate: +2 health" [GainHealth 2],
          Choice "Consort: +2 spirit" [GainSpirit 2],
          Choice "Provision: +2 supply" [GainSupply 2],
          Choice "Plan: +2 momentum" [GainMomentum 2]
        ]
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Você não encontra ajuda aqui.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getForgeABondConsequences :: Dice.RollResult -> [Consequence]
getForgeABondConsequences result = case result of
  Dice.StrongHit -> [Narrative "[Vínculo formado.]", MarkBondProgress]
  Dice.WeakHit -> [Narrative "[Eles pedem mais de você. Faça ou jure voto.]"]
  Dice.Miss -> [TriggerMove PayThePrice, Narrative "[Você não consegue o vínculo.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getTestYourBondConsequences :: Dice.RollResult -> [Consequence]
getTestYourBondConsequences result = case result of
  Dice.StrongHit ->
    [ PlayerChoice
        [ Choice "Ganhe +1 spirit" [GainSpirit 1],
          Choice "Ganhe +2 momentum" [GainMomentum 2]
        ],
      Narrative "[O vínculo se fortalece.]"
    ]
  Dice.WeakHit -> [Narrative "[Prove sua lealdade ou perca o vínculo.]"]
  Dice.Miss -> [TriggerMove PayThePrice, Narrative "[Vínculo quebrado.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getAidYourAllyConsequences :: Dice.RollResult -> [Consequence]
getAidYourAllyConsequences result = case result of
  Dice.StrongHit ->
    [ AddBonus (GameContext.ActiveBonus GameContext.NextRoll 1 "Ajuda de Aliado"),
      Narrative "[Você recebe +1 no próximo roll (representando ajuda do aliado).]"
    ]
  Dice.WeakHit ->
    [ GainMomentum 1,
      Narrative "[Você ajuda mas se expõe a perigo.]"
    ]
  Dice.Miss -> [TriggerMove PayThePrice, Narrative "[Você falha e complica a situação.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getEnterTheFrayConsequences :: Dice.RollResult -> [Consequence]
getEnterTheFrayConsequences result = case result of
  Dice.StrongHit ->
    [ GainMomentum 2,
      Narrative "[Você tem iniciativa. Crie combat progress track com rank do inimigo.]"
    ]
  Dice.WeakHit ->
    [ PlayerChoice
        [ Choice
            "Bolster your position: +2 momentum"
            [GainMomentum 2],
          Choice
            "Prepare to act: Ganhe iniciativa"
            [Narrative "[Você tem iniciativa agora.]"]
        ]
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Combate começa em desvantagem. Inimigo tem iniciativa.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getStrikeConsequences :: Dice.RollResult -> [Consequence]
getStrikeConsequences result = case result of
  Dice.StrongHit ->
    [ Narrative "[Inflija dano +1. Você retém iniciativa.]",
      Narrative "[(Marque progresso no combat track: harm base + 1)]"
    ]
  Dice.WeakHit ->
    [ Narrative "[Inflija seu dano e perca iniciativa.]",
      Narrative "[(Marque progresso no combat track. Inimigo tem iniciativa.)]"
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Seu ataque falha. Inimigo tem iniciativa.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getClashConsequences :: Dice.RollResult -> [Consequence]
getClashConsequences result = case result of
  Dice.StrongHit ->
    [ PlayerChoice
        [ Choice
            "Bolster your position: +1 momentum, você tem iniciativa"
            [GainMomentum 1, Narrative "[Você tem iniciativa.]"],
          Choice
            "Find an opening: Inflija +1 harm, você tem iniciativa"
            [Narrative "[Inflija harm +1. Você tem iniciativa.]"]
        ],
      Narrative "[Inflija seu dano.]"
    ]
  Dice.WeakHit ->
    [ TriggerMove PayThePrice,
      Narrative "[Inflija seu dano, mas inimigo retém iniciativa.]"
    ]
  Dice.Miss ->
    [ TriggerMove PayThePrice,
      Narrative "[Você está em desvantagem. Inimigo retém iniciativa.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getTurnTheTideConsequences :: Dice.RollResult -> [Consequence]
getTurnTheTideConsequences result = case result of
  Dice.StrongHit -> [Narrative "[Você retoma iniciativa. Resetar momentum para +2.]"]
  Dice.WeakHit -> [Narrative "[Não retoma iniciativa, mas pode continuar.]"]
  Dice.Miss -> [TriggerMove PayThePrice, Narrative "[Situação piora drasticamente.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getEndTheFightConsequences :: Dice.RollResult -> [Consequence]
getEndTheFightConsequences result = case result of
  Dice.StrongHit ->
    [ Narrative "[Você vence decisivamente. O combate termina e você pode marcar experiência.]",
      Narrative "[Ganhe experiência baseada no rank do inimigo (troublesome=1, dangerous=2, etc.).]"
    ]
  Dice.WeakHit ->
    [ Narrative "[Você vence, mas há uma complicação ou custo.]",
      PlayerChoice
        [ Choice "O inimigo foge ou escapa" [Narrative "[O inimigo foge para lutar outro dia.]"],
          Choice "Você sofre dano antes de vencer" [TriggerMove EndurHarm],
          Choice "Você ganha, mas perde algo importante" [LoseSupply 1],
          Choice "A vitória atrai atenção indesejada" [TriggerOracle "Default"]
        ]
    ]
  Dice.Miss ->
    [ Narrative "[Você não consegue finalizar o combate.]",
      PlayerChoice
        [ Choice "O inimigo contra-ataca com força" [TriggerMove EndurHarm],
          Choice "Você perde a iniciativa e deve recuar" [TriggerMove PayThePrice],
          Choice "O combate continua em desvantagem" [LoseMomentum 2, Narrative "[Continue o combate. Inimigo tem iniciativa.]"]
        ]
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getEndurHarmConsequences :: Dice.RollResult -> [Consequence]
getEndurHarmConsequences result = case result of
  Dice.StrongHit ->
    [ PlayerChoice
        [ Choice
            "Shake it off: -1 momentum, +1 health (se health > 0)"
            [LoseMomentum 1, GainHealth 1],
          Choice
            "Embrace the pain: +1 momentum"
            [GainMomentum 1]
        ]
    ]
  Dice.WeakHit ->
    [ Narrative "[Você prossegue apesar do dano.]"
    ]
  Dice.Miss ->
    [ LoseMomentum 1,
      TriggerOracle "\"Resistir Dano\"",
      Narrative "[Sofra -1 momentum.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getFaceDeathConsequences :: Dice.RollResult -> [Consequence]
getFaceDeathConsequences result = case result of
  Dice.StrongHit -> [GainHealth 1, Narrative "[Você sobrevive por pouco. +1 health.]"]
  Dice.WeakHit -> [Narrative "[Você fica incapacitado ou morrendo. Aliado deve intervir.]"]
  Dice.Miss -> [Narrative "[Você morre. Escreva seu epitáfio.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getEndurStressConsequences :: Dice.RollResult -> [Consequence]
getEndurStressConsequences result = case result of
  Dice.StrongHit ->
    [ PlayerChoice
        [ Choice
            "Shake it off: -1 momentum, +1 spirit (se spirit > 0)"
            [LoseMomentum 1, GainSpirit 1],
          Choice
            "Embrace the pain: +1 momentum"
            [GainMomentum 1]
        ]
    ]
  Dice.WeakHit ->
    [ Narrative "[Você prossegue apesar do stress.]"
    ]
  Dice.Miss ->
    [ LoseMomentum 1,
      TriggerOracle "\"Resistir Estresse\"",
      Narrative "[Sofra -1 momentum.]"
    ]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getFaceDesolationConsequences :: Dice.RollResult -> [Consequence]
getFaceDesolationConsequences result = case result of
  Dice.StrongHit -> [GainSpirit 1, Narrative "[Você encontra forças. +1 spirit.]"]
  Dice.WeakHit -> [Narrative "[Você está desfeito. Continue com desvantagem.]"]
  Dice.Miss -> [Narrative "[Você é consumido. Abandone sua missão ou aja contra companheiros.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getOutOfSupplyConsequences :: Dice.RollResult -> [Consequence]
getOutOfSupplyConsequences result = case result of
  Dice.StrongHit -> [Narrative "[Você se vira sem supply.]"]
  Dice.WeakHit -> [LoseMomentum 1, Narrative "[Sofra -1 momentum.]"]
  Dice.Miss -> [TriggerMove PayThePrice, Narrative "[Você enfrenta sérias privações.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getFaceSetbackConsequences :: Dice.RollResult -> [Consequence]
getFaceSetbackConsequences result = case result of
  Dice.StrongHit -> [Narrative "[Você mantém seu progresso.]"]
  Dice.WeakHit -> [Narrative "[Perca metade do progresso (arredonde para cima).]"]
  Dice.Miss -> [Narrative "[Perca todo o progresso.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getForsakeVowConsequences :: Dice.RollResult -> [Consequence]
getForsakeVowConsequences result = case result of
  Dice.StrongHit -> [Narrative "[Você encontra redenção. +1 spirit.]"]
  Dice.WeakHit -> [LoseSpirit 1, Narrative "[Sofra -1 spirit.]"]
  Dice.Miss -> [LoseSpirit 2, Narrative "[Você é desonrado. -2 spirit.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

getAdvanceConsequences :: Dice.RollResult -> [Consequence]
getAdvanceConsequences _ =
  [ Narrative "[Você pode gastar experiência para melhorar seu personagem]",
    PlayerChoice
      [ Choice
          "Adquirir novo asset (2 XP)"
          [PickAsset "Escolha um novo asset para adquirir"],
        Choice
          "Melhorar asset existente (2 XP)"
          [ChooseAssetImprovement "Escolha um asset e skill para melhorar"],
        Choice
          "Não gastar XP agora"
          [Narrative "[~XP guardado para uso futuro.]"]
      ]
  ]

getAskOracleConsequences :: Dice.RollResult -> [Consequence]
getAskOracleConsequences _result = [TriggerOracle "Default"]

getReachDestinationConsequences :: Dice.RollResult -> [Consequence]
getReachDestinationConsequences result = case result of
  Dice.StrongHit ->
    [ Narrative "[Você chega ao seu destino e tudo está como esperava.]",
      Narrative "[A jornada foi bem-sucedida e você pode prosseguir com seus planos.]",
      Narrative "[Ganhe experiência baseada no rank da jornada (troublesome=1, dangerous=2, etc.).]"
    ]
  Dice.WeakHit ->
    [ Narrative "[Você chega ao destino, mas há uma complicação.]",
      PlayerChoice
        [ Choice "O caminho ou método foi mais custoso" [LoseSupply 1],
          Choice "O tempo foi maior que o esperado" [LoseMomentum 1],
          Choice "Você encontra um problema no destino" [TriggerOracle "Default"],
          Choice "Você chega exausto pelo caminho" [TriggerMove EndurHarm]
        ]
    ]
  Dice.Miss ->
    [ Narrative "[Você não consegue alcançar o destino.]",
      PlayerChoice
        [ Choice "Você se perde e precisa encontrar o caminho" [TriggerMove FaceDanger],
          Choice "Um obstáculo bloqueia seu caminho" [TriggerMove PayThePrice],
          Choice "Você deve reafirmar sua jornada (-2 momentum)" [LoseMomentum 2],
          Choice "A jornada é mais longa que esperado (continuar)" [Narrative "[Continue marcando progresso na jornada.]"]
        ]
    ]
  Dice.InvalidRoll ->
    [Narrative "[Rolagem inválida.]"]

getDefaultConsequences :: Dice.RollResult -> [Consequence]
getDefaultConsequences result = case result of
  Dice.StrongHit -> [Narrative "[Sucesso total!]"]
  Dice.WeakHit -> [Narrative "[Sucesso parcial.]"]
  Dice.Miss -> [Narrative "[Falha.]"]
  Dice.InvalidRoll -> [Narrative "[Rolagem inválida.]"]

parseMoveType :: T.Text -> Maybe MoveType
parseMoveType text =
  case T.toLower . T.strip $ text of
    -- Inglês (compatibilidade)
    "paytheprice" -> Just PayThePrice
    "facedanger" -> Just FaceDanger
    "gatherinformation" -> Just GatherInformation
    "secureadvantage" -> Just SecureAdvantage
    "undertakejourney" -> Just UndertakeJourney
    "heal" -> Just Heal
    "resupply" -> Just Resupply
    "makecamp" -> Just MakeCamp
    "compel" -> Just CompelAction
    "sojourn" -> Just Sojourn
    "drawthecircle" -> Just DrawTheCircle
    "forgeabond" -> Just ForgeABond
    "testyourbond" -> Just TestYourBond
    "aidyourally" -> Just AidYourAlly
    "enterthefray" -> Just EnterTheFray
    "strike" -> Just Strike
    "clash" -> Just Clash
    "turnthetide" -> Just TurnTheTide
    "endfight" -> Just EndTheFight
    "battle" -> Just Battle
    "endureharm" -> Just EndurHarm
    "facedeath" -> Just FaceDeath
    "endurestress" -> Just EndurStress
    "facedesolation" -> Just FaceDesolation
    "outofsupply" -> Just OutOfSupply
    "faceasetback" -> Just FaceSetback
    "swearironvow" -> Just SwearIronVow
    "reachmilestone" -> Just ReachMilestone
    "fulfillyourvow" -> Just FulfillYourVow
    "forsakeyourvow" -> Just ForsakeYourVow
    "advance" -> Just Advance
    "reachdestination" -> Just ReachYourDestination
    "writeyourepilogue" -> Just WriteYourEpilogue
    "askoracle" -> Just AskTheOracle
    -- Português
    "pagaropreco" -> Just PayThePrice
    "enfrentarperigo" -> Just FaceDanger
    "coletarinformacao" -> Just GatherInformation
    "segurarvantagem" -> Just SecureAdvantage
    "empreenderjornada" -> Just UndertakeJourney
    "curar" -> Just Heal
    "reabastecer" -> Just Resupply
    "fazercampo" -> Just MakeCamp
    "convencer" -> Just CompelAction
    "desenharcirculo" -> Just DrawTheCircle
    "forjarvinculo" -> Just ForgeABond
    "testarvinculo" -> Just TestYourBond
    "ajudaraliado" -> Just AidYourAlly
    "entrarnofronte" -> Just EnterTheFray
    "atacar" -> Just Strike
    "confrontar" -> Just Clash
    "virarmesa" -> Just TurnTheTide
    "terminarbatalha" -> Just EndTheFight
    "batalhar" -> Just Battle
    "enfrentardano" -> Just EndurHarm
    "enfrentarmorte" -> Just FaceDeath
    "enfrentarestresse" -> Just EndurStress
    "enfrentardesolacao" -> Just FaceDesolation
    "semsuprimento" -> Just OutOfSupply
    "enfrentarreves" -> Just FaceSetback
    "jurarvoto" -> Just SwearIronVow
    "alcancarmarco" -> Just ReachMilestone
    "cumprirvoto" -> Just FulfillYourVow
    "abanarvoto" -> Just ForsakeYourVow
    "avancar" -> Just Advance
    "alcancardestino" -> Just ReachYourDestination
    "escreverepilogo" -> Just WriteYourEpilogue
    "oraculo" -> Just AskTheOracle
    _ -> Nothing

parseStat :: T.Text -> Maybe Stat
parseStat text =
  case T.toLower . T.strip $ text of
    "iron" -> Just Iron
    "edge" -> Just Edge
    "heart" -> Just Heart
    "shadow" -> Just Shadow
    "wits" -> Just Wits
    _ -> Nothing
