{-# LANGUAGE OverloadedStrings #-}
{- | Implementação do serviço de Moves do Ironsworn
     
     Implementa os moves do sistema Ironsworn com:
     - Rolagens automáticas
     - Avaliação de resultados
     - Consequências automáticas
     - Menus de escolha interativos
-}
module System.Impl.MoveService (newHandle) where

import qualified System.MoveContract as Move
import qualified System.GameContextContract as GameContext
import qualified System.DiceContract as Dice
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.DiceContract (RollResult (..))
import Control.Monad (when, foldM)
import System.IO (hFlush, stdout)
import qualified Data.Maybe

-- | Cria novo handle para o serviço de Moves
newHandle :: Dice.Handle -> IO Move.Handle
newHandle diceH = do
  return $ Move.Handle
    { Move.executeMove = executeMoveImpl diceH
    , Move.executeMoveWithRoll = executeMoveWithRollImpl diceH
    , Move.processChoice = processChoiceImpl
    , Move.applyConsequences = applyConsequencesImpl diceH
    , Move.showChoices = showChoicesImpl
    , Move.parseMoveType = parseMoveTypeImpl
    , Move.parseStat = parseStatImpl
    }

-- | Executa um move (rola dados automaticamente)
executeMoveImpl :: Dice.Handle -> Move.MoveType -> Maybe Move.Stat -> GameContext.Context -> GameContext.Handle -> IO (Move.MoveResult, GameContext.Context)
executeMoveImpl diceH moveType maybeStat ctx ctxH = do
  -- Rola 1d6 + 2d10
  rolls <- Dice.roll diceH "1d6,2d10"
  
  case rolls of
    [(_, actionDie), (_, ch1), (_, ch2)] -> 
      executeMoveWithRollImpl diceH moveType maybeStat actionDie (ch1, ch2) ctx ctxH
    _ -> do
      -- Erro na rolagem
      let errorResult = Move.MoveResult moveType InvalidRoll 0 (0, 0) 0 False []
      return (errorResult, ctx)

-- | Executa um move com rolagem já feita
executeMoveWithRollImpl :: Dice.Handle -> Move.MoveType -> Maybe Move.Stat -> Int -> (Int, Int) -> GameContext.Context -> GameContext.Handle -> IO (Move.MoveResult, GameContext.Context)
executeMoveWithRollImpl _diceH moveType maybeStat actionDie (ch1, ch2) ctx ctxH = do
  -- Nome do move para filtrar bônus
  let moveName = Move.moveTypeToText moveType
  
  -- Calcula modificador do stat
  let statModifier = case maybeStat of
        Just stat -> getStatValue stat ctx
        Nothing -> 0
  
  -- Obtém bônus ativos aplicáveis
  let applicableBonuses = GameContext.getApplicableBonuses ctxH ctx (Just moveName)
  let bonusTotal = sum (map GameContext.bonusValue applicableBonuses)
  
  -- Mostra bônus se houver
  when (bonusTotal > 0) $ do
    putStrLn $ "\n[Bônus ativos: +" ++ show bonusTotal ++ "]"
    mapM_ (\b -> putStrLn $ "  • " ++ T.unpack (GameContext.bonusDescription b) ++ ": +" ++ show (GameContext.bonusValue b)) applicableBonuses
  
  let totalModifier = statModifier + bonusTotal
  let actionTotal = actionDie + totalModifier
  let rollResult = evaluateRoll actionTotal ch1 ch2
  let isMatch = ch1 == ch2

  -- Mostra informações da rolagem
  putStrLn $ "\n>>> " ++ T.unpack (Move.moveTypeToText moveType) ++ " <<<"
  when (Data.Maybe.isJust maybeStat) $
    putStrLn $ "Usando: " ++ maybe "" (T.unpack . Move.statToText) maybeStat ++ " (+" ++ show statModifier ++ ")"
  
  putStrLn "\nRolagem:"
  putStrLn $ "  Action Die (d6): " ++ show actionDie
  if totalModifier > 0
    then do
      when (statModifier > 0) $
        putStrLn $ "  + Stat: " ++ show statModifier
      when (bonusTotal > 0) $
        putStrLn $ "  + Bônus: " ++ show bonusTotal
      putStrLn $ "  = Total: " ++ show actionTotal
    else
      putStrLn $ "  = Total: " ++ show actionTotal
  putStrLn $ "  Challenge Dice: " ++ show ch1 ++ ", " ++ show ch2

  when isMatch $
    putStrLn "\n⚠ MATCH detectado!"

  -- Obtém consequências baseadas no move e resultado
  consequences <- getMoveConsequences moveType rollResult isMatch

  putStrLn $ "\nResultado: " ++ showRollResult rollResult

  -- Adiciona log do resultado do move
  let moveLog = Move.moveTypeToText moveType <> ": " <> T.pack (showRollResult rollResult)
  ctxWithLog <- GameContext.addLogEntry ctxH ctx moveLog

  -- Consome bônus ANTES de aplicar novas consequências
  -- (assim bônus gerados NESTE move não são consumidos imediatamente)
  ctxWithBonusesConsumed <- GameContext.consumeBonuses ctxH ctxWithLog (Just moveName)
  
  -- Aplica consequências (pode adicionar novos bônus)
  updatedCtx <- applyConsequencesImplInternal _diceH consequences ctxWithBonusesConsumed ctxH
  
  let moveResult = Move.MoveResult
        { Move.moveExecuted = moveType
        , Move.rollResult = rollResult
        , Move.actionDie = actionDie
        , Move.challengeDice = (ch1, ch2)
        , Move.modifier = totalModifier
        , Move.matchOccurred = isMatch
        , Move.consequencesApplied = consequences
        }
  
  return (moveResult, updatedCtx)

-- | Obtém valor de um stat do contexto
getStatValue :: Move.Stat -> GameContext.Context -> Int
getStatValue stat ctx =
  let attrs = GameContext.attributes . GameContext.mainCharacter $ ctx
  in case stat of
    Move.Iron -> GameContext.iron attrs
    Move.Edge -> GameContext.edge attrs
    Move.Heart -> GameContext.heart attrs
    Move.Shadow -> GameContext.shadow attrs
    Move.Wits -> GameContext.wits attrs

-- | Avalia rolagem
evaluateRoll :: Int -> Int -> Int -> RollResult
evaluateRoll action ch1 ch2
  | action > ch1 && action > ch2 = StrongHit
  | action > ch1 || action > ch2 = WeakHit
  | otherwise = Miss

-- | Formata resultado
showRollResult :: RollResult -> String
showRollResult StrongHit = "STRONG HIT ✓"
showRollResult WeakHit = "WEAK HIT ~"
showRollResult Miss = "MISS ✗"
showRollResult InvalidRoll = "INVALID"

-- | Retorna consequências de um move baseado no resultado
getMoveConsequences :: Move.MoveType -> RollResult -> Bool -> IO [Move.Consequence]
getMoveConsequences moveType result isMatch = do
  let baseConsequences = case moveType of
        -- Fate Moves
        Move.PayThePrice -> getPayThePriceConsequences result isMatch
        Move.AskTheOracle -> getAskOracleConsequences result
        
        -- Adventure Moves
        Move.FaceDanger -> getFaceDangerConsequences result isMatch
        Move.GatherInformation -> getGatherInformationConsequences result isMatch
        Move.SecureAdvantage -> getSecureAdvantageConsequences result
        Move.Heal -> getHealConsequences result
        Move.Resupply -> getResupplyConsequences result
        Move.MakeCamp -> getMakeCampConsequences result
        Move.UndertakeJourney -> getUndertakeJourneyConsequences result
        
        -- Relationship Moves
        Move.CompelAction -> getCompelConsequences result
        Move.Sojourn -> getSojournConsequences result
        Move.ForgeABond -> getForgeABondConsequences result
        Move.TestYourBond -> getTestYourBondConsequences result
        Move.AidYourAlly -> getAidYourAllyConsequences result
        
        -- Combat Moves
        Move.EnterTheFray -> getEnterTheFrayConsequences result
        Move.Strike -> getStrikeConsequences result
        Move.Clash -> getClashConsequences result
        Move.TurnTheTide -> getTurnTheTideConsequences result
        
        -- Suffer Moves
        Move.EndurHarm -> getEndurHarmConsequences result
        Move.FaceDeath -> getFaceDeathConsequences result
        Move.EndurStress -> getEndurStressConsequences result
        Move.FaceDesolation -> getFaceDesolationConsequences result
        Move.OutOfSupply -> getOutOfSupplyConsequences result
        Move.FaceSetback -> getFaceSetbackConsequences result
        
        -- Quest Moves
        Move.SwearIronVow -> getSwearIronVowConsequences result
        Move.ReachMilestone -> getReachMilestoneConsequences result
        Move.ForsakeYourVow -> getForsakeVowConsequences result
        Move.Advance -> getAdvanceConsequences result
        
        _ -> getDefaultConsequences result
  
  return baseConsequences

-- | Pay the Price - Sofra as consequências (baseado nas regras oficiais)
-- O jogador NÃO rola dados neste move, ele ESCOLHE entre 3 opções
getPayThePriceConsequences :: RollResult -> Bool -> [Move.Consequence]
getPayThePriceConsequences _result _isMatch =
  [ Move.PlayerChoice
      [ Move.Choice "Fazer o resultado negativo mais óbvio acontecer (escolha narrativa)" 
          [Move.Narrative "Descreva o que acontece de pior..."]
      
      , Move.Choice "Visualizar dois resultados e usar Ask the Oracle (sim/não)" 
          [Move.Narrative "Role 1d2: 1=primeiro resultado, 2=segundo resultado"]
      
      , Move.Choice "Rolar na tabela Pay the Price (use :oracle \"Pay the Price\")" 
          [Move.Narrative "Use: :oracle \"Pay the Price\" para ver o que acontece"]
      ]
  ]

-- | Face Danger - Enfrente um perigo (CORRIGIDO conforme PDF)
getFaceDangerConsequences :: RollResult -> Bool -> [Move.Consequence]
getFaceDangerConsequences result isMatch =
  let matchPenalty = ([Move.LoseMomentum 1 | isMatch])
  in matchPenalty ++ case result of
    StrongHit ->
      [ Move.GainMomentum 1
      , Move.Narrative "Você é bem-sucedido."
      ]

    WeakHit ->
      [ Move.PlayerChoice
          [ Move.Choice "Delayed, lose advantage, or face new danger (-1 momentum)" 
              [Move.LoseMomentum 1]
          , Move.Choice "You are tired or hurt (Endure Harm - 1 harm)" 
              [Move.TriggerMove Move.EndurHarm]
          , Move.Choice "You are dispirited or afraid (Endure Stress - 1 stress)" 
              [Move.TriggerMove Move.EndurStress]
          , Move.Choice "You sacrifice resources (-1 supply)" 
              [Move.LoseSupply 1]
          ]
      , Move.Narrative "Você teve sucesso, mas enfrenta um custo problemático."
      ]

    Miss ->
      [ Move.TriggerMove Move.PayThePrice
      , Move.Narrative "Você falha ou seu progresso é prejudicado."
      ]

    InvalidRoll ->
      [Move.Narrative "Rolagem inválida."]

-- | Gather Information - Colete informações (CORRIGIDO conforme PDF)
getGatherInformationConsequences :: RollResult -> Bool -> [Move.Consequence]
getGatherInformationConsequences result isMatch =
  let matchBonus = if isMatch then [Move.GainMomentum 1, Move.Narrative "Você descobre algo excepcional!"] else []
  in case result of
    StrongHit ->
      [ Move.GainMomentum 2
      , Move.Narrative "Você descobre algo útil e específico. O caminho é claro."
      ] ++ matchBonus

    WeakHit ->
      [ Move.GainMomentum 1
      , Move.Narrative "A informação complica sua missão ou introduz um novo perigo."
      ] ++ matchBonus

    Miss ->
      [ Move.TriggerMove Move.PayThePrice
      , Move.Narrative "Sua investigação revela uma ameaça terrível ou verdade indesejada."
      ]

    InvalidRoll ->
      [Move.Narrative "Rolagem inválida."]

-- | Swear an Iron Vow - Jure um voto (roll +heart)
getSwearIronVowConsequences :: RollResult -> [Move.Consequence]
getSwearIronVowConsequences result = case result of
  StrongHit ->
    [ Move.GainMomentum 2
    , Move.Narrative "Você está determinado. O próximo passo é claro."
    ]
  WeakHit ->
    [ Move.GainMomentum 1
    , Move.Narrative "Você está determinado, mas enfrenta incerteza ou dúvida."
    ]
  Miss ->
    [ Move.PlayerChoice
        [ Move.Choice "Aceitar o desafio (-2 momentum)" [Move.LoseMomentum 2]
        , Move.Choice "Prove-se primeiro (Face Danger)" [Move.TriggerMove Move.FaceDanger]
        ]
    , Move.Narrative "Seu voto é posto em questão..."
    ]
  InvalidRoll ->
    [Move.Narrative "Rolagem inválida."]

-- | Undertake a Journey - Viaje (roll +wits)
-- Se partindo de bond community, adiciona +1 no primeiro roll
getUndertakeJourneyConsequences :: RollResult -> [Move.Consequence]
getUndertakeJourneyConsequences result = case result of
  StrongHit ->
    [ Move.PlayerChoice
        [ Move.Choice "Recursos sábios: marque progresso" []
        , Move.Choice "Velocidade: marque progresso, +1 momentum, -1 supply" 
            [Move.GainMomentum 1, Move.LoseSupply 1]
        ]
    , Move.Narrative "Você alcança um waypoint."
    ]
  WeakHit ->
    [ Move.LoseSupply 1
    , Move.Narrative "Você alcança um waypoint e marca progresso, mas perde supply."
    ]
  Miss ->
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Você é impedido por um evento perigoso."
    ]
  InvalidRoll ->
    [Move.Narrative "Rolagem inválida."]

-- | Reach a Milestone - Alcance um marco
getReachMilestoneConsequences :: RollResult -> [Move.Consequence]
getReachMilestoneConsequences result = case result of
  StrongHit ->
    [ Move.Narrative "Marco alcançado! Marque progresso no seu voto."
    ]
  WeakHit ->
    [ Move.Narrative "Marco alcançado com complicação."
    ]
  Miss ->
    [ Move.Narrative "Você não completa o marco ainda."
    ]
  InvalidRoll ->
    [Move.Narrative "Rolagem inválida."]

-- | Secure an Advantage - Ganhe vantagem (CORRIGIDO conforme PDF)
getSecureAdvantageConsequences :: RollResult -> [Move.Consequence]
getSecureAdvantageConsequences result = case result of
  StrongHit -> 
    [ Move.PlayerChoice
        [ Move.Choice "Take control: Faça outro move agora com +1" 
            [Move.AddBonus (GameContext.ActiveBonus GameContext.NextRoll 1 "Take Control")]
        , Move.Choice "Prepare to act: +2 momentum" 
            [Move.GainMomentum 2]
        ]
    , Move.Narrative "Você ganha vantagem."
    ]
  
  WeakHit -> 
    [ Move.GainMomentum 1
    , Move.Narrative "Sua vantagem é de curta duração."
    ]
  
  Miss -> 
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Você falha ou suas suposições te traem."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Heal - Recupere health (CORRIGIDO - roll +wits, ou +wits/+iron menor se self)
-- NOTA: Sistema de "wounded" não implementado (seria flag no Context)
getHealConsequences :: RollResult -> [Move.Consequence]
getHealConsequences result = case result of
  StrongHit -> 
    [ Move.GainHealth 2
    , Move.Narrative "Seu cuidado é útil. Pode limpar 'wounded' e ganhar até +2 health."
    ]
  
  WeakHit -> 
    [ Move.PlayerChoice
        [ Move.Choice "Ganhe +2 health, sofra -1 supply" 
            [Move.GainHealth 2, Move.LoseSupply 1]
        , Move.Choice "Ganhe +2 health, sofra -1 momentum" 
            [Move.GainHealth 2, Move.LoseMomentum 1]
        ]
    , Move.Narrative "Como Strong Hit, mas sofra -1 supply ou -1 momentum."
    ]
  
  Miss -> 
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Sua ajuda é ineficaz."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Resupply - Recupere supply (CORRIGIDO conforme PDF - roll +wits)
getResupplyConsequences :: RollResult -> [Move.Consequence]
getResupplyConsequences result = case result of
  StrongHit -> 
    [ Move.GainSupply 2
    , Move.Narrative "Você reforça seus recursos."
    ]
  
  WeakHit -> 
    [ Move.PlayerChoice
        [ Move.Choice "Ganhe +1 supply, sofra -1 momentum" 
            [Move.GainSupply 1, Move.LoseMomentum 1]
        , Move.Choice "Ganhe +2 supply, sofra -2 momentum" 
            [Move.GainSupply 2, Move.LoseMomentum 2]
        ]
    , Move.Narrative "Você pode ganhar até +2 supply, mas sofre -1 momentum para cada."
    ]
  
  Miss -> 
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Você não encontra nada útil."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Make Camp - Acampe (CORRIGIDO conforme PDF - roll +supply)
getMakeCampConsequences :: RollResult -> [Move.Consequence]
getMakeCampConsequences result = case result of
  StrongHit -> 
    [ Move.Narrative "Escolha DUAS opções:"
    , Move.PlayerChoice
        [ Move.Choice "Recuperate: +1 health" [Move.GainHealth 1]
        , Move.Choice "Partake: -1 supply, +1 health" [Move.LoseSupply 1, Move.GainHealth 1]
        , Move.Choice "Relax: +1 spirit" [Move.GainSpirit 1]
        , Move.Choice "Focus: +1 momentum" [Move.GainMomentum 1]
        , Move.Choice "Prepare: +1 quando Undertake Journey" 
            [Move.AddBonus (GameContext.ActiveBonus (GameContext.NextMove "Undertake a Journey") 1 "Prepared")]
        ]
    ]
  
  WeakHit -> 
    [ Move.Narrative "Escolha UMA opção:"
    , Move.PlayerChoice
        [ Move.Choice "Recuperate: +1 health" [Move.GainHealth 1]
        , Move.Choice "Partake: -1 supply, +1 health" [Move.LoseSupply 1, Move.GainHealth 1]
        , Move.Choice "Relax: +1 spirit" [Move.GainSpirit 1]
        , Move.Choice "Focus: +1 momentum" [Move.GainMomentum 1]
        , Move.Choice "Prepare: +1 quando Undertake Journey" 
            [Move.AddBonus (GameContext.ActiveBonus (GameContext.NextMove "Undertake a Journey") 1 "Prepared")]
        ]
    ]
  
  Miss -> 
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Você não encontra conforto."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Compel - Persuadir (CORRIGIDO - roll +heart/+iron/+shadow)
-- +1 se share bond
getCompelConsequences :: RollResult -> [Move.Consequence]
getCompelConsequences result = case result of
  StrongHit ->
    [ Move.GainMomentum 1
    , Move.Narrative "Eles farão o que você quer ou compartilharão o que sabem."
    , Move.Narrative "Se usar para Gather Information, faça esse move agora com +1."
    ]
  
  WeakHit ->
    [ Move.Narrative "Como Strong Hit, mas eles pedem algo em retorno."
    , Move.Narrative "Visualize o que eles querem (Ask the Oracle se incerto)."
    ]
  
  Miss ->
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Eles recusam ou fazem demanda que custa caro."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Sojourn - Descanse em comunidade (CORRIGIDO - roll +heart, +1 se bond)
-- Strong Hit: Escolha 2 (ou 3 se bond). Weak Hit: Escolha 1 (ou 2 se bond)
getSojournConsequences :: RollResult -> [Move.Consequence]
getSojournConsequences result = case result of
  StrongHit ->
    [ Move.Narrative "Escolha DUAS das opções abaixo (+1 se tiver bond):"
    , Move.PlayerChoice
        [ Move.Choice "Mend: Clear wounded, +1 health" [Move.GainHealth 1]
        , Move.Choice "Hearten: Clear shaken, +1 spirit" [Move.GainSpirit 1]
        , Move.Choice "Equip: Clear unprepared, +1 supply" [Move.GainSupply 1]
        , Move.Choice "Recuperate: +2 health" [Move.GainHealth 2]
        , Move.Choice "Consort: +2 spirit" [Move.GainSpirit 2]
        , Move.Choice "Provision: +2 supply" [Move.GainSupply 2]
        , Move.Choice "Plan: +2 momentum" [Move.GainMomentum 2]
        ]
    ]
  
  WeakHit -> 
    [ Move.Narrative "Escolha UMA opção (+1 se tiver bond):"
    , Move.PlayerChoice
        [ Move.Choice "Mend: Clear wounded, +1 health" [Move.GainHealth 1]
        , Move.Choice "Hearten: Clear shaken, +1 spirit" [Move.GainSpirit 1]
        , Move.Choice "Equip: Clear unprepared, +1 supply" [Move.GainSupply 1]
        , Move.Choice "Recuperate: +2 health" [Move.GainHealth 2]
        , Move.Choice "Consort: +2 spirit" [Move.GainSpirit 2]
        , Move.Choice "Provision: +2 supply" [Move.GainSupply 2]
        , Move.Choice "Plan: +2 momentum" [Move.GainMomentum 2]
        ]
    ]
  
  Miss -> 
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Você não encontra ajuda aqui."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Forge a Bond - Crie vínculo (roll +heart)
getForgeABondConsequences :: RollResult -> [Move.Consequence]
getForgeABondConsequences result = case result of
  StrongHit -> [Move.Narrative "Vínculo formado. Marque bond progress."]
  WeakHit -> [Move.Narrative "Eles pedem mais de você. Faça ou jure voto."]
  Miss -> [Move.TriggerMove Move.PayThePrice, Move.Narrative "Você não consegue o vínculo."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Test Your Bond - Teste vínculo (roll +heart)
getTestYourBondConsequences :: RollResult -> [Move.Consequence]
getTestYourBondConsequences result = case result of
  StrongHit ->
    [ Move.PlayerChoice
        [ Move.Choice "Ganhe +1 spirit" [Move.GainSpirit 1]
        , Move.Choice "Ganhe +2 momentum" [Move.GainMomentum 2]
        ]
    , Move.Narrative "O vínculo se fortalece."
    ]
  WeakHit -> [Move.Narrative "Prove sua lealdade ou perca o vínculo."]
  Miss -> [Move.TriggerMove Move.PayThePrice, Move.Narrative "Vínculo quebrado."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Aid Your Ally - Ajude aliado (roll +heart)
getAidYourAllyConsequences :: RollResult -> [Move.Consequence]
getAidYourAllyConsequences result = case result of
  StrongHit -> 
    [ Move.AddBonus (GameContext.ActiveBonus GameContext.NextRoll 1 "Ajuda de Aliado")
    , Move.Narrative "Você recebe +1 no próximo roll (representando ajuda do aliado)."
    ]
  WeakHit -> 
    [ Move.GainMomentum 1
    , Move.Narrative "Você ajuda mas se expõe a perigo."
    ]
  Miss -> [Move.TriggerMove Move.PayThePrice, Move.Narrative "Você falha e complica a situação."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Enter the Fray - Inicie combate (CORRIGIDO conforme PDF)
-- Roll: +heart (facing), +shadow (ambush/surprise), +wits (ambushed)
getEnterTheFrayConsequences :: RollResult -> [Move.Consequence]
getEnterTheFrayConsequences result = case result of
  StrongHit -> 
    [ Move.GainMomentum 2
    , Move.Narrative "Você tem iniciativa. Crie combat progress track com rank do inimigo."
    ]
  
  WeakHit ->
    [ Move.PlayerChoice
        [ Move.Choice "Bolster your position: +2 momentum" 
            [Move.GainMomentum 2]
        , Move.Choice "Prepare to act: Ganhe iniciativa" 
            [Move.Narrative "Você tem iniciativa agora."]
        ]
    ]
  
  Miss -> 
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Combate começa em desvantagem. Inimigo tem iniciativa."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Strike - Ataque (CORRIGIDO - roll +iron close, +edge range)
-- Requer: Iniciativa. Marca progress no combat track.
getStrikeConsequences :: RollResult -> [Move.Consequence]
getStrikeConsequences result = case result of
  StrongHit ->
    [ Move.Narrative "Inflija dano +1. Você retém iniciativa."
    , Move.Narrative "(Marque progresso no combat track: harm base + 1)"
    ]
  
  WeakHit ->
    [ Move.Narrative "Inflija seu dano e perca iniciativa."
    , Move.Narrative "(Marque progresso no combat track. Inimigo tem iniciativa.)"
    ]
  
  Miss ->
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Seu ataque falha. Inimigo tem iniciativa."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Clash - Defenda/Contra-ataque (CORRIGIDO - roll +iron close, +edge range)
-- Requer: Inimigo com iniciativa. Marca progress no combat track.
getClashConsequences :: RollResult -> [Move.Consequence]
getClashConsequences result = case result of
  StrongHit ->
    [ Move.PlayerChoice
        [ Move.Choice "Bolster your position: +1 momentum, você tem iniciativa" 
            [Move.GainMomentum 1, Move.Narrative "Você tem iniciativa."]
        , Move.Choice "Find an opening: Inflija +1 harm, você tem iniciativa" 
            [Move.Narrative "Inflija harm +1. Você tem iniciativa."]
        ]
    , Move.Narrative "Inflija seu dano."
    ]
  
  WeakHit ->
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Inflija seu dano, mas inimigo retém iniciativa."
    ]
  
  Miss ->
    [ Move.TriggerMove Move.PayThePrice
    , Move.Narrative "Você está em desvantagem. Inimigo retém iniciativa."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Turn the Tide - Vire o jogo
getTurnTheTideConsequences :: RollResult -> [Move.Consequence]
getTurnTheTideConsequences result = case result of
  StrongHit -> [Move.Narrative "Você retoma iniciativa. Resetar momentum para +2."]
  WeakHit -> [Move.Narrative "Não retoma iniciativa, mas pode continuar."]
  Miss -> [Move.TriggerMove Move.PayThePrice, Move.Narrative "Situação piora drasticamente."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Endure Harm - Sofra dano (roll +health ou +iron, o maior)
-- NOTA: Este move é especial - precisa do valor de harm como parâmetro
-- Implementação simplificada: apenas as consequências pós-rolagem
getEndurHarmConsequences :: RollResult -> [Move.Consequence]
getEndurHarmConsequences result = case result of
  StrongHit ->
    [ Move.PlayerChoice
        [ Move.Choice "Shake it off: -1 momentum, +1 health (se health > 0)" 
            [Move.LoseMomentum 1, Move.GainHealth 1]
        , Move.Choice "Embrace the pain: +1 momentum" 
            [Move.GainMomentum 1]
        ]
    ]
  
  WeakHit -> 
    [ Move.Narrative "Você prossegue apesar do dano."
    ]
  
  Miss -> 
    [ Move.LoseMomentum 1
    , Move.TriggerOracle "Endure Harm"  -- Se health = 0, executa oráculo automaticamente
    , Move.Narrative "Sofra -1 momentum."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Face Death - À beira da morte
getFaceDeathConsequences :: RollResult -> [Move.Consequence]
getFaceDeathConsequences result = case result of
  StrongHit -> [Move.GainHealth 1, Move.Narrative "Você sobrevive por pouco. +1 health."]
  WeakHit -> [Move.Narrative "Você fica incapacitado ou morrendo. Aliado deve intervir."]
  Miss -> [Move.Narrative "Você morre. Escreva seu epitáfio."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Endure Stress - Sofra stress (roll +spirit ou +heart, o maior)
-- Similar a Endure Harm mas para dano mental
getEndurStressConsequences :: RollResult -> [Move.Consequence]
getEndurStressConsequences result = case result of
  StrongHit -> 
    [ Move.PlayerChoice
        [ Move.Choice "Shake it off: -1 momentum, +1 spirit (se spirit > 0)"
            [Move.LoseMomentum 1, Move.GainSpirit 1]
        , Move.Choice "Embrace the pain: +1 momentum"
            [Move.GainMomentum 1]
        ]
    ]
  
  WeakHit -> 
    [ Move.Narrative "Você prossegue apesar do stress."
    ]
  
  Miss -> 
    [ Move.LoseMomentum 1
    , Move.TriggerOracle "Endure Stress"  -- Se spirit = 0, executa oráculo automaticamente
    , Move.Narrative "Sofra -1 momentum."
    ]
  
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Face Desolation - À beira do colapso
getFaceDesolationConsequences :: RollResult -> [Move.Consequence]
getFaceDesolationConsequences result = case result of
  StrongHit -> [Move.GainSpirit 1, Move.Narrative "Você encontra forças. +1 spirit."]
  WeakHit -> [Move.Narrative "Você está desfeito. Continue com desvantagem."]
  Miss -> [Move.Narrative "Você é consumido. Abandone sua missão ou aja contra companheiros."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Out of Supply - Sem suprimentos
getOutOfSupplyConsequences :: RollResult -> [Move.Consequence]
getOutOfSupplyConsequences result = case result of
  StrongHit -> [Move.Narrative "Você se vira sem supply."]
  WeakHit -> [Move.LoseMomentum 1, Move.Narrative "Sofra -1 momentum."]
  Miss -> [Move.TriggerMove Move.PayThePrice, Move.Narrative "Você enfrenta sérias privações."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Face a Setback - Revés em progress
getFaceSetbackConsequences :: RollResult -> [Move.Consequence]
getFaceSetbackConsequences result = case result of
  StrongHit -> [Move.Narrative "Você mantém seu progresso."]
  WeakHit -> [Move.Narrative "Perca metade do progresso (arredonde para cima)."]
  Miss -> [Move.Narrative "Perca todo o progresso."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Forsake Your Vow - Abandone voto
getForsakeVowConsequences :: RollResult -> [Move.Consequence]
getForsakeVowConsequences result = case result of
  StrongHit -> [Move.Narrative "Você encontra redenção. +1 spirit."]
  WeakHit -> [Move.LoseSpirit 1, Move.Narrative "Sofra -1 spirit."]
  Miss -> [Move.LoseSpirit 2, Move.Narrative "Você é desonrado. -2 spirit."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Advance - Ganhe XP
getAdvanceConsequences :: RollResult -> [Move.Consequence]
getAdvanceConsequences result = case result of
  StrongHit -> [Move.Narrative "Gaste 3 XP para adicionar asset ou upgrade."]
  WeakHit -> [Move.Narrative "Gaste XP (conforme regras)."]
  Miss -> [Move.Narrative "XP insuficiente."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Ask the Oracle - Consulte oráculo (não usa roll padrão)
getAskOracleConsequences :: RollResult -> [Move.Consequence]
getAskOracleConsequences _result =
  [Move.Narrative "Use :oracle \"Nome\" para consultar oráculos."]

-- | Consequências padrão para moves não implementados
getDefaultConsequences :: RollResult -> [Move.Consequence]
getDefaultConsequences result = case result of
  StrongHit -> [Move.Narrative "Sucesso total!"]
  WeakHit -> [Move.Narrative "Sucesso parcial."]
  Miss -> [Move.Narrative "Falha."]
  InvalidRoll -> [Move.Narrative "Rolagem inválida."]

-- | Interface pública (compatibilidade)
applyConsequencesImpl :: Dice.Handle -> Move.Handle -> [Move.Consequence] -> GameContext.Context -> GameContext.Handle -> IO GameContext.Context
applyConsequencesImpl diceH _moveH consequences ctx ctxH = do
  applyConsequencesImplInternal diceH consequences ctx ctxH

-- | Implementação interna que executa moves automaticamente
applyConsequencesImplInternal :: Dice.Handle -> [Move.Consequence] -> GameContext.Context -> GameContext.Handle -> IO GameContext.Context
applyConsequencesImplInternal diceH consequences ctx ctxH = do
  foldM (applyConsequence diceH ctxH) ctx consequences
  where
    applyConsequence :: Dice.Handle -> GameContext.Handle -> GameContext.Context -> Move.Consequence -> IO GameContext.Context
    applyConsequence diceHandle ctxHandle currentCtx cons = case cons of
      Move.LoseHealth amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newHealth = max 0 (GameContext.health res - amount)
        let newRes = res { GameContext.health = newHealth }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Perdeu " ++ show amount ++ " health (atual: " ++ show newHealth ++ ")"
        when (newHealth == 0) $
          putStrLn "  ⚠ Health = 0!"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.LoseSpirit amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newSpirit = max 0 (GameContext.spirit res - amount)
        let newRes = res { GameContext.spirit = newSpirit }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Perdeu " ++ show amount ++ " spirit (atual: " ++ show newSpirit ++ ")"
        when (newSpirit == 0) $
          putStrLn "  ⚠ Spirit = 0!"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.LoseSupply amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newSupply = max 0 (GameContext.supply res - amount)
        let newRes = res { GameContext.supply = newSupply }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Perdeu " ++ show amount ++ " supply (atual: " ++ show newSupply ++ ")"
        when (newSupply == 0) $
          putStrLn "  ⚠ Supply = 0!"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.LoseMomentum amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newMomentum = GameContext.momentum res - amount
        let newRes = res { GameContext.momentum = newMomentum }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Perdeu " ++ show amount ++ " momentum (atual: " ++ show newMomentum ++ ")"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.GainHealth amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newHealth = min 5 (GameContext.health res + amount)
        let newRes = res { GameContext.health = newHealth }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Ganhou " ++ show amount ++ " health (atual: " ++ show newHealth ++ ")"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.GainSpirit amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newSpirit = min 5 (GameContext.spirit res + amount)
        let newRes = res { GameContext.spirit = newSpirit }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Ganhou " ++ show amount ++ " spirit (atual: " ++ show newSpirit ++ ")"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.GainSupply amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newSupply = min 5 (GameContext.supply res + amount)
        let newRes = res { GameContext.supply = newSupply }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Ganhou " ++ show amount ++ " supply (atual: " ++ show newSupply ++ ")"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.GainMomentum amount -> do
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        let newMomentum = min 10 (GameContext.momentum res + amount)
        let newRes = res { GameContext.momentum = newMomentum }
        let newChar = char { GameContext.resources = newRes }
        putStrLn $ "  → Ganhou " ++ show amount ++ " momentum (atual: " ++ show newMomentum ++ ")"
        return $ currentCtx { GameContext.mainCharacter = newChar }

      Move.Narrative text -> do
        putStrLn $ "\n" ++ T.unpack text
        return currentCtx

      Move.PlayerChoice choices -> do
        maybeChoice <- showChoicesImpl choices
        case maybeChoice of
          Just choice -> do
            putStrLn $ "\nVocê escolheu: " ++ T.unpack (Move.choiceDescription choice)
            applyConsequencesImplInternal diceHandle (Move.choiceConsequences choice) currentCtx ctxHandle
          Nothing -> do
            putStrLn "\nNenhuma escolha válida."
            return currentCtx

      Move.TriggerMove _nextMove -> do
        -- Por enquanto apenas informa, execução de moves encadeados
        -- será implementada no ActionService
        putStrLn "  → Você deve executar outro move!"
        return currentCtx
      
      Move.AddBonus bonus -> do
        putStrLn $ "  → Bônus adicionado: " ++ T.unpack (GameContext.bonusDescription bonus) ++ " (+" ++ show (GameContext.bonusValue bonus) ++ ")"
        GameContext.addBonus ctxHandle currentCtx bonus
      
      Move.TriggerOracle oracleName -> do
        -- Verifica condição antes de executar
        let char = GameContext.mainCharacter currentCtx
        let res = GameContext.resources char
        
        let shouldTrigger = case T.unpack oracleName of
              "Endure Harm" -> GameContext.health res == 0
              "Endure Stress" -> GameContext.spirit res == 0
              _ -> True  -- Outros oráculos sempre executam
        
        if shouldTrigger
          then do
            putStrLn $ "  → Invocando oráculo " ++ T.unpack oracleName ++ "..."
            return currentCtx
          else
            return currentCtx

-- | Processa uma escolha do jogador (compatibilidade)
processChoiceImpl :: Move.Choice -> GameContext.Context -> GameContext.Handle -> IO GameContext.Context
processChoiceImpl _ ctx _ctxH = do return ctx

-- | Mostra menu de escolhas e obtém resposta do jogador
showChoicesImpl :: [Move.Choice] -> IO (Maybe Move.Choice)
showChoicesImpl choices = do
  putStrLn "\n=== Escolha uma opção ==="
  mapM_ showChoice (zip [(1 :: Int)..] choices)
  putStr "\nDigite o número da sua escolha: "
  hFlush stdout  -- Garante que o prompt seja exibido antes de ler
  input <- TIO.getLine

  case TR.decimal input of
    Right (n, _) | n > 0 && n <= length choices ->
      return $ Just (choices !! (n - 1))
    _ -> do
      putStrLn "Opção inválida."
      return Nothing
  where
    showChoice (idx, choice) =
      putStrLn $ show idx ++ ". " ++ T.unpack (Move.choiceDescription choice)

-- | Parse nome de move
parseMoveTypeImpl :: T.Text -> Maybe Move.MoveType
parseMoveTypeImpl text =
  case T.toLower . T.strip $ text of
    -- Fate Moves
    "paytheprice" -> Just Move.PayThePrice

    -- Adventure Moves
    "facedanger" -> Just Move.FaceDanger
    "gatherinformation" -> Just Move.GatherInformation
    "secureadvantage" -> Just Move.SecureAdvantage
    "undertakejourney" -> Just Move.UndertakeJourney
    "heal" -> Just Move.Heal
    "resupply" -> Just Move.Resupply
    "makecamp" -> Just Move.MakeCamp

    -- Relationship Moves
    "compel" -> Just Move.CompelAction
    "sojourn" -> Just Move.Sojourn
    "drawthecircle" -> Just Move.DrawTheCircle
    "forgeabond" -> Just Move.ForgeABond
    "testyourbond" -> Just Move.TestYourBond
    "aidyourally" -> Just Move.AidYourAlly
    
    -- Combat Moves
    "enterthefray" -> Just Move.EnterTheFray
    "strike" -> Just Move.Strike
    "clash" -> Just Move.Clash
    "turnthetide" -> Just Move.TurnTheTide
    "endthefight" -> Just Move.EndTheFight
    "battle" -> Just Move.Battle
    
    -- Suffer Moves
    "endureharm" -> Just Move.EndurHarm
    "facedeath" -> Just Move.FaceDeath
    "endurestress" -> Just Move.EndurStress
    "facedesolation" -> Just Move.FaceDesolation
    "outofsupply" -> Just Move.OutOfSupply
    "faceasetback" -> Just Move.FaceSetback
    
    -- Quest Moves
    "swearironvow" -> Just Move.SwearIronVow
    "reachmilestone" -> Just Move.ReachMilestone
    "fulfillyourvow" -> Just Move.FulfillYourVow
    "forsakeyourvow" -> Just Move.ForsakeYourVow
    "advance" -> Just Move.Advance
    
    -- Progress Moves
    "reachdestination" -> Just Move.ReachYourDestination
    "writeyourepilogue" -> Just Move.WriteYourEpilogue
    
    -- Fate Moves
    "askoracle" -> Just Move.AskTheOracle

    _ -> Nothing

-- | Parse stat (apenas nome)
parseStatImpl :: T.Text -> Maybe Move.Stat
parseStatImpl text =
  case T.toLower . T.strip $ text of
    "iron" -> Just Move.Iron
    "edge" -> Just Move.Edge
    "heart" -> Just Move.Heart
    "shadow" -> Just Move.Shadow
    "wits" -> Just Move.Wits
    _ -> Nothing



