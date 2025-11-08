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
import System.DiceContract (RollResult (..))
import qualified System.ConsequenceContract as Consequence
import System.ConsequenceContract (Consequence(..), Choice(..), MoveType(..))

-- | Cria novo handle para o serviço de Moves
newHandle :: Dice.Handle -> IO Move.Handle
newHandle diceH = do
  return $ Move.Handle
    { Move.executeMove = executeMoveImpl diceH
    , Move.executeMoveWithRoll = executeMoveWithRollImpl diceH
    , Move.showChoices = showChoicesImpl
    , Move.parseMoveType = parseMoveTypeImpl
    , Move.parseStat = parseStatImpl
    }

-- | Executa um move (rola dados automaticamente) - retorna apenas consequências
executeMoveImpl :: Dice.Handle -> MoveType -> Maybe Move.Stat -> GameContext.Attributes -> GameContext.Resources -> IO [Consequence]
executeMoveImpl diceH moveType maybeStat attrs resources = do
  -- Rola 1d6 + 2d10
  rolls <- Dice.roll diceH "1d6,2d10"
  
  case rolls of
    [(_, actionDie), (_, ch1), (_, ch2)] -> 
      executeMoveWithRollImpl diceH moveType maybeStat actionDie (ch1, ch2) attrs resources
    _ -> do
      -- Erro na rolagem
      return [Narrative "Erro na rolagem de dados"]

-- | Executa um move com rolagem já feita - retorna apenas as consequências
executeMoveWithRollImpl :: Dice.Handle -> MoveType -> Maybe Move.Stat -> Int -> (Int, Int) -> GameContext.Attributes -> GameContext.Resources -> IO [Consequence]
executeMoveWithRollImpl _diceH moveType maybeStat actionDie (ch1, ch2) attrs _resources = do
  -- Calcula modificador do stat
  let statModifier = case maybeStat of
        Just stat -> getStatValue stat attrs
        Nothing -> 0
  
  let totalModifier = statModifier  -- Por enquanto sem bônus, ActionService gerenciará
  let actionTotal = actionDie + totalModifier
  let rollResult = evaluateRoll actionTotal ch1 ch2
  let isMatch = ch1 == ch2

  -- Obtém consequências baseadas no move e resultado
  consequences <- getMoveConsequences moveType rollResult isMatch
  
  -- Adiciona informações da rolagem como narrativa
  let rollInfo = Narrative $ T.pack $ 
        "\n>>> " ++ T.unpack (Consequence.moveTypeToText moveType) ++ " <<<\n" ++
        "Action Die: " ++ show actionDie ++ " + " ++ show totalModifier ++ " = " ++ show actionTotal ++ "\n" ++
        "Challenge Dice: " ++ show ch1 ++ ", " ++ show ch2 ++ "\n" ++
        "Resultado: " ++ showRollResult rollResult ++
        (if isMatch then "\n[!] MATCH detectado!" else "")
  
  return $ rollInfo : consequences

-- | Obtém valor de um stat dos atributos
getStatValue :: Move.Stat -> GameContext.Attributes -> Int
getStatValue stat attrs = case stat of
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
showRollResult StrongHit = "[+] STRONG HIT"
showRollResult WeakHit = "[~] WEAK HIT"
showRollResult Miss = "[X] MISS"
showRollResult InvalidRoll = "INVALID"

-- | Retorna consequências de um move baseado no resultado
getMoveConsequences :: MoveType -> RollResult -> Bool -> IO [Consequence]
getMoveConsequences moveType result isMatch = do
  let baseConsequences = case moveType of
        -- Fate Moves
        PayThePrice -> getPayThePriceConsequences result isMatch
        AskTheOracle -> getAskOracleConsequences result
        
        -- Adventure Moves
        FaceDanger -> getFaceDangerConsequences result isMatch
        GatherInformation -> getGatherInformationConsequences result isMatch
        SecureAdvantage -> getSecureAdvantageConsequences result
        Heal -> getHealConsequences result
        Resupply -> getResupplyConsequences result
        MakeCamp -> getMakeCampConsequences result
        UndertakeJourney -> getUndertakeJourneyConsequences result
        
        -- Relationship Moves
        CompelAction -> getCompelConsequences result
        Sojourn -> getSojournConsequences result
        ForgeABond -> getForgeABondConsequences result
        TestYourBond -> getTestYourBondConsequences result
        AidYourAlly -> getAidYourAllyConsequences result
        
        -- Combat Moves
        EnterTheFray -> getEnterTheFrayConsequences result
        Strike -> getStrikeConsequences result
        Clash -> getClashConsequences result
        TurnTheTide -> getTurnTheTideConsequences result
        
        -- Suffer Moves
        EndurHarm -> getEndurHarmConsequences result
        FaceDeath -> getFaceDeathConsequences result
        EndurStress -> getEndurStressConsequences result
        FaceDesolation -> getFaceDesolationConsequences result
        OutOfSupply -> getOutOfSupplyConsequences result
        FaceSetback -> getFaceSetbackConsequences result
        
        -- Quest Moves
        SwearIronVow -> getSwearIronVowConsequences result
        ReachMilestone -> getReachMilestoneConsequences result
        ForsakeYourVow -> getForsakeVowConsequences result
        Advance -> getAdvanceConsequences result
        
        _ -> getDefaultConsequences result
  
  return baseConsequences

-- | Pay the Price - Sofra as consequências (baseado nas regras oficiais)
-- O jogador NÃO rola dados neste move, ele ESCOLHE entre 3 opções
getPayThePriceConsequences :: RollResult -> Bool -> [Consequence]
getPayThePriceConsequences _result _isMatch =
  [ PlayerChoice
      [ Choice "Fazer o resultado negativo mais óbvio acontecer (escolha narrativa)" 
          [Narrative "Descreva o que acontece de pior..."]
      
      , Choice "Visualizar dois resultados e usar Ask the Oracle (sim/não)" 
          [ Narrative "Role 1d2: 1=primeiro resultado, 2=segundo resultado"
          , TriggerOracle "Default"
          ]
      
      , Choice "Rolar na tabela Pagar o Preço" 
          [TriggerOracle "\"Pagar o Preço\""]
      ]
  ]

-- | Face Danger - Enfrente um perigo (CORRIGIDO conforme PDF)
getFaceDangerConsequences :: RollResult -> Bool -> [Consequence]
getFaceDangerConsequences result isMatch =
  let matchPenalty = ([LoseMomentum 1 | isMatch])
  in matchPenalty ++ case result of
    StrongHit ->
      [ GainMomentum 1
      , Narrative "Você é bem-sucedido."
      ]

    WeakHit ->
      [ PlayerChoice
          [ Choice "Delayed, lose advantage, or face new danger (-1 momentum)" 
              [LoseMomentum 1]
          , Choice "You are tired or hurt (Endure Harm - 1 harm)" 
              [TriggerMove EndurHarm]
          , Choice "You are dispirited or afraid (Endure Stress - 1 stress)" 
              [TriggerMove EndurStress]
          , Choice "You sacrifice resources (-1 supply)" 
              [LoseSupply 1]
          ]
      , Narrative "Você teve sucesso, mas enfrenta um custo problemático."
      ]

    Miss ->
      [ TriggerMove PayThePrice
      , Narrative "Você falha ou seu progresso é prejudicado."
      ]

    InvalidRoll ->
      [Narrative "Rolagem inválida."]

-- | Gather Information - Colete informações (CORRIGIDO conforme PDF)
getGatherInformationConsequences :: RollResult -> Bool -> [Consequence]
getGatherInformationConsequences result isMatch =
  let matchBonus = if isMatch then [GainMomentum 1, Narrative "Você descobre algo excepcional!"] else []
  in case result of
    StrongHit ->
      [ GainMomentum 2
      , Narrative "Você descobre algo útil e específico. O caminho é claro."
      ] ++ matchBonus

    WeakHit ->
      [ GainMomentum 1
      , Narrative "A informação complica sua missão ou introduz um novo perigo."
      ] ++ matchBonus

    Miss ->
      [ TriggerMove PayThePrice
      , Narrative "Sua investigação revela uma ameaça terrível ou verdade indesejada."
      ]

    InvalidRoll ->
      [Narrative "Rolagem inválida."]

-- | Swear an Iron Vow - Jure um voto (roll +heart)
getSwearIronVowConsequences :: RollResult -> [Consequence]
getSwearIronVowConsequences result = case result of
  StrongHit ->
    [ GainMomentum 2
    , Narrative "Você está determinado. O próximo passo é claro."
    ]
  WeakHit ->
    [ GainMomentum 1
    , Narrative "Você está determinado, mas enfrenta incerteza ou dúvida."
    ]
  Miss ->
    [ PlayerChoice
        [ Choice "Aceitar o desafio (-2 momentum)" [LoseMomentum 2]
        , Choice "Prove-se primeiro (Face Danger)" [TriggerMove FaceDanger]
        ]
    , Narrative "Seu voto é posto em questão..."
    ]
  InvalidRoll ->
    [Narrative "Rolagem inválida."]

-- | Undertake a Journey - Viaje (roll +wits)
-- Se partindo de bond community, adiciona +1 no primeiro roll
getUndertakeJourneyConsequences :: RollResult -> [Consequence]
getUndertakeJourneyConsequences result = case result of
  StrongHit ->
    [ PlayerChoice
        [ Choice "Recursos sábios: marque progresso" []
        , Choice "Velocidade: marque progresso, +1 momentum, -1 supply" 
            [GainMomentum 1, LoseSupply 1]
        ]
    , Narrative "Você alcança um waypoint."
    ]
  WeakHit ->
    [ LoseSupply 1
    , Narrative "Você alcança um waypoint e marca progresso, mas perde supply."
    ]
  Miss ->
    [ TriggerMove PayThePrice
    , Narrative "Você é impedido por um evento perigoso."
    ]
  InvalidRoll ->
    [Narrative "Rolagem inválida."]

-- | Reach a Milestone - Alcance um marco
getReachMilestoneConsequences :: RollResult -> [Consequence]
getReachMilestoneConsequences result = case result of
  StrongHit ->
    [ Narrative "Marco alcançado! Marque progresso no seu voto."
    ]
  WeakHit ->
    [ Narrative "Marco alcançado com complicação."
    ]
  Miss ->
    [ Narrative "Você não completa o marco ainda."
    ]
  InvalidRoll ->
    [Narrative "Rolagem inválida."]

-- | Secure an Advantage - Ganhe vantagem (CORRIGIDO conforme PDF)
getSecureAdvantageConsequences :: RollResult -> [Consequence]
getSecureAdvantageConsequences result = case result of
  StrongHit -> 
    [ PlayerChoice
        [ Choice "Take control: Faça outro move agora com +1" 
            [AddBonus (GameContext.ActiveBonus GameContext.NextRoll 1 "Take Control")]
        , Choice "Prepare to act: +2 momentum" 
            [GainMomentum 2]
        ]
    , Narrative "Você ganha vantagem."
    ]
  
  WeakHit -> 
    [ GainMomentum 1
    , Narrative "Sua vantagem é de curta duração."
    ]
  
  Miss -> 
    [ TriggerMove PayThePrice
    , Narrative "Você falha ou suas suposições te traem."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Heal - Recupere health (CORRIGIDO - roll +wits, ou +wits/+iron menor se self)
-- NOTA: Sistema de "wounded" não implementado (seria flag no Context)
getHealConsequences :: RollResult -> [Consequence]
getHealConsequences result = case result of
  StrongHit -> 
    [ GainHealth 2
    , Narrative "Seu cuidado é útil. Pode limpar 'wounded' e ganhar até +2 health."
    ]
  
  WeakHit -> 
    [ PlayerChoice
        [ Choice "Ganhe +2 health, sofra -1 supply" 
            [GainHealth 2, LoseSupply 1]
        , Choice "Ganhe +2 health, sofra -1 momentum" 
            [GainHealth 2, LoseMomentum 1]
        ]
    , Narrative "Como Strong Hit, mas sofra -1 supply ou -1 momentum."
    ]
  
  Miss -> 
    [ TriggerMove PayThePrice
    , Narrative "Sua ajuda é ineficaz."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Resupply - Recupere supply (CORRIGIDO conforme PDF - roll +wits)
getResupplyConsequences :: RollResult -> [Consequence]
getResupplyConsequences result = case result of
  StrongHit -> 
    [ GainSupply 2
    , Narrative "Você reforça seus recursos."
    ]
  
  WeakHit -> 
    [ PlayerChoice
        [ Choice "Ganhe +1 supply, sofra -1 momentum" 
            [GainSupply 1, LoseMomentum 1]
        , Choice "Ganhe +2 supply, sofra -2 momentum" 
            [GainSupply 2, LoseMomentum 2]
        ]
    , Narrative "Você pode ganhar até +2 supply, mas sofre -1 momentum para cada."
    ]
  
  Miss -> 
    [ TriggerMove PayThePrice
    , Narrative "Você não encontra nada útil."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Make Camp - Acampe (CORRIGIDO conforme PDF - roll +supply)
getMakeCampConsequences :: RollResult -> [Consequence]
getMakeCampConsequences result = case result of
  StrongHit -> 
    [ Narrative "Escolha DUAS opções:"
    , PlayerChoice
        [ Choice "Recuperate: +1 health" [GainHealth 1]
        , Choice "Partake: -1 supply, +1 health" [LoseSupply 1, GainHealth 1]
        , Choice "Relax: +1 spirit" [GainSpirit 1]
        , Choice "Focus: +1 momentum" [GainMomentum 1]
        , Choice "Prepare: +1 quando Undertake Journey" 
            [AddBonus (GameContext.ActiveBonus (GameContext.NextMove "Undertake a Journey") 1 "Prepared")]
        ]
    ]
  
  WeakHit -> 
    [ Narrative "Escolha UMA opção:"
    , PlayerChoice
        [ Choice "Recuperate: +1 health" [GainHealth 1]
        , Choice "Partake: -1 supply, +1 health" [LoseSupply 1, GainHealth 1]
        , Choice "Relax: +1 spirit" [GainSpirit 1]
        , Choice "Focus: +1 momentum" [GainMomentum 1]
        , Choice "Prepare: +1 quando Undertake Journey" 
            [AddBonus (GameContext.ActiveBonus (GameContext.NextMove "Undertake a Journey") 1 "Prepared")]
        ]
    ]
  
  Miss -> 
    [ TriggerMove PayThePrice
    , Narrative "Você não encontra conforto."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Compel - Persuadir (CORRIGIDO - roll +heart/+iron/+shadow)
-- +1 se share bond
getCompelConsequences :: RollResult -> [Consequence]
getCompelConsequences result = case result of
  StrongHit ->
    [ GainMomentum 1
    , Narrative "Eles farão o que você quer ou compartilharão o que sabem."
    , Narrative "Se usar para Gather Information, faça esse move agora com +1."
    ]
  
  WeakHit ->
    [ Narrative "Como Strong Hit, mas eles pedem algo em retorno."
    , PlayerChoice
        [ Choice "Decida sem consultar o oráculo" []
        , Choice "Pergunte ao oráculo (sim/não) para saber o que eles exigem"
            [TriggerOracle "Default"]
        ]
    ]
  
  Miss ->
    [ TriggerMove PayThePrice
    , Narrative "Eles recusam ou fazem demanda que custa caro."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Sojourn - Descanse em comunidade (CORRIGIDO - roll +heart, +1 se bond)
-- Strong Hit: Escolha 2 (ou 3 se bond). Weak Hit: Escolha 1 (ou 2 se bond)
getSojournConsequences :: RollResult -> [Consequence]
getSojournConsequences result = case result of
  StrongHit ->
    [ Narrative "Escolha DUAS das opções abaixo (+1 se tiver bond):"
    , PlayerChoice
        [ Choice "Mend: Clear wounded, +1 health" [GainHealth 1]
        , Choice "Hearten: Clear shaken, +1 spirit" [GainSpirit 1]
        , Choice "Equip: Clear unprepared, +1 supply" [GainSupply 1]
        , Choice "Recuperate: +2 health" [GainHealth 2]
        , Choice "Consort: +2 spirit" [GainSpirit 2]
        , Choice "Provision: +2 supply" [GainSupply 2]
        , Choice "Plan: +2 momentum" [GainMomentum 2]
        ]
    ]
  
  WeakHit -> 
    [ Narrative "Escolha UMA opção (+1 se tiver bond):"
    , PlayerChoice
        [ Choice "Mend: Clear wounded, +1 health" [GainHealth 1]
        , Choice "Hearten: Clear shaken, +1 spirit" [GainSpirit 1]
        , Choice "Equip: Clear unprepared, +1 supply" [GainSupply 1]
        , Choice "Recuperate: +2 health" [GainHealth 2]
        , Choice "Consort: +2 spirit" [GainSpirit 2]
        , Choice "Provision: +2 supply" [GainSupply 2]
        , Choice "Plan: +2 momentum" [GainMomentum 2]
        ]
    ]
  
  Miss -> 
    [ TriggerMove PayThePrice
    , Narrative "Você não encontra ajuda aqui."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Forge a Bond - Crie vínculo (roll +heart)
getForgeABondConsequences :: RollResult -> [Consequence]
getForgeABondConsequences result = case result of
  StrongHit -> [Narrative "Vínculo formado.", MarkBondProgress]
  WeakHit -> [Narrative "Eles pedem mais de você. Faça ou jure voto."]
  Miss -> [TriggerMove PayThePrice, Narrative "Você não consegue o vínculo."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Test Your Bond - Teste vínculo (roll +heart)
getTestYourBondConsequences :: RollResult -> [Consequence]
getTestYourBondConsequences result = case result of
  StrongHit ->
    [ PlayerChoice
        [ Choice "Ganhe +1 spirit" [GainSpirit 1]
        , Choice "Ganhe +2 momentum" [GainMomentum 2]
        ]
    , Narrative "O vínculo se fortalece."
    ]
  WeakHit -> [Narrative "Prove sua lealdade ou perca o vínculo."]
  Miss -> [TriggerMove PayThePrice, Narrative "Vínculo quebrado."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Aid Your Ally - Ajude aliado (roll +heart)
getAidYourAllyConsequences :: RollResult -> [Consequence]
getAidYourAllyConsequences result = case result of
  StrongHit -> 
    [ AddBonus (GameContext.ActiveBonus GameContext.NextRoll 1 "Ajuda de Aliado")
    , Narrative "Você recebe +1 no próximo roll (representando ajuda do aliado)."
    ]
  WeakHit -> 
    [ GainMomentum 1
    , Narrative "Você ajuda mas se expõe a perigo."
    ]
  Miss -> [TriggerMove PayThePrice, Narrative "Você falha e complica a situação."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Enter the Fray - Inicie combate (CORRIGIDO conforme PDF)
-- Roll: +heart (facing), +shadow (ambush/surprise), +wits (ambushed)
getEnterTheFrayConsequences :: RollResult -> [Consequence]
getEnterTheFrayConsequences result = case result of
  StrongHit -> 
    [ GainMomentum 2
    , Narrative "Você tem iniciativa. Crie combat progress track com rank do inimigo."
    ]
  
  WeakHit ->
    [ PlayerChoice
        [ Choice "Bolster your position: +2 momentum" 
            [GainMomentum 2]
        , Choice "Prepare to act: Ganhe iniciativa" 
            [Narrative "Você tem iniciativa agora."]
        ]
    ]
  
  Miss -> 
    [ TriggerMove PayThePrice
    , Narrative "Combate começa em desvantagem. Inimigo tem iniciativa."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Strike - Ataque (CORRIGIDO - roll +iron close, +edge range)
-- Requer: Iniciativa. Marca progress no combat track.
getStrikeConsequences :: RollResult -> [Consequence]
getStrikeConsequences result = case result of
  StrongHit ->
    [ Narrative "Inflija dano +1. Você retém iniciativa."
    , Narrative "(Marque progresso no combat track: harm base + 1)"
    ]
  
  WeakHit ->
    [ Narrative "Inflija seu dano e perca iniciativa."
    , Narrative "(Marque progresso no combat track. Inimigo tem iniciativa.)"
    ]
  
  Miss ->
    [ TriggerMove PayThePrice
    , Narrative "Seu ataque falha. Inimigo tem iniciativa."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Clash - Defenda/Contra-ataque (CORRIGIDO - roll +iron close, +edge range)
-- Requer: Inimigo com iniciativa. Marca progress no combat track.
getClashConsequences :: RollResult -> [Consequence]
getClashConsequences result = case result of
  StrongHit ->
    [ PlayerChoice
        [ Choice "Bolster your position: +1 momentum, você tem iniciativa" 
            [GainMomentum 1, Narrative "Você tem iniciativa."]
        , Choice "Find an opening: Inflija +1 harm, você tem iniciativa" 
            [Narrative "Inflija harm +1. Você tem iniciativa."]
        ]
    , Narrative "Inflija seu dano."
    ]
  
  WeakHit ->
    [ TriggerMove PayThePrice
    , Narrative "Inflija seu dano, mas inimigo retém iniciativa."
    ]
  
  Miss ->
    [ TriggerMove PayThePrice
    , Narrative "Você está em desvantagem. Inimigo retém iniciativa."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Turn the Tide - Vire o jogo
getTurnTheTideConsequences :: RollResult -> [Consequence]
getTurnTheTideConsequences result = case result of
  StrongHit -> [Narrative "Você retoma iniciativa. Resetar momentum para +2."]
  WeakHit -> [Narrative "Não retoma iniciativa, mas pode continuar."]
  Miss -> [TriggerMove PayThePrice, Narrative "Situação piora drasticamente."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Endure Harm - Sofra dano (roll +health ou +iron, o maior)
-- NOTA: Este move é especial - precisa do valor de harm como parâmetro
-- Implementação simplificada: apenas as consequências pós-rolagem
getEndurHarmConsequences :: RollResult -> [Consequence]
getEndurHarmConsequences result = case result of
  StrongHit ->
    [ PlayerChoice
        [ Choice "Shake it off: -1 momentum, +1 health (se health > 0)" 
            [LoseMomentum 1, GainHealth 1]
        , Choice "Embrace the pain: +1 momentum" 
            [GainMomentum 1]
        ]
    ]
  
  WeakHit -> 
    [ Narrative "Você prossegue apesar do dano."
    ]
  
  Miss -> 
    [ LoseMomentum 1
    , TriggerOracle "\"Resistir Dano\""  -- Se health = 0, executa oráculo automaticamente
    , Narrative "Sofra -1 momentum."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Face Death - À beira da morte
getFaceDeathConsequences :: RollResult -> [Consequence]
getFaceDeathConsequences result = case result of
  StrongHit -> [GainHealth 1, Narrative "Você sobrevive por pouco. +1 health."]
  WeakHit -> [Narrative "Você fica incapacitado ou morrendo. Aliado deve intervir."]
  Miss -> [Narrative "Você morre. Escreva seu epitáfio."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Endure Stress - Sofra stress (roll +spirit ou +heart, o maior)
-- Similar a Endure Harm mas para dano mental
getEndurStressConsequences :: RollResult -> [Consequence]
getEndurStressConsequences result = case result of
  StrongHit -> 
    [ PlayerChoice
        [ Choice "Shake it off: -1 momentum, +1 spirit (se spirit > 0)"
            [LoseMomentum 1, GainSpirit 1]
        , Choice "Embrace the pain: +1 momentum"
            [GainMomentum 1]
        ]
    ]
  
  WeakHit -> 
    [ Narrative "Você prossegue apesar do stress."
    ]
  
  Miss -> 
    [ LoseMomentum 1
    , TriggerOracle "\"Resistir Estresse\""  -- Se spirit = 0, executa oráculo automaticamente
    , Narrative "Sofra -1 momentum."
    ]
  
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Face Desolation - À beira do colapso
getFaceDesolationConsequences :: RollResult -> [Consequence]
getFaceDesolationConsequences result = case result of
  StrongHit -> [GainSpirit 1, Narrative "Você encontra forças. +1 spirit."]
  WeakHit -> [Narrative "Você está desfeito. Continue com desvantagem."]
  Miss -> [Narrative "Você é consumido. Abandone sua missão ou aja contra companheiros."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Out of Supply - Sem suprimentos
getOutOfSupplyConsequences :: RollResult -> [Consequence]
getOutOfSupplyConsequences result = case result of
  StrongHit -> [Narrative "Você se vira sem supply."]
  WeakHit -> [LoseMomentum 1, Narrative "Sofra -1 momentum."]
  Miss -> [TriggerMove PayThePrice, Narrative "Você enfrenta sérias privações."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Face a Setback - Revés em progress
getFaceSetbackConsequences :: RollResult -> [Consequence]
getFaceSetbackConsequences result = case result of
  StrongHit -> [Narrative "Você mantém seu progresso."]
  WeakHit -> [Narrative "Perca metade do progresso (arredonde para cima)."]
  Miss -> [Narrative "Perca todo o progresso."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Forsake Your Vow - Abandone voto
getForsakeVowConsequences :: RollResult -> [Consequence]
getForsakeVowConsequences result = case result of
  StrongHit -> [Narrative "Você encontra redenção. +1 spirit."]
  WeakHit -> [LoseSpirit 1, Narrative "Sofra -1 spirit."]
  Miss -> [LoseSpirit 2, Narrative "Você é desonrado. -2 spirit."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Advance - Ganhe XP
getAdvanceConsequences :: RollResult -> [Consequence]
getAdvanceConsequences result = case result of
  StrongHit -> [Narrative "Gaste 3 XP para adicionar asset ou upgrade."]
  WeakHit -> [Narrative "Gaste XP (conforme regras)."]
  Miss -> [Narrative "XP insuficiente."]
  InvalidRoll -> [Narrative "Rolagem inválida."]

-- | Ask the Oracle - Consulte oráculo (não usa roll padrão)
getAskOracleConsequences :: RollResult -> [Consequence]
getAskOracleConsequences _result = [TriggerOracle "Default"]

-- | Consequências padrão para moves não implementados
getDefaultConsequences :: RollResult -> [Consequence]
getDefaultConsequences result = case result of
  StrongHit -> [Narrative "Sucesso total!"]
  WeakHit -> [Narrative "Sucesso parcial."]
  Miss -> [Narrative "Falha."]
  InvalidRoll -> [Narrative "Rolagem inválida."]


-- | Implementação temporária: retorna a primeira opção
-- TODO: A TUI deveria lidar com as escolhas do usuário
showChoicesImpl :: [Choice] -> IO (Maybe Choice)
showChoicesImpl [] = return Nothing
showChoicesImpl (firstChoice:_) = return $ Just firstChoice

-- | Parse nome de move
parseMoveTypeImpl :: T.Text -> Maybe MoveType
parseMoveTypeImpl text =
  case T.toLower . T.strip $ text of
    -- Fate Moves
    "paytheprice" -> Just PayThePrice

    -- Adventure Moves
    "facedanger" -> Just FaceDanger
    "gatherinformation" -> Just GatherInformation
    "secureadvantage" -> Just SecureAdvantage
    "undertakejourney" -> Just UndertakeJourney
    "heal" -> Just Heal
    "resupply" -> Just Resupply
    "makecamp" -> Just MakeCamp

    -- Relationship Moves
    "compel" -> Just CompelAction
    "sojourn" -> Just Sojourn
    "drawthecircle" -> Just DrawTheCircle
    "forgeabond" -> Just ForgeABond
    "testyourbond" -> Just TestYourBond
    "aidyourally" -> Just AidYourAlly
    
    -- Combat Moves
    "enterthefray" -> Just EnterTheFray
    "strike" -> Just Strike
    "clash" -> Just Clash
    "turnthetide" -> Just TurnTheTide
    "endthefight" -> Just EndTheFight
    "battle" -> Just Battle
    
    -- Suffer Moves
    "endureharm" -> Just EndurHarm
    "facedeath" -> Just FaceDeath
    "endurestress" -> Just EndurStress
    "facedesolation" -> Just FaceDesolation
    "outofsupply" -> Just OutOfSupply
    "faceasetback" -> Just FaceSetback
    
    -- Quest Moves
    "swearironvow" -> Just SwearIronVow
    "reachmilestone" -> Just ReachMilestone
    "fulfillyourvow" -> Just FulfillYourVow
    "forsakeyourvow" -> Just ForsakeYourVow
    "advance" -> Just Advance
    
    -- Progress Moves
    "reachdestination" -> Just ReachYourDestination
    "writeyourepilogue" -> Just WriteYourEpilogue
    
    -- Fate Moves
    "askoracle" -> Just AskTheOracle

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



