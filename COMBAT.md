# Sistema de Combate - Ironsworn

## Visão Geral

O combate em Ironsworn é **abstrato e narrativo**, não usa grid ou miniaturas. Em vez de rastrear posições exatas e HP individuais, o sistema usa um **Progress Track de Combate** que representa o progresso geral do confronto. O combate é resolvido através de uma série de **moves** (movimentos) que determinam quem tem iniciativa e como o conflito progride.

---

## Conceitos Fundamentais

### 1. Progress Track de Combate

Cada combate usa um **Progress Track** com:
- **10 boxes** (cada box = 4 ticks)
- **Total de 40 ticks** possíveis
- **Rank de dificuldade** do inimigo determina quantos ticks você marca por ação bem-sucedida

**Ranks e Progresso:**
- **Troublesome**: 12 ticks por mark (3 boxes)
- **Dangerous**: 8 ticks por mark (2 boxes) 
- **Formidable**: 4 ticks por mark (1 box)
- **Extreme**: 2 ticks por mark
- **Epic**: 1 tick por mark

### 2. Iniciativa

A **iniciativa** determina quem age no combate:
- Quem tem iniciativa pode usar **Strike** (ataque)
- Quem não tem iniciativa deve usar **Clash** (defesa/contra-ataque)
- A iniciativa muda baseada nos resultados dos moves

### 3. Harm (Dano)

O dano em combate é representado por:
- **Progresso no Combat Track**: Cada ataque bem-sucedido marca progresso
- **Harm base**: Dano base do personagem (geralmente baseado em atributos/equipamento)
- **Harm adicional**: Bônus de +1 em Strong Hits ou escolhas específicas

---

## Moves de Combate

### Enter the Fray (Iniciar Combate)

**Quando usar:** No início de um combate ou quando você entra em um conflito.

**Rolagem:** 
- **+heart** se enfrentando diretamente (facing)
- **+shadow** se emboscando/surpreendendo (ambush/surprise)
- **+wits** se você foi emboscado (ambushed)

**Resultados:**

**Strong Hit:**
- Ganha +2 momentum
- Você tem iniciativa
- Crie um Combat Progress Track com o rank do inimigo

**Weak Hit:**
- Escolha uma opção:
  - **Bolster your position**: +2 momentum
  - **Prepare to act**: Ganhe iniciativa

**Miss:**
- Combate começa em desvantagem
- Inimigo tem iniciativa
- Execute Pay The Price

---

### Strike (Ataque)

**Quando usar:** Quando você tem iniciativa e quer atacar.

**Requisito:** Você deve ter iniciativa para usar este move.

**Rolagem:**
- **+iron** para combate corpo a corpo (close combat)
- **+edge** para combate à distância (ranged combat)

**Resultados:**

**Strong Hit:**
- Inflija dano +1 (harm base + 1)
- Você **retém iniciativa**
- Marque progresso no combat track: harm base + 1

**Weak Hit:**
- Inflija seu dano base
- Você **perde iniciativa** (inimigo ganha)
- Marque progresso no combat track

**Miss:**
- Seu ataque falha
- Inimigo tem iniciativa
- Execute Pay The Price

---

### Clash (Defesa/Contra-ataque)

**Quando usar:** Quando o inimigo tem iniciativa e você precisa se defender ou contra-atacar.

**Requisito:** O inimigo deve ter iniciativa para você usar este move.

**Rolagem:**
- **+iron** para combate corpo a corpo (close combat)
- **+edge** para combate à distância (ranged combat)

**Resultados:**

**Strong Hit:**
- Inflija seu dano base
- Escolha uma opção:
  - **Bolster your position**: +1 momentum, você ganha iniciativa
  - **Find an opening**: Inflija +1 harm, você ganha iniciativa

**Weak Hit:**
- Inflija seu dano base
- Inimigo **retém iniciativa**
- Execute Pay The Price

**Miss:**
- Você está em desvantagem
- Inimigo retém iniciativa
- Execute Pay The Price

---

### Turn the Tide (Virar o Jogo)

**Quando usar:** Quando você está em desvantagem e precisa recuperar controle do combate. Especialmente útil quando seu momentum está baixo.

**Rolagem:** Não usa atributo específico (move especial)

**Resultados:**

**Strong Hit:**
- Você retoma iniciativa
- Resetar momentum para +2

**Weak Hit:**
- Não retoma iniciativa, mas pode continuar lutando

**Miss:**
- Situação piora drasticamente
- Execute Pay The Price

---

### End the Fight (Finalizar Combate)

**Quando usar:** Quando você quer tentar finalizar o combate usando o Progress Track.

**Tipo:** Progress Move (usa o sistema de progress rolls)

**Como funciona:**
1. Você deve ter um Combat Progress Track ativo
2. Faça um **Progress Roll** usando o score atual do track
3. O resultado determina como o combate termina

**Progress Roll:**
- Role 1d6 (action die) + 2d10 (challenge dice)
- Compare o action die com os challenge dice
- Use o score do progress track como modificador

**Resultados:**

**Strong Hit:**
- Você vence o combate de forma decisiva
- Ganha experiência baseada no rank do inimigo:
  - Troublesome: 1 XP
  - Dangerous: 2 XP
  - Formidable: 3 XP
  - Extreme: 4 XP
  - Epic: 5 XP

**Weak Hit:**
- Você vence, mas com complicações
- Ganha experiência (metade do rank, arredondado para cima)
- Pode sofrer consequências narrativas

**Miss:**
- Você falha em finalizar o combate
- Pode perder progresso no track
- O combate continua ou você foge/recua

---

## Moves Relacionados ao Combate

### Endure Harm (Resistir Dano)

**Quando usar:** Quando você sofre dano físico no combate.

**Rolagem:** 
- **+health** ou **+iron** (use o maior)

**Resultados:**

**Strong Hit:**
- Escolha uma opção:
  - **Shake it off**: -1 momentum, +1 health (se health > 0)
  - **Embrace the pain**: +1 momentum

**Weak Hit:**
- Você prossegue apesar do dano

**Miss:**
- Sofra -1 momentum
- Se health = 0, execute oráculo "Resistir Dano" automaticamente

---

### Face Death (À Beira da Morte)

**Quando usar:** Quando você está gravemente ferido ou em perigo mortal.

**Resultados:**

**Strong Hit:**
- Você sobrevive por pouco
- +1 health

**Weak Hit:**
- Você fica incapacitado ou morrendo
- Aliado deve intervir

**Miss:**
- Você morre
- Escreva seu epitáfio

---

## Fluxo de Combate Típico

### 1. Início do Combate

```
1. Execute Enter the Fray
   - Determine quem tem iniciativa
   - Crie Combat Progress Track com rank do inimigo
   - Ganhe momentum se bem-sucedido
```

### 2. Durante o Combate

**Se você tem iniciativa:**
- Use **Strike** para atacar
- Em Strong Hit: mantenha iniciativa e marque progresso
- Em Weak Hit: perca iniciativa mas marque progresso
- Em Miss: perca iniciativa e sofra consequências

**Se o inimigo tem iniciativa:**
- Use **Clash** para defender/contra-atacar
- Em Strong Hit: ganhe iniciativa e marque progresso
- Em Weak Hit: mantenha defesa mas sofra consequências
- Em Miss: continue em desvantagem

**Se em desvantagem:**
- Use **Turn the Tide** para recuperar controle
- Especialmente útil com momentum baixo

### 3. Sofrendo Dano

- Quando sofrer dano, execute **Endure Harm**
- Se health chegar a 0, pode precisar executar **Face Death**

### 4. Finalizando o Combate

- Quando o Progress Track estiver suficientemente preenchido
- Execute **End the Fight** (Progress Move)
- Role contra o progress score
- Determine o resultado final do combate

---

## Estratégias e Dicas

### Gerenciamento de Iniciativa

- **Manter iniciativa** é crucial para controlar o combate
- Strong Hits em Strike mantêm você no controle
- Use Clash estrategicamente para recuperar iniciativa

### Progress Track

- **Múltiplos inimigos**: Você pode agrupar inimigos em um único track aumentando o rank
- **Progresso rápido**: Ranks mais baixos (Troublesome, Dangerous) marcam mais progresso por ação
- **Progresso lento**: Ranks mais altos (Extreme, Epic) requerem mais ações para completar

### Momentum

- Momentum alto ajuda em rolagens
- Turn the Tide reseta momentum para +2, útil quando está baixo
- Alguns moves de combate podem ganhar/perder momentum

### Harm e Recursos

- **Health** é crucial: se chegar a 0, você pode morrer
- Use **Endure Harm** estrategicamente para recuperar health
- **Momentum** pode ser gasto para melhorar resultados

---

## Combate em Grupo

### Múltiplos Inimigos

- Você pode criar um único Progress Track para múltiplos inimigos
- Aumente o rank do track para representar a dificuldade adicional
- Exemplo: 3 inimigos Dangerous podem ser um track Formidable

### Aliados

- Aliados podem ajudar através do move **Aid Your Ally**
- Aliados podem intervir em **Face Death** (Weak Hit)
- Bonds podem fornecer bônus em certas situações

---

## Combate vs. Outros Tipos de Conflito

### Combate Abstrato

O combate em Ironsworn não requer:
- Grid ou miniaturas
- Posicionamento exato
- Contagem de HP individual
- Turnos rígidos

### Foco Narrativo

- O combate é **narrativo e flexível**
- Os moves guiam a narrativa, não o contrário
- Você descreve o que acontece baseado nos resultados dos moves
- O Progress Track representa o estado geral do conflito

---

## Exemplo de Combate

### Cenário: Enfrentando um Lobo Perigoso

**1. Enter the Fray (+heart, enfrentando diretamente)**
- Resultado: Strong Hit
- Você tem iniciativa
- Crie Combat Track "Lobo Perigoso" (rank: Dangerous)
- Ganhe +2 momentum

**2. Strike (+iron, combate corpo a corpo)**
- Resultado: Strong Hit
- Inflija harm +1
- Mantenha iniciativa
- Marque 8 ticks no track (Dangerous = 2 boxes = 8 ticks)

**3. Strike novamente**
- Resultado: Weak Hit
- Inflija harm base
- Perda de iniciativa
- Marque mais 8 ticks (total: 16 ticks = 4 boxes)

**4. Clash (+iron, defesa)**
- Resultado: Strong Hit
- Escolha: "Find an opening"
- Inflija harm +1
- Ganhe iniciativa
- Marque mais 8 ticks (total: 24 ticks = 6 boxes)

**5. Strike (+iron)**
- Resultado: Strong Hit
- Inflija harm +1
- Mantenha iniciativa
- Marque mais 8 ticks (total: 32 ticks = 8 boxes)

**6. End the Fight (Progress Roll)**
- Score atual: 8 boxes
- Action Die: 4
- Challenge Dice: 2, 5
- Resultado: Strong Hit (4 > 2 e 4 > 5? Não, mas com score 8...)
- Você vence o combate!
- Ganhe 2 XP (rank Dangerous)

---

## Moves de Combate no Sistema

No sistema implementado, os seguintes moves de combate estão disponíveis:

- **Enter the Fray**: `:move enterthefray [heart|shadow|wits]`
- **Strike**: `:move strike [iron|edge]`
- **Clash**: `:move clash [iron|edge]`
- **Turn the Tide**: `:move turnthetide`
- **End the Fight**: `:move endthefight` (usa Progress Roll)
- **Endure Harm**: `:move endureharm` (quando sofre dano)
- **Face Death**: `:move facedeath` (quando em perigo mortal)

---

## Referências

- **Ironsworn Rulebook**: Seção de Combate
- **Ironsworn SRD**: Combat Moves
- **Código do Sistema**: `src/System/Move.hs` - Funções de combate

---

## Notas de Implementação

No sistema atual:
- O Combat Progress Track é criado automaticamente quando você executa Enter the Fray
- A iniciativa é rastreada narrativamente (através de mensagens)
- O sistema marca progresso automaticamente nos moves Strike e Clash
- End the Fight usa o sistema de Progress Rolls padrão
- Harm é representado através do progresso no track, não HP individual

