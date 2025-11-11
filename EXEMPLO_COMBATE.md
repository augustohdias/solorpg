# Exemplo de Combate Completo - Ironsworn

Este documento demonstra um combate completo usando **APENAS** os comandos disponíveis na implementação atual do sistema.

## ⚠️ Nota Importante

O sistema atual **não possui um comando direto** para criar um Progress Track de Combate. O comando `:vow` cria apenas tracks do tipo `Vow`. Para este exemplo, assumimos que o track de combate foi criado previamente ou será criado através de uma extensão futura do sistema.

**Comandos disponíveis para combate:**
- `:move enterthefray [heart|shadow|wits]` - Iniciar combate
- `:move strike [iron|edge]` - Ataque (requer iniciativa)
- `:move clash [iron|edge]` - Defesa/Contra-ataque (quando inimigo tem iniciativa)
- `:move turnthetide` - Recuperar controle
- `:move endthefight` - Finalizar combate (usa progress roll)
- `:move endureharm` - Resistir dano físico
- `:progress "<nome>"` - Marcar progresso manualmente no track
- `:fulfill "<nome>"` - Fazer progress roll para finalizar
- `:tracks` - Ver todos os tracks ativos
- `:char` - Ver status do personagem

---

## Cenário: Enfrentando um Lobo Perigoso (Rank: Dangerous)

**Setup inicial:**
- Personagem com atributos: Iron +2, Edge +1, Heart +2
- Health: 5/5, Momentum: +2
- Inimigo: Lobo Perigoso (Rank Dangerous = 8 ticks por mark)

---

## FASE 1: Iniciar Combate

### 1.1 Enter the Fray (Strong Hit)

**Comando:**
```
:move enterthefray heart
```

**Resultado esperado:**
```
>>> EnterTheFray <<<
Action Die: 4 + 2 = 6
Challenge Dice: 3, 5
Resultado: [+] STRONG HIT

[+] Momentum: +2
Você tem iniciativa. Crie combat progress track com rank do inimigo.
```

**Ações do jogador:**
- O sistema indica que você tem iniciativa
- Você precisa criar um Combat Progress Track chamado "Lobo Perigoso" com rank Dangerous
- **Nota:** No sistema atual, isso precisaria ser feito manualmente ou através de uma extensão

**Estado após:**
- Momentum: +4 (era +2, ganhou +2)
- Iniciativa: Você tem

---

## FASE 2: Ataques (Você tem Iniciativa)

### 2.1 Strike - Strong Hit

**Comando:**
```
:move strike iron
```

**Resultado esperado:**
```
>>> Strike <<<
Action Die: 5 + 2 = 7
Challenge Dice: 4, 6
Resultado: [+] STRONG HIT

Inflija dano +1. Você retém iniciativa.
(Marque progresso no combat track: harm base + 1)
```

**Ações do jogador:**
- Você inflige dano +1 (harm base +1)
- Você mantém iniciativa
- Marque progresso no track: **8 ticks** (Dangerous = 2 boxes = 8 ticks)

**Comando para marcar progresso:**
```
:progress "Lobo Perigoso"
```

**Resultado:**
```
[+] Progresso marcado: Lobo Perigoso (2/10 boxes, 8/40 ticks)
```

**Estado após:**
- Combat Track: 8/40 ticks (2 boxes)
- Iniciativa: Você ainda tem

---

### 2.2 Strike - Weak Hit

**Comando:**
```
:move strike iron
```

**Resultado esperado:**
```
>>> Strike <<<
Action Die: 3 + 2 = 5
Challenge Dice: 2, 6
Resultado: [~] WEAK HIT

Inflija seu dano e perca iniciativa.
(Marque progresso no combat track. Inimigo tem iniciativa.)
```

**Ações do jogador:**
- Você inflige dano base
- Você perde iniciativa (inimigo ganha)
- Marque progresso: **+8 ticks**

**Comando:**
```
:progress "Lobo Perigoso"
```

**Resultado:**
```
[+] Progresso marcado: Lobo Perigoso (4/10 boxes, 16/40 ticks)
```

**Estado após:**
- Combat Track: 16/40 ticks (4 boxes)
- Iniciativa: Inimigo tem

---

## FASE 3: Defesa (Inimigo tem Iniciativa)

### 3.1 Clash - Strong Hit

**Comando:**
```
:move clash iron
```

**Resultado esperado:**
```
>>> Clash <<<
Action Die: 6 + 2 = 8
Challenge Dice: 3, 5
Resultado: [+] STRONG HIT

Inflija seu dano.
[Escolha uma opção para continuar o movimento]
```

**Opções apresentadas:**
1. Bolster your position: +1 momentum, você tem iniciativa
2. Find an opening: Inflija +1 harm, você tem iniciativa

**Ações do jogador:**
- Escolha a opção 2 (Find an opening)
- Você inflige harm +1
- Você ganha iniciativa
- Marque progresso: **+8 ticks**

**Comando:**
```
#!choice!# 2
```

**Depois:**
```
:progress "Lobo Perigoso"
```

**Resultado:**
```
[+] Progresso marcado: Lobo Perigoso (6/10 boxes, 24/40 ticks)
```

**Estado após:**
- Combat Track: 24/40 ticks (6 boxes)
- Iniciativa: Você tem novamente
- Momentum: +1 (se escolheu opção 1)

---

### 3.2 Clash - Weak Hit

**Comando:**
```
:move clash iron
```

**Resultado esperado:**
```
>>> Clash <<<
Action Die: 4 + 2 = 6
Challenge Dice: 5, 7
Resultado: [~] WEAK HIT

Inflija seu dano, mas inimigo retém iniciativa.
[Executando PayThePrice automaticamente...]
```

**Ações do jogador:**
- Você inflige dano base
- Inimigo mantém iniciativa
- Pay The Price é executado automaticamente
- Marque progresso: **+8 ticks**

**Comando:**
```
:progress "Lobo Perigoso"
```

**Estado após:**
- Combat Track: 32/40 ticks (8 boxes)
- Iniciativa: Inimigo ainda tem
- Possíveis consequências de Pay The Price (dano, perda de momentum, etc.)

---

### 3.3 Clash - Miss (Fail)

**Comando:**
```
:move clash iron
```

**Resultado esperado:**
```
>>> Clash <<<
Action Die: 2 + 2 = 4
Challenge Dice: 5, 6
Resultado: [X] MISS

Você está em desvantagem. Inimigo retém iniciativa.
[Executando PayThePrice automaticamente...]
```

**Ações do jogador:**
- Seu contra-ataque falha completamente
- Inimigo mantém iniciativa
- Pay The Price é executado (provavelmente sofrerá dano)

**Possível Pay The Price:**
- Você pode precisar executar `:move endureharm` se sofrer dano físico

**Estado após:**
- Iniciativa: Inimigo tem
- Possível perda de Health ou Momentum

---

## FASE 4: Sofrendo Dano

### 4.1 Endure Harm - Strong Hit

**Comando:**
```
:move endureharm
```

**Resultado esperado:**
```
>>> EndurHarm <<<
Action Die: 5 + 2 = 7
Challenge Dice: 3, 4
Resultado: [+] STRONG HIT

[Escolha uma opção para continuar o movimento]
```

**Opções:**
1. Shake it off: -1 momentum, +1 health (se health > 0)
2. Embrace the pain: +1 momentum

**Ações do jogador:**
- Escolha a opção 1 para recuperar health

**Comando:**
```
#!choice!# 1
```

**Estado após:**
- Momentum: -1
- Health: +1 (se estava abaixo do máximo)

---

### 4.2 Endure Harm - Weak Hit

**Comando:**
```
:move endureharm
```

**Resultado esperado:**
```
>>> EndurHarm <<<
Action Die: 4 + 2 = 6
Challenge Dice: 4, 7
Resultado: [~] WEAK HIT

Você prossegue apesar do dano.
```

**Estado após:**
- Você continua lutando, mas sem benefícios extras

---

### 4.3 Endure Harm - Miss (Fail)

**Comando:**
```
:move endureharm
```

**Resultado esperado:**
```
>>> EndurHarm <<<
Action Die: 2 + 2 = 4
Challenge Dice: 5, 6
Resultado: [X] MISS

Sofra -1 momentum.
[*] Consultando oráculo "Resistir Dano" automaticamente...
```

**Ações do jogador:**
- Você perde -1 momentum
- Oráculo "Resistir Dano" é consultado automaticamente
- Possíveis consequências adicionais

---

## FASE 5: Recuperar Controle

### 5.1 Turn the Tide - Strong Hit

**Comando:**
```
:move turnthetide
```

**Resultado esperado:**
```
>>> TurnTheTide <<<
Action Die: 6
Challenge Dice: 2, 4
Resultado: [+] STRONG HIT

Você retoma iniciativa. Resetar momentum para +2.
```

**Estado após:**
- Iniciativa: Você tem
- Momentum: Resetado para +2

---

### 5.2 Turn the Tide - Weak Hit

**Comando:**
```
:move turnthetide
```

**Resultado esperado:**
```
>>> TurnTheTide <<<
Action Die: 5
Challenge Dice: 4, 6
Resultado: [~] WEAK HIT

Não retoma iniciativa, mas pode continuar.
```

**Estado após:**
- Iniciativa: Ainda com o inimigo
- Você pode continuar tentando

---

### 5.3 Turn the Tide - Miss (Fail)

**Comando:**
```
:move turnthetide
```

**Resultado esperado:**
```
>>> TurnTheTide <<<
Action Die: 2
Challenge Dice: 5, 6
Resultado: [X] MISS

Situação piora drasticamente.
[Executando PayThePrice automaticamente...]
```

**Estado após:**
- Situação piora
- Pay The Price executado
- Possíveis consequências severas

---

## FASE 6: Finalizar Combate

### 6.1 Verificar Progresso Atual

**Comando:**
```
:tracks
```

**Resultado esperado:**
```
=== Progress Tracks Ativos ===

• Lobo Perigoso
  Tipo: Combat
  Rank: Dangerous
  Progresso: 8/10 (32/40)
  ████████░░ 80%
```

---

### 6.2 End the Fight - Strong Hit

**Comando:**
```
:move endthefight
```

**Nota:** Este move deve fazer um progress roll automaticamente usando o score atual do track.

**Resultado esperado (se implementado):**
```
>>> EndTheFight <<<
Progress Score: 8
Challenge Dice: 3, 5
Resultado: [+] STRONG HIT

Você vence o combate de forma decisiva!
[+] Experiência adicionada: +2
```

**Alternativa usando comando direto:**
```
:fulfill "Lobo Perigoso"
```

**Resultado esperado:**
```
Progress Roll: Score 8 vs Challenge Dice (3, 5)
Resultado: [+] STRONG HIT

Você vence o combate de forma decisiva!
[+] Experiência adicionada: +2
```

**Estado após:**
- Combat Track: Completado
- Experiência: +2 XP (rank Dangerous)
- Combate finalizado

---

### 6.3 End the Fight - Weak Hit

**Comando:**
```
:fulfill "Lobo Perigoso"
```

**Resultado esperado:**
```
Progress Roll: Score 8 vs Challenge Dice (5, 7)
Resultado: [~] WEAK HIT

Você vence, mas com complicações.
[+] Experiência adicionada: +1
```

**Estado após:**
- Combate vencido, mas com complicações narrativas
- Experiência: +1 XP (metade do rank, arredondado)

---

### 6.4 End the Fight - Miss (Fail)

**Comando:**
```
:fulfill "Lobo Perigoso"
```

**Resultado esperado:**
```
Progress Roll: Score 8 vs Challenge Dice (8, 9)
Resultado: [X] MISS

Progress roll falhou!
```

**Estado após:**
- Combate continua
- Você pode precisar marcar mais progresso antes de tentar novamente

**Ações do jogador:**
- Continue usando Strike/Clash para marcar mais progresso
- Tente novamente quando o score estiver maior

---

## Resumo dos Comandos Usados

### Moves de Combate:
1. `:move enterthefray [heart|shadow|wits]` - Iniciar combate
2. `:move strike [iron|edge]` - Ataque (requer iniciativa)
3. `:move clash [iron|edge]` - Defesa/Contra-ataque
4. `:move turnthetide` - Recuperar controle
5. `:move endthefight` - Finalizar combate
6. `:move endureharm` - Resistir dano

### Comandos de Progress:
1. `:progress "<nome>"` - Marcar progresso no track
2. `:fulfill "<nome>"` - Fazer progress roll
3. `:tracks` - Ver todos os tracks

### Comandos de Status:
1. `:char` - Ver status do personagem
2. `:show` - Ver logs da sessão

### Comandos de Recursos:
1. `:addres health:+1` - Adicionar health
2. `:addres momentum:+1` - Adicionar momentum

---

## Fluxo Completo de Combate (Resumido)

```
1. :move enterthefray heart
   → Strong Hit: Ganha iniciativa, cria track

2. :move strike iron
   → Strong Hit: Mantém iniciativa, marca progresso
   → Weak Hit: Perde iniciativa, marca progresso
   → Miss: Perde iniciativa, Pay The Price

3. :move clash iron (quando inimigo tem iniciativa)
   → Strong Hit: Escolhe opção, ganha iniciativa
   → Weak Hit: Inimigo mantém iniciativa, Pay The Price
   → Miss: Inimigo mantém iniciativa, Pay The Price

4. :move endureharm (quando sofre dano)
   → Strong Hit: Escolhe recuperar ou ganhar momentum
   → Weak Hit: Continua apesar do dano
   → Miss: Perde momentum, oráculo automático

5. :move turnthetide (quando em desvantagem)
   → Strong Hit: Retoma iniciativa, reset momentum
   → Weak Hit: Continua mas sem retomar
   → Miss: Situação piora, Pay The Price

6. :progress "Nome do Track" (marcar progresso manualmente)
   → Marca ticks baseado no rank

7. :fulfill "Nome do Track" (finalizar combate)
   → Strong Hit: Vitória decisiva, XP completo
   → Weak Hit: Vitória com complicações, XP reduzido
   → Miss: Falha, combate continua
```

---

## Observações Importantes

1. **Criação de Combat Track:** O sistema atual não possui comando direto para criar um Combat Progress Track. Seria necessário:
   - Extensão do comando `:vow` para aceitar tipo Combat
   - Novo comando `:combat "<nome>" <rank>`
   - Criação automática quando Enter the Fray tem Strong Hit

2. **Marcação Manual de Progresso:** O comando `:progress` marca progresso baseado no rank do track, mas não diferencia entre diferentes tipos de harm.

3. **End the Fight:** O move `:move endthefight` deveria fazer progress roll automaticamente, mas pode ser necessário usar `:fulfill` diretamente.

4. **Iniciativa:** O sistema rastreia iniciativa narrativamente através de mensagens, não há um estado explícito de iniciativa.

5. **Harm e Progresso:** O sistema marca progresso manualmente, não automaticamente baseado em harm infligido.

---

## Exemplo de Sessão Completa

```
> :char
[Mostra status do personagem]

> :move enterthefray heart
[Strong Hit - ganha iniciativa]

> :move strike iron
[Strong Hit - mantém iniciativa]
> :progress "Lobo Perigoso"
[8 ticks marcados]

> :move strike iron
[Weak Hit - perde iniciativa]
> :progress "Lobo Perigoso"
[16 ticks marcados]

> :move clash iron
[Strong Hit - escolhe opção]
> #!choice!# 2
[Ganha iniciativa, harm +1]
> :progress "Lobo Perigoso"
[24 ticks marcados]

> :move strike iron
[Miss - Pay The Price]
> :move endureharm
[Weak Hit - continua]

> :move clash iron
[Strong Hit - escolhe opção]
> #!choice!# 1
[Ganha iniciativa, momentum +1]
> :progress "Lobo Perigoso"
[32 ticks marcados]

> :tracks
[Mostra progresso: 8/10 boxes]

> :fulfill "Lobo Perigoso"
[Strong Hit - vitória! +2 XP]
```

---

Este exemplo demonstra como usar todos os comandos disponíveis para realizar um combate completo, cobrindo Strong Hits, Weak Hits e Fails em cada fase do combate.

