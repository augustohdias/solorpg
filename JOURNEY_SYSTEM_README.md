# Sistema de Jornadas - IronSworn SoloRPG

## Implementação Completa do Sistema de Jornadas

O sistema de jornadas foi implementado seguindo fielmente as regras do IronSworn, integrando-se completamente com o sistema de progress tracks existente e o sistema de combate.

## Funcionalidades Implementadas

### 1. **Criação de Journey Tracks**
```
:journey "Cidade Distante" dangerous
```
- Cria um progress track do tipo Journey 
- Suporta todos os ranks: troublesome, dangerous, formidable, extreme, epic
- Integrado com o sistema multiplayer para sincronização

### 2. **Movimento Undertake a Journey**
```
:move UndertakeJourney wits
:move empreenderjornada wits
```

**Mecânicas implementadas:**
- **Strong Hit**: Escolha entre recursos sábios (progresso apenas) ou velocidade (progresso + momentum - supply)
- **Weak Hit**: Marca progresso automaticamente, mas perde 1 supply
- **Miss**: Executa "Pay the Price" automaticamente

**Consequências fiéis ao IronSworn:**
- `MarkJourneyProgress`: Nova consequência que marca progresso no journey track ativo
- Integração automática com sistema de progress tracks
- Sincronização multiplayer para journey tracks

### 3. **Movimento Reach Your Destination**  
```
:move ReachYourDestination
:move alcancardestino
```

**Implementação baseada em Progress Roll:**
- **Strong Hit**: Chegada bem-sucedida sem complicações
- **Weak Hit**: Chegada com complicação (escolha entre custos, tempo ou problemas)
- **Miss**: Falha na chegada (escolha entre se perder, obstáculos ou reafirmar jornada)

### 4. **Integração com Sistema Existente**

#### **Progress Tracks**
- Journey tracks aparecem no comando `:tracks`
- Suporte a `:progress` para marcar progresso manual
- Suporte a `:fulfill` para fazer progress rolls
- Suporte a `:abandon` para abandonar jornadas

#### **Sistema de Consequências**
- Nova consequência `MarkJourneyProgress` integrada ao sistema
- Automação completa do progresso durante jornadas
- Integração com sistema de escolhas do jogador

#### **Multiplayer** 
- Journey tracks são sincronizados automaticamente
- Progresso compartilhado entre jogadores
- Suporte completo para sessões multiplayer

## Arquitetura da Implementação

### **Módulos Modificados:**

1. **`System.Constants`**
   - Adicionadas mensagens e formatação para journey tracks
   - `formatJourneyTrackCreated` para formatação consistente
   - `msgJourneyTrackUsage` para ajuda ao usuário

2. **`System.ConsequenceContract`**
   - Nova consequência `MarkJourneyProgress`
   - Mantém compatibilidade com sistema existente

3. **`System.Move`**
   - Melhorias em `getUndertakeJourneyConsequences`
   - Nova função `getReachDestinationConsequences`
   - Integração com sistema de escolhas

4. **`System.Action`**
   - Nova função `markJourneyProgressTrack`
   - Nova função `createJourneyTrack`
   - Novo `ActionType.CreateJourneyTrack`
   - Processamento de `MarkJourneyProgress`

5. **`MainLoop`**
   - Novo comando `:journey` mapeado para `CreateJourneyTrack`

### **Fidelidade ao IronSworn:**

✅ **Journey Tracks com Progress System**
✅ **Undertake a Journey com escolhas corretas (recursos vs velocidade)**  
✅ **Reach Your Destination baseado em progress roll**
✅ **Integração com Supply, Momentum e outros recursos**
✅ **Sistema de waypoints e progresso automático**
✅ **Complicações e falhas conforme as regras**

## Exemplos de Uso

### Exemplo Completo de Jornada:

```bash
# 1. Criar personagem
:create Kael iron:3 edge:2 heart:1 shadow:2 wits:2

# 2. Criar uma jornada
:journey "Torre Antiga" formidable

# 3. Empreender a jornada (usando Wits como stat principal para jornadas)
:move UndertakeJourney wits

# 4. (Em Strong Hit, escolher uma opção)
# 5. Repetir :move UndertakeJourney conforme necessário

# 6. Ver progresso
:tracks

# 7. Quando tiver progresso suficiente, tentar alcançar destino
:fulfill "Torre Antiga"
```

### Comandos Disponíveis:

- `:journey "Destino" <rank>` - Criar journey track
- `:move UndertakeJourney <stat>` - Avançar na jornada  
- `:move ReachYourDestination` - Tentar alcançar destino
- `:tracks` - Ver todos os progress tracks incluindo jornadas
- `:progress "Nome da Jornada"` - Marcar progresso manual
- `:fulfill "Nome da Jornada"` - Fazer progress roll

## Integração com Combat System

O sistema de jornadas compartilha a mesma arquitetura do sistema de combate:
- Progress tracks unificados
- Mesmo sistema de sincronização multiplayer  
- Mesma interface de usuário
- Sistema de consequências comum

Isso permite usar combat tracks e journey tracks simultaneamente, mantendo a experiência consistente do IronSworn onde jornadas e combates podem se intercalar naturalmente.

---

**Implementação**: Completamente fiel às especificações do IronSworn
**Integração**: Total com sistemas existentes (combate, progress tracks, multiplayer)
**Usabilidade**: Interface consistente com comandos existentes
