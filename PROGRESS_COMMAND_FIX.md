# Fix: Comando :progress com Número de Ticks

## Problema Identificado

O comando `:progress "Piratas" 15` estava interpretando toda a string `"Piratas" 15` como um nome único de progress track, em vez de separar o nome da track ("Piratas") do número de ticks a marcar (15).

## Causa do Problema

A função `markProgress` no módulo `Action.hs` estava fazendo parsing simplificado:

```haskell
-- ANTES (problemático)
markProgress tuiOutputChannel input = do
  let trackName = T.strip input  -- Tratava toda entrada como nome
```

Isso fazia com que `"Piratas" 15` fosse tratado como um nome de track único, não encontrando nenhuma track com esse nome exato.

## Solução Implementada

### Nova Função de Parsing

Implementei uma função `parseProgressInput` que:

1. **Tenta parsing com aspas**: `"Nome da Track" número`
2. **Fallback sem aspas**: `NomeTrack número`
3. **Suporte opcional**: Se não tiver número, usa progresso padrão

### Comportamentos Suportados

#### ✅ **Com Aspas e Número**
```bash
:progress "Piratas Perigosos" 15
:progress "Vila Próxima" 8
:progress "Jornada Longa" 25
```

#### ✅ **Com Aspas Sem Número** (progresso padrão)
```bash  
:progress "Piratas Perigosos"
:progress "Vila Próxima"
```

#### ✅ **Sem Aspas Com Número** (track sem espaços)
```bash
:progress Piratas 15
:progress Vila 8
```

#### ✅ **Sem Aspas Sem Número** (progresso padrão)
```bash
:progress Piratas
:progress Vila
```

### Lógica de Progresso

- **Sem número**: Usa `Progress.markProgress` (progresso baseado no rank da track)
- **Com número**: Usa `Progress.markProgressTicks` com o número específico

### Feedback Melhorado

#### Exemplo de Saída:
```
[+] Progresso marcado em Piratas (15 ticks): 4/10 boxes (20/40 ticks)
[+] Progresso marcado em Vila (progresso padrão): 2/10 boxes (8/40 ticks)
```

## Arquitetura da Correção

### Função `parseProgressInput`

```haskell
parseProgressInput :: T.Text -> Maybe (T.Text, Maybe Int)
```

1. **Tenta `parseQuotedString`**: Para strings entre aspas
2. **Fallback para palavras**: Separa primeira palavra como nome
3. **Parse de número**: Converte segunda parte para Int (se existir)
4. **Tolerante a erros**: Ignora números inválidos e usa progresso padrão

### Integração com Sistema Existente

- ✅ Mantém compatibilidade com comandos existentes
- ✅ Preserva funcionalidade de progresso padrão
- ✅ Mantém sincronização multiplayer
- ✅ Funciona com todos os tipos de tracks (Combat, Journey, Vow, Bond)

## Exemplos de Uso

### Cenário 1: Track de Combate
```bash
# Criar track
:combat "Bandidos" dangerous

# Marcar progresso específico
:progress "Bandidos" 8

# Ver resultado
:tracks
```

### Cenário 2: Track de Jornada
```bash
# Criar jornada
:journey "Cidade Distante" formidable

# Progresso padrão (baseado no rank)
:progress "Cidade Distante"

# Progresso específico
:progress "Cidade Distante" 12
```

### Cenário 3: Múltiplas Tracks
```bash
:combat "Lobo" troublesome
:journey "Torre" dangerous
:vow "Vingança" extreme

# Diferentes tipos de progresso
:progress "Lobo" 12          # 12 ticks específicos
:progress "Torre"            # Progresso padrão (8 ticks por dangerous)
:progress "Vingança" 2       # 2 ticks específicos
```

## Mensagens de Erro Melhoradas

### Uso Incorreto:
```bash
:progress
```
**Resultado:**
```
Uso: :progress "<nome do track>" [número de ticks]
Exemplos: :progress "Piratas" 15  ou  :progress "Vila Próxima"
```

### Track Não Encontrada:
```bash
:progress "Inexistente" 10
```
**Resultado:**
```
Track não encontrado: Inexistente
```

## Impacto da Correção

- ✅ **Flexibilidade**: Suporte a progresso específico E padrão
- ✅ **Usabilidade**: Comandos mais intuitivos e poderosos
- ✅ **Compatibilidade**: Não quebra funcionalidades existentes
- ✅ **Robustez**: Parsing tolerante a diferentes formatos
- ✅ **Feedback**: Mensagens mais claras e informativas

A correção torna o comando `:progress` muito mais útil e flexível, permitindo controle preciso sobre o progresso das tracks!
