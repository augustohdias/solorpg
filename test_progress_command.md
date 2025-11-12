# Teste do Comando :progress - Correção

## Comandos para Testar a Correção

Execute estes comandos no SoloRPG para verificar se o fix está funcionando:

### Teste 1: Progresso com Número Específico (Problema Original)
```bash
# Criar personagem e track
:create ProgressTest iron:2 edge:2 wits:2
:combat "Piratas Perigosos" dangerous

# O comando que estava com problema - agora deve funcionar
:progress "Piratas Perigosos" 15

# Verificar resultado
:tracks

# Deve mostrar algo como: 3/10 boxes (15/40 ticks)
```

### Teste 2: Diferentes Formatos de Comando
```bash
# Criar várias tracks para teste
:journey "Vila Distante" troublesome
:vow "Vingar Pai" formidable
:combat "Lobo" dangerous

# Testar diferentes formatos
:progress "Vila Distante" 12        # Com aspas e número
:progress "Vingar Pai"              # Com aspas, sem número (padrão)
:progress Lobo 8                    # Sem aspas, com número
:progress Lobo                      # Sem aspas, sem número (padrão)
```

### Teste 3: Tracks com Nomes Complexos
```bash
# Criar tracks com nomes que têm espaços
:combat "Bandidos do Vale Sombrio" extreme
:journey "Caminho para as Montanhas Geladas" epic

# Testar progresso específico
:progress "Bandidos do Vale Sombrio" 5
:progress "Caminho para as Montanhas Geladas" 2

# Ver resultado
:tracks
```

### Teste 4: Progresso Padrão vs Específico
```bash
# Criar track troublesome (progresso padrão = 12 ticks)
:combat "Goblin" troublesome

# Comparar progresso padrão vs específico
:progress "Goblin"         # Deve marcar 12 ticks (padrão troublesome)
:tracks                    # Ver resultado

:progress "Goblin" 5       # Deve marcar 5 ticks específicos
:tracks                    # Ver resultado
```

### Teste 5: Tratamento de Erros
```bash
# Testar comandos inválidos
:progress                           # Sem argumentos
:progress "Track Inexistente" 10    # Track que não existe
:progress "Goblin" abc             # Número inválido (deve ignorar e usar padrão)
```

## O Que Observar

### ✅ **Funcionamento Correto:**

1. **Parsing com aspas**: `"Nome Track" 15` deve funcionar
2. **Parsing sem aspas**: `NomeTrack 15` deve funcionar  
3. **Progresso específico**: Número exato de ticks marcado
4. **Progresso padrão**: Quando não especificar número
5. **Mensagens claras**: Feedback sobre qual tipo de progresso foi marcado

### ✅ **Exemplos de Saída Esperada:**

```bash
# Com número específico
[+] Progresso marcado em Piratas (15 ticks): 3/10 boxes (15/40 ticks)

# Progresso padrão  
[+] Progresso marcado em Vila (progresso padrão): 3/10 boxes (12/40 ticks)
```

### ✅ **Comportamento de Tracks por Rank:**

- **Troublesome**: 12 ticks por progresso padrão
- **Dangerous**: 8 ticks por progresso padrão
- **Formidable**: 4 ticks por progresso padrão  
- **Extreme**: 2 ticks por progresso padrão
- **Epic**: 1 tick por progresso padrão

### ❌ **Problemas (se ainda existissem):**

- Comando `"Track" 15` não funciona
- Toda entrada tratada como nome único
- Sem diferenciação entre progresso padrão e específico

## Teste Completo Rápido

```bash
# Setup
:create TestProgress iron:2 edge:2 wits:2
:combat "Test Track" dangerous

# O comando problemático original
:progress "Test Track" 15

# Verificar se funcionou
:tracks

# Deve mostrar progresso com 15 ticks em vez de erro "Track não encontrado"
```

## Comandos de Utilidade

```bash
# Ver todas as tracks ativas
:tracks

# Ver informações do personagem
:char

# Fazer progress roll quando apropriado
:fulfill "Nome da Track"
```

A correção resolve completamente o problema de parsing do comando `:progress`!
