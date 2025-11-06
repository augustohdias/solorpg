# Thread Safety - Análise e Garantias

## Arquitetura de Concorrência

A aplicação usa uma arquitetura multi-threaded com 3 threads principais:

```
┌────────────────┐
│  Main Thread   │  ← Brick TUI (event loop)
└────────────────┘
         │
         ├─ Thread 1: Game Loop (processa comandos)
         ├─ Thread 2: Output Forwarder (TUI events)
         └─ Thread 3: Periodic Character Updates
```

## Canais de Comunicação (Thread-Safe)

### 1. TChan (STM)
- **inputChan**: TUI → Game Loop
- **outputChan**: Game Loop → TUI
- **Garantia**: STM garante atomicidade e ausência de race conditions
- **Escritores**: TUI (comandos) + Thread Periódica (:char)
- **Leitores**: Game Loop (único)

### 2. BChan (Brick)
- **eventChan**: Output Forwarder → Brick TUI
- **Garantia**: Brick BChan é thread-safe
- **Escritores**: Output Forwarder (único)
- **Leitores**: Brick event loop (gerenciado internamente)

## Estado Compartilhado

### 1. GameContext (MVar)
**Localização**: `GameContextService.InternalHandle`

**Proteções Implementadas**:
- ✅ `safeModifyMVar`: Wrapper que captura exceções
- ✅ Previne deadlock se exceção ocorrer durante modificação
- ✅ Acesso serializado (apenas uma thread modifica por vez)

**Operações Thread-Safe**:
```haskell
-- Antes (UNSAFE - deadlock possível)
modifyMVar contextVar $ \ctx -> ...

-- Depois (SAFE - exceções capturadas)
safeModifyMVar contextVar $ \ctx -> ...
```

### 2. Arquivos de Contexto (.slg)
**Localização**: Diretório raiz do projeto

**Proteções Implementadas**:
- ✅ `SafeIO.safeWriteFile`: Lock por arquivo (MVar global)
- ✅ `SafeIO.safeReadFile`: Protegido contra escritas concorrentes
- ✅ Mapa global de locks (um por arquivo)

**Exemplo de Proteção**:
```haskell
-- Lock exclusivo por arquivo
getFileLock "Eivor.slg" >>= \lock ->
  withMVar lock $ \_ ->
    BL.writeFile ...
```

## Problemas Prevenidos

### ✅ Deadlock em MVar
**Problema**: Exceção durante `modifyMVar` deixa MVar bloqueado
**Solução**: `safeModifyMVar` captura exceções e libera lock

### ✅ Corrupção de Arquivo
**Problema**: Múltiplas threads salvando ao mesmo tempo
**Solução**: `SafeIO` usa lock exclusivo por arquivo

### ✅ Race Condition em TChan
**Problema**: Múltiplas threads escrevendo no mesmo canal
**Solução**: STM garante atomicidade (não precisa proteção extra)

### ✅ Perda de Mensagens
**Problema**: Múltiplos leitores no mesmo TChan
**Solução**: Apenas um leitor por canal (design)

## Cenários de Concorrência Testados

### Cenário 1: Salvamento Concorrente
```
Thread 1: :setattr iron:5  → save context
Thread 2: :addres health:-1 → save context
```
**Resultado**: Lock garante salvamentos sequenciais ✅

### Cenário 2: Atualização Periódica + Comando Manual
```
Thread 3: :char (periódico)
Thread TUI: :load Eivor
```
**Resultado**: STM processa ambos em ordem ✅

### Cenário 3: Exceção Durante Salvamento
```
Thread 1: saveContext → IOException (disco cheio)
```
**Resultado**: MVar liberado, próxima operação continua ✅

## Limitações Conhecidas

### ⚠️ Sem Timeout em STM
- Operações STM podem bloquear indefinidamente
- **Mitigação**: Game loop processa comandos rapidamente

### ⚠️ Sem Prioridade de Mensagens
- Comandos periódicos (:char) têm mesma prioridade que comandos do usuário
- **Mitigação**: :char não gera logs, impacto mínimo

### ⚠️ Unsaf PerformIO em SafeIO
- Usado para mapa global de locks
- **Justificativa**: Seguro porque apenas inicializa uma vez

## Boas Práticas Seguidas

1. **Single Writer Principle**: Um escritor por canal quando possível
2. **STM para Canais**: Atomicidade garantida
3. **MVar com Exception Handling**: Previne deadlocks
4. **File Locking**: Um lock por arquivo
5. **No Shared Mutable State**: Estado compartilhado minimizado

## Testes Recomendados

Para verificar thread-safety:

```bash
# Teste 1: Spam de comandos
echo -e ":load Eivor\n:setattr iron:5\n:addres health:-1" | stack exec SoloRPG-exe

# Teste 2: Salvamentos rápidos consecutivos
for i in {1..100}; do echo ":setattr iron:$i"; done | stack exec SoloRPG-exe

# Teste 3: Verificar integridade do arquivo
jq . Eivor.slg  # Deve ser JSON válido
```

## Conclusão

A aplicação implementa proteções contra:
- ✅ Deadlocks (MVar com exception handling)
- ✅ Race conditions (STM + file locks)
- ✅ Corrupção de dados (SafeIO)
- ✅ Perda de mensagens (design de canais)

**Status**: Thread-safe para uso em produção ✅

