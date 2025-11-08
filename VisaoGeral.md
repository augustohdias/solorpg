# SoloRPG – Visão Geral de Componentes e Responsabilidades  
*Documento de referência para novos prompts (versão beta .1)*

---

## 1. Arquitetura Geral

- **Objetivo**: Console RPG inspirado em Ironsworn, com motor de jogo separado de uma TUI (Text User Interface) reativa.
- **Paradigma**: IO assíncrono + estado persistente via `STM` (Software Transactional Memory).
- **Fluxo principal**:
  1. Player interage pela TUI (`UI.hs`), que alimenta comandos em `TChan`.
  2. Loop de jogo (`MainLoop.hs`) processa comandos com `ActionService`.
  3. Eventos resultantes são emitidos num canal de saída; a TUI os renderiza.

---

## 2. Componentes Principais

| Componente                                  | Responsabilidade central                                                                                       | Módulos-chave                                                       |
|---------------------------------------------|------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------|
| **Entrada/Interface (TUI)**                  | Renderizar estado, coletar comandos e manter feedback visual.                                                    | `src/UI.hs`                                                          |
| **Loop de Processamento**                    | Ler comandos, delegar ao motor (`ActionService`) e emitir eventos.                                               | `src/MainLoop.hs`, `src/System/ActionContract.hs`                    |
| **Services/Contracts (Domínio de jogo)**    | Implementar comportamentos concretos: contexto, movimentos, progresso, help, oráculos etc.                       | `src/System/Impl/*`, `src/System/*Contract.hs`                       |
| **Persistência e contexto**                  | Carregar/salvar personagens, progressos, logs e estado do jogo.                                                  | `src/System/Impl/GameContextService.hs`, `src/System/GameContextContract.hs` |
| **Eventos & Comunicação**                    | Transportar mensagens entre motor e TUI via STM.                                                                 | `src/System/Tui/Comm.hs`, `System.ActionContract`, `MainLoop`        |
| **Aplicação (`Main.hs`)**                    | Jonção de dependências, criação de recursos (`tchan`s, handles) e bootstrap.                                     | `app/Main.hs`                                                       |

---

## 3. Aplicação (`app/Main.hs`)

- **`module Main (main)`**:
  - Cria canais STM (`inputChan`, `outputChan`).
  - Inicializa handles para cada serviço:
    - Dados (`DiceService`), contexto (`GameContextService`), progresso, oráculos, help, movimentos.
  - Cria `ActionService` com todos os handles.
  - Chama `forkIO` para `runGameLoop` passando o handle do motor e canais.
  - Invoca `runTui`, que:
    - Inicializa estado visual.
    - Cria threads: leitura de eventos, atualização periódica de ficha.
  - Depende de `async` para threads auxiliares.

---

## 4. Main Loop (`src/MainLoop.hs`)

- **Intensidade**: Worker central que consome comandos.
- **Assinatura**:  
  `runGameLoop :: Action.Handle -> TChan T.Text -> TChan GameOutput -> IO ()`
  - Lê continuamente do `inputChan`.
  - **Parsing**: Converte texto para `ActionType` + payload (`System.ActionContract`).
  - Chama `Action.process` com handle e payload.
  - Emite `GameOutput` pelo `outputChan`.
  - Em caso de resultado `False` (sinal de `Exit`), finaliza com `exitSuccess`.

---

## 5. TUI (`src/UI.hs`)

### 5.1 Estado (`TuiState`)
- Campos:
  - `editor :: Ed.Editor T.Text Name`: campo de entrada.
  - `logs :: Vec.Vector T.Text`: narrativa e resultados.
  - `systemMessages :: Vec.Vector T.Text`: notificações de sistema/motor.
  - `character :: Maybe GameContext.MainCharacter`: ficha atual.
  - `inputChan :: TChan T.Text`: ponte para comandos a processar.
  - `viewportFocus :: FocusRing Name`: determina qual viewport (log/sistema) está ativa para rolagem.

### 5.2 Eventos e Renderização
- `handleEvent :: BrickEvent Name GameOutput -> EventM Name TuiState ()`:
  - Lida com `VtyEvent` (teclado):
    - `Enter`: envia comando atual ao motor, limpa editor.
    - `Tab` / `Shift+Tab`: alterna focus ring.
    - `Up/Down/PageUp/PageDown`: rolagem na viewport ativa.
    - `Ctrl+C` / `Esc`: finaliza (`halt`).
  - `AppEvent`:
    - `LogEntry msg msgType`: adiciona a `logs` ou `systemMessages`, autoscroll.
    - `CharacterUpdate`: atualiza `character`, invalida cache.
    - `GameEnd`: chama `halt`.
  - Usa `MonadState` (`mtl`) + `microlens` para atualizar campos.

### 5.3 Layout
- Cabeçalho informativo (instruções de foco).
- Painéis:
  - Esquerda: ficha do personagem (25 colunas).
  - Centro: log narrativo (auto wrap).
  - Direita: mensagens de sistema (40 colunas).
- Campo inferior: linha de comando (`Editor`).
- Limpa terminal com `ansi-terminal` ao sair (comportamento estilo vim).

### 5.4 Threads auxiliares
- Leitor de eventos (`outputChan` → `BChan`).
- Thread heartbeat (2s) enviando `":char"` para manter ficha atualizada no UI.
- Em ambos os casos, `STM` garante sincronização.

---

## 6. Action Service (`src/System/Impl/ActionService.hs`)

### 6.1 Propósito
- Concretiza `Action.Handle`:
  - Analisa tipos de comandos (`ActionType`).
  - Despacha para funções específicas (logs, dados, progressos, movimentos).
  - Emite mensagens para TUI (`LogEntry`, `CharacterUpdate`).

### 6.2 Funcionalidades principais
1. **Gestão de narrativa** (`AddStoryLog`, `LogEntry`).
2. **Dados** (`RollDice` → `DiceService`).
3. **Sessão** (`Show`, `Exit`).
4. **Personagem**:
   - Criar, carregar, mostrar.
   - Atualizar atributos/recursos (definição absoluta e deltas).
   - Fichas + logs.
5. **Movimentos** (`Move`):
   - Interpreta comandos `:move`.
   - Lida com progressos (vows, journeys).
   - Invoca serviços de `ProgressService`, `OracleService`, `MoveService`.
6. **Oráculos / Help / Bonds**.

### 6.3 Eventos gerados
- As ações chamam `logMessage`, `writeTChan` para comunicar a TUI.
- `executeOracleConsequence`: aplica efeitos de oráculos (inclusive triggers automáticos).
- Mantém consistência com `GameContextService`.

---

## 7. Contracts e Services

### 7.1 Pattern: Contract vs Impl
- `System/<x>Contract.hs`: define tipos e assinatura (`Handle`) para serviços.
- `System/Impl/<x>Service.hs`: implementação concreta, geralmente dependente de IO.

### 7.2 Principais contratos
- `ActionContract`: enum de `ActionType`, `Handle` com `process`.
- `GameContextContract`: tipos para contexto, atributos, recursos, progress track, bonds.
- `MoveContract`: parse de movimentos, dados de `Trigger`.
- `ProgressContract`: `ProgressRollResult`, ranks, ticks.
- `OracleContract`: interface para consultar oráculos.
- `HelpContract`: `showHelp`, `parseTopic`.

### 7.3 Serviços concretos
- **DiceService**: rolagem pseudo-aleatória, trata expressões `NdX`.
- **GameContextService**: persistência (gerência `.slg`), atributos, recursos, progress track, bonds.
- **ProgressService**: `markProgress`, `rollProgress`, interpretações.
- **MoveService**: resultados e consequências de moves.
- **OracleService**: carrega oráculos (JSON), responde queries.
- **HelpService**: carrega textos de ajuda (por tópico).
- **SessionLogService**, **Util/SafeIO**: utilidades/persistência.

---

## 8. Comunicação TUI ↔ Motor (`System/Tui/Comm.hs`)

- `data GameOutput`:
  - `LogEntry T.Text MessageType`
  - `CharacterUpdate GameContext.MainCharacter`
  - `GameEnd`
- `MessageType`:
  - `NarrativeMessage`
  - `SystemMessage`
- O `MainLoop` escreve `GameOutput` no `outputChan` (STM), TUI consome via `brick` (`BChan`).

---

## 9. WordFlow do Sistema

1. **Entrada**:
   - Usuário digita comando (ex.: `:move FaceDanger iron`, narrativa pura ou `:exit`).
   - `Editor` captura, TUI envia para `inputChan`.

2. **Processamento**:
   - `MainLoop` lê, faz parsing → `ActionType`.
   - `ActionService` fará orquestracão da ação, invocando o módulo responsável cabível
   - Estado global é atualizado via `GameContextService`, etc.
   - `ActionService` comunica resultado ao TUI via channel

3. **Saída/Eventos**:
   - `ActionService` cria mensagens: narrativa (`LogEntry Narrative`), sistema, updates de personagem.
   - `MainLoop` publica no `outputChan`.
   - TUI (via `Brick`) recebe e atualiza `logs`, `systemMessages`, `character`.

4. **Render**:
   - Layout atualizado; rolagem, wraps e foco conforme `FocusRing`.
   - Sistema pode enviar notificações assíncronas (oráculo triggering, `GameEnd`).

---

## 10. Histórico de Mudanças (Release beta .1)

**Principais eventos recentes**:
1. Migração completa do `UI.hs`, abandonando API legada (`Next`, `continue`).
2. Integração do foco múltiplo, mensagens sistêmicas e nova UX.
3. Inclusão de heartbeat para `:char` e limpeza do terminal pós-execução.
4. Ampliação das dependências (`mtl`, `microlens`, `async`, `ansi-terminal`, `text-zipper`).
5. Sincronização da documentação com a API moderna do Brick.

---

## 11. Observações Operacionais

- **Build**: `stack build`.
- **Execução**: `stack exec SoloRPG-exe`.
- **Hot reload**: Use `stack ghci` conforme necessário; lembre de lidar com warnings de múltiplos `Paths_*`.
- **Persistência**: Arquivos `.json` armazenados; logs de sessão gerenciados via `GameContextService`.
- **Extensões linguísticas**: `OverloadedStrings`, `RankNTypes`, etc., em módulos específicos.
- **Próximos passos** (potenciais):
  - Exibir menu de multipla escolha oriundo de consequencias de movimentos
  - Testes automatizados para `ActionService` e parsing.

---

## 12. Uso deste Documento

- **Objetivo**: ponto de partida para novos prompts, seja para corrigir, estender ou documentar outros aspectos.
- **Como usar**: mantenha-o atualizado ao adicionar features, serviços ou mudar APIs.
- **Recomendação**: referenciar seções específicas ao solicitar novas funcionalidades (ex.: “Veja seção 6.1 sobre movimentos antes de propor mudança nos progressos”).

---

> _“Cada comando digitado na linha inferior da TUI percorre o loop, transforma o estado e retorna como narrativa ou estatística na borda da interface. Este documento existe para guiar esse caminho.”_