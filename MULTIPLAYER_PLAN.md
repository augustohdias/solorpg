# Planejamento: Implementação de Multiplayer em LAN

## 1. Visão Geral da Arquitetura

### 1.1 Modelo Cliente-Servidor
- **Host (Servidor)**: Jogador que inicia a sessão com `:host`
- **Clientes**: Jogadores que se conectam com `:connect <ip:port>`
- **Comunicação**: TCP/IP para confiabilidade na transmissão de dados

### 1.2 Fluxo de Conexão
```
Usuário 1 (Host):
  1. Carrega personagem
  2. Executa :host
  3. Sistema mostra IP:porta
  4. Aguarda conexões

Usuário 2 (Cliente):
  1. Carrega personagem
  2. Executa :connect <ip:port>
  3. Aguarda aprovação do host

Host recebe solicitação:
  1. Mostra na TUI: "Jogador X quer se conectar. Aceitar?"
  1.1. Duas opções devem ser selecionáveis via TUI: Sim e Não (reutilizar código similar a outros menus de seleção que existem na TUI)
  2. Se aceito: adiciona cliente à lista de participantes
  3. Se recusado: notifica cliente e fecha conexão
```

## 2. Componentes a Implementar

### 2.1 Módulo de Rede (`System.Network`)
**Responsabilidades:**
- Gerenciar conexões TCP
- Serializar/deserializar mensagens (usando Aeson/JSON)
- Manter lista de clientes conectados
- Threads de recepção/envio

**Estrutura:**
```haskell
-- Tipos de mensagens de rede
data NetworkMessage
  = ConnectionRequest T.Text  -- Nome do jogador solicitando conexão
  | ConnectionAccepted
  | ConnectionRejected        -- Motivo da rejeição
  | StoryLogEntry T.Text      -- Entrada de log narrativo
  | SyncRequest               -- Solicitação de sincronização
  | Heartbeat                 -- Mantém conexão viva

-- Estado do servidor
data ServerState = ServerState
  { serverSocket :: Socket
  , serverClients :: MVar [ClientConnection]
  , serverPort :: PortNumber
  }

-- Estado do cliente
data ClientState = ClientState
  { clientSocket :: Socket
  , clientHost :: String
  , clientPort :: PortNumber
  , clientConnected :: MVar Bool
  }
```

### 2.2 Integração com Action.hs
**Modificações necessárias:**
- Adicionar novos `ActionType`: `HostSession`, `ConnectSession`
- Modificar `addStoryLog` para broadcast quando em modo multiplayer
- Interceptar todas as ações que modificam `sessionLog` para sincronização

**Novos comandos:**
```haskell
data ActionType
  = ...
  | HostSession      -- Inicia servidor
  | ConnectSession   -- Conecta a servidor
  | DisconnectSession -- Desconecta da sessão
```

### 2.3 Integração com GameContext.hs
**Modificações necessárias:**
- Adicionar flag `isMultiplayer :: Bool` ao `Context`
- Adicionar `multiplayerSessionId :: Maybe T.Text` para identificar sessão
- Modificar `addLogEntry` para aceitar origem (player name)
- Criar função `syncLogEntry` que adiciona log sem trigger de broadcast

**Estrutura de log sincronizado:**
```haskell
data SyncLogEntry = SyncLogEntry
  { syncPlayerName :: T.Text
  , syncLogText :: T.Text
  , syncTimestamp :: UTCTime
  }
```

### 2.4 UI para Multiplayer
**Modificações em UI.hs:**
- Adicionar widget para mostrar status de conexão
- Mostrar IP:porta quando host ativo
- Prompt de aprovação de conexão (similar ao ChoicePrompt)
- Indicador visual de jogadores conectados

**Novos GameOutput:**
```haskell
data GameOutput
  = ...
  | ConnectionStatus T.Text Bool  -- Status da conexão
  | HostInfo T.Text PortNumber     -- IP e porta do host
  | ConnectionRequestPrompt T.Text  -- Solicitação de conexão
  | PlayerList [T.Text]             -- Lista de jogadores conectados
```

## 3. Protocolo de Comunicação

### 3.1 Formato de Mensagens
Todas as mensagens serão serializadas em JSON usando Aeson:
```haskell
instance ToJSON NetworkMessage
instance FromJSON NetworkMessage
```

### 3.2 Tipos de Mensagens

**ConnectionRequest:**
```json
{
  "type": "ConnectionRequest",
  "playerName": "NomeDoJogador",
  "characterName": "NomeDoPersonagem"
}
```

**ConnectionAccepted:**
```json
{
  "type": "ConnectionAccepted",
  "sessionId": "uuid-da-sessao"
}
```

**ConnectionRejected:**
```json
{
  "type": "ConnectionRejected",
  "reason": "Conexão recusada pelo host"
}
```

**StoryLogEntry:**
```json
{
  "type": "StoryLogEntry",
  "playerName": "NomeDoJogador",
  "logText": "Texto do log",
  "timestamp": "2025-01-01T12:00:00Z"
}
```

### 3.3 Fluxo de Sincronização

**Quando cliente envia story log:**
1. Cliente adiciona ao seu contexto local
2. Cliente envia `StoryLogEntry` para host
3. Host recebe e adiciona ao seu contexto
4. Host faz broadcast para todos os outros clientes (excluindo remetente)
5. Cada cliente recebe e adiciona ao seu contexto
6. Se a mensagem começar com o símbolo ~ é um segredo e não deve ser feito o broadcast, deve ser salva apenas no log local

**Quando host envia story log:**
1. Host adiciona ao seu contexto local
2. Host faz broadcast para todos os clientes
3. Cada cliente recebe e adiciona ao seu contexto

## 4. Implementação Detalhada

### 4.1 Fase 1: Infraestrutura de Rede
**Arquivos a criar:**
- `src/System/Network.hs` - Módulo principal de rede
- `src/System/Network/Server.hs` - Lógica do servidor
- `src/System/Network/Client.hs` - Lógica do cliente
- `src/System/Network/Protocol.hs` - Definição do protocolo

**Dependências a adicionar:**
- `network` - Para sockets TCP
- `bytestring` - Já existe, para I/O de rede

### 4.2 Fase 2: Integração com Action
**Modificações:**
- Adicionar `HostSession` e `ConnectSession` ao `ActionType`
- Implementar `hostSession` e `connectSession` em Action.hs
- Modificar `addStoryLog` para verificar modo multiplayer
- Criar função `broadcastStoryLog` que envia para rede

### 4.3 Fase 3: Integração com GameContext
**Modificações:**
- Adicionar campos de multiplayer ao `Context`
- Criar função `addSyncLogEntry` que não dispara broadcast
- Modificar `addLogEntry` para incluir origem do log

### 4.4 Fase 4: UI e Feedback
**Modificações:**
- Adicionar widgets de status de conexão
- Implementar prompt de aprovação de conexão
- Mostrar lista de jogadores conectados
- Feedback visual de sincronização

### 4.5 Fase 5: Tratamento de Erros
**Cenários a tratar:**
- Cliente desconecta inesperadamente
- Host fecha servidor
- Timeout de conexão
- Falha na serialização/deserialização
- Conexão recusada (porta já em uso, etc.)

## 5. Estrutura de Arquivos

```
src/
  System/
    Network.hs              -- Módulo principal (exports)
    Network/
      Server.hs            -- Servidor TCP
      Client.hs            -- Cliente TCP
      Protocol.hs          -- Protocolo de mensagens
      Types.hs             -- Tipos de dados de rede
    Action.hs              -- Modificado: novos comandos
    GameContext.hs         -- Modificado: suporte multiplayer
    Tui/
      Comm.hs              -- Modificado: novos GameOutput
  UI.hs                    -- Modificado: widgets multiplayer
  MainLoop.hs              -- Modificado: integração rede
```

## 6. Detalhes Técnicos

### 6.1 Threading
- **Servidor**: Thread principal aceita conexões, thread por cliente para receber
- **Cliente**: Thread para receber mensagens do servidor
- **Broadcast**: Thread separada no servidor para enviar mensagens

### 6.2 Sincronização de Estado
- Usar `MVar` para estado compartilhado (lista de clientes)
- Usar `STM` para comunicação entre threads de rede e game loop
- Evitar race conditions com locks apropriados

### 6.3 Identificação de Jogadores
- Usar nome do personagem como identificador
- Host mantém lista de jogadores conectados
- Cada mensagem inclui nome do remetente

### 6.4 Heartbeat
- Enviar heartbeat a cada 30 segundos
- Se não receber heartbeat por 90 segundos, considerar desconectado
- Limpar conexões mortas automaticamente

## 7. Fluxo de Dados Detalhado

### 7.1 Iniciar Host
```
1. Usuário executa :host
2. Action.hostSession cria ServerState
3. Abre socket TCP na porta (ex: 8080)
4. Obtém IP local da máquina
5. Envia GameOutput com HostInfo(ip, port)
6. UI mostra IP:porta
7. Thread aceita conexões em loop
```

### 7.2 Conectar Cliente
```
1. Usuário executa :connect 192.168.1.100:8080
2. Action.connectSession cria ClientState
3. Conecta ao socket do host
4. Envia ConnectionRequest com nome do jogador
5. Aguarda resposta (ConnectionAccepted/Rejected)
6. Se aceito, inicia thread de recepção
7. Envia GameOutput com ConnectionStatus
```

### 7.3 Aprovar Conexão (Host)
```
1. Host recebe ConnectionRequest
2. Envia GameOutput ConnectionRequestPrompt
3. UI mostra popup de aprovação
4. Usuário escolhe aceitar/recusar
5. Se aceito: envia ConnectionAccepted, adiciona à lista
6. Se recusado: envia ConnectionRejected, fecha conexão
```

### 7.4 Broadcast de Story Log
```
1. Jogador (host ou cliente) adiciona story log
2. Se modo multiplayer:
   a. Adiciona ao contexto local
   b. Se cliente: envia StoryLogEntry para host
   c. Se host: faz broadcast para todos clientes
3. Cada receptor adiciona ao seu contexto
```

## 8. Considerações de Segurança

### 8.1 Validação
- Validar nome do jogador (tratar caracteres especiais, pois o nome deve suportar qualquer caractere UTF-8 válido)
- Limitar tamanho de mensagens a 1000 caracteres
- Validar formato de IP:porta

### 8.2 Proteção
- Host pode recusar conexões
- Limitar número máximo de clientes (opcional, configurável)
- Timeout de inatividade

## 9. Testes

### 9.1 Testes Unitários
- Serialização/deserialização de mensagens
- Parsing de IP:porta
- Validação de nomes de jogadores

### 9.2 Testes de Integração
- Conexão host-cliente
- Broadcast de mensagens
- Desconexão e reconexão
- Múltiplos clientes simultâneos

## 10. Ordem de Implementação

1. **Etapa 1**: Criar módulo Network com tipos básicos e protocolo
2. **Etapa 2**: Implementar servidor (aceitar conexões, gerenciar clientes)
3. **Etapa 3**: Implementar cliente (conectar, enviar/receber mensagens)
4. **Etapa 4**: Adicionar comandos :host e :connect em Action.hs
5. **Etapa 5**: Integrar broadcast de story logs
6. **Etapa 6**: Modificar GameContext para suportar multiplayer
7. **Etapa 7**: Adicionar UI para status de conexão e prompts
8. **Etapa 8**: Implementar heartbeat e tratamento de desconexões
9. **Etapa 9**: Testes e refinamentos

## 11. Questões Pendentes para Decisão

1. **Porta padrão**: Qual porta usar? 9876 
2. **Formato de timestamp**: UTC? Timezone local? UTC
3. **Limite de jogadores**: Máximo configurável ou ilimitado? ilimitado por padrão, configurável
4. **Sincronização de outros dados**: Apenas logs ou também progress tracks, bonds, etc.? Devem ser compartilhados progress tracks EXCLUSIVAMENTE de combate e jornada. Vows e bonds são pessoais.
5. **Persistência de sessão**: Salvar informações da sessão multiplayer no savefile? Sim, o savefile deve conter tudo que foi feito na sessão multiplayer como se tivesse sido feito em uma sessão solo.
6. **Podem existir vows em conjunto?** Jogadores podem criar um voto compartilhado através do comando :sharedvow. Neste caso deverá ser feito o broadcast do voto para todos os jogadores da sessão (se cliente: envia para o host, host faz broadcast; se host: host faz broadcast). Todo tick de progresso adicionado ao vow deverá ser feito broadcast para os jogadores na sessão. Experiencia ao completar o vow deverá ser compartilhada.

---

## Confirmação

Este planejamento detalha a implementação completa do sistema multiplayer em LAN. 
A arquitetura foi projetada para ser:
- **Modular**: Componentes separados e reutilizáveis
- **Extensível**: Fácil adicionar novos tipos de mensagens
- **Robusta**: Tratamento de erros e desconexões
- **Não-invasiva**: Mínimas mudanças no código existente

**Posso prosseguir com a implementação seguindo este planejamento?** Sim
