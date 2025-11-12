# Issues Conhecidas

## Multiplayer

### Issue #1: IP Local Mostrado como 0.0.0.0
**Status:** Aberto  
**Prioridade:** Média  
**Descrição:**  
Ao executar o comando `:host`, o IP local exibido aparece como `0.0.0.0:9876` ao invés do IP real da interface de rede (ex: `192.168.1.100:9876`).

**Impacto:**  
Jogadores não conseguem conectar ao host porque o IP exibido não é válido para conexão em LAN.

**Tentativas de Resolução:**  
- Implementada função `getLocalIP` que tenta obter IP real da interface de rede
- Filtragem de IPs localhost (127.x.x.x)
- Priorização de IPs privados (192.168.x.x, 10.x.x.x, 172.16-31.x.x)
- Uso de `getAddrInfo` com diferentes estratégias

**Próximos Passos:**  
- Investigar uso de bibliotecas específicas para obter interfaces de rede (ex: `network-info`)
- Considerar uso de comandos do sistema operacional (`ip addr`, `ifconfig`, etc.)
- Implementar fallback para mostrar mensagem instruindo usuário a descobrir IP manualmente

**Arquivos Relacionados:**
- `src/System/Network/Server.hs` - Função `getLocalIP`
