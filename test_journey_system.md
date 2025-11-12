# Teste do Sistema de Jornadas - IronSworn

## Comandos para Testar

Execute estes comandos no SoloRPG para testar o sistema de jornadas:

```bash
# 1. Criar um personagem de teste
:create TestKael iron:3 edge:2 heart:1 shadow:2 wits:3

# 2. Criar uma jornada (troublesome para testar rapidamente)
:journey "Vila Próxima" troublesome

# 3. Ver o journey track criado
:tracks

# 4. Fazer o primeiro Undertake a Journey
:move UndertakeJourney wits

# 5. (Escolher uma das opções apresentadas)

# 6. Ver progresso após a escolha
:tracks

# 7. Repetir Undertake a Journey se necessário
:move empreenderjornada wits

# 8. Quando tiver progresso suficiente, tentar alcançar destino
:move ReachYourDestination

# 9. Alternativamente, pode fazer um progress roll direto
:fulfill "Vila Próxima"
```

## Comandos Alternativos para Testar

```bash
# Testar jornada mais longa (dangerous)
:journey "Cidade Distante" dangerous

# Testar em português 
:move empreenderjornada wits

# Testar reach destination em português
:move alcancardestino

# Marcar progresso manual
:progress "Vila Próxima"

# Abandonar jornada se necessário
:abandon "Vila Próxima"
```

## O que Esperar

### Ao criar Journey Track:
- Mensagem confirmando criação
- Informação sobre progresso por waypoint
- Track aparece em `:tracks`

### Ao fazer Undertake a Journey:
- **Strong Hit**: Duas escolhas (recursos sábios vs velocidade)
- **Weak Hit**: Progresso automático + perda de supply 
- **Miss**: Pay the Price executado automaticamente

### Ao fazer Reach Your Destination:
- **Strong Hit**: Chegada bem-sucedida
- **Weak Hit**: Complicação com escolhas
- **Miss**: Falha com opções de recuperação

### Progress Tracks:
- Journey tracks aparecem junto com combat tracks e vows
- Progresso mostrado em boxes (0-10) e ticks (0-40)
- Rank e tipo claramente identificados
