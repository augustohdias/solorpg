# Fix: Sistema de Bônus Persistentes

## Problema Identificado

Os bônus persistentes não estavam sendo aplicados aos movimentos porque a função `executeMoveWithRoll` no módulo `Move.hs` não estava considerando os bônus disponíveis no contexto do jogo.

## Solução Implementada

### Modificações no `src/System/Move.hs`:

1. **Obtenção do Contexto**: A função agora obtém o contexto atual do jogo para acessar os bônus ativos.

2. **Cálculo de Bônus**: Calcula os bônus aplicáveis ao movimento específico:
   - Bônus persistentes (sempre aplicados)
   - Bônus de próxima rolagem (NextRoll)
   - Bônus de movimento específico (NextMove)

3. **Aplicação dos Bônus**: Inclui os bônus no modificador total do movimento.

4. **Consumo de Bônus**: Consome automaticamente os bônus apropriados após o movimento:
   - Bônus `NextRoll` são consumidos
   - Bônus `NextMove` específicos são consumidos
   - Bônus `Persistent` **NÃO** são consumidos (mantêm-se ativos)

5. **Feedback Visual**: Mostra informação detalhada sobre os modificadores aplicados.

## Exemplo de Funcionamento

### Antes da Correção:
```
>>> EnfrentarPerigo <<<
Action Die: 4 + 2 = 6
Challenge Dice: 3, 7
Resultado: [~] Sucesso Parcial!
```

### Após a Correção:
```
>>> EnfrentarPerigo <<<  
Action Die: 4 + 3 (stat: +2, bonus: +1) = 7
Challenge Dice: 3, 7
Resultado: [+] Sucesso Total!
  [Bônus consumido: Preparado +1]
```

## Testes para Verificar a Correção

Execute estes comandos no SoloRPG para testar:

```bash
# 1. Criar personagem
:create TestChar iron:2 edge:3 heart:1 shadow:2 wits:2

# 2. Adicionar bônus persistente
:bonus persistent +1 "Bônus Permanente"

# 3. Fazer um movimento (deve aplicar o bônus)
:move FaceDanger edge

# 4. Verificar que o bônus persistente ainda está ativo
:move SecureAdvantage wits

# 5. Adicionar bônus temporário
:bonus nextroll +2 "Preparado"

# 6. Fazer movimento (deve aplicar ambos os bônus)
:move GatherInformation wits

# 7. Fazer outro movimento (só deve aplicar o persistente)
:move FaceDanger edge
```

## Tipos de Bônus e Comportamento

### Bônus Persistent
- **Aplicado**: A todos os movimentos
- **Consumido**: Nunca (permanece ativo)
- **Exemplo**: Vantagem de asset, condição especial

### Bônus NextRoll  
- **Aplicado**: À próxima rolagem de qualquer movimento
- **Consumido**: Após ser usado uma vez
- **Exemplo**: "Prepare to act" do Secure Advantage

### Bônus NextMove
- **Aplicado**: Apenas ao movimento específico nomeado
- **Consumido**: Após ser usado no movimento correto
- **Exemplo**: "+1 quando Undertake Journey" do Make Camp

## Arquitetura da Correção

A correção mantém a arquitetura existente e adiciona:

1. **Transparência**: Mostra claramente quais bônus foram aplicados
2. **Automação**: Consome bônus apropriados automaticamente  
3. **Persistência**: Mantém bônus persistentes ativos
4. **Compatibilidade**: Funciona com todo o sistema existente

## Impacto

- ✅ Bônus persistentes agora funcionam corretamente
- ✅ Todos os tipos de bônus são aplicados apropriadamente  
- ✅ Feedback visual melhorado
- ✅ Compatibilidade total com sistema existente
- ✅ Não quebra funcionalidades existentes

A correção resolve completamente o problema dos bônus persistentes não sendo aplicados aos movimentos.
