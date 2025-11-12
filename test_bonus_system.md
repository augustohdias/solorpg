# Teste do Sistema de Bônus - Correção

## Comandos para Testar os Bônus Persistentes

Execute estes comandos no SoloRPG para verificar se a correção está funcionando:

### Teste 1: Bônus Persistente Básico
```bash
# Criar personagem com stats baixos para ver o efeito dos bônus
:create BonusTest iron:1 edge:1 heart:1 shadow:1 wits:1

# Adicionar bônus persistente
:bonus persistent +2 "Treinamento Especial"

# Fazer movimento - deve mostrar bonus aplicado
:move FaceDanger edge

# O resultado deve mostrar algo como:
# Action Die: X + 3 (stat: +1, bonus: +2) = Y
```

### Teste 2: Combinação de Bônus
```bash
# Adicionar bônus temporário
:bonus nextroll +1 "Preparado"

# Fazer movimento - deve aplicar ambos
:move SecureAdvantage wits

# Deve mostrar: (stat: +1, bonus: +3)
# Após o movimento, apenas o persistente deve restar
```

### Teste 3: Bônus Específico de Movimento
```bash
# Adicionar bônus para movimento específico
:bonus nextmove:FaceDanger +2 "Cautela Extra"

# Fazer Face Danger - deve aplicar persistente + específico
:move FaceDanger iron

# Fazer outro movimento - só deve aplicar persistente
:move GatherInformation wits
```

### Teste 4: Verificar Que Persistente Não é Consumido
```bash
# Fazer vários movimentos seguidos
:move FaceDanger edge
:move SecureAdvantage heart  
:move GatherInformation shadow

# Todos devem mostrar o bônus persistente sendo aplicado
```

## O Que Observar

### ✅ Funcionamento Correto:
- Bônus aparece na mensagem de resultado
- Bônus persistente é aplicado a todos os movimentos
- Bônus temporários são consumidos após uso
- Modificador total inclui stat + bonus

### ❌ Problemas (se ainda existissem):
- Bônus não aparece na rolagem
- Modificador total igual apenas ao stat
- Bônus persistente desaparece após uso

## Exemplo de Saída Esperada

```
>>> EnfrentarPerigo <<<
Action Die: 3 + 3 (stat: +1, bonus: +2) = 6
Challenge Dice: 2, 8  
Resultado: [~] Sucesso Parcial!
  [Bônus consumido: Preparado +1]

>>> SegurarVantagem <<<
Action Die: 5 + 3 (stat: +1, bonus: +2) = 8
Challenge Dice: 4, 6
Resultado: [+] Sucesso Total!
```

## Comandos de Utilidade

```bash
# Ver todos os tracks ativos (não mostra bônus, mas confirma contexto)
:tracks

# Ver recursos do personagem
:char

# Adicionar XP se necessário para testar assets
:addres experience:+5

# Resetar se necessário (criar novo personagem)
:create NovoTeste iron:2 edge:2 heart:2 shadow:2 wits:2
```

A correção garante que os bônus persistentes agora funcionem corretamente em todos os movimentos!
