# Oráculos do SoloRPG

## Formato JSON

Cada arquivo `.json` neste diretório define um oráculo:

```json
{
  "oracleName": "Nome do Oráculo",
  "oracleDescription": "Descrição do oráculo",
  "oracleDice": "1d100",
  "oracleEntries": [
    {"entryRange": [1, 20], "entryText": "Resultado para 1-20"},
    {"entryRange": [21, 50], "entryText": "Resultado para 21-50"},
    {"entryRange": [51, 100], "entryText": "Resultado para 51-100"}
  ]
}
```

## Campos

- **oracleName**: Nome único do oráculo (usado nos comandos)
- **oracleDescription**: Descrição opcional
- **oracleDice**: Dado usado (1d100, 1d20, 2d10, etc)
- **oracleEntries**: Lista de entradas
  - **entryRange**: [min, max] - intervalo de valores
  - **entryText**: Texto do resultado

## Uso no Jogo

```bash
# Rolar automaticamente
:oracle "Nome do Oráculo"

# Consultar valor específico
:oracle "Nome do Oráculo" 42

# Via chaining
:roll 1d100 :over :oracle "Nome do Oráculo"

# Listar oráculos
:oracle

# Ver oráculo completo
:oracle NomeDoOráculo
```

## Exemplos de Oráculos Ironsworn

- **Action/Theme** - Gerador de eventos
- **Character Descriptors** - Características de NPCs
- **Settlement Troubles** - Problemas de comunidades
- **Combat Action** - Ações inimigas
- **Mystic Backlash** - Efeitos de magia
- **Region** - Tipos de região
- **Location** - Locais específicos
- **Coastal Waters** - Eventos marítimos

Crie seus próprios oráculos customizados!

