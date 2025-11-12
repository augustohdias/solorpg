import json
import os
import re
import unicodedata

# === CONFIGURAÇÕES ===
input_file = "AssetsList.json"        # arquivo JSON com a lista completa
output_dir = "./"       # pasta onde os arquivos individuais serão salvos

# === FUNÇÕES ===

def to_capitalized_camel_case(text):
    # Remove acentos e pontuação
    text = ''.join(
        c for c in unicodedata.normalize('NFKD', text)
        if not unicodedata.combining(c)
    )
    # Remove caracteres não alfanuméricos e capitaliza
    parts = re.split(r'[^a-zA-Z0-9]+', text)
    return ''.join(p.capitalize() for p in parts if p)

# === PROCESSAMENTO ===

# Cria a pasta de saída
os.makedirs(output_dir, exist_ok=True)

# Carrega o arquivo principal
with open(input_file, "r", encoding="utf-8") as f:
    data = json.load(f)

# Verifica se é uma lista
if not isinstance(data, list):
    raise ValueError("O arquivo AssetList.json deve conter uma lista de objetos JSON.")

# Gera um arquivo separado para cada asset
for asset in data:
    name = asset.get("name", "SemNome")
    filename = to_capitalized_camel_case(name) + ".json"
    filepath = os.path.join(output_dir, filename)
    
    # Salva o JSON individual
    with open(filepath, "w", encoding="utf-8") as f:
        json.dump(asset, f, ensure_ascii=False, indent=4)

print(f"✅ {len(data)} arquivos JSON criados na pasta '{output_dir}'.")

