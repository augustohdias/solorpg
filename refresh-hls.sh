#!/bin/bash
# Script para atualizar o projeto e reiniciar o HLS

echo "Limpando caches antigos..."
rm -rf .hie

echo "Recompilando projeto..."
stack clean
stack build

echo "Pronto! Agora reinicie o HLS no Cursor IDE."
echo "Use: Ctrl+Shift+P -> 'Haskell: Restart Haskell LSP Server'"

