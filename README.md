# Verificação de expressões por Tableaux

### Alunos: Marcos Hayman e Daniel Bougleux

### Disciplina: Linguagens de Programação (TCC00304)

## Gerar um executável

Altere a linha 7 no arquivo [validateTree.hs](validateTree.hs), altere a constante "formula" para representar a fórmula que você quer avaliar e então utilize o seguinte commando para compilar:

```shell
ghc --make validateTree.hs
```

## Compilando interativamente

Altere a linha 7 no arquivo [validateTree.hs](validateTree.hs), altere a constante "formula" para representar a fórmula que você quer avaliar e utilize o seguinte comando para compilar:

```shell
ghci validateTree.hs
```

e dentro do ghci rode:

```shell
main
```

(Rodando pelo ghci costuma ser mais rápido)

## Da montagem da fórmula

A fórmula não pode ter espaços, usa a notação infixa, e são necessários parenteses para se denotar a prioridade das operações.
Os caracteres designados para cada operação são os seguintes:

```json
{
  "e": "^",
  "ou": "v",
  "implicação": ">",
  "negação": "~"
}
```

## Da notação

Ao final da execução é sempre exibida a árvore Tableaux, os caminhos que chegam a uma contradição são representados por uma folha "False:X"
