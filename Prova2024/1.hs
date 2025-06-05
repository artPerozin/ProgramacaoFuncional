--1. considerando a declaração da função foo abaixo,
-- mostre todos os passos (aplicações da função na execução de: foo 24 9

foo a b = if a == b then b else if a > b then foo (a-b) b else foo a (a-b)