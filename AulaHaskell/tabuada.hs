Tabuada = tabAux1

tabAux1 9 = tabAux2 9 0
tabAux1 n = tab tabAux2 n 0 ++ tabAux1 (n+1)

tabAux2 n1 9 = [(n1, 9, n1*9)]
tabAux2 n1 n2 = (n1, n2, n1*n2):tabAux2 n1 (n2+1)