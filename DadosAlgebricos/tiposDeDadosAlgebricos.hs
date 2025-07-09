data DiaDaSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
    deriving (Show, Eq, Ord)

data Booleano = Verdadeiro | Falso
    deriving (Show, Eq)

proxDiaUtil :: DiaDaSemana -> DiaDaSemana
proxDiaUtil Domingo = Segunda
proxDiaUtil Sabado  = Segunda
proxDiaUtil Sexta   = Segunda
proxDiaUtil Quinta  = Sexta
proxDiaUtil Quarta  = Quinta
proxDiaUtil Terca   = Quarta
proxDiaUtil Segunda = Terca

ehDiaUtil d = d > Domingo && d < Sabado
