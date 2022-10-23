# PFL 1st Project

A nossa representação passou por criar um novo data type chamado "Mono", que representa um monómio. Consideramos que um polinómio é um conjunto de monómios.

**data Mono = Mono {vars :: [Var], coef :: Integer} deriving (Eq,Ord, Show)**

## Um "Mono" é constituído por 3 parâmetros:
- Exemplo: mono1 = [(Mono [(Var "x" 2)] 0), (Mono [(Var "y" 1)] 2), (Mono [(Var "z" 1)] 5), (Mono [(Var "y" 1)] 1), (Mono [(Var "y" 2)] 7)] corresponde a (0*x^2 + 2*y + 5*z + y + 7*y^2)
- O primeiro parâmetro corresponde à variável. Ex:["**x**","**y**",etc]
- O segundo parâmetro corresponde ao valor (inteiro) a que a variável está elevada. Ex:[x^**2**]
- O terceiro e úlitmo parâmetro corresponde ao valor inteiro pelo qual a varável está a ser multiplicada (coeficiente). Ex:[**2***x]

## Polinómio para String:
- Inicialmente foi feito um split nos "+" e os "-" ficam representados como "+-" e os espaços são posteriormente retirados.
- De seguida, é apresentada a representação final em forma de string como se pode ver nos exemplos e testando o programa.

**Turma 12, grupo 4**<br />
João Malva, up202006605<br />
João Félix, up202008867
