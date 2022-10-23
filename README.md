# PFL 1st Project

A nossa representação passou por criar dois novos data types chamados "Var" e "Mono", que representam um monómio. Consideramos que um polinómio é um conjunto de monómios.

**data Mono = Mono {vars :: [Var], coef :: Integer} deriving (Eq,Ord, Show)**

## Um "Var" representa um par (variável, expoente)
- Exemplo: ( Var "x" 2 ) representa x^2
- Um monomio tem em si contido [Var] que representam as suas variáveis.

## Um "Mono" é constituído por 2 parâmetros:
- Exemplo: Mono 
- O primeiro parâmetro corresponde às variáveis (Var). Ex:[Var "x" 2, Var "y" 3] -> x^2 * y^3
- O segundo parâmetro corresponde ao valor (inteiro) pelo qual as variáveis são multiplicadas. Corresponde ao coeficiente.

## Leitura de String:
- Ao receber a string todos os "-" são substituidos por "+-" e todos os espaços (" ") são removidos.
- De seguida, foi feito um split nos "+" obtendo todas as parcelas que correspondem ao polinómio.
- Transformando as parcelas em monomios obtém-se uma lista de monomios (polinimio) pelo qual é possivel realizar as operações.
- Após a realização da operação o resultado é apresentado de volta em string, construido a string monómio a monómio. 

## Inputs
- As multiplicações têm que estar explícitas com 'x', ex: 2*x*y
- As variáveis quando elevadas a um número diferente de 1 têm que usar o caratér '^', ex: x^3
- As parcelas têm que estar dívididas com '+' ou '-'
- Não admite a utilização de '(' e ')'
- Ex: 2*x^3*y +5*z -3*y +1
 
## Funcionalidades
- Normalização: são somados os monómios cujas variáveis sejam iguais e são eliminados monómios com coeficente 0, são apresentados pela ordem de grau.
- Soma: soma dois polinómios, somando os monómios com variáveis iguais e apresenta o resultado na forma normal.
- Multiplicação: realiza multiplicações entre um monómio e um polinomio1 por cada monómio presente num polinomio2. O resultado é apresentado na forma normal.
- Derivação: realiza a derivação em função de apenas uma variável e apresenta o resultado na forma normal.

## Chamada a funções
- Normalização: normalization "String-Polinomio"
- Soma: addiction "String-Polinomio-1" "String-Polinomio-2"
- Multiplicar: multiplication "String-Polinomio-1" "String-Polinomio-2"
- Derivação: derivation "variavel-pela-qual-se-deriva" "String-Polinomio"

**Turma 12, grupo 4**<br />
João Malva, up202006605<br />
João Félix, up202008867
