module Poli where

import Data.List
import Data.List.Split


--Data Types

{- Var represents a variable that is composed of a name ("x", "y", ...) and an integer exponent -}
data Var = Var { 
    var :: String, 
    expoent :: Integer
} deriving (Eq,Ord, Show)

{- Mono represents a monomial that is composed of a list of variables ([Var]) and an integer coefficient -}
data Mono = Mono { 
    vars :: [Var],
    coef :: Integer
} deriving (Eq,Ord, Show)

--Normalization

{- Deletes variables whose exponent is 0 -}
assert :: Mono -> Mono
assert m = Mono [y | y <- (vars m), (expoent y /= 0)] (coef m)

{- Remove the monomials that have coefficient 0 -}
clean :: [Mono] -> [Mono]
clean [] = []
clean x = map assert [y | y <- x, ((coef y) /= 0 )]

{- Sort the variables present in the received monomial and return it -}
orderMono :: Mono -> Mono
orderMono x =  Mono (sort (vars x)) (coef x)

{- Joins monomials with the same variables returning a list of simplified monomials -}
joinMono :: [Mono] -> [Mono]
joinMono [] = []
joinMono (x:xs) = [Mono (vars x) new_coef] ++ (joinMono [y | y <- xs, (vars x /= vars y)])
    where new_coef = sum (map coef [y | y <- (x:xs), (vars x == vars y)] ) 

{- Returns the degree of the monomial -}
degree :: Mono -> Integer
degree x = sum (map expoent (vars x))

{- Auxiliar function to order with the monomial degree -}
higherDegree :: Mono -> Mono -> Ordering
higherDegree x y 
    | degree x >= degree y = LT
    | degree x < degree y = GT

{- Returns a polynomial (list of monomials) in normal form -}
normalize :: [Mono] -> [Mono]
normalize [] = []
normalize ms = sortBy higherDegree (clean (joinMono (sort (map orderMono ms))))


--Addiction

{- Returns a polynomial in normal form resulting from the sum of the polynomials given as input -}
add :: [Mono] -> [Mono] -> [Mono]
add x y = normalize (x ++ y)

{- Return a monomial multiplied by -1 -}
negMultiply :: Mono -> Mono
negMultiply x = Mono (vars x) ((coef x) * (-1))

{- Returns a polynomial in normal form resulting from the subtraction of the polynomials given as input -}
subtraction :: [Mono] -> [Mono] -> [Mono]
subtraction x y = normalize (x ++ (map negMultiply y))


--Multiplication

{- Add the exponents of the variables that are equal -}
addVars :: [Var] -> [Var] -> [Var]
addVars [] [] = []
addVars [] y = y
addVars x [] = x
addVars (x:xs) (y:ys) = [Var (var x) (sum (map expoent ([z | z <- (x:xs), (var z == var x)] ++ [z | z <- (y:ys), (var z == var x)]))) ] ++ (addVars [z | z <- (x:xs), (var z /= var x)] [z | z <- (y:ys), (var z /= var x)])

{- Apply multiplication between 2 monomials -}
multiplyMono :: Mono -> Mono -> Mono
multiplyMono x y = Mono (addVars (vars x) (vars y)) ((coef x) * (coef y) )

{- Apply multiplication between 1 monomial and 1 polynomial -}
multMP :: Mono -> [Mono] -> [Mono]
multMP x [] = []
multMP x (y:ys) 
    | (coef x == 0) = [] ++ (multMP x ys)
    | otherwise = [multiplyMono x y] ++ (multMP x ys)

{- Apply multiplication between 2 polynomials -}
multiplyPoly :: [Mono] -> [Mono] -> [Mono]
multiplyPoly [] [] = []
multiplyPoly [] y  = []
multiplyPoly x  [] = []
multiplyPoly (x:xs) y = (multMP x y) ++ (multiplyPoly xs y)

{- Returns, in normal form, the multiplication between 2 polynomials -}
mult :: [Mono] -> [Mono] -> [Mono]
mult [] [] = []
mult [] y  = []
mult x  [] = []
mult x  y  = normalize (multiplyPoly x y)


--Derivation

{- Returns the list index of the variable being derived -}
findIdx :: String -> [Var] -> Int -> Int
findIdx s [] idx = (-1)
findIdx s (v:vs) idx
    | s == var v = idx
    | otherwise = findIdx s vs (idx+1)

{- Derive a monomial as a function of the received variables ( f(x) = -3*x^2  then f'(x) = -3*2*x   ) -}
deriveMono :: String -> Mono -> Mono
deriveMono v m 
    | (vars m) == [] =  Mono [] 0
    | (idx == (-1))  =  Mono [] 0
    | otherwise      =  orderMono ( 
        Mono (
                [y | y <- (vars m),  (var y /= v)] ++ [Var v ((expoent ((vars m)!!idx)) - 1)]
            ) 
            (
                (coef m) * (expoent ((vars m )!!idx) )
            )
    )
    where idx = findIdx v (vars m) 0 

{- Derive a polynomial as a function of the received variables -}
derivePoly :: String -> [Mono] -> [Mono]
derivePoly v [] = []
derivePoly v (p:ps) = [deriveMono v p] ++ derivePoly v ps

{- Retorna, na forma normal, a  de um polinomio em função de uma variável -}
derive :: String -> [Mono] -> [Mono]
derive v [] = []
derive v p = normalize  (derivePoly v p)


--String Inputs & Outputs

{- Returns a string with the data of the variables of a monomial -}
showVars :: Mono -> String
showVars m  
    | vars m == [] = ""
    | length (vars m) == 1 = var ((vars m)!!0) ++ s
    | otherwise = var ((vars m)!!0)  ++ s  ++ "*"++ showVars (Mono [y | y <- (vars m), (var y /= var ((vars m)!!0)) ] (coef m))
    where s = if (expoent ((vars m)!!0) == 1) then "" else ("^" ++ show (expoent ((vars m)!!0)))

{- Returns a string with the monomial data -}
outPutMono :: Mono -> String
outPutMono m = s  ++ (showVars m)
    where 
        mul = if (vars m /= []) then "*" else ""
        s = if (abs (coef m) /= 1) then ( show (abs (coef m)) ++ mul) else (if (vars m == []) then "1" else "")

{- Returns a string with the arrangement of the polynomial-}
outPutPoly :: [Mono] -> String
outPutPoly [] = ""
outPutPoly (x:xs) = minum ++  (outPutMono x) ++ plus ++ (outPutPoly xs)
    where
        minum = if (coef x < 0) then " - " else ""
        plus = if (length xs > 0)
            then (if (coef (head xs) >= 0) then " + " else "")
            else ""

{- Remove spaces and replaces  '-' with "+-"-}
getExpression :: String -> String
getExpression [] = []
getExpression (x:xs) = s ++ getExpression xs
    where s = if (x == '-') then "+-" else ( if ( x == ' ') then "" else [x])

{- Read a Var from a string exp: ["x", "2"] -> Var "x" 2
if the exponent does not exist in the list then the variable is assumed to be raised to 1 -}
getVar :: [String] -> Var
getVar x 
    | (length x > 1) = Var (head x) (read (last x) :: Integer) 
    | (length x == 1) = Var (head x) 1

{- Receives a string and returns a monomial with a variable or just a coefficient -}
getSingleMono :: String -> Mono
getSingleMono x 
    | (x!!0 == '-') = if (x!!1 >= '0' && x!!1 <= '9' ) then (Mono [] (read x ::Integer)) else Mono [getVar (splitOn "^" (tail x))] (-1)
    | otherwise     = if (x!!0 >= '0' && x!!0 <= '9')  then (Mono [] (read x ::Integer)) else Mono [getVar (splitOn "^" x)] 1

{- Joins monomial through multiplication to form the monomial indicated by the string -}
getMono :: [String] -> Mono
getMono [] = Mono [] 1 
getMono (x:xs) = multiplyMono (getSingleMono x) (getMono xs) 

{- Splits the string into pieces and returns a list with the monomials calculated from the resulting string list -}
getInput :: String -> [Mono]
getInput [] = []
getInput x = map getMono (map (splitOn "*" ) (filter (/="")(splitOn "+" (getExpression x))))


--Principal Fucntions 

{- Takes a polynomial in a string and returns it in its normal form -}
normalization :: String -> String 
normalization [] = []
normalization x  = outPutPoly (normalize (getInput x) )

{- Takes two polynomials in a string and returns their multiplication -}
multiplication :: String -> String -> String
multiplication [] y = []
multiplication x [] = []
multiplication x y = outPutPoly (mult (getInput x) (getInput y))

{- Takes two polynomials in a string and returns their addiction -}
addiction :: String -> String -> String
addiction x [] = x
addiction [] y = y
addiction x y = outPutPoly (add (getInput x) (getInput y))

{- Receives a variable and a string polynomial and returns the derivation of the polynomial by the variable -}
derivation :: String -> String -> String
derivation [] x  = []
derivation v  [] = []
derivation v  x  = outPutPoly (derive v (getInput x))

