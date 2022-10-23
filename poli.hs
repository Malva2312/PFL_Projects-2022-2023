import Data.List
import Data.List.Split

data Var = Var { 
    var :: String, 
    expoent :: Integer
} deriving (Eq,Ord, Show)

data Mono = Mono { 
    vars :: [Var],
    coef :: Integer
} deriving (Eq,Ord, Show)

orderMono :: Mono -> Mono
orderMono x =  Mono (sort (vars x)) (coef x)

equalVars :: Mono -> Mono -> Bool
equalVars x y = (vars x == vars y)

notEqualVars :: Mono -> Mono -> Bool
notEqualVars x y = not (equalVars x y)

normalize :: [Mono] -> [Mono]
normalize [] = []
normalize (x:xs) = [Mono (vars x) new_coef] ++ (normalize [y | y <- xs, (vars x /= vars y)])
    where new_coef = sum (map coef [y | y <- (x:xs), (vars x == vars y)] ) --  [coef y | y <- (x:xs), (vars x == vars y)] )   

norm :: [Mono] -> [Mono]
norm [] = []
norm ms = clean (normalize (sort (map orderMono ms)))

addiction :: [Mono] -> [Mono] -> [Mono]
addiction x y = norm (x ++ y)

negMultiply :: Mono -> Mono
negMultiply x = Mono (vars x) ((coef x) * (-1))

subtraction :: [Mono] -> [Mono] -> [Mono]
subtraction x y = norm (x ++ (map negMultiply y))

clean :: [Mono] -> [Mono]
clean [] = []
clean x = [y | y <- x, ((coef y) /= 0 )]

addVars :: [Var] -> [Var] -> [Var]
addVars [] [] = []
addVars [] y = y
addVars x [] = x
addVars (x:xs) (y:ys) = [Var (var x) (sum (map expoent ([z | z <- (x:xs), (var z == var x)] ++ [z | z <- (y:ys), (var z == var x)]))) ] ++ (addVars [z | z <- (x:xs), (var z /= var x)] [z | z <- (y:ys), (var z /= var x)])


multiplyMono :: Mono -> Mono -> Mono
multiplyMono x y = Mono (addVars (vars x) (vars y)) ((coef x) * (coef y) )


multiply :: Mono -> [Mono] -> [Mono]
multiply x [] = []
multiply x (y:ys) 
    | (coef x == 0) = [] ++ (multiply x ys)
    | otherwise = [multiplyMono x y] ++ (multiply x ys)


multiplyPoly :: [Mono] -> [Mono] -> [Mono]
multiplyPoly [] [] = []
multiplyPoly [] y  = []
multiplyPoly x  [] = []
multiplyPoly (x:xs) y = (multiply x y) ++ (multiplyPoly xs y)

mult :: [Mono] -> [Mono] -> [Mono]
mult [] [] = []
mult [] y  = []
mult x  [] = []
mult x  y  = norm (multiplyPoly x y)

findIdx :: String -> [Var] -> Int -> Int
findIdx s [] idx = (-1)
findIdx s (v:vs) idx
    | s == var v = idx
    | otherwise = findIdx s vs (idx+1)

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

derivePoly :: String -> [Mono] -> [Mono]
derivePoly v [] = []
derivePoly v (p:ps) = [deriveMono v p] ++ derivePoly v ps

derive :: String -> [Mono] -> [Mono]
derive v [] = []
derive v p = norm  (derivePoly v p)


showVars :: Mono -> String
showVars m  
    | vars m == [] = ""
    | length (vars m) == 1 = var ((vars m)!!0) ++ s
    | otherwise = var ((vars m)!!0)  ++ s  ++ "*"++ showVars (Mono [y | y <- (vars m), (var y /= var ((vars m)!!0)) ] (coef m))
    where s = if (expoent ((vars m)!!0) == 1) then "" else ("^" ++ show (expoent ((vars m)!!0)))

outPutMono :: Mono -> String
outPutMono m = s  ++ (showVars m)
    where s = if (coef m /= 1) then (show (abs (coef m)) ++ "*") else ""

outPutPoly :: [Mono] -> String
outPutPoly [] = ""
outPutPoly (x:xs) 
    | length xs > 0 = (outPutMono x) ++ signal ++ (outPutPoly xs)
    | otherwise = (outPutMono x)
    where signal = if (coef (head xs) >= 0) then " + " else " - "

getExpression :: String -> String
getExpression [] = []
getExpression (x:xs) = s ++ getExpression xs
    where s = if (x == '-') then "+-" else ( if ( x == ' ') then "" else [x])

-- splitOn '+' -> splitOn '*' -> splitOn '^' -> 

getVar :: [String] -> Var
getVar x 
    | (length x > 1) = Var (head x) (read (last x) :: Integer)
    | (length x == 1) = Var (head x) 1 

getSingleMono :: String -> Mono
--getSingleMono [] = Mono [] 1
getSingleMono x 
    | (x!!0 == '-') = if (x!!1 >= '0' && x!!1 <= '9' ) then (Mono [] (read x ::Integer)) else Mono [getVar (splitOn "^" (tail x))] (-1)
    | otherwise     = if (x!!0 >= '0' && x!!0 <= '9')  then (Mono [] (read x ::Integer)) else Mono [getVar (splitOn "^" x)] 1


getMono :: [String] -> Mono
getMono [] = Mono [] 1 
getMono (x:xs) = multiplyMono (getSingleMono x) (getMono xs) 


getInput :: String -> [Mono]
getInput [] = []
getInput x = map getMono (map (splitOn "*" ) (splitOn "+" (getExpression x))) -- remove '+'

a = Mono [Var "x" 3] 3
b = Mono [Var "y" 2] 6
c = Mono [Var "z" 5] 2
d = Mono [Var "x" 5] 9
n = Mono [Var "y" 2] 5

x0 = Var "x" 0
x1 = Var "x" 1
x2 = Var "x" 2
x3 = Var "x" 3

y0 = Var "y" 0 
y1 = Var "y" 1
y2 = Var "y" 2
y3 = Var "y" 3

z0 = Var "z" 0 
z1 = Var "z" 1
z2 = Var "z" 2
z3 = Var "z" 3


poly1 = [ 
    (Mono [(Var "x" 2)] 0),
    (Mono [(Var "y" 1)] 2),
    (Mono [(Var "z" 1)] 5),
    (Mono [(Var "y" 1)] 1),
    (Mono [(Var "y" 2)] (-7))]