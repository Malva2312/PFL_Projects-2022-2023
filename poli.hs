import Data.List


data Var = Var { 
    var :: String, 
    expoent :: Integer
} deriving (Eq,Ord, Show)

data Mono = Mono { 
    vars :: [Var],
    coef :: Integer
} deriving (Eq,Ord, Show)

-- let x = Mono (sort (vars x)) (coef x)
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
mult x  y  = normalize (multiplyPoly x y)

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


mono1 = [ 
    (Mono [(Var "x" 2)] 0),
    (Mono [(Var "y" 1)] 2),
    (Mono [(Var "z" 1)] 5),
    (Mono [(Var "y" 1)] 1),
    (Mono [(Var "y" 2)] 7)]