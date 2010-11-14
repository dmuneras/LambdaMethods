module Interpolacion where

import GramaticaAbstracta
import Semantica
import SistemasEcuaciones
import FuncionesAuxiliaresSE

{-METODO DE NEWTON CON DIFERENCIAS DIVIDIDAS
NOTA: Este metodo tiene dos partes: Una que evalua un punto nuevo en el polinomio y da el resultado de la interpolacion y otra que 
entrega el polinomio interpolante escrito por medio de nuestra gramatica de funciones. Esta basado en la implementacion hecha por 
Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1.
-}

--Funcion que halla el Polinomio Interpolante de Newton de una serie de puntos x con sus respectivas evaluaciones fx
newtonPolinomio :: [Double] -> [Double] -> Func
newtonPolinomio x fx = makePolN (coefB fx x n [] 0) x
                       where n = length fx -1

--Funcion que interpola un valor dadas una lista de puntos x y sus correspondientes fx usando el Polinomio Interpolante de Newton
newtonEvaluado :: [Double] -> [Double] -> Double -> Func
newtonEvaluado x fx v = eval (newtonPolinomio x fx) ('x', FConst v)

--Funcion que halla los b del polinomio por medio de las diferencias divididas (Basada en la practica referenciada)
coefB ::[Double]->[Double]->Int->[Double]->Int->[Double]
coefB fx x n fn i 
      | i<= n = coefB fx x n (fn++[(ff 0 i)]) (i+1)
      | otherwise = fn
      where ff j i 
               | i /= j =  ((ff j (i-1)) - (ff (j+1) i))/((x !! j) - (x !! i))
               | i == j = fx !! i

--Funcion que convierte la lista de doubles en una lista de Funciones Constantes
makeCons :: [Double] -> [Func]
makeCons [] = []
makeCons (b:bs) = [FPar (FConst b)] ++ makeCons bs

--Funcion que convierte la lista de valores de x en una lista de funciones de la forma (x - xi)
makeRes :: [Double] -> [Func]
makeRes [] = []
makeRes (x:xs) = [FPar (FRes (FVar 'x') (FConst x))] ++ makeRes xs

--Funcion que toma la lista de bi y la lista de las restas (x - xi) y las convierte en multiplicaciones
makeMultN :: [Func] -> [Func] -> Int -> Int -> [Func]
makeMultN [] [] 0 _  = []
makeMultN b x i n
          | (i == 1) = [FMult (b!!i) (x!!(i-1))]
          | (i < n) = ant ++ [FMult (b!!i) (foldl1 (FMult) (factors x i n))]
          | (otherwise) = []
          where ant = makeMultN b x (i-1) n

--Funcion que saca los factores que multiplican a un bi
factors :: [Func] -> Int -> Int -> [Func]
factors x i n
        | (i == 1) = [x!!(i-1)]
        | (i < n) = ant ++ [x!!(i-1)]
        | (otherwise) = error "Fuera de rango"
        where ant = factors x (i-1) n

--Funcion que toma una lista de funciones y las suma
makeSum :: [Func] -> Func
makeSum t = foldl1 (FSum) t

--Funcion que utiliza las funciones anteriores y da el Polinomio Interpolante de Newton sumando el termino b0 al resto de la suma
makePolN :: [Double] -> [Double] -> Func
makePolN b x = FSum ((makeCons b)!!0) (makeSum (makeMultN (makeCons b) (makeRes x) (length x -1) (length x)))


{-METODO DE LAGRANGE
NOTA: Este metodo tiene dos partes: Una que evalua un punto nuevo en el polinomio y da el resultado de la interpolacion y otra que 
entrega el polinomio interpolante escrito por medio de nuestra gramatica de funciones. Esta basado en la implementacion hecha por 
Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1.
-}

--Funcion que halla el Polinomio Interpolante de Lagrange de una serie de puntos x con sus respectivas evaluaciones fx
lagrangePolinomio :: [Double] -> [Double] -> Func
lagrangePolinomio x fx = makePolL (makeMultL (coefL x n 0 []) (makeCons fx) n n)
                         where n = length x -1

--Funcion que interpola un valor dadas una lista de puntos x y sus correspondientes fx usando el Polinomio Interpolante de Lagrange
lagrangeEvaluado :: [Double] -> [Double] -> Double -> Func
lagrangeEvaluado x fx v = eval (lagrangePolinomio x fx) ('x',FConst v)

--Funcion que halla las funciones L del polinomio (Basada en la practica referenciada)
coefL :: [Double] -> Int -> Int -> [Func] -> [Func]
coefL x n i l 
      | i<=n = coefL x n (i+1) (l++[FPar (FDiv (prodArriba i 0 []) (prodAbajo i 0 []))])
      | otherwise = l
      where prodArriba i j li
                       | j==i = prodArriba i (j+1) li
                       | j<=n = prodArriba i (j+1) (li++[FPar (FRes (FVar 'x') (FConst(x!!j)))])
                       | otherwise = foldl1 (FMult) li
            prodAbajo i j li
                      | j == i = prodAbajo i (j+1) li
                      | j <= n = prodAbajo i (j+1) (li++[FConst ((x!!i)-(x!!j))])
                      | otherwise = reduccion (foldl1 (FMult) li)

--Funcion que toma la lista de Li y la lista de los fx y las convierte en multiplicaciones
makeMultL :: [Func] -> [Func] -> Int -> Int -> [Func]
makeMultL [] [] _ _ = []
makeMultL l fx i n
          | (i == 0) = [FMult (l!!0) (fx!!0)]
          | (i <= n) = ant ++ [FMult (l!!i) (fx!!i)]
          where ant = makeMultL l fx (i-1) n

--Funcion que utiliza las funciones anteriores y da el Polinomio Interpolante de Lagrange sumando los terminos de la forma Li(x)f(x)
makePolL :: [Func] -> Func
makePolL t = makeSum t

{-SPLINES LINEALES
NOTA: Este metodo tiene dos partes: Una que evalua un punto nuevo en el polinomio y da el resultado de la interpolacion y otra que 
entrega el polinomio interpolante escrito por medio de nuestra gramatica de funciones. Esta basado en la implementacion hecha por 
Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1.
-}

--Funcion que halla la funcion por tramos de trazadores lineales dados unos puntos x y sus respectivos fx
spLinEcuaciones x fx = ecsLin x fx n
                       where n = length x -1

--Funcion que dados unos puntos x, fx, y un valor a interpolar busca el tramo al que pertenece el valor y evalua la ecuacion
spLinEvaluado x fx v = eval (regLin x fx (srchValue x v)) ('x',FConst v)

--Funcion que devuelve una lista con las ecuaciones lineales para cada tramo de la lista de puntos
ecsLin x fx i
       | (i == 1) = [(show (regLin x fx i)) ++ "para " ++ show (x!!(i-1)) ++ " <= x <= " ++ show (x!!i)]
       | (i < n) = ant ++ [(show (regLin x fx i)) ++ "para " ++ show (x!!(i-1)) ++ " <= x <= " ++ show (x!!i)]
       where n = length x
             ant = ecsLin x fx (i-1)

--Funcion que hace la regresion lineal para un tramo i. Basada en la practica referenciada
regLin x fx i = FSum (FConst (fst(ifx))) (FMult (FPar (FConst m)) (FPar (FRes (FVar 'x') (FConst (fst(ix))))))
       where m = (fst(ifx) - snd(ifx))/(fst(ix)-snd(ix))
             ix = fst(par)
             ifx = (fx!!(snd(par)),fx!!(snd(par) +1))
             par = srchIndex x i

--Funcion que busca el indice del tramo al que pertenece una ecuacion. Basada en la practica referenciada
srchIndex x i 
          | n1 /= 0 = ((last(fst(mitd)),head(snd(mitd))),n)
          | otherwise = ((head(snd(mitd)),(snd(mitd)!!1)),n)
          where mitd = break (>=(x!!i)) x
                n = if(n1==0) then (n1) else (n1-1)
                n1 = length(fst(mitd))

--Funcion que dado un valor de x y una lista de puntos de x, busca a que tramo pertenece. Basada en la practica referenciada
srchValue x v = if (n1 == 0) then (n1+1) else (n1)
                where n1 = length (fst mitd)
                      mitd = break (>=v) x


{-FUNCIONES GENERALES DE SPLINES CUADRATICOS Y CUBICOS
-}

--Funcion que halla los terminos independientes de las ecuaciones de empalme. Basada en la practica referenciada
empalB :: [Double] -> [Double] -> Int -> Int -> [Double]
empalB fx l i n
           | i<=(n-1) = empalB fx (l++[fx!!i,fx!!i]) (i+1) n
           | otherwise = l

--Funcion que halla los terminos independientes de las ecuaciones de los extremos. Basada en la practica referenciada
extB :: [Double] -> Int -> [Double]
extB fx n = [fx!!0,fx!!n]

{-SPLINES CUADRATICOS
NOTA: Este metodo tiene dos partes: Una que evalua un punto nuevo en el polinomio y da el resultado de la interpolacion y otra que 
entrega el polinomio interpolante escrito por medio de nuestra gramatica de funciones. Esta basado en la implementacion hecha por 
Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1.
-}

--Funcion que retorna los valores de los coeficientes
coefSpCuad :: [Double] -> [Double] -> [(Integer,Double)]
coefSpCuad x fx = eGaussPParcial (matrizSpCuad x fx) (fromIntegral (length x -1)*3)

--Funcion que arma la matriz aumentada del sistema de ecuaciones
matrizSpCuad :: [Double] -> [Double] -> Matriz
matrizSpCuad x fx = leerMatrizAu (fromIntegral (length x -1)* 3) (sistmEcSpCuad x fx)

--Funcion que une todas las ecuaciones del sistema en una lista sencilla. Basada en la practica referenciada
sistmEcSpCuad :: [Double] -> [Double] -> [Double]
sistmEcSpCuad x fx = (empalCuad x fx [] 1 n neq)++(extCuad x fx neq n)++(derCuad x n neq [] 1)++(sdaDerCuad neq)
                     where n = length x -1
                           neq  = (n)*3

--Funcion que halla las ecuaciones de empalme. Basada en la practica referenciada
empalCuad :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> [Double]
empalCuad x fx l i n neq
          | i<=(n-1) = empalCuad x fx (l++(take ((i-1)*3) [0,0..])++[(x!!i)^2,x!!i,1]++(take (neq-3*i) [0,0..])++[b1]++(take (3*i) [0,00..])++[(x!!i)^2,x!!i,1]++(take (neq -3-(i)*3) [0,0..])++[b2]) (i+1) n neq
          | otherwise = l
          where b = empalB fx [] 1 n
                b1 = if (i==1) then (b!!(i-1)) else (b!!i)
                b2 = if (i==1) then (b!!i) else (b!!(i+1))

--Funcion que halla las ecuaciones de los extremos. Basada en la practica referenciada
extCuad :: [Double] -> [Double] -> Int -> Int -> [Double]
extCuad x fx neq n = ([(x!!0)^2,x!!0,1]++(take zeros [0,0..])++[b!!0]++(take zeros [0,0..])++[(x!!n)^2,x!!n,1]++[b!!1])
                     where zeros = div (neq*2-6) 2
                           b = extB fx n

--Funcion que halla las funciones de las derivadas en los nodos internos y las iguala. Basada en la practica referenciada
derCuad :: [Double] -> Int -> Int -> [Double] ->Int -> [Double]
derCuad x n neq l i
        | i<=(n-1) = derCuad x n neq (l++(take ((i-1)*3) [0,0..])++[2*(x!!i),1,0,-2*(x!!i),-1,0]++(take ((neq -3 -i*3)+1) [0,0..])) (i+1)
        | otherwise = l

--Funcion que halla la segunda derivada de la primera ecuacion igualada a cero. Basada  en la practica referenciada
sdaDerCuad :: Int -> [Double]
sdaDerCuad neq = [1]++(take neq [0,0..])

{-SPLINES CUBICOS
NOTA: Este metodo tiene dos partes: Una que evalua un punto nuevo en el polinomio y da el resultado de la interpolacion y otra que 
entrega el polinomio interpolante escrito por medio de nuestra gramatica de funciones. Esta basado en la implementacion hecha por 
Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1.
-}

--Funcion que retorna los valores de los coeficientes
coefSpCub :: [Double] -> [Double] -> [(Integer,Double)]
coefSpCub x fx = eGaussPParcial (matrizSpCub x fx) (fromIntegral (length x -1)*4)

--Funcion que arma la matriz aumentada del sistema de ecuaciones
matrizSpCub :: [Double] -> [Double] -> Matriz
matrizSpCub x fx = leerMatrizAu (fromIntegral (length x -1)* 4) (sistmEcSpCub x fx)

--Funcion que une todas las ecuaciones del sistema en una lista sencilla. Basada en la practica referenciada
sistmEcSpCub :: [Double] -> [Double] -> [Double]
sistmEcSpCub x fx = (empalCub x fx [] 1 n neq)++(extCub x fx neq n)++(derCub x n neq [] 1)++(sdaDerCub x n neq [] 1)++(sdaDerExtCub x neq n)
                    where n = length x -1
                          neq  = (n)*4

--Funcion que halla las ecuaciones de empalme. Basada en la practica referenciada
empalCub :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> [Double]
empalCub x fx l i n neq
         | i<=(n-1) = empalCub x fx (l++(take ((i-1)*4) [0,0..])++[(x!!i)^3,(x!!i)^2,x!!i,1]++(take (neq-4*i) [0,0..])++[b1]++(take (4*i) [0,0..])++[(x!!i)^3,(x!!i)^2,x!!i,1]++(take (neq -4-(i)*4) [0,0..])++[b2]) (i+1) n neq
         | otherwise = l
         where b = empalB fx [] 1 n
               b1 = if (i==1) then (b!!(i-1)) else (b!!i)
               b2 = if (i==1) then (b!!i) else (b!!(i+1))

--Funcion que halla las ecuaciones de los extremos. Basada en la practica referenciada
extCub :: [Double] -> [Double] -> Int -> Int -> [Double]
extCub x fx neq n = ([(x!!0)^3,(x!!0)^2,x!!0,1]++(take zeros [0,0..])++[b!!0]++(take zeros [0,0..])++[(x!!n)^3,(x!!n)^2,x!!n,1]++[b!!1])
                    where zeros = div (neq*2-8) 2
                          b = extB fx n

--Funcion que halla las funciones de las derivadas en los nodos internos y las iguala. Basada en la practica referenciada
derCub :: [Double] -> Int -> Int -> [Double] -> Int -> [Double]
derCub x n neq l i
       | i<=(n-1) = derCub x n neq (l++(take ((i-1)*4) [0,0..])++[3*(x!!i)^2,2*(x!!i),1,0,-3*(x!!i)^2,-2*(x!!i),-1,0]++(take ((neq -4 -i*4)+1) [0,0..])) (i+1)
       | otherwise = l

--Funcion que halla las funciones de las segundas derivadas en los nodos internos y las iguala. Basada en la practica referenciada
sdaDerCub :: [Double] -> Int -> Int -> [Double] -> Int -> [Double]
sdaDerCub x n neq l i 
          | i<=(n-1) = sdaDerCub x n neq (l++(take ((i-1)*4) [0,0..])++[6*(x!!i),2,0,0,-6*(x!!i),-2,0,0]++(take ((neq -4 -i*4)+1) [0,0..])) (i+1)
          | otherwise = l

--Funcion que halla las segundas derivadas de los extremos y las iguala a cero. Basada en la practica referenciada
sdaDerExtCub :: [Double] -> Int -> Int -> [Double]
sdaDerExtCub x neq n = [6*(x!!0),2,0,0]++(take ((neq*2 -8)+1) [0,0..])++[6*(x!!n),2,0,0,0]

--Valores para prueba
x = [2.3,2.4,2.5,2.6]
fx = [17.997784,15.850776,14.140572,12.720153]
x2 :: [Double]
x2 = [-1,1,2,4]
fx2 :: [Double]
fx2 = [4,1,3,-2]
x3 = [3.2,3.4,3.6,3.8]
fx3 = [-6.421269,-7.88968,-9.411188,-10.990671]
x4 :: [Double]
x4 = [1,3,4,5]
fx4 = [3,1,3.5,2]