module SistemasEcuaciones where

import GramaticaAbstracta
import Data.Array
import Data.List
import FuncionesAuxiliaresSE


{- ELIMINACION GAUSIANA-}

{-Funcion que entrega la matriz 'a' con la eliminacion gaussiana simple aplicada en la etapa k (ultima)
-}
matrizEGaussSim :: Matriz -> Integer ->Matriz
matrizEGaussSim a n = etapak a n (n-1)

{-Funcion que calcula la matriz 'a' en la etapa pasada como parametro aplicando eliminacion gaussiana simple
-}
etapak :: Matriz -> Integer -> Integer -> Matriz
etapak a n 1 = actualizar a (indOper a (submatriz (operm a 1) (n-1)) 1)
etapak a n k = actualizar ant (indOper ant (submatriz (operm ant k) (n-k) )k )
               where ant = etapak a n (k-1)

{-Funcion transforma la lista de nuevas filas para la etapa k arrojadas en nuevasFilas y las transforma en una matriz que a su vez sera agregada en la matriz principal debajo de la fila pivote
-}
submatriz :: Matriz -> Integer -> Matriz
submatriz a i = listArray ((1,1),(i,n)) (map (\x -> snd x) (nuevasFilas a i))
                where n = snd(snd(bounds(a)))

{-Funcion que reune todas las nuevas filas en una etapa k
-}
nuevasFilas :: Matriz -> Integer -> [((Integer,Integer),Double)]
nuevasFilas a 1 = nuevaFila a 1
nuevasFilas a i = (nuevasFilas a (i-1)) ++ (nuevaFila a i)

{-Funcion que calcula una nueva fila en una etapa k
-}
nuevaFila :: Matriz -> Integer -> [((Integer,Integer),Double)]
nuevaFila a i = (map (\x -> (((fst x), ((snd x) - (f x * mk))))) (darFila a (i+1)))
    where mk = multFila a 1 (i+1)
          f x = ( snd (head (filter (\y -> snd (fst(y)) == snd (fst x)) (filaPivote a))))

{-Funcion que devuelve la submatriz que sera procesada dependiendo de la etapa k en la que va el proceso
-}
operm :: Matriz -> Integer -> Matriz
operm a k
      | (snd(snd(bounds a)) == fst (snd(bounds a))) = leerMatriz (n-(k-1)) ar
      | otherwise = leerMatrizAu (n-k) ar
            where n = (snd(snd(bounds a)))
                  ar = (map (\x -> snd x)(filter (\x -> (snd(fst(x)) >= k && (fst(fst(x))) >= k)) (assocs a)))

{-PIVOTEO TOTAL-}

matrizEGaussTotal :: Matriz -> Integer -> Matriz
matrizEGaussTotal a n = etapakTotal a n (n-1)
 
pivoteoTotal :: Matriz -> Integer -> Matriz 
pivoteoTotal om i = cambioFilas (cambioColumnas om i (snd(fst(buscarMayorTotal om i)))) i f 
                    where f = fst(fst(buscarMayorTotal om i)) 

buscarMayorTotal :: Matriz -> Integer -> ((Integer,Integer), Double)
buscarMayorTotal m c
                 | snd(snd(bounds m)) == n = head(filter (\x -> (snd x == (mayor m))) (assocs m))
                 | otherwise = head(filter (\x -> (snd x == (mayor2 (f (assocs a))))) (f (assocs a)))
                               where a = m // [((i,(n+1)),0)|i <- [1..n]]
                                     n = fst(snd(bounds m))
                                     f l = filter (\x -> fst(fst(x)) >= c) l


{-Funcion que calcula la matriz 'a' en la etapa pasada como parametro aplicando eliminacion gaussiana con pivoteo total-}
etapakTotal :: Matriz -> Integer -> Integer -> Matriz
etapakTotal a n k
              | (k == 1) = actualizar ap (indOper ap (submatriz (operm ap 1) (n-1)) 1)
              | (k > 1) = actualizar ant (indOper ant (submatriz (operm ant k) (n-k) )k )
              | (otherwise) = error "Fuera de rango, valor negativo"
              where ap = pivoteoTotal a k
                    ant = pivoteoTotal (etapakTotal a n (k-1)) k
{- PIVOTEO PARCIAL-}

{-Funcion que entrega la matriz 'a' con la eliminacion gaussiana con pivoteo parcial aplicada en la etapa k (ultima)
-}
matrizEGaussParcial :: Matriz -> Integer ->Matriz
matrizEGaussParcial a n = etapakParcial a n (n-1)

{-Funcion que realiza el pivoteo parcial en la submatriz que se esta operando
-}
pivoteoParcial :: Matriz -> Integer -> Matriz
pivoteoParcial om i = cambioFilas om i (filaMayor (buscarMayorParcial om i))
                    where filaMayor x = fst(fst x)

buscarMayorParcial :: Matriz -> Integer -> ((Integer,Integer),Double)
buscarMayorParcial m c = head (filter (\x -> (snd x == (mayor2 (f (assocs(darColumna m c )))))) (f (assocs (darColumna m c ))) )
                         where f l = filter (\x -> fst(fst(x)) >= c) l
                               


{-Funcion que calcula la matriz 'a' en la etapa pasada como parametro aplicando eliminacion gaussiana con pivoteo parcial
-}
etapakParcial :: Matriz -> Integer -> Integer -> Matriz
etapakParcial a n k
              | (k == 1) = actualizar ap (indOper ap  (submatriz (operm ap 1) (n-1)) 1)
              | (k > 1) = actualizar  ant (indOper ant (submatriz (operm ant k) (n-k))k )
              | (otherwise) = error "Fuera de rango, valor negativo"
              where ap = pivoteoParcial a k
                    ant = pivoteoParcial (etapakParcial a n (k-1)) k

{-SUSTITUCIONES-}

{-Funcion que realiza sustitucion regresiva sobre una matriz (aumentada) triangular inferior
-}
sustReg :: Matriz -> Integer -> Integer -> [(Integer,Double)]
sustReg a n k 
    | k == 0 = []
    | k == n = [(k,a!(n,(n+1)) / a!(n,n))]
    | otherwise = [(k,((a!(k,(n+1)) - suma (darFila a k))) / a!(k,k))] ++ sustReg a n (k+1)
    where suma l = sum (map (\y -> mult y  (sustReg a n (k+1))) (filter (\x -> snd x /= 0.0 && col x /= k)(tail (reverse l))))
          col x = snd (fst x)

{-Funcion que realiza sustitucion progresiva sobre una matriz (aumentada) triangular superior
-}
sustProg :: Matriz -> Integer -> Integer -> [(Integer, Double)]
sustProg a n k
    | k == n+1 = []
    | k == 1   = [(k,a!(k,(n+1)) / a!(k,k))]
    | otherwise = [(k, ((a!(k,(n+1)) - suma (darFila a k))) / a!(k,k))] ++ sustProg a n (k-1)
    where suma l = sum (map (\y -> mult y (sustProg a n (k-1))) (filter (\x -> snd x /= 0.0 && col x /= k) (tail (reverse l))))
          col x = snd (fst x)

{-Funcion que multiplica un coeficiente dado con las variable asociada al mismo en una lista de variables despejadas
-}
mult :: ((Integer,Integer),Double) -> [(Integer,Double)] -> Double
mult a l = snd a * snd (head (filter(\x -> fst x == col a) l))
           where col y = snd(fst y)

{-METODOS-}

--Eliminacion Gaussiana Simple
eGaussSim :: Matriz -> Integer -> [(Integer,Double)]
eGaussSim au n = sustReg (matrizEGaussSim au n) n 1

--Eliminacion Gaussiana con Pivoteo Parcial
eGaussPParcial :: Matriz -> Integer -> [(Integer,Double)]
eGaussPParcial au n = sustReg (matrizEGaussParcial au n) n 1

eGaussPTotal :: Matriz -> Integer -> [(Integer,Double)]
eGaussPTotal au n = sustReg (matrizEGaussTotal au n) n 1

{-METODOS ITERATIVOS-}
--Jacobi: Funcion que recibe los parametros iniciales y llama al ciclo prinicpal
jacobi :: Matriz -> [(Integer,Double)] -> Matriz -> Integer -> Double -> Double -> Integer -> [(Integer,Double)]
jacobi a x0 b n lam tol iter = jacobi' a x0 b n lam (tol+1) tol iter

--Funcion que aplica el metodo
jacobi' :: Matriz -> [(Integer,Double)] -> Matriz -> Integer -> Double -> Double -> Double -> Integer -> [(Integer,Double)]
jacobi' a x0 b n lam  e tol i
        | (e > tol && i > 0) = jacobi' a x1 b n lam err tol (i-1)
        | (e <= tol) = x0
        | (otherwise) = error "El metodo no converge en las iteraciones dadas"
        where x1 = x1Jacobi a x0 b lam n n
              err = normMax x1 x0

--Funcion que halla los nuevos valores para el metodo de Jacobi
x1Jacobi :: Matriz -> [(Integer,Double)] -> Matriz -> Double -> Integer -> Integer -> [(Integer,Double)]
x1Jacobi a x0 b lam n i
         | (i == 1) = [(1,(lam) * (((b!(1,1)) - suma2) / (a!(1,1))) + (1-lam) * (snd (head x0)))]
         | (i > 1)  = [(i,(lam) * (((b!(i,1)) - suma1 - suma2) / (a!(i,i))) + (1-lam) * (snd (head ant)))] ++ (x1Jacobi a x0 b lam n (i-1))
         | (otherwise) = error "Fuera de rango, valor negativo"
         where suma1 = sum (map (\x -> mult x x0) (filter (\y -> col y <= (i-1)) (darFila a i)))
               suma2 = sum (map (\x -> mult x x0) (filter (\y -> col y >= (i+1)) (darFila a i)))
               col e = snd (fst e)
               ant = filter (\x -> fst x == i) x0

--Gauss Seidel: Funcion que recibe los parametros iniciales y llama al ciclo principal
gaussSeidel :: Matriz -> [(Integer,Double)] -> Matriz -> Integer -> Double -> Double -> Integer -> [(Integer,Double)]
gaussSeidel a x0 b n lam tol iter = gaussSeidel' a x0 b n lam (tol+1) tol iter

--Funcion que aplica el metodo
gaussSeidel' :: Matriz -> [(Integer,Double)] -> Matriz -> Integer -> Double -> Double -> Double -> Integer -> [(Integer,Double)]
gaussSeidel' a x0 b n lam e tol i
             | (e > tol && i > 0) = gaussSeidel' a x1 b n lam err tol (i-1)
             | (e <= tol) = x0
             | (otherwise) = error "El metodo no converge en las iteraciones dadas"
             where x1 = x1GaussS a x0 b lam n n
                   err = normMax x1 x0

--Funcion que halla los nuevos valores para el metodo de Gauss Seidel
x1GaussS :: Matriz -> [(Integer,Double)] -> Matriz -> Double -> Integer -> Integer -> [(Integer,Double)]
x1GaussS a x0 b lam n i
         | (i == 1) = [(1,(lam) * (((b!(1,1)) - suma2) / (a!(1,1))) + (1-lam) * (snd (head x0)))]
         | (i > 1)  = [(i,(lam) * (((b!(i,1)) - suma1 - suma2) / (a!(i,i))) + (1-lam) * (snd (head ant)))] ++ (x1GaussS a x0 b lam n (i-1))
         | (otherwise) = error "Fuera de rango, valor negativo"
         where suma1 = sum (map (\x -> mult x x1) (filter (\y -> col y <= (i-1)) (darFila a i)))
               suma2 = sum (map (\x -> mult x x0) (filter (\y -> col y >= (i+1)) (darFila a i)))
               col e = snd (fst e)
               x1 = x1GaussS a x0 b lam n (i-1)
               ant = filter (\x -> fst x == i) x0

--Funcion que aplica la Norma Maxima a dos vectores (actual y anterior) de valores
normMax :: [(Integer, Double)] -> [(Integer,Double)] -> Double
normMax x1 x0 = maximum (toListSim (map (\x -> res x x0) x1)) / maximum (map (\x -> abs x) (toListSim x1))

--Funcion que resta un x actual con su respectivo x anterior
res :: (Integer,Double) -> [(Integer,Double)] -> (Integer,Double)
res x1 x0 = (1,abs (snd x1 - snd (head (filter (\x -> (fst x) == (fst x1)) x0))))

--Funcion que transforma una lista de valores con indices en una lista de valores
toListSim :: [(Integer,Double)] -> [Double]
toListSim [x] = [snd x]
toListSim (x:xs) = [snd x] ++ toListSim xs

{-FUNCIONES MULTIPLICACION DE MATRICES-}

identidad :: Integer -> Matriz 
identidad n = (leerMatriz n (ceros (n*n))) // [((i,i),1)|i <- [1..n]] 

matrizNula :: Integer -> Matriz
matrizNula n = leerMatriz n (ceros (n*n)) 

ceros :: Integer -> [Double]
ceros 0 = []
ceros 1 = [0]
ceros n = (ceros (n-1)) ++ [0]
 

matMult:: (Ix a, Ix b, Ix c, Num d) => Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y = accumArray (+) 0 resultBounds [((i,j), x!(i,k) * y!(k,j)) | i <- range (li,ui), j <- range (lj',uj') ,
                                                                               k <- range (lj,uj)  ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: límites incompatibles"
  

{-PARA PRUEBAS-}
m1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]
m2 = leerMatriz 4 [20,1,3,2,4,60,-3,-7,1,2,50,7,-2,-7,4,18]
m3 = leerMatriz 3 [10,2,5,3,12,2,-4,-5,15]
m4 = leerMatriz 4 [2,-3,10,-7,3,12,-16,16,14,-18,40,-7,16,-8,-50,6]
m4au = leerMatrizAu 4 [2,-3,10,-7,20, 3,12,-16,16,30 ,14,-18,40,-7, 25 ,16,-8,-50,6,-18]
m5 = leerMatriz 4 [-7,2,-3,4,5,-1,14,-1,1,9,-7,5,-12,13,-8,-4]
m2au = leerMatrizAu 4 [20,1,3,2,1, 4,60,-3,-7,1, 1,2,50,7,1, -2,-7,4,18,1]
l1 = leerMatrizAu 4 [1,0,0,0,0,0.28,1,0,0,0,0.38,0.27,1,0,0,(-0.14),(-0.17),0.27,1,1]
l2 = leerMatrizAu 3 [1,0,0,9,1.25,1,0,7,0.25,0.7142857143,1,12]
--Para probar metodos iterativos
iter1 = leerMatriz 3 [-17,-2,8,5,-12,-1,-3,7,-16]
biter1 = leerB 3 [38,43,56]
x01 = [(1,(-2.23)),(2,(-3.58)),(3,(-3.5))]
iter2 = leerMatriz 3 [4,-2,1,3,-7,4,5,-6,10]
biter2 = leerB 3 [8,12,8]
x02 = [(1,1.8),(2,(-1.54)),(3,0.72)]
iter3 = leerMatriz 4 [31,-2,7,-8,3,21,-6,-2,5,-2,16,-4,7,-5,4,-38]
biter3 = leerB 4 [20,-50,16,-16]
x03 = [(1,0.64),(2,(-2.47)),(3,0.48),(4,0.91)]

mm1 = leerMatriz 2 [1,2,3,4]
mm2 = leerMatriz 2 [2,3,4,5]
mparcial = leerMatrizAu 3 [5,-12,-1,43,-3,7,-16,56,-17,-2,8,38]
mparcial' = leerMatrizAu 3 [2,-9,-1,-29, -3,-1,13,-54,-12,1,-2,-80]

matrizPrueba = array ((1,1),(9,10)) [((1,1),9.0),((1,2),3.0),((1,3),1.0),((1,4),0.0),((1,5),0.0),((1,6),0.0),((1,7),0.0),((1,8),0.0),((1,9),0.0),((1,10),1.0),((2,1),0.0),((2,2),0.0),((2,3),0.0),((2,4),9.0),((2,5),3.0),((2,6),1.0),((2,7),0.0),((2,8),0.0),((2,9),0.0),((2,10),1.0),((3,1),0.0),((3,2),0.0),((3,3),0.0),((3,4),16.0),((3,5),4.0),((3,6),1.0),((3,7),0.0),((3,8),0.0),((3,9),0.0),((3,10),3.5),((4,1),0.0),((4,2),0.0),((4,3),0.0),((4,4),0.0),((4,5),0.0),((4,6),0.0),((4,7),16.0),((4,8),4.0),((4,9),1.0),((4,10),3.5),((5,1),1.0),((5,2),1.0),((5,3),1.0),((5,4),0.0),((5,5),0.0),((5,6),0.0),((5,7),0.0),((5,8),0.0),((5,9),0.0),((5,10),3.0),((6,1),0.0),((6,2),0.0),((6,3),0.0),((6,4),0.0),((6,5),0.0),((6,6),0.0),((6,7),25.0),((6,8),5.0),((6,9),1.0),((6,10),2.0),((7,1),6.0),((7,2),1.0),((7,3),0.0),((7,4),-6.0),((7,5),-1.0),((7,6),0.0),((7,7),0.0),((7,8),0.0),((7,9),0.0),((7,10),0.0),((8,1),0.0),((8,2),0.0),((8,3),0.0),((8,4),8.0),((8,5),1.0),((8,6),0.0),((8,7),-8.0),((8,8),-1.0),((8,9),0.0),((8,10),0.0),((9,1),1.0),((9,2),0.0),((9,3),0.0),((9,4),0.0),((9,5),0.0),((9,6),0.0),((9,7),0.0),((9,8),0.0),((9,9),0.0),((9,10),0.0)]


cinco = leerMatrizAu 5 [3,-4,7,-16,14,20,7,21,17,4,-5,12,3,-6,23,-17,19,36,16,-37,41,-5,8,-50,7,-16,4,-9,-30,19]

simpleCinco = leerMatrizAu 5 [10,5,5,5,5,5,8,20,5,5,5,5,9,9,20,5,5,5,7,5,4,11,4,4,1,1,1,1,1,1]