module SistemasEcuaciones where
import GramaticaAbstracta
import Data.Array
import FuncionesAuxiliaresSE


{- ELIMINACION GAUSIANA-}

matrizEGaussSim :: Matriz -> Integer ->Matriz
matrizEGaussSim a n = etapak a n (n-1)

etapak :: Matriz -> Integer -> Integer -> Matriz
etapak a n 1 = actualizar a (indOper a (submatriz (operm a 1) (n-1)) 1)
etapak a n k = actualizar (etapak a n (k-1)) (indOper (etapak a n (k-1)) (submatriz (operm (etapak a n (k-1)) k) (n-k) )k )

{-Funcion que se encarga de las transformaciones de fila en cada etapa k, utiliza la funcion etapai' que a su vez utiliza la funcion etapai''-}

submatriz :: Matriz -> Integer -> Matriz
submatriz a i = listArray ((1,1),(i,(i+2))) (map (\x -> snd x) (nuevasFilas a i))

nuevasFilas :: Matriz -> Integer -> [((Integer,Integer),Double)]
nuevasFilas a 1 = nuevaFila a 1
nuevasFilas a i = (nuevasFilas a (i-1)) ++ (nuevaFila a i)

nuevaFila :: Matriz -> Integer -> [((Integer,Integer),Double)]
nuevaFila a i = (map (\x -> (((fst x), ((snd x) - (f x * mk))))) (darFila a (i+1)))
    where mk = multFila a 1 (i+1)
          f x = ( snd (head (filter (\y -> snd (fst(y)) == snd (fst x)) (filaPivote a))))

{-FUNCIONES DE ACTUALIZACION DE MATRIZ-}

{-Funcion que devuelve la submatriz que sera procesada dependiendo de la etapa k en la que va el proceso-}
operm :: Matriz -> Integer -> Matriz
operm a k
      | (snd(snd(bounds a)) == fst (snd(bounds a))) = leerMatriz (n-(k-1)) ar
      | otherwise = leerMatrizAu (n-k) ar
            where n = (snd(snd(bounds a)))
                  ar = (map (\x -> snd x)(filter (\x -> (snd(fst(x)) >= k && (fst(fst(x))) >= k)) (assocs a)))

{-Funcion que da a cada nuevo valor su respectivo indice para ser ubicado en la matriz -}
indOper :: Matriz -> Matriz -> Integer -> Matriz
indOper a om k
        |(snd(snd(bounds a)) == fst (snd(bounds a))) = listArray (((k+1),k), (n,n)) (elems om)
        | otherwise = listArray (((k+1),k), ((n-1),n)) (elems om)
                 where n = snd(snd(bounds a))

{-Funcion que actualiza la matriz con sus nuevos valores -}
actualizar :: Matriz -> Matriz -> Matriz
actualizar a e
           | (snd(snd(bounds a)) == fst (snd(bounds a))) = leerMatriz n ar
           | otherwise = leerMatrizAu (n-1) ar
                where n = snd(snd(bounds a))
                      ar = (map (\x -> snd x)(map (\x -> (act' x (assocs e))) (assocs a)))

{-Funcion auxiliar que se encarga de sustituir un valor viejo por uno nuevo en la matriz -}
act' :: ((Integer,Integer),Double) -> [((Integer,Integer),Double)] -> ((Integer,Integer),Double)
act' b e
     | (filter (\x -> (fst x) == (fst b))e) /= [] = head(filter (\x -> (fst x) == (fst b))e)
     | otherwise = b



{- PIVOTEO PARCIAL-}

matrizEGaussParcial :: Matriz -> Integer ->Matriz
matrizEGaussParcial a n = etapakParcial a n (n-1)

pivoteoParcial :: Matriz -> Matriz 
pivoteoParcial om = cambioFilas om 1 (filaMayor (buscarMayorParcial om 1))
                    where filaMayor x = fst(fst x)

etapakParcial :: Matriz -> Integer -> Integer -> Matriz
etapakParcial a n 1 = actualizar ap (indOper ap (submatriz (operm ap 1) (n-1)) 1)
                      where ap = pivoteoParcial a
etapakParcial a n k = actualizar (etapakParcial ap n (k-1)) (indOper (etapakParcial ap n (k-1)) (submatriz (operm (etapakParcial ap n (k-1)) k) (n-k) )k )
                      where ap = pivoteoParcial a
{-SUSTITUCIONES-}

sustReg :: Matriz -> Integer -> Integer -> [(Integer,Double)]
sustReg a n k 
    | k == 0 = []
    | k == n = [(k,a!(n,(n+1)) / a!(n,n))]
    | otherwise = [(k,(a!(k,(n+1)) - suma (darFila a k)) / a!(k,k))] ++ sustReg a n (k+1)
    where suma l = suma' (map (\y -> mult y  (sustReg a n (k+1))) (filter (\x -> snd x /= 0.0 && col x /= k)(tail (reverse l))))
          col x = snd (fst x)

mult :: ((Integer,Integer),Double) -> [(Integer,Double)] -> Double
mult a l = snd a * snd (head (filter(\x -> fst x == col a) l))
           where col y = snd(fst y)

suma' [] = 0
suma' [x] = x
suma' (x:xs) = x + suma' xs

{-Metodos-}
eGaussSim :: Matriz -> Integer -> [(Integer,Double)]
eGaussSim au n = sustReg (matrizEGaussSim au n) n 1

eGaussPParcial :: Matriz -> Integer -> [(Integer,Double)]
eGaussPParcial au n = sustReg (matrizEGaussParcial au n) n 1

{-PARA PRUEBAS-}
m1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]
m2 = leerMatriz 4 [20,1,3,2,4,60,-3,-7,1,2,50,7,-2,-7,4,18]
m3 = leerMatriz 3 [10,2,5,3,12,2,-4,-5,15]
m4 = leerMatriz 4 [2,-3,10,-7,3,12,-16,16,14,-18,40,-7,16,-8,-50,6]
m4au = leerMatrizAu 4 [2,-3,10,-7,20, 3,12,-16,16,30 ,14,-18,40,-7, 25 ,16,-8,-50,6,-18]
m5 = leerMatriz 4 [-7,2,-3,4,5,-1,14,-1,1,9,-7,5,-12,13,-8,-4]
m2au = leerMatrizAu 4 [20,1,3,2,1, 4,60,-3,-7,1, 1,2,50,7,1, -2,-7,4,18,1]

