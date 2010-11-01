module FuncionesAuxiliaresSE where
import GramaticaAbstracta
import Data.Array


{-FUNCIONES PARA LECTURA DE MATRICES-}

leerMatriz :: Integer -> [Double] -> Matriz
leerMatriz n elem = listArray ((1,1),(n,n)) elem

leerMatrizAu :: Integer -> [Double] -> Matriz
leerMatrizAu n elem = listArray ((1,1),(n,n+1)) elem

leerB :: Integer -> [Double] -> Matriz
leerB n elem = listArray ((1,1),(n,1)) elem

{-FUNCIONES AUXILIARES PARA PIVOTEOS -}

mayor ::Matriz -> Double
mayor m = mayor' (elems m)

mayor' :: [Double] -> Double
mayor' [] = 0
mayor' [a] = a
mayor' (x:xs)
       | abs x > abs (mayor' xs) = x
       | otherwise = mayor' xs

buscarMayorTotal :: Matriz -> ((Integer,Integer), Double)
buscarMayorTotal m = head(filter (\x -> (snd x == (mayor m))) (assocs m))

buscarMayorParcial :: Matriz -> Integer -> ((Integer,Integer),Double)
buscarMayorParcial m c = head (filter (\x -> (snd x == (mayor (darColumna m c )))) (assocs (darColumna m c )))

{-La funcion cambio de fila fue tomada del manual "una introduccion agradable a haskell", su funcionamiento se basa en el uso de la funcion //, la cual es una funcion de actualizacion para array, aclaramos que al ser una funcion que no utiliza monadas oviamente no modifica la matriz original, simplemente construye una matriz diferente-}

{-Tomado de la pagina => http://www.lcc.uma.es/~blas/pfHaskell/gentle/arrays.html-}
cambioFilas :: Matriz -> Integer -> Integer -> Matriz
cambioFilas a i i'= a // [assoc | j <- [jLo..jHi], assoc <- [((i ,j), a!(i',j)), ((i',j), a!(i, j))] ]
                   where ((iLo,jLo),(iHi,jHi)) = bounds a

{-La funcion cambio de columna es casi identica a la anterior su unico cambio es que actualizamos los indices de la columna-}
cambioColumnas :: Matriz -> Integer -> Integer -> Matriz
cambioColumnas a j j' = a // [assoc | i <- [iLo..iHi], assoc <- [((i,j), a!(i,j')) , ((i,j'), a!(i,j))] ]
                   where ((iLo,jLo),(iHi,jHi)) = bounds a

{-FUNCIONES AYUDA PARA GAUSIANA SIMPLE-}

{-Funcion que retorna una columna c de la matriz a, necesita que ingresen la matriz, la columna deseada y el numero de filas total-}
darColumna :: Matriz -> Integer -> Matriz
darColumna m c = listArray ((1,c),(i,c))(map (\y -> snd y)(filter (\x -> (snd (fst x)) == c ) (assocs m)))
                 where i = (fst(last(indices m)) )

{-Funciones que me dan la fila completa o la fila a operar dependiendo de la etapa-}
darFila :: Matriz -> Integer -> [((Integer, Integer),Double)]
darFila m f = filter (\x -> (fst(fst(x))) == f ) (assocs m)

{-Funcion que retorna la parte de la fila que esta debajo del elemento pivote-}
filaPivote :: Matriz -> [((Integer,Integer), Double)]
filaPivote m = filter (\x -> col x >= (0)) (darFila m 1)
               where col x = snd(fst(x))

{- FUNCIONES PARA HALLAR Y VERIFICAR LOS MULTIPLICADORES DE UNA MATRIZ -}

multsEtapa :: Matriz -> Integer -> [((Integer,Integer),Double)]
multsEtapa m k = filter (\x -> fst(fst x) > fst(fst (numDiagonal m k))) (f m k)
                where f m k = map (\x -> (fst x,((snd x)/(snd (numDiagonal m k))))) (tail(assocs(darColumna m k)))

numDiagonal :: Matriz -> Integer -> ((Integer,Integer), Double)
numDiagonal m k = head(filter(\x -> fst(fst(x)) == k) (f m))
                  where f m = filter (\x -> (fst(fst(x)))== (snd(fst(x)))) (assocs m)

multFila :: Matriz -> Integer -> Integer -> Double
multFila m k f = snd(head(filter (\x -> fst(fst x) == f) (multsEtapa m k)))

{- FUNCIONES DE ACTUALIZACION DE MATRICES -}

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

{-Funcion que da a cada nuevo valor su respectivo indice para ser ubicado en la matriz -}
indOper :: Matriz -> Matriz -> Integer -> Matriz
indOper a om k
        |(snd(snd(bounds a)) == fst (snd(bounds a))) = listArray (((k+1),k), (n,n)) (elems om)
        | otherwise = listArray (((k+1),k), ((n-1),n)) (elems om)
                 where n = snd(snd(bounds a))