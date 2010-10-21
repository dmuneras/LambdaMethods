module FuncionesAuxiliaresSE  where
import GramaticaAbstracta
import Data.Array


{-FUNCIONES PARA LECTURA DE MATRICES-}

leerMatriz :: Integer -> [Double] -> Matriz
leerMatriz n elem = listArray ((1,1),(n,n)) elem

leerMatrizAu :: Integer -> [Double] -> Matriz
leerMatrizAu n elem = listArray ((1,1),(n,n+1)) elem

leerB :: Integer -> [Double] -> Matriz
leerB n elem = listArray ((1,1),(1,n)) elem


{-FUNCIONES AUXILIARES PARA PIVOTEOS -}


mayor ::Matriz -> Double
mayor m = mayor' (elems m)

mayor' :: [Double] -> Double
mayor' [] = 0
mayor' [a] =  a
mayor' (x:xs) 
       | x > (mayor' xs) = x
       | otherwise = mayor' xs


buscarMayorTotal :: Matriz -> ((Integer,Integer), Double)
buscarMayorTotal m = head(filter (\x -> (snd x == (mayor m))) (assocs m))

buscarMayorParcial :: Matriz -> Integer -> ((Integer,Integer),Double)
buscarMayorParcial m  c = head (filter (\x -> (snd x == (mayor (darColumna m c )))) (assocs (darColumna m c )))



{-La funcion cambio de fila fue tomada del manual "una introduccion agradable a haskell", su funcionamiento se basa en el uso de la funcion //, la cual es una funcion de actualizacion para array, aclaramos que al ser una funcion que no utiliza monadas oviamente no modifica la matriz original, simplemente construye una matriz diferente-}

{-Tomado de la pagina => http://www.lcc.uma.es/~blas/pfHaskell/gentle/arrays.html-}
cambioFilas :: (Ix a, Ix b, Enum b) => Array (a,b) c -> a -> a -> Array (a,b) c
cambioFilas a i i'=  a // [assoc | j <- [jLo..jHi], assoc <- [((i ,j), a!(i',j)), ((i',j), a!(i, j))] ]
                   where ((iLo,jLo),(iHi,jHi)) = bounds a

{-La funcion cambio de columna es casi identica a la anterior su unico cambio es que actualizamos los indices de la columna-}
cambioColumnas :: (Ix a, Ix b, Enum b) =>  Array (b,a) c -> a -> a -> Array (b,a) c
cambioColumnas a j j'  = a // [assoc | i <- [iLo..iHi], assoc <- [((i,j), a!(i,j')) , ((i,j'), a!(i,j))] ]
                   where ((iLo,jLo),(iHi,jHi)) = bounds a


{-FUNCIONES AYUDA PARA GAUSIANA SIMPLE-}

{-Funcion que retorna una columna de la c matriz a, necesita que ingresen la matriz, la columna deseada y el numero de filas total de la fila-}
darColumna :: Matriz -> Integer -> Matriz
darColumna m c = listArray ((1,c),(i,c))(map (\y -> snd y)(filter (\x -> (snd (fst x)) == c ) (assocs m)))
                 where i = (fst(last(indices m)) )

{-Funciones que me dan la fila completa o la fila a operar dependiendo de la etapa-}
darFila :: Matriz -> Integer -> [((Integer, Integer),Double)]
darFila m f  =  filter (\x -> (fst(fst(x))) == f ) (assocs m)

--numsFila m f = map (\x -> snd x)(darFila m f)

operFila :: Matriz -> Integer -> [((Integer,Integer), Double)]
operFila m f = filter (\x -> snd(fst(x)) >= (f-1)) (darFila m f)

{- FUNCIONES PARA HALLAR Y VERIFICAR LOS MULTIPLICADORES DE UNA MATRIZ -}

multsEtapa :: Matriz -> Integer -> [((Integer,Integer),Double)]
multsEtapa m k = filter (\x -> fst(fst x) > fst(fst (numDiagonal m k))) (f  m k) 
                where f m k = map (\x -> (fst x,((snd x)/(snd (numDiagonal m k))))) (tail(assocs(darColumna m k)))

numDiagonal :: Matriz -> Integer -> ((Integer,Integer), Double)
numDiagonal m k = head(filter(\x -> fst(fst(x)) == k) (f  m))
                  where f m =  filter (\x -> (fst(fst(x)))== (snd(fst(x)))) (assocs m)

multFila :: Matriz -> Integer -> Integer -> Double
multFila m k f =  snd(head(filter (\x -> fst(fst x) == f) (multsEtapa m k)))