module SistemasEcuaciones where
import GramaticaAbstracta
import Data.Array

{-FUNCIONES PARA LECTURA DE MATRICES-}

leerMatriz :: Integer -> [Double] -> Matriz
leerMatriz n elem = listArray ((1,1),(n,n)) elem

leerMatrizAu :: Integer -> [Double] -> Matriz
leerMatrizAu n elem = listArray ((1,1),(n+1,n)) elem


{-FUNCIONES AUXILIARES PARA PIVOTEOS -}

{-tomado de http://ronnyml.wordpress.com/category/haskell/-}

quickSort::Ord a=>[a]->[a]
quickSort [] = []
quickSort (x:xs) = quickSort(menores) ++ [x] ++ quickSort(mayores)
   where
      menores = [y | y <-xs, y < x]
      mayores = [z | z <-xs, z >= x] 


{- Funcion que retorna el valor del mayor -}

mayor ::Matriz -> Double
mayor m = last(quickSort (elems m))


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
darColumna m c = listArray ((1,c),((fst(last(indices m)) ),c))(map (\y -> snd y)(filter (\x -> (snd (fst x)) == c ) (assocs m)))

{-Funciones que me dan la fila completa o la fila a operar dependiendo de la etapa-}
darFila m f  =  filter (\x -> (fst(fst(x))) == f ) (assocs m)
numsFila m f = map (\x -> snd x)(darFila m f)

operFila :: Matriz -> Integer -> [((Integer,Integer), Double)]
operFila m f = filter (\x -> snd(fst(x)) >= (f-1)) (darFila m f)

{- FUNCIONES PARA HALLAR LOS MULTIPLICADORES DE UNA MATRIZ -}

multsEtapa :: Matriz -> Integer -> [((Integer,Integer),Double)]
multsEtapa m k = filter (\x -> fst(fst x) > fst(fst (numDiagonal m k))) (f  m k) 
                where f m k = map (\x -> (fst x,((snd x)/(snd (numDiagonal m k))))) (tail(assocs(darColumna m k)))

multFila :: Matriz -> Integer -> Integer -> Double
multFila m k f =  snd(head(filter (\x -> fst(fst x) == f) (multsEtapa m k)))

numDiagonal :: Matriz -> Integer -> ((Integer,Integer), Double)
numDiagonal m k = head(filter(\x -> fst(fst(x)) == k) (f  m))
                  where f m =  filter (\x -> (fst(fst(x)))== (snd(fst(x)))) (assocs m)

{- ELIMINACION GAUSIANA-}

etapak :: Matriz -> Integer -> Integer -> Matriz
etapak a n 1 = actualizar a (indOper a (etapaj (operm a 1) (n-1))1)
etapak a n k = actualizar (etapak a n (k-1)) (indOper (etapak a n (k-1)) (etapaj (operm (etapak a n (k-1)) k) (n-k) )k )


etapaj'' :: Matriz -> Integer -> [((Integer,Integer),Double)]
etapaj'' a j = (map (\x -> (((fst x), ((snd x) - (f x * mk))))) (darFila a (j+1)))
    where mk = multFila a 1 (j+1)
          f x = ( snd (head (filter (\y -> snd (fst(y)) == snd (fst x)) (operFila a 1))))

etapaj' :: Matriz -> Integer -> [((Integer,Integer),Double)]
etapaj' a 1  = etapaj'' a 1 
etapaj' a j = (etapaj' a (j-1)) ++ (etapaj'' a j)

etapaj :: Matriz -> Integer -> Matriz 
etapaj a j = listArray ((1,1),((j+1),(j))) (map (\x -> snd x) (etapaj' a j))


operm :: Matriz -> Integer -> Matriz
operm a 1 =  a
operm a k = leerMatriz  (snd(snd(bounds a))-1) (map (\x -> snd x)(filter (\x -> (snd(fst(x)) >= k && (fst(fst(x))) >= k)) (assocs a)))

indOper :: Matriz -> Matriz -> Integer -> Matriz
indOper a om k = listArray (((k+1),k), (n,n)) (elems om)
                 where n = snd(snd(bounds a))


actualizar :: Matriz -> Matriz -> Matriz
actualizar a e = leerMatriz n (map (\x -> snd x)(map (\x -> (act' x (assocs e)) ) (assocs a)))
                  where n = snd(snd(bounds a))

act' :: ((Integer,Integer),Double) -> [((Integer,Integer),Double)] -> ((Integer,Integer),Double) 
act' b e 
     | (filter (\x -> (fst x) == (fst b))e) /= [] = head(filter (\x -> (fst x) == (fst b))e)
     | otherwise = b
 

{-PARA PRUEBAS-}
m1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]
m2 = leerMatriz 4 [20,1,3,2,4,60,-3,-7,1,2,50,7,-2,-7,4,18]
m3 = leerMatriz 3 [10,2,5,3,12,2,-4,-5,15]
prueba = leerMatriz 3 [59.8,-3.6,-7.4,1.95,49.85,6.9,-6.9,4.3,18.2]
--m21 = matrizEtapaj m2 3