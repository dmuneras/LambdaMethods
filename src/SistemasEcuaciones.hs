module SistemasEcuaciones where
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

{-Funcion que devuelve la submatriz que sera procesada dependiendo de la etapa k en la que va el proceso-}
operm :: Matriz -> Integer -> Matriz
operm a k = leerMatrizAu (n-k) (map (\x -> snd x)(filter (\x -> (snd(fst(x)) >= k && (fst(fst(x))) >= k)) (assocs a)))
            where n = (snd(snd(bounds a)))

{-Funcion que da a cada nuevo valor su respectivo indice para ser ubicado en la matriz A-}
indOper :: Matriz -> Matriz -> Integer -> Matriz
indOper a om k = listArray (((k+1),k), ((n-1),n)) (elems om)
                 where n = snd(snd(bounds a))

{-Funcion que actualiza la matriz A con sus nuevos valores -}
actualizar :: Matriz -> Matriz -> Matriz
actualizar a e = leerMatrizAu n (map (\x -> snd x)(map (\x -> (act' x (assocs e)) ) (assocs a)))
                  where n = fst(snd(bounds a))

{-Funcion auxiliar que se encarga de sustituir un valor viejo por uno nuevo en la matriz A -}
act' :: ((Integer,Integer),Double) -> [((Integer,Integer),Double)] -> ((Integer,Integer),Double) 
act' b e 
     | (filter (\x -> (fst x) == (fst b))e) /= [] = head(filter (\x -> (fst x) == (fst b))e)
     | otherwise = b


{- ELIMINACION GAUSIANA-}

eliminacionGaussiana :: Matriz -> Integer -> Matriz
eliminacionGaussiana a n = etapak a n (n-1)

etapak :: Matriz -> Integer -> Integer -> Matriz
etapak a n 1 = actualizar a (indOper a (etapaj (operm a 1) (n-1))1)
etapak a n k = actualizar (etapak a n (k-1)) (indOper (etapak a n (k-1)) (etapaj (operm (etapak a n (k-1)) k) (n-k) )k )

{-Funcion que se encarga de las transformaciones de fila en cada etapa k, utiliza la funcion etapaj' que a su vez utiliza la funcion etapaj''-}
etapaj :: Matriz -> Integer -> Matriz 
etapaj a j = listArray ((1,1),(j,(j+2))) (map (\x -> snd x) (etapaj' a j))

etapaj' :: Matriz -> Integer -> [((Integer,Integer),Double)]
etapaj' a 1  = etapaj'' a 1 
etapaj' a j = (etapaj' a (j-1)) ++ (etapaj'' a j)

etapaj'' :: Matriz -> Integer -> [((Integer,Integer),Double)]
etapaj'' a j = (map (\x -> (((fst x), ((snd x) - (f x * mk))))) (darFila a (j+1)))
    where mk = multFila a 1 (j+1)
          f x = ( snd (head (filter (\y -> snd (fst(y)) == snd (fst x)) (operFila a 1))))


{-SUSTITUCIONES-}

xn :: Matriz -> Integer -> Integer ->  Double
xn a n k 
       | k == n = (eliminacionGaussiana a n)!(n,(n+1)) /(eliminacionGaussiana a n)!(n,n)
       | otherwise = (m!(k,k) - m!(k,k+1)*(xn a n (k+1)))/ m!(k,k)
                     where m = eliminacionGaussiana a n
                   

{-PARA PRUEBAS-}
m1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]
m2 = leerMatriz 4 [20,1,3,2,4,60,-3,-7,1,2,50,7,-2,-7,4,18]
m3 = leerMatriz 3 [10,2,5,3,12,2,-4,-5,15]
m4 = leerMatriz 4 [2,-3,10,-7,3,12,-16,16,14,-18,40,-7,16,-8,-50,6]
m4au =  leerMatrizAu 4 [2,-3,10,-7,20,   3,12,-16,16,30  ,14,-18,40,-7, 25 ,16,-8,-50,6,-18]
m5 = leerMatriz 4 [-7,2,-3,4,5,-1,14,-1,1,9,-7,5,-12,13,-8,-4]
m2au =  leerMatrizAu 4 [20,1,3,2,1,  4,60,-3,-7,1,  1,2,50,7,1, -2,-7,4,18,1]



