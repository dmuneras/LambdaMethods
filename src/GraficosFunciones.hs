module GraficosFunciones where 

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )
import qualified Data.Time as Time 
import Semantica
import GramaticaAbstracta
import EcuacionesNoLineales

 

{- Función que grafica una funcion en un plano xy 
   FUNCIONAMIENTO: La función de graficacion Plot2D es una función que recibe una lista de valores para sustituir en
                   el segundo parametro, que consiste en una función de tipo a -> a, en nuestro caso es una función 
                   Double -> Double.
-}

graficaXY :: Func -> IO () 
graficaXY f = do 
                Plot.plot (X11.cons) (Plot2D.function Graph2D.lines (linearScale 500 (-10,10)) (\x -> funcGraf f  x))
                return ()
                       
   

{- Función de ayuda para cumplir con el tipo de parametro de la función a gráficar. -}
funcGraf :: Func -> Double -> Double
funcGraf f x = sacarNum (reduccion (sust f ('x', (FConst x))))
