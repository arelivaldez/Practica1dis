--2. Hipotenusa de un triangulo rectángulo:
--La función hipotenusa debe recibir dos parámetros de tipo flotante b y h donde b representa
--la base y h la altura. La función debe devolver un valor de tipo flotante que represente el valor
--de la hipotenusa que se calcula respecto a la base y altura del triángulo rectángulo.
--NOTA: La hipotenusa de un triángulo rectángulo está definida como

hipotenusa :: (Float) -> (Float) -> Float
hipotenusa x y = sqrt ((x^2)+(y^2))

--4. Raíces de una ecuación cuadrática:
--La función raices debe recibir tres parámetros flotantes a, b y c que representan la ecuación

--ax2 + bx + c = 0

--La función debe devolver una tupla de dos elementos de tipo flotante que representan las raíces
--de una ecuación cuadrática.

raices :: Float -> Float -> Float -> (Float,Float)
raices a b c = (((-b + sqrt (b^2-4*a*c))/(2*a)),((-b - sqrt (b^2-4*a*c))/(2*a)))


--6. Función comparador:
--La función comparador recibe dos parámetros de tipo entero x y y. 
--Y la función devuelve un valor específico de tipo entero de acuerdo a los siguientes casos:
--6.1 0, si x es igual a y.
--6.2 -1, si x es menor a y.
--6.3 1, si x es mayor a y.

comparador :: Int -> Int -> Int 
comparador x y = if x == y 
                    then 0
                    else if x < y 
                        then -1
                        else 1

--8. Números ordenados de forma descendente:
--La función es Descendente recibe cuatro parámetros de tipo entero x, y, z y w. La función
--debe devolver una valor de tipo booleano de acuerdo a los siguientes casos:
--8.1. True, si los números fueron ingresados de manera descendente.
--8.2. False, si los números no fueron ingresados de manera descendente

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x > y && y > z && z > w
                                                then True
                                                else False
