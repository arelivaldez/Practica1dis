--1. Distancia entre dos puntos en el plano cartesiano:
--La funcion distanciaPuntos debe recibir dos parametros que seran tuplas de dos elementos de
--tipo flotante respectivamente, es decir, (x1, y1) y (x2 y y2). La funcion debe devolver un valor
--de tipo flotante que represente la distancia entre los puntos (x1, y1) y (x2 y y2).

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

--2. Hipotenusa de un triangulo rectángulo:
--La función hipotenusa debe recibir dos parámetros de tipo flotante b y h donde b representa
--la base y h la altura. La función debe devolver un valor de tipo flotante que represente el valor
--de la hipotenusa que se calcula respecto a la base y altura del triángulo rectángulo.
--NOTA: La hipotenusa de un triángulo rectángulo está definida como

hipotenusa :: (Float) -> (Float) -> Float
hipotenusa x y = sqrt ((x^2)+(y^2))

--3. Pendiente de la recta que pasa por dos puntos:
--La funcion pendiente debe recibir dos parametros que seran tuplas de dos elementos de tipo
--flotante respectivamente, es decir, (x1, y1) y (x2 y y2). La funci on debe devolver un valor de
--tipo flotante que represente la pendiente de la recta que pasa por dos puntos.

pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (x1,y1) (x2,y2) = (y2-y1)/(x2-x1)

--4. Raíces de una ecuación cuadrática:
--La función raices debe recibir tres parámetros flotantes a, b y c que representan la ecuación
--ax2 + bx + c = 0
--La función debe devolver una tupla de dos elementos de tipo flotante que representan las raíces
--de una ecuación cuadrática.

raices :: Float -> Float -> Float -> (Float,Float)
raices a b c = (((-b + sqrt (b^2-4*a*c))/(2*a)),((-b - sqrt (b^2-4*a*c))/(2*a)))

--5. Area de un triangulo por medio de la formula de Heron  :
--La funcion areaTriangulo recibe tres parametros de tipo flotante x, y y z que representan las
--longitudes de los tres lados de un triangulo. La funcion debe devolver un valor de tipo flotante
--que representa el area de un triangulo calculado por medio de la formula de Heron.

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z = sqrt(((x+y+z)/2)*(((x+y+z)/2)-x)*(((x+y+z)/2)-y)*(((x+y+z)/2)-z))

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
                        
--7. La funcion maximo deber recibir los parametros x, y y z. La funcion debe devolver el entero
--maximo entre x, y y z.

maximo :: Int -> Int -> Int -> Int 
maximo x y z = if x >= y && x >= z 
                    then x 
                    else if y >= z 
                        then y  
                        else z
                        
--8. Números ordenados de forma descendente:
--La función es Descendente recibe cuatro parámetros de tipo entero x, y, z y w. La función
--debe devolver una valor de tipo booleano de acuerdo a los siguientes casos:
--8.1. True, si los números fueron ingresados de manera descendente.
--8.2. False, si los números no fueron ingresados de manera descendente

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x > y && y > z && z > w
                                                then True
                                                else False
