--1. Distancia entre dos puntos en el plano cartesiano:
--La funcion distanciaPuntos debe recibir dos parametros que seran tuplas de dos elementos de
--tipo flotante respectivamente, es decir, (x1, y1) y (x2 y y2). La funcion debe devolver un valor
--de tipo flotante que represente la distancia entre los puntos (x1, y1) y (x2 y y2).

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

--3. Pendiente de la recta que pasa por dos puntos:
--La funcion pendiente debe recibir dos parametros que seran tuplas de dos elementos de tipo
--flotante respectivamente, es decir, (x1, y1) y (x2 y y2). La funci on debe devolver un valor de
--tipo flotante que represente la pendiente de la recta que pasa por dos puntos.

pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (x1,y1) (x2,y2) = (y2-y1)/(x2-x1)

--5. Area de un triangulo por medio de la formula de Heron  :
--La funcion areaTriangulo recibe tres parametros de tipo flotante x, y y z que representan las
--longitudes de los tres lados de un triangulo. La funcion debe devolver un valor de tipo flotante
--que representa el area de un triangulo calculado por medio de la formula de Heron.

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z = sqrt(((x+y+z)/2)*(((x+y+z)/2)-x)*(((x+y+z)/2)-y)*(((x+y+z)/2)-z))

--La funcion maximo deber recibir los parametros x, y y z. La funcion debe devolver el entero
--maximo entre x, y y z.

maximo :: Int -> Int -> Int -> Int 
maximo x y z = if x >= y && x >= z 
                    then x 
                    else if y >= z 
                        then y  
                        else z