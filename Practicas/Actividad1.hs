-- Jose Manuel Beauregard Mendez

-- Numero que satisfagan z^2==x^2+y^2
[(x,y,z)| x<-[1..100], y<-[1..100], z<-[1..100], z^2==x^2+y^2]

-- Numero negativos
[y|x<-[-1000..1000], let y = x<0]

-- La suma de todos los numeros impares abajo de 1000000
sum[x|x<-[1..1000000], mod x 2 == 1]

-- Combinar un text en minúsculas, pasarlo a mayúsculas (import Data.Char)
map(toUpper)"Hellaso"

-- Recursividad, numero maximo en una lista
let maxlist[] = e
maxlist[e] = e
maxlist (h:t) = max h (maxlist t)

-- Factorial
let factorial 0 = 1
factorial n = n * (factorial(n-1));

-- Funcion que obtiene n ultimos elementos de una list
let lastt n i list = lastt n-1 i+1
lastt 
