pmq(X,Y) :- between(0,X,Y), Y mod 2 =:= 0.

desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), X < Y.

%coprimos usando funcion gcd
% coprimos(+X, +Y)
coprimos(X, Y) :- generarPares(X, Y), gcd(X, Y) =:= 1.

% generarPares(?X, ?Y)
generarPares(X, Y) :- 
    desde2(2, S), %Genero numeros hasta el infinito
    paresSuman(S, X, Y). %Buscamos los dos X, Y que sumen S

% paresSuman(+S, ?X, ?Y)
paresSuman(S, X, Y) :- 
    S1 is S - 1, 
    between(1, S1, X), %Entre 1 y S-1 anda generando todos los numeros y ponelos en X
    Y is S - X.  %Ahora Y tiene que ser S - X que generamos, esto se devuelve al generar pares