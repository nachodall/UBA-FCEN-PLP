% cuadradoSemiLatino(+N, -XS) :- XS es una matriz de tamaño NxN donde cada fila suma el mismo valor.
cuadradoSemiLatino(N, XS) :-
    between(0, inf, S),          % Genera S en orden creciente comenzando desde 0
    matriz(N, S, XS).

% matriz(+N, +S, -M) :- M es una matriz de tamaño NxN donde cada fila suma S.
matriz(N, S, M) :-
    length(M, N),                % La matriz tiene N filas
    generarFilas(N, S, M).

% generarFilas(+N, +S, -Filas) :- Genera N filas que suman S.
generarFilas(0, _, []).
generarFilas(N, S, [F|Fs]) :-
    N > 0,
    fila(N, S, F),               % Genera una fila de tamaño N que sume S
    N1 is N - 1,
    generarFilas(N1, S, Fs).     % Genera recursivamente las filas restantes

% fila(+N, +S, -F) :- F es una fila de tamaño N que suma S.
fila(N, S, F) :-
    length(F, N),                % La fila tiene N elementos
    sumlist(F, S).               % La suma de los elementos de la fila es S

% sumlist(+Lista, ?Suma) :- Suma es la suma de los elementos de Lista.
sumlist([], 0).
sumlist([H|T], S) :-
    sumlist(T, S1),
    S is H + S1.
