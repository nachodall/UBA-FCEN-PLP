% intersección(+L1, +L2, -L3)

% intersección([], _, []).
% intersección([X|L1], L2, [X|L3]) :- member(X,L2), not(member(X,L1)), intersección(L1,L2,L3).
% intersección([X|L1], L2, L3) :- not(member(X,L2)), intersección(L1,L2,L3).

intersección([], _, []).

% Caso X no pertenece a L2
intersección([X|L1], L2, Rs) :-
    not(member(X, L2)),
    intersección(L1, L2, Rs).

% Caso X sí pertenece a L2.
intersección([X|L1], L2, [X|L3]) :-
    member(X, L2),
    borrar(L1, X, T1), %borra todas las apariciones de X si las hay
    intersección(T1, L2, L3).

%-----------------------------------------------------
% partir(+N, +L, ?L1, ?L2) l1 tiene los n primer elementos de l y el resto en l2 

partir(0, L, [], L).
partir(N, [H|T], [H|L1], L2) :-
    N > 0,             	  % Asegura que N sea mayor que 0
    N1 is N - 1,         % Decrementa N
    partir(N1, T, L1, L2). % Llama recursivamente con el nuevo N y la cola de la lista

%-----------------------------------------------------

% borrar(+ListaOriginal, +X, -ListaSinXs) 
borrar([], _, []).
borrar([X|Xs], X, Ys) :- borrar(Xs, X, Ys).
borrar([X|Xs], Y, [X|Ys]) :- X \= Y, borrar(Xs, Y, Ys).

%---------------------------------------------------
long([], 0).
long([_|XS], L) :-
    long(XS, L1),
    L is L1 + 1.

%---------------------------------------------------
sacarDuplicados([],[]).
sacarDuplicados([X|XS], L) :-
 	sacarDuplicados(XS,LREC), % llamo recursivo hasta lista vacia
 	borrar(LREC, X, LREC2), % saco X del llamado recursivo
 	append([X], LREC2, L). % agrego X

%---------------------------------------------------
insertarAtras(X,L,LX) :- append(L,[X], LX).
insertarAdelante(X,L,LX) :- append([X], L, LX).
insertarGeneral(X,L,LX) :- append(I,D,L),append(I,[X|D],LX). % 

%----------------------------------------------------
permutacion([],[]).
permutacion([X|XS],L) :- permutacion(XS,L1), insertarGeneral(X,L1,L). % va insertando en todos lados 

%---------------------------------------------------
reparto([],0,[]).
reparto(L,N,[Xs|Ys]) :- N>0 ,N2 is N-1, append(Xs,D,L), reparto(D,N2,Ys).
reparto([],N,[[]|Ys]) :- N>0, N2 is N-1, reparto([],N2,Ys).
