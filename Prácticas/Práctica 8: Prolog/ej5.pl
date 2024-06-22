%last(?L, ?U)
last([X], X).
last([_|YS], X) :- last(YS, X).
lastAp(L,U) :- append(_,[U],L).


%reverse(+L, -L1)
reverse([], []).
reverse([X|XS], L) :- reverse(XS, L1), append(L1, [X], L) .

%prefijo(?P, +L)
prefijo(P, L) :- append(P, _, L).

%prefijo(?P, +L)
sufijo(S,L) :- append(_,S,L).

%sublista(?S, +L)
sublista([], _).
sublista([X|Xs], L) :- prefijo(P, L), sufijo([X|Xs], P).

%pertenece(?X, +L)
pertenece(X, L) :- sublista([X], L).