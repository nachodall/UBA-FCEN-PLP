% inorderAB
inorder(nil,[]).
inorder(bin(Izq,R,Der),L) :- 
	inorder(Izq, L1),
	inorder(Der,L2),
	append(L1,[R|L2],L).

%arbolConListaInorder
arbolConLista([],nil).
arbolConLista(L, bin(Izq,R,Der)) :- inorder(bin(Izq,R,Der), L).

% esABB
esABB(nil).
esABB(bin(Izq,R,Der)) :- esABB(Izq), esABB(Der), todosMayores(R,Der), todosMenores(R,Izq).
todosMayores(_,nil).
todosMayores(V, bin(Izq,R,Der)) :- todosMayores(V,Izq), todosMayores(V,Der), R >= V.
todosMenores(_,nil).
todosMenores(V, bin(Izq,R,Der)) :- todosMenores(V,Izq), todosMenores(V,Der), R =< V.

% ABBInsertar(+x, +t, +tRes)
ABBInsertar(X, nil, bin(nil,X,nil)).
ABBInsertar(X, bin(Izq, R, Der), bin(Izq, R, Der)). % ABB no tiene repetidos
ABBInsertar(X, bin(Izq, R, Der), bin(Izq2, R2, Der2)) :- X > R, ABBInsertar(X,Der,Der2).
ABBInsertar(X, bin(Izq, R, Der), bin(Izq2, R2, Der2)) :- X < R, ABBInsertar(X,Izq,Izq2).
