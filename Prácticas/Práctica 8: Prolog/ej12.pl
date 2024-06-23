%vacio
vacio(nil).

%raiz
raiz(bin(_, V, _),V).

%altura
altura(nil, 0).
altura(bin(I,_,D),H) :- altura(I,HI), altura(D,HD), H is 1 + max(HD, HI).

%cantNodos
cantNodos(nil, 0).
cantNodos(bin(I,_,D),C) :- cantNodos(I,CI), cantNodos(D,CD), C is 1 + CI + CD.
