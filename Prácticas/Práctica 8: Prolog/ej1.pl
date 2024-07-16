padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

hijo(X,Y) :- padre(Y,X).

hermano(X,Y) :- hijo(X,Z), hijo(Y,Z), X \= Y.

descendiente(X,Y) :- padre(Y,X).
descendiente(X,Y) :- padre(Y,Z), descendiente(X,Z).	

ancestro(X,X).
ancestro(X,Y) :- ancestro(Z,Y), padre(X,Z).

ancestro2(X,Y) :- padre(X,Y).
ancestro2(X,Y) :- padre(X,Z), ancestro2(Z,Y).