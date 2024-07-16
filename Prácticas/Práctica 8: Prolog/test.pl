natural(succ(X)) :- natural(X).
natural(cero).

menor(cero,succ(X)) :- natural(X).
menor(succ(X),succ(Y)) :- menor(X,Y).


entre(X,Y,X) :- X =< Y.
entre(X,Y,Z) :- X < Y, X2 is X + 1, entre(X2, Y, Z).

long([],0).
long([X|XS],N) :- long(XS,N2), N is N2+1.

iesimo(0,[X|_],X).
iesimo(I,[_|XS],X) :- iesimo(I2, XS, X), I is I2 + 1.

%Y tiene que estar instanciado
desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), X < Y.

%pmq(+X, -Y) que genera todos los naturales pares menores o iguales a X
pmq(X,Y) :- between(0,X,Y), Y mod 2 =:= 0.

%coprimos(-X, -Y) usando gcd 
%no instancio ni x ni y
paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X.
generarPares(X,Y) :- desde2(1,S), paresSuman(S,X,Y).
coprimos(X,Y) :- generarPares(X,Y), gcd(X,Y) =:= 1.

%corteMasParejo(+L, -L1, -L2), se puede usar sumlist, poner doble parentesis
corteMasParejo(L, L1, L2) :- unCorte(L,L1,L2,D), not((unCorte(_,_,_,D2), D2 < D)).

uncorte(L,L1,L2,D) :- append(L1, L2, L), sumlist(L1,S1), sumlist(L2,S2), D is abs(S1-S2). 

%ground era caso esta todo instanciado
esTriangulo(tri(A,B,C)) :- A<B+C, B<A+C, C<A+B.
armarTriplas(P,A,B,C) :- between(0,P,A), S is P-A, between(0,S,B), C is S-B.
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A+B+C. %caso trianguo y perimetro instanciados
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), nonvar(P), armarTriplas(P,A,B,C), esTriangulo(tri(A,B,C)). %caso perimetro instanciado y tri no
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P =:= A,B,C.

%generador de triangulos sin dar repetidos
triangulo(T) :- perimetro(T,_).
