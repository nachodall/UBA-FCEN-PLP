esTriangulo(tri(A,B,C)) :- 
    A > 0, B > 0, C > 0, 
    A < B + C, B < A + C, C < A + B.

%perimetro(?T, ?P), primero voy a hacer el caso que viene instanciado T 

perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A + B + C.

%ahora si no esta instanciado T 
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), armarTriplasDeP(P,A,B,C), esTriangulo(tri(A,B,C)).
armarTriplasDeP(P,A,B,C) :- desde2(1,P), between(0,P,A), S is P-A , between(0,S,B), C is S-B.  

desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), X < Y.