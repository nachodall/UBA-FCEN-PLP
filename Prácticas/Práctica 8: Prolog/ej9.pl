desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).
% solo se debe instanciar el primer valor 

desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), X < Y.