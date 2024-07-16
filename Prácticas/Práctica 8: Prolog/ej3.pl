natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X,X) :- natural(X). 

%al llamar a menorOIgual(0,X) el programa se cuelga ya que buscara unificar X infinitamente al succ(X)


%Como Prolog resuelve las reglas de arriba hacia abajo, de izquierda a derecha, es
%importante el orden en que definimos las reglas. Los "casos base" tienen que estar
%primeros, y también es importante cortar el "caso recursivo" si ya no hay soluciones
%válidas. Si no cuando pedimos más resultados a Prolog podemos nunca llegar al false y
%entrar en una ejecución infinita. (thx honi)

menorOIgual2(X, X) :- natural(X).
menorOIgual2(X, suc(Y)) :- menorOIgual2(X, Y).