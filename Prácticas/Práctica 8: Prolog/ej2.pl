
vecino(X, Y, [X|[Y|Ls]]).	
vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls).

%al invertir las reglas, me da el orden de los resultados al reves, primero buscara vecinos mas a la izquierda y luego a la derecha.