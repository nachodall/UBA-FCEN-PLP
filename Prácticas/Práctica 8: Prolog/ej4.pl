juntar([], L, L).
juntar([X|L1], L2, [X|L3]) :- juntar(L1, L2, L3). 