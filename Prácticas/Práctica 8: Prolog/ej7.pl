palindromo(L,L1) :- reverse(L,L2), append(L,L2,L1).

iésimo(1, [X|_], X).
iésimo(I, [_|YS], X) :-
    I > 1,                  % Asegura que el índice sea mayor que 1
    I1 is I - 1,            % Decrementa el índice
    iésimo(I1, YS, X).    % Llama recursivamente con el nuevo índice y la cola de la lista
