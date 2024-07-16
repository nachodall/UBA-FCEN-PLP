aplanar([],[]).

%caso aplanar lista
aplanar([HS|XS], L) :- 
	aplanar(HS, HSA), %aplano la cabeza
	aplanar(XS, RECL), %aplano el resto de la lista
	append(HSA, RECL, L). %uno las dos recursiones 

%caso aplanar elemento
aplanar([H|XS], L) :-
	not(aplanar(H,_)), %intento aplanar la cabeza, si no se puede entonces es elemento
	aplanar(XS, RECL), %aplano recursivamente
	append([H],RECL,L). %junto todo pedazo de crack