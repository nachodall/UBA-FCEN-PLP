frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).
leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% si se pone en leGusta(X) :- frutal(X), cremoso(X), !. solo nos da la primer respuesta. lo mismo en cucurucho.
% cucurucho(X,Y) :- leGusta(X), !, leGusta(Y). solo me deja un solo gusto como x, me varia el y
% leGusta(X) :- frutal(X), !, cremoso(X). solo me da una
