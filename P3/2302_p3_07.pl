% Adrián Fernández
% Santiago González-Carvajal


% Ejercicio 1
pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [F|Rs]) :- pertenece_m(X, F); pertenece_m(X, Rs).


% Ejercicio 2
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|L], R) :- invierte(L, LI), concatena(LI, [X], R).


% Ejercicio 3
insert([X-P], [], [X-P]).
insert([X-P], [F-Q|Rs], R) :- P<Q, insert([X-P], [Rs], R).


