pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, L).