% Adrián Fernández
% Santiago González-Carvajal


% Ejercicio 1
pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [F|Rs]) :- pertenece_m(X, F); pertenece_m(X, Rs).


% Ejercicio 2
concatena([], L, L).
concatena([X|R1], L2, [X|R3]) :- concatena(R1, L2, R3).

invierte([], []).
invierte([X|L], R) :- invierte(L, LI), concatena(LI, [X], R).


% Ejercicio 3
insert([X-P], [], [X-P]).
insert([X-P], [Y-Q|Rs], R) :- P=<Q, concatena([X-P], [Y-Q|Rs], R).
insert([X-P], [Y-Q|Rs], R) :- P>Q, insert([X-P], Rs, I), concatena([Y-Q], I, R).


% Ejercicio 4.1 elem_count(X, L, Xn).
elem_count(_, [], 0).
elem_count(X, [X|Rs], Xn) :- elem_count(X, Rs, C), Xn is C+1.
elem_count(X, [Y|Rs], Xn) :- elem_count(X, Rs, Xn), X \= Y.


% Ejercicio 4.2
list_count([], _, []).
list_count([X|R1], L2, [X-Xn|R3]) :- elem_count(X, L2, Xn), list_count(R1, L2, R3).


% Ejercicio 5
sort_list([], []).
sort_list([X-P|Rs], SL) :- sort_list(Rs, SLr), insert([X-P], SLr, SL).


% Ejercicio 6
build_tree([X-_], tree(X, nil, nil)).
build_tree([F, S|Rs], tree(1, Ti, Td)) :- Rs = [], build_tree([F], Ti), build_tree([S], Td).
build_tree([F, S|Rs], tree(1, Ti, Td)) :- Rs \= [], build_tree([F], Ti), build_tree([S|Rs], Td).


% Ejercicio 7.1
encode_elem(X, [], tree(X, nil, nil)).
encode_elem(X, [0], tree(1, Ti, _)) :- encode_elem(X, [], Ti).
encode_elem(X, [1|Rs], tree(1, _, Td)) :- encode_elem(X, Rs, Td).


% Ejercicio 7.2
encode_list([], [], _).
encode_list([X|R1], [Y|R2], T) :- encode_elem(X, Y, T), encode_list(R1, R2, T).


% Ejercicio 8
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
encode(L1, L2) :- dictionary(D), list_count(D, L1, LC), sort_list(LC, SL), invierte(SL, IL), build_tree(IL, T), encode_list(L1, L2, T).
