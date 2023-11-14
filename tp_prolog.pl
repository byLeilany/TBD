
% ejemplo(+Codigo, -E)
ejemplo(c4, [(n1,n2),(n2,n3),(n3,n4),(n4,n1)]).


% Ejercicio 1
% armar_grafo(+E,-Grafo).
%armar_grafo([],grafo([],[])).
armar_grafo(XS, grafo(C, XS)) :- armar_grafoaux(XS, C, N).

armar_grafoaux( [], [], 0).
armar_grafoaux( [(X,Y) | Xs],[(X,N1) | G], N1) :- armar_grafoaux(Xs, G, N),  member((Y,_), G) , not(member((X,_), G)), N1 is N + 1.
armar_grafoaux( [(X,Y) | Xs],[(Y, N1) | G], N1) :- armar_grafoaux(Xs, G, N),  member((X,_), G) , not(member((Y,_), G)), N1 is N + 1.
armar_grafoaux( [(X,Y) | Xs],[(X,N1), (Y, N2) | G], N2) :- armar_grafoaux(Xs, G, N) ,not(member((Y,_), G)) , not(member((X,_), G))  , N1 is N + 1,  N2 is N + 2.

%,not(member((Y,_), G)) , not(member((X,_), G))
%armar_grafo([(n1,n2),(n2,n3)],X).

% Ejercicio 2
% color_nodo(+Grafo, +Nodo, ?Color).
color_nodo(grafo([(N,C)],_),N,C).
color_nodo(grafo([(X,C)|XS],_),X,C).
color_nodo(grafo([(X,_)|XS],_),N,C) :- N \= X , color_nodo(grafo(XS,_),N,Col2), C is Col2.

%color_nodo(grafo([(n1,1),(n2,2),(n3,3),(n4,4),(n5,5)],[(n1,n2),(n2,n3),(n4,n5)]),n4,C).


% Ejercicio 3
% vecino(+G, ?V, ?W).
vecino(grafo(_ ,[]),V ,W ).
vecino(grafo(_ ,[(X,Y)| XS ]),V ,W ).
vecino(grafo(_ ,[(X,Y)| XS ]), X, W) :- vecino(grafo(_,XS),X,W).
vecino(grafo(_ ,[(X,Y)| XS ]), V, W) :- V \= X, W \= Y, W \= X, V \= Y,  vecino(grafo(_,XS),V,W).

%vecino(G, V, W) :- var(W), var(V), vecinovar(G,V,W).
%vecino(grafo(_ ,[(V,W)| XS ]), V, W) :- nonvar(W), ,vecino(grafo(_ ,[(V,W)| XS ]), W,V )  .
%vecino(grafo(_ ,[(W,V)| XS ]), V, W) :- nonvar(V).


%vecino(grafo([(n1,1),(n2,2),(n3,3),(n4,4),(n5,5)],[(n1,n2),(n2,n3),(n4,n5)]),W,V).

% Ejercicio 4
% colores_vecinos(+G, +Nodo, -Colores).


% Ejercicio 5
% pintar_nodo(+Paleta, ?Grafo, +Nodo).


% Ejercicio 6
% pintar_grafo(+Paleta, ?Grafo).


% Ejercicio 7
% mismo_color(+G,+V,+W)


% Ejercicio 8
% es_valido(+Grafo)


% Ejercicio 9
% coloreo(+G, -Coloreo).


%TESTS
test(1) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3),colores_vecinos(G, n1, [3]).
test(2) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3),color_nodo(G,n2,2), colores_vecinos(G, n1, LC), sort(LC,[2,3]).
test(3) :- ejemplo(c4, E), armar_grafo(E, G), colores_vecinos(G, n1, []).
test(4) :- es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 2)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)])).
test(5) :- not(es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 1)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)]))).
test(6) :- findall(CS,(ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS)),L), length(L,38).
test(7) :- ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS), sort(CS, [(n1, 2), (n2, 1), (n3, 2), (n4, 3)]).

tests :- forall(between(1,7,N), test(N)). % Hacer sus propios tests y cambiar el 10 por la cantidad de tests que tengan.
