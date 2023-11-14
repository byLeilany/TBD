
% ejemplo(+Codigo, -E)
ejemplo(c4, [(n1,n2),(n2,n3),(n3,n4),(n4,n1)]).


% Ejercicio 1
% armar_grafo(+E,-Grafo).
armar_grafo([], grafo([], [])).
armar_grafo([(X,Y)|E], grafo([(X,_A), (Y,_B)| C], [(X,Y)|E])) :- armar_grafo(E, grafo(C, E)), not(member((X,_), C)), not(member((Y,_), C)).
armar_grafo([(X,Y)|E], grafo([(X,_)| C], [(X,Y)|E])) :- armar_grafo(E, grafo(C, E)), member((Y,_), C), not(member((X,_), C)).
armar_grafo([(X,Y)|E], grafo([(Y,_)| C], [(X,Y)|E])) :- armar_grafo(E, grafo(C, E)), member((X,_), C), not(member((Y,_), C)).
armar_grafo([(X,Y)|E], grafo(C, [(X,Y)|E])) :- armar_grafo(E, grafo(C, E)), member((X,_), C), member((Y,_), C).


%armar_grafo([(n1,n2),(n2,n3)],X).

% Ejercicio 2
% color_nodo(+Grafo, +Nodo, ?Color).
color_nodo(grafo(Colores,_),N,C) :- member((N,C), Colores).

%color_nodo(grafo([(n2, _6606),  (n3, _6606),  (n4, _6606),  (n1, _6606)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)]),n4,C).

% Ejercicio 3
% vecino(+G, ?V, ?W).
vecino(grafo(_ ,E), V, W) :- member((V, W), E).
vecino(grafo(_ ,E), V, W) :- member((W, V), E).

%vecino(grafo([(n1,1),(n2,2),(n3,3),(n4,4),(n5,5)],[(n1,n2),(n2,n3),(n4,n5)]),W,V).
%grafo([(n2, _6606),  (n3, _6606),  (n4, _6606),  (n1, _6606)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)])
% Ejercicio 4
% colores_vecinos(+G, +Nodo, -Colores).
colores_vecinos(G,Nodo,Res) :- findall(Color, (vecino(G,Nodo,Vec), color_nodo(G,Vec,Color), nonvar(Color)), Res). 

% Ejercicio 5
%pintar nodo(+Paleta, +?Grafo, +Nodo)
pintar_nodo(Paleta, G, Nodo) :- between(1, Paleta, S), colores_vecinos(G, Nodo, ColV), not(member(S, ColV)), color_nodo(G, Nodo, S).

% Ejercicio 6
% pintar grafo(+Paleta,+?Grafo)
pintar_grafo(Paleta, grafo(Colores,Aristas)) :- pintar_aux(Paleta, Colores, grafo(Colores, Aristas)).

pintar_aux(_, [], _). 
pintar_aux(Paleta, [(Nodo, C) | CS], Grafo) :- var(C), pintar_aux(Paleta, CS, Grafo), pintar_nodo(Paleta, Grafo, Nodo).
pintar_aux(Paleta, [(Nodo, C) | CS], Grafo) :- nonvar(C), pintar_aux(Paleta, CS, Grafo).


% Ejercicio 7)
% mismo_color(+G,+V,+W)
mismo_color(Grafo, V, W) :- color_nodo(Grafo, V, _C), color_nodo(Grafo, W, _C).


% Ejercicio 8
% es_valido(+Grafo)
es_valido(grafo(Colores, E)) :- es_valido_aux(Colores, grafo(Colores, E)).

es_valido_aux([], G).
es_valido_aux([(Nodo, Color) | Colores], G) :- not((vecino(G, Nodo, Vecino), member((Vecino,Color), Colores))), es_valido_aux(Colores, G).

%es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 2)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)])).

% Ejercicio 9
% coloreo(+G, -Coloreo).
coloreo(Grafo, Coloreo) :- coloreos_validos_posibles(Grafo, Coloreo), ok_los_nums(Coloreo).

coloreos_validos_posibles(grafo(ListaCol, Aristas), ListaCol) :- length(ListaCol, N), pintar_grafo(N, grafo(ListaCol, Aristas)), es_valido(grafo(Coloreo,Aristas)).

% chequea_validez(grafo(Coloreo, Aristas)).
ok_los_nums(Coloreo) :- max_color(Coloreo, Max), todos_hasta_max(Coloreo, Max).

max_color([(Nodo, Color)], Color).
max_color([(Nodo, Col) | Colores], Max) :- max_color(Colores, MaxRec), Max is max(Col, MaxRec).  

todos_hasta_max(Coloreo, Max) :- forall(between(1, Max, N), member((_,N), Coloreo)).

%TESTS
test(1) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3),colores_vecinos(G, n1, [3]).
test(2) :- ejemplo(c4, E), armar_grafo(E, G), color_nodo(G, n4, 3),color_nodo(G,n2,2), colores_vecinos(G, n1, LC), sort(LC,[2,3]).
test(3) :- ejemplo(c4, E), armar_grafo(E, G), colores_vecinos(G, n1, []).
test(4) :- es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 2)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)])).
test(5) :- not(es_valido(grafo([(n2, 1),  (n3, 2),  (n4, 1),  (n1, 1)], [(n1, n2),  (n2, n3),  (n3, n4),  (n4, n1)]))).
test(6) :- findall(CS,(ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS)),L), length(L,38).
test(7) :- ejemplo(c4, E), armar_grafo(E, G), coloreo(G, CS), sort(CS, [(n1, 2), (n2, 1), (n3, 2), (n4, 3)]).

tests :- forall(between(1,7,N), test(N)). % Hacer sus propios tests y cambiar el 10 por la cantidad de tests que tengan.
