
%----------------------------------------------------------------------------------------
                            /* Predicados auxiliares */


%----------------------------------------------------------------------------------------
% Escrever uma lista para o stdout

displayList([]).
displayList([A|B]) :-
  format('\nNodo:~w',A),
  displayList(B).

displayListNormal([]).
displayListNormal([A|B]) :-
  format('~w ',A),
  displayListNormal(B).

displayListSlash([]).
displayListSlash([[A]/X|B]) :-
  format('~w | Value = ~w;\n', [A, X]),
  displayListSlash(B).

%----------------------------------------------------------------------------------------
% Escrever um valor no stdout

displayCost(R) :- format('\nCost=~3f',R).
displayNode(R) :- format('\nNode=~w',R).
displayGeneral(Text, R) :- format('\n~w = ~w',[Text, R]).

%----------------------------------------------------------------------------------------
% Verifica se um elemento existe numa lista

membro(X, [X|_]).
membro(X, [_|Xs]):-
        membro(X,Xs).

membros([], _).
membros([X|Xs], Members) :-
        membro(X, Members),
        membros(Xs, Members).

%----------------------------------------------------------------------------------------
% Calcula o inverso de uma lista (not working properly)

inverteLista([],Z,Z).
inverteLista([H|T],Z,Acc) :- inverteLista(T,Z,[H|Acc]).

%----------------------------------------------------------------------------------------
% Calcula o tamanho de uma lista

lengthList([], Res):-
    Res is 0.

lengthList([X|Y], Res):-
    lengthList(Y, L),
    Res is L + 1.

%----------------------------------------------------------------------------------------
% Append de uma lista noutra lista

appendToList([],L,L).
appendToList([H|T],L,[H|Z]):- appendToList(T,L,Z).

%----------------------------------------------------------------------------------------
% Merge de duas listas

mergeList([],L,L ).
mergeList([H|T],L,[H|M]):-
  mergeList(T,L,M).

%----------------------------------------------------------------------------------------
% Merge de varias listas numa

flatten(List, FlatList) :-
    flatten(List, [], FlatList0),
    !,
    FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
    var(Var),
    !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
    !,
    flatten(Hd, FlatHeadTail, List),
    flatten(Tl, Tail, FlatHeadTail).
flatten(NonList, Tl, [NonList|Tl]).

%----------------------------------------------------------------------------------------
% Verificar se é sublista

isSublist( [], _ ).
isSublist( [X|XS], [X|XSS] ) :- isSublist( XS, XSS ).
isSublist( [X|XS], [_|XSS] ) :- isSublist( [X|XS], XSS ).

%----------------------------------------------------------------------------------------
% Remover o ultimo elemento

remove_last([X|Xs], Ys) :-
   remove_last_aux(Xs, Ys, X).

remove_last_aux([], [], _).
remove_last_aux([X1|Xs], [X0|Ys], X0) :-  
   remove_last_aux(Xs, Ys, X1).

%----------------------------------------------------------------------------------------
% Remover o primeiro elemento

remove_head([_|Tail], Tail).

%----------------------------------------------------------------------------------------
% obter Primeiro elemento de uma lista

first_list([H], H).
first_list([H|_], H).