
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
  format('~w',A),
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