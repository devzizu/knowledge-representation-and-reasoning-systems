
%----------------------------------------------------------------------------------------
                            /* Predicados auxiliares */


%----------------------------------------------------------------------------------------
% Escrever uma lista para o stdout

writeList([]).
writeList([X|L]):- write(X), write(','), writeList(L).

displayList([]).
displayList([A|B]) :-
  format('~w -> ,',A),
  displayList(B).

%----------------------------------------------------------------------------------------
% Verifica se um elemento existe numa lista

membro(X, [X|_]).
membro(X, [_|Xs]):-
        membro(X,Xs).

%----------------------------------------------------------------------------------------
% Calcula o inverso de uma lista (not working properly)

inverteLista([],Z,Z).
inverteLista([H|T],Z,Acc) :- inverteLista(T,Z,[H|Acc]).