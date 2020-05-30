
%----------------------------------------------------------------------------------------
                            /* Predicados auxiliares */

% Escrever uma lista para o stdout
writeList([]).
writeList([X|L]):- write(X), write(','), writeList(L).

% Verifica se um elemento existe numa lista
membro(X, [X|_]).
membro(X, [_|Xs]):-
        membro(X,Xs).

% Calcula o inverso de uma lista
inverso(Xs, Ys) :- inverso(Xs, [], Ys).
inverso([], Xs, Ys).
inverso([X|Xs], Ys, Zs) :- inverso(Xs,[X|Ys], Zs).

% Membros
membros([], _).
membros([X|Xs], Members) :-
        membro(X, Members),
        membros(Xs, Members).

% Predicado de negação
nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

%---------------------------------------------------------------------------------------------

% Distancia euclidiana entre dois pontos (simplificado para latitude e longitude)

% Exemplo: distanciaEuclidiana(183, 595, D).

distanciaEuclidiana(N1, N2, Distance) :-
        paragem(N1,Lat1,Long1,_,_,_,_,_,_,_),
        paragem(N2,Lat2,Long2,_,_,_,_,_,_,_),
        X is (Lat2-Lat1), 
        Y is (Long2-Long1),
        Distance is sqrt(X^2 + Y^2).