
% Algotitmos de procura

%----------------------------------------------------------------------------------
% Depth-first Search

resolve_depthFirst(Paragens, Inicial, Final, [Inicial|Caminho]) :-
	depthFirstSearch(Paragens, Inicial, Final, Caminho).

depthFirstSearch(Paragens, Nodo, Final, []) :-
	Nodo == Final.

depthFirstSearch(Paragens, Nodo, Final, [ProxNodo|Caminho]) :-
        adjacenteDF(Paragens, Nodo, ProxNodo),
	depthFirstSearch(Paragens, ProxNodo, Final, Caminho).	

adjacenteDF(Paragens, Nodo, ProxNodo) :- 
	ligacao(Nodo, ProxNodo, _),
        existeNodo(Nodo, Paragens),
        existeNodo(ProxNodo, Paragens).

existeNodo(Nodo, [paragem(Nodo, _,_,_,_,_,_,_,_,_)|Tail]).
existeNodo(Nodo, [P|Tail]) :- existeNodo(Nodo, Tail).

%----------------------------------------------------------------------------------
% Breath-first Search

resolve_breathFirst(Paragens, Inicial, Final, CaminhoFinal) :-
        breathFirstSearch(Paragens, [[Inicial]], Final, Solucao),
        inverteLista(Solucao, CaminhoFinal, []).

breathFirstSearch(Paragens, [[Nodo|Caminho]|_], Final, [Nodo|Caminho]) :-
        Nodo == Final.

breathFirstSearch(Paragens, [[N|Caminho]|CaminhoList], Final, Solucao) :-
        bagof([M, N|Caminho],
        %setof([M, N|Caminho],
        (adjacenteBF(Paragens, N, M), \+ membro(M, [N|Caminho])), NovosCaminhos),
        append(CaminhoList, NovosCaminhos, Res), !,
        breathFirstSearch(Paragens, Res, Final, Solucao);
        breathFirstSearch(Paragens, CaminhoList, Final, Solucao).

adjacenteBF(Paragens, Nodo, NodoAdj) :- 
	ligacao(Nodo, NodoAdj, _),
        existeNodo(Nodo, Paragens),
        existeNodo(NodoAdj, Paragens).

%----------------------------------------------------------------------------------
% A* Search

resolve_astar(Paragens, Inicial, Final, CaminhoFinal/Custo) :-
	distanciaEuclidiana(Inicial, Final, Estima),
	astarSearch(Paragens, [[Inicial]/0/Estima], Final, CaminhoReversed/Custo/_),
        inverteLista(CaminhoReversed, CaminhoFinal, []).

astarSearch(Paragens, Caminhos, Final, Caminho) :-
	bestPath(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_, Nodo == Final.

astarSearch(Paragens, Caminhos, Final, SolucaoCaminho) :-
	bestPath(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expand_astar(Paragens, MelhorCaminho, Final, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        astarSearch(Paragens, NovoCaminhos, Final, SolucaoCaminho).		

bestPath([Caminho], Caminho) :- !.

bestPath([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	bestPath([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
	
bestPath([_|Caminhos], MelhorCaminho) :- 
	bestPath(Caminhos, MelhorCaminho).

expand_astar(Paragens, Caminho, Final, ExpCaminhos) :-
	findall(NovoCaminho, adjacenteAStar(Paragens, Caminho, Final, NovoCaminho), ExpCaminhos).

adjacenteAStar(Paragens, [Nodo|Caminho]/Custo/_, Final, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
	        verificarLigacaoAStar(Paragens, Nodo, ProxNodo),
                \+ membro(ProxNodo, Caminho),
	        distanciaEuclidiana(
                        Nodo, ProxNodo, PassoCusto
                ),
                NovoCusto is Custo + PassoCusto,
                distanciaEuclidiana(
                        ProxNodo, Final, Est
                ).

verificarLigacaoAStar(Paragens, Nodo, ProxNodo) :- 
	ligacao(Nodo, ProxNodo, _),
        existeNodo(Nodo, Paragens),
        existeNodo(ProxNodo, Paragens).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

%----------------------------------------------------------------------------------
