/*
        SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
*/

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% Dados para a base de conhecimento
:- include('include/paragensLigacoes').
% Regras auxiliares
:- include('include/auxiliar').
% Algumas heuristicas aplicadas nos algoritmos de procura
:- include('include/heuristicas').


% ---------------------------------Algotitmos de procura---------------------------------


%----------------------------------------------------------------------------------
% Depth-first Search

% Q1: resolve_depthFirst(183, 594, C), displayList(C).

resolve_depthFirst(Inicial, Final, [Inicial|Caminho]) :-
	depthFirstSearch(Inicial, Final, Caminho).

depthFirstSearch(Nodo, Final, []) :-
	Nodo == Final.

depthFirstSearch(Nodo, Final, [ProxNodo|Caminho]) :-
	adjacente(Nodo, ProxNodo),
	depthFirstSearch(ProxNodo, Final, Caminho).	

adjacente(Nodo, ProxNodo) :- 
	ligacao(Nodo, ProxNodo, _).

%----------------------------------------------------------------------------------
% Breath-first Search

% Q1: resolve_breathFirst(183, 594, C), displayList(C).

resolve_breathFirst(Inicial, Final, CaminhoFinal) :-
        breathFirstSearch([[Inicial]], Final, Solucao),
        inverteLista(Solucao, CaminhoFinal, []).

breathFirstSearch([[Nodo|Caminho]|_], Final, [Nodo|Caminho]) :-
        Nodo == Final.

breathFirstSearch([[N|Caminho]|CaminhoList], Final, Solucao) :-
        bagof([M, N|Caminho],
        (ligacao(N, M, _), \+ membro(M, [N|Caminho])), NovosCaminhos),
        append(CaminhoList, NovosCaminhos, Res), !,
        breathFirstSearch(Res, Final, Solucao);
        breathFirstSearch(CaminhoList, Final, Solucao).

%----------------------------------------------------------------------------------
% A* Search

% Q1: resolve_astar(183, 595, Caminho/CostTime), displayList(Caminho).

resolve_astar(Inicial, Final, CaminhoFinal/Custo) :-
	distanciaEuclidiana(Inicial, Final, Estima),
	astarSearch([[Inicial]/0/Estima], Final, CaminhoReversed/Custo/_),
        inverteLista(CaminhoReversed, CaminhoFinal, []).

astarSearch(Caminhos, Final, Caminho) :-
	bestPath(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_, Nodo == Final.

astarSearch(Caminhos, Final, SolucaoCaminho) :-
	bestPath(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expand_astar(MelhorCaminho, Final, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        astarSearch(NovoCaminhos, Final, SolucaoCaminho).		

bestPath([Caminho], Caminho) :- !.

bestPath([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	bestPath([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
	
bestPath([_|Caminhos], MelhorCaminho) :- 
	bestPath(Caminhos, MelhorCaminho).

expand_astar(Caminho, Final, ExpCaminhos) :-
	findall(NovoCaminho, adjacenteAStar(Caminho, Final, NovoCaminho), ExpCaminhos).

adjacenteAStar([Nodo|Caminho]/Custo/_, Final, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
	        ligacao(Nodo, ProxNodo, _),
                \+ membro(ProxNodo, Caminho),
	        distanciaEuclidiana(
                        Nodo, ProxNodo, PassoCusto
                ),
                NovoCusto is Custo + PassoCusto,
                distanciaEuclidiana(
                        ProxNodo, Final, Est
                ).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

%----------------------------------------------------------------------------------
