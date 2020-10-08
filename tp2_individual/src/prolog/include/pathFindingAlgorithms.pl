
%----------------------------------------------------------------------------------
% Algotitmos de procura informada
%----------------------------------------------------------------------------------

%----------------------------------------------------------------------------------
% Depth-first Search

resolve_depthFirst(Paragens, Inicial, Final, [(Inicial,'Start')|Caminho]) :-
	depthFirstSearch(Paragens, Inicial, Final, Caminho).

depthFirstSearch(Paragens, Final, Final, []) :- !.

depthFirstSearch(Paragens, Nodo, Final, [(ProxNodo,Carreira)|Caminho]) :-
        adjacenteDF(Paragens, Nodo, ProxNodo, Carreira),
	depthFirstSearch(Paragens, ProxNodo, Final, Caminho).	

adjacenteDF(Paragens, Nodo, ProxNodo, Carreira) :- 
	ligacao(Nodo, ProxNodo, Carreira),
        existeNodo(Nodo, Paragens),
        existeNodo(ProxNodo, Paragens).

existeNodo(Nodo, [paragem(Nodo, _,_,_,_,_,_,_,_,_)|Tail]).
existeNodo(Nodo, [P|Tail]) :- existeNodo(Nodo, Tail).

%----------------------------------------------------------------------------------
% Breath-first Search

resolve_breathFirst(Paragens, Inicial, Final, CaminhoFinal) :-
        breathFirstSearch(Paragens, [[(Inicial,'Start')]], Final, Solucao),
        inverteLista(Solucao, CaminhoFinal, []).

breathFirstSearch(Paragens, [[(Nodo,C)|Caminho]|_], Final, [(Nodo,C)|Caminho]) :-
        Nodo == Final.

breathFirstSearch(Paragens, [[(N,C)|Caminho]|CaminhoList], Final, Solucao) :-
        bagof([(M, Carreira), (N,C)|Caminho],
        %setof([(M, Carreira), (N,C)|Caminho],
        (adjacenteBF(Paragens, N, (M, Carreira)), \+ membro((M, Carreira), [(N,C)|Caminho])), NovosCaminhos),
        append(CaminhoList, NovosCaminhos, Res), !,
        breathFirstSearch(Paragens, Res, Final, Solucao);
        breathFirstSearch(Paragens, CaminhoList, Final, Solucao).

adjacenteBF(Paragens, Nodo, (NodoAdj,Carreira)) :- 
	ligacao(Nodo, NodoAdj, Carreira),
        existeNodo(Nodo, Paragens),
        existeNodo(NodoAdj, Paragens).

%----------------------------------------------------------------------------------
% Algotitmos de procura não-informada
%----------------------------------------------------------------------------------
    
%----------------------------------------------------------------------------------
% A* Search

resolve_astar(Paragens, Inicial, Final, CaminhoFinal/Custo) :-
	distanciaEuclidiana(Inicial, Final, Estima),
	astarSearch(Paragens, [[(Inicial,'Start')]/0/Estima], Final, CaminhoReversed/Custo/_),
        inverteLista(CaminhoReversed, CaminhoFinal, []).

astarSearch(Paragens, Caminhos, Final, Caminho) :-
	bestPathAsearch(Caminhos, Caminho),
	Caminho = [(Nodo,C)|_]/_/_, Nodo == Final.

astarSearch(Paragens, Caminhos, Final, SolucaoCaminho) :-
	bestPathAsearch(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expand_astar(Paragens, MelhorCaminho, Final, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        astarSearch(Paragens, NovoCaminhos, Final, SolucaoCaminho).		

bestPathAsearch([Caminho], Caminho) :- !.

bestPathAsearch([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	bestPathAsearch([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
	
bestPathAsearch([_|Caminhos], MelhorCaminho) :- 
	bestPathAsearch(Caminhos, MelhorCaminho).

expand_astar(Paragens, Caminho, Final, ExpCaminhos) :-
	findall(NovoCaminho, adjacenteAStar(Paragens, Caminho, Final, NovoCaminho), ExpCaminhos).

adjacenteAStar(Paragens, [(Nodo,C1)|Caminho]/Custo/_, Final, [(ProxNodo,C2),(Nodo,C1)|Caminho]/NovoCusto/Est) :-
	        verificarLigacaoAStar(Paragens, Nodo, (ProxNodo,C2)),
                \+ membro((ProxNodo,C2), Caminho),
	        distanciaEuclidiana(
                        Nodo, ProxNodo, PassoCusto
                ),
                NovoCusto is Custo + PassoCusto,
                distanciaEuclidiana(
                        ProxNodo, Final, Est
                ).

verificarLigacaoAStar(Paragens, Nodo, (ProxNodo,C2)) :- 
	ligacao(Nodo, ProxNodo, C2),
        existeNodo(Nodo, Paragens),
        existeNodo(ProxNodo, Paragens).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

%----------------------------------------------------------------------------------
% Greedy Search

resolve_greedy(Paragens, Inicial, Final, Caminho/Custo):- 
        distanciaEuclidiana(Inicial, Final, Estima),
        greedySearch(Paragens, [[(Inicial, 'Start')]/0/Estima], Final, InvCaminho/Custo/_),
	inverteLista(InvCaminho, Caminho, []).

greedySearch(Paragens, Caminhos, Final, Caminho):- 
        bestPathGreedy(Caminhos, Caminho),
        Caminho = [(Nodo,C)|_]/_/_, Nodo == Final.

greedySearch(Paragens, Caminhos, Final, SolucaoCaminho):-
        bestPathGreedy(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expand_greedy(Paragens, MelhorCaminho, Final, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        greedySearch(Paragens, NovoCaminhos, Final, SolucaoCaminho).

bestPathGreedy([Caminho],Caminho):- !.

bestPathGreedy([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho):- 
        Est1 =< Est2, !, 
        bestPathGreedy([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
																			
bestPathGreedy([_|Caminhos],MelhorCaminho):- 
        bestPathGreedy(Caminhos, MelhorCaminho).

expand_greedy(Paragens, Caminho, Final, ExpCaminhos):- 
        findall(NovoCaminho, adjacenteAStar(Paragens, Caminho, Final, NovoCaminho), ExpCaminhos).