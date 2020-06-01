/*
        SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - Projeto Individual
*/

% default flags setup
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% Dados para a base de conhecimento
:- include('include/paragensLigacoes').
% Regras auxiliares
:- include('include/auxiliar').
% Algumas heuristicas aplicadas nos algoritmos de procura
:- include('include/heuristicas').
% Alguns algoritmos de pesquisa informada e não-informada utilizados
:- include('include/pathFindingAlgorithms').

%----------------------------------------------------------------------------------
% 1) Calcular o trajeto entre dois pontos:

calcula_trajeto(Nodo_A, Nodo_B) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), displayList(Caminho).
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), displayList(Caminho).
        %resolve_astar(Nodo_A, Nodo_B, Caminho/CostTime), displayList(Caminho), displayCost(CostTime).

%       Ex_1: calcula_trajeto(183,594). 
%       Ex_2: calcula_trajeto(183,595).

%----------------------------------------------------------------------------------
% 2) Selecionar apenas algumas das operadoras de transporte para um determinado percurso:

calcula_trajeto_operadoras(Nodo_A, Nodo_B, ListaOperadoras) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        filterOperadoras(ListaOperadoras, Paragens, [], ParagensOperadoras),
        resolve_depthFirst(ParagensOperadoras, Nodo_A, Nodo_B, Caminho), displayList(Caminho).

filterOperadoras([], Paragens,Acc,Res) :- appendToList(Acc, [], Res).
filterOperadoras([Oper|Tail], Paragens, Acc, Res) :-
        findall(paragem(A,B,C,D,E,F,Oper,H,I,J), paragem(A,B,C,D,E,F,Oper,H,I,J), Filtered),
        appendToList(Filtered, Acc, R1),
        filterOperadoras(Tail, Paragens, R1, Res).


%       Ex_1: calcula_trajeto_operadoras(183, 595, ['Vimeca']) //finds
%       Ex_2: calcula_trajeto_operadoras(183, 595, ['Carris']) //no
%       Ex_2: calcula_trajeto_operadoras(183, 182, ['Vimeca', 'SCoTTURB']) //finds