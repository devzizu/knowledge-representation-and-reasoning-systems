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

%------------------------------------------------------------------------------------------------------------------
% 1) Calcular o trajeto entre dois pontos:

calcula_trajeto(Nodo_A, Nodo_B) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

%       Ex_1: calcula_trajeto(183,594). 
%       Ex_2: calcula_trajeto(183,595).

%------------------------------------------------------------------------------------------------------------------
% 2) Selecionar apenas algumas das operadoras de transporte para um determinado percurso:

calcula_trajeto_operadoras(Nodo_A, Nodo_B, ListaOperadoras) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        incluemOperadoras(ListaOperadoras, Paragens, [], ParagensOperadoras),
        resolve_depthFirst(ParagensOperadoras, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(ParagensOperadoras, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_astar(ParagensOperadoras, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

incluemOperadoras([], Paragens,Acc,Res) :- appendToList(Acc, [], Res).
incluemOperadoras([Oper|Tail], Paragens, Acc, Res) :-
        findall(paragem(A,B,C,D,E,F,Oper,H,I,J), paragem(A,B,C,D,E,F,Oper,H,I,J), Filtered),
        appendToList(Filtered, Acc, R1),
        incluemOperadoras(Tail, Paragens, R1, Res).
        
%       Ex_1: calcula_trajeto_operadoras(183, 595, ['Vimeca']) //finds
%       Ex_2: calcula_trajeto_operadoras(183, 595, ['Carris']) //no
%       Ex_3: calcula_trajeto_operadoras(183, 182, ['Vimeca', 'SCoTTURB']) //finds

%------------------------------------------------------------------------------------------------------------------
% 3) Excluir um ou mais operadores de transporte para o percurso

calcula_trajeto_excluir_operadoras(Nodo_A, Nodo_B, ListaOperadoras) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        excluemOperadoras(Paragens, ListaOperadoras, [], ParagensSemOperadoras),
        resolve_depthFirst(ParagensSemOperadoras, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

excluemOperadoras([], _, Acc, Res) :- appendToList(Acc, [], Res).
excluemOperadoras(Paragens, [], Acc, Res) :- appendToList(Acc, [], Res).
excluemOperadoras([paragem(A,B,C,D,E,F,Oper,H,I,J)|RestoDasParagens], ListaOperadoras, Acc, Res) :-
        (
                membro(Oper, ListaOperadoras) ->
                        excluemOperadoras(RestoDasParagens, ListaOperadoras, Acc, Res)
                ; 
                appendToList([paragem(A,B,C,D,E,F,Oper,H,I,J)], Acc, R1),
                excluemOperadoras(RestoDasParagens, ListaOperadoras, R1, Res)
        ).

%       Ex_1: calcula_trajeto_excluir_operadoras(183, 595, ['Vimeca'])
%       Ex_2: calcula_trajeto_excluir_operadoras(183, 595, ['Carris'])
%       Ex_3: calcula_trajeto_excluir_operadoras(183, 182, ['Vimeca', 'SCoTTURB'])

%------------------------------------------------------------------------------------------------------------------
% 4) Identificar quais as paragens com o maior número de carreiras num determinado percurso.

calcula_trajeto_nr_carreiras(Nodo_A, Nodo_B) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho),
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho),
        %resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).
        calcular_paragens(Caminho, Reversed),
        inverteLista(Reversed, Normal, []),
        format('~w', ['\n\nParagens:\n\n']), 
        displayListSlash(Normal),
        mais_carreiras_paragens(Normal, -1, P, Res, Pres),
        displayGeneral('\nParagem com maior nr. de carreiras\n', Pres),
        displayGeneral('Nr. de carreiras\n', Res).

calcular_paragens([], []).
calcular_paragens(Caminho, Paragens) :-
        calcular_paragens_aux(Caminho, [], Paragens).

calcular_paragens_aux([], Acc, Result) :- appendToList(Acc, [], Result).
calcular_paragens_aux([Nodo|RestoCaminho], Acc, Result) :-
        membro(Nodo, Caminho),
        findall(paragem(Nodo,B,C,D,E,F,G,H,I,J), paragem(Nodo,B,C,D,E,F,G,H,I,J), Paragem),
        calcular_ligacoes(Nodo, [], ListaLigacoes),
        lengthList(ListaLigacoes, Tamanho),
        appendToList([Paragem/Tamanho], Acc, R1),
        calcular_paragens_aux(RestoCaminho, R1, Result).

calcular_ligacoes(Nodo, Acc, Result) :-
        findall(ligacao(Nodo, F1, C1), ligacao(Nodo, F1, C1), Ligacao1),
        findall(ligacao(F2, Nodo, C2), ligacao(F2, Nodo, C2), Ligacao2),
        appendToList(Ligacao1, Acc, R1),
        appendToList(Ligacao2, R1, Result).

mais_carreiras_paragens([], Max, P, Max, Pres) :- Pres = P, !.
mais_carreiras_paragens([[P]/NR_CARR|RestoDasParagens], Max, Paragem, Res, Pres) :-
        (
                Max =< NR_CARR ->
                        A1 is NR_CARR,
                        P1 = P,
                        mais_carreiras_paragens(RestoDasParagens, A1, P1, Res, Pres)
                ;
                        mais_carreiras_paragens(RestoDasParagens, Max, Paragem, Res, Pres)
        ).

%       Ex_1: calcula_trajeto_nr_carreiras(183,595).

%------------------------------------------------------------------------------------------------------------------
% 5) Escolher o menor percurso (usando critério menor número de paragens).

