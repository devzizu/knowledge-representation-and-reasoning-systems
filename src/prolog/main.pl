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

calcula_trajeto(Nodo_A, Nodo_B, Caminho/CostTime) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        %resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = '>', format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = '>', format('~w', ['\nCaminho:\n']), displayList(Caminho).
        resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

%       Ex_1: calcula_trajeto(183,791, C). 
%       Ex_2: calcula_trajeto(183,595, C).
%       Ex_3: findall(C, calcula_trajeto(183,791, C), AllResults).
%       Ex_4: findall(C, calcula_trajeto(375,791, C), AllResults).

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
calcular_paragens_aux([(Nodo,Carr)|RestoCaminho], Acc, Result) :-
        membro(Nodo, Caminho),
        findall(paragem(Nodo,B,C,D,E,F,G,H,I,J), paragem(Nodo,B,C,D,E,F,G,H,I,J), Paragem),
        calcular_ligacoes(Nodo, [], ListaLigacoes),
        sort(ListaLigacoes, R2),
        lengthList(R2, Tamanho),
        appendToList([Paragem/Tamanho], Acc, R1),
        calcular_paragens_aux(RestoCaminho, R1, Result).

calcular_ligacoes(Nodo, Acc, Result) :-
        findall(C1, ligacao(Nodo, F1, C1), L1),
        findall(C2, ligacao(F2, Nodo, C2), L2),
        appendToList(L1, Acc, R1),
        appendToList(L2, R1, R2),
        flatten(R2,X),
        sort(X, Result).

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

menor_percurso(Nodo_A, Nodo_B, Caminhos) :-
        findall(C, calcula_trajeto(Nodo_A, Nodo_B, C), Caminhos),
        menos_paragens(Caminhos, 9999, L, R1, R2),
        displayGeneral('\nCaminho com menor numero de paragens:\n', R2),
        displayGeneral('Numero de paragens:\n', R1).

menos_paragens([], Min, P, Min, Pres) :- Pres = P, !.
menos_paragens([C1/X|Caminhos], Min, Paragem, Res, Pres) :-
        (
                lengthList(C1, NR_PAR),
                Min > NR_PAR ->
                        A1 is NR_PAR,
                        P1 = C1,
                        menos_paragens(Caminhos, A1, P1, Res, Pres)
                ;
                        menos_paragens(Caminhos, Min, Paragem, Res, Pres)
        ).

%       Ex_1: menor_percurso(183,595,R).

%------------------------------------------------------------------------------------------------------------------
% 6) Escolher o percurso mais rápido (usando critério da distância).

mais_rapido(Nodo_A, Nodo_B, Caminho/CostTime) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

%------------------------------------------------------------------------------------------------------------------
% 7) Escolher o percurso que passe apenas por abrigos com publicidade.

com_publicidade(Nodo_A, Nodo_B, Caminhos) :-
        findall(paragem(A,B,C,D,E,'Yes',G,H,I,J), paragem(A,B,C,D,E,'Yes',G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = '>', format('~w', ['\nCaminho:\n']), displayList(Caminho).

%------------------------------------------------------------------------------------------------------------------
% 8) Escolher o percurso que passe apenas por paragens abrigadas.

com_publicidade(Nodo_A, Nodo_B, Caminhos) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        filtrar_com_abrigo(Paragens, Filtrado),
        resolve_depthFirst(Filtrado, Nodo_A, Nodo_B, Caminho), CostTime = '>', format('~w', ['\nCaminho:\n']), displayList(Caminho).

filtrar_com_abrigo([],Acc,Res) :- appendToList(Acc, [], Res).
filtrar_com_abrigo([paragem(A,B,C,D,'Sem Abrigo',F,G,H,I,J)|RestoDasParagens], Acc, Res) :-
        filtrar_com_abrigo(RestoDasParagens, Acc, Res). 
filtrar_com_abrigo([P|RestoDasParagens], Acc, Res) :-
        appendToList([P], Acc, R1),
        displayGeneral('ab',P), 
        filtrar_com_abrigo(RestoDasParagens, R1, Res).
