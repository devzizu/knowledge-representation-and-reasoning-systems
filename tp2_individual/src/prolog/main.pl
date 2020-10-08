/*
Course: SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 (Projeto Individual)
Author: João Pedro Rodrigues Azevedo A85227
*/

% default flags setup for prolog database
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% Dados para a base de conhecimento obtidos pelo parser
:- include('include/paragensLigacoes').
% Regras auxiliares
:- include('include/auxiliar').
% Algumas heuristicas aplicadas nos algoritmos de procura
:- include('include/heuristicas').
% Alguns algoritmos de pesquisa informada e não-informada utilizados
:- include('include/pathFindingAlgorithms').

%------------------------------------------------------------------------------------------------------------------

                        /* Querys para o Sistema de procura de percursos */

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: calcula_trajeto(Nodo_A, Nodo_B, Caminho/CostTime)
Descrição: O cálculo do trajeto entre dois pontos pode ser obtido através deste predicado indicando o nodo inicial 
e final, sendo obtido o Caminho e o Custo (quando aplicável). Podem ser utilizadas pesquisas BF, DF, A* e Greedy na generalização
de um caminho no sistema de paragens.
*/

% Query 1) Calcular o trajeto entre dois pontos:

calcula_trajeto(Nodo_A, Nodo_B, Caminho/CostTime) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        %resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = 'n/a'.%, format('~w', ['\nCaminho:\n']), displayList(Caminho).
        resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = 'n/a'.%, format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).
        %resolve_greedy(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

% Exemplos de aplicação:

%       Ex_1: calcula_trajeto(183,791, C).
%       Ex_2: calcula_trajeto(183,595, C).
%       Ex_3: calcula_trajeto(354,79, C).
%       Ex_4: findall(C, calcula_trajeto(183,595, C), AllResults).
%       Ex_5: findall(C, calcula_trajeto(183,185, C), AllResults).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: calcula_trajeto_operadoras(Nodo_A, Nodo_B, ListaOperadoras)
Descrição: Este predicado corresponde a uma especialização do calcula_trajeto no sentido em que permite especificar uma lista
de operadoras que devem ser usadas num percurso obrigatoriamente.
*/

% Query 2) Selecionar apenas algumas das operadoras de transporte para um determinado percurso:

calcula_trajeto_operadoras(Nodo_A, Nodo_B, ListaOperadoras) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        incluemOperadoras(ListaOperadoras, Paragens, [], ParagensOperadoras),
        resolve_depthFirst(ParagensOperadoras, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(ParagensOperadoras, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_astar(ParagensOperadoras, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).
        %resolve_greedy(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

% Permite filtrar a lista inicial de todas as paragens, obtendo em Res as que incluem as operadoras especificadas:

incluemOperadoras([], Paragens,Acc,Res) :- appendToList(Acc, [], Res).
incluemOperadoras([Oper|Tail], Paragens, Acc, Res) :-
        findall(paragem(A,B,C,D,E,F,Oper,H,I,J), paragem(A,B,C,D,E,F,Oper,H,I,J), Filtered),
        appendToList(Filtered, Acc, R1),
        incluemOperadoras(Tail, Paragens, R1, Res).
  
% Exemplos de aplicação:
      
%       Ex_1: calcula_trajeto_operadoras(183, 595, ['Vimeca']).
%       Ex_2: calcula_trajeto_operadoras(183, 595, ['Carris']).
%       Ex_3: calcula_trajeto_operadoras(183, 182, ['Vimeca', 'SCoTTURB']).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: calcula_trajeto_excluir_operadoras(Nodo_A, Nodo_B, ListaOperadoras)
Descrição: Este predicado corresponde a uma especialização do calcula_trajeto no sentido em que permite especificar uma lista
de operadoras que devem ser excluidas de um caminho.
*/

% Query 3) Excluir um ou mais operadores de transporte para o percurso

calcula_trajeto_excluir_operadoras(Nodo_A, Nodo_B, ListaOperadoras) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        excluemOperadoras(Paragens, ListaOperadoras, [], ParagensSemOperadoras),
        resolve_depthFirst(ParagensSemOperadoras, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).
        %resolve_greedy(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

% Permite filtrar a lista inicial de todas as paragens, obtendo em Res as que excluem as operadoras especificadas:

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

% Exemplos de aplicação:
      
%       Ex_1: calcula_trajeto_excluir_operadoras(183, 595, ['Vimeca']).
%       Ex_2: calcula_trajeto_excluir_operadoras(183, 595, ['Carris']).
%       Ex_3: calcula_trajeto_excluir_operadoras(183, 595, ['Vimeca', 'SCoTTURB']).
%       Ex_4: calcula_trajeto_excluir_operadoras(183, 182, ['Vimeca', 'SCoTTURB']).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: calcula_trajeto_nr_carreiras(Nodo_A, Nodo_B)
Descrição: Este predicado permite obter, para um determinado trajeto, para cada paragem o seu número de carreiras e
a paragem com mais carreiras.
*/

% Query 4) Identificar quais as paragens com o maior número de carreiras num determinado percurso.

calcula_trajeto_nr_carreiras(Nodo_A, Nodo_B) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho),
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), format('~w', ['\nCaminho:\n']), displayList(Caminho),
        %resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime),
        %resolve_greedy(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime),
        calcular_paragens(Caminho, Reversed),
        inverteLista(Reversed, Normal, []),
        format('~w', ['\n\nParagens:\n\n']), 
        displayListSlash(Normal),
        mais_carreiras_paragens(Normal, -1, P, Res, Pres),
        displayGeneral('\nParagem com maior nr. de carreiras\n', Pres),
        displayGeneral('Nr. de carreiras\n', Res).

% Permite calcular para um dado caminho, para cada paragem do mesmo, o numero de carreiras que por lá passam

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

% Permite obter, para cada nodo, a lista de carreiras que por lá passam

calcular_ligacoes(Nodo, Acc, Result) :-
        findall(C1, ligacao(Nodo, F1, C1), L1),
        findall(C2, ligacao(F2, Nodo, C2), L2),
        appendToList(L1, Acc, R1),
        appendToList(L2, R1, R2),
        flatten(R2,X),
        sort(X, Result).

% Permite obter o numero e a paragem que tem mais carreiras

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

% Exemplos de aplicação:
   
%       Ex_1: calcula_trajeto_nr_carreiras(183,595).
%       Ex_2: calcula_trajeto_nr_carreiras(183,791).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: menor_percurso(Nodo_A, Nodo_B, Caminhos)
Descrição: Este predicado permite calcular o percurso com menor numero de paragens.
*/

% Query 5) Escolher o menor percurso (usando critério menor número de paragens).

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

% Exemplos de aplicação:

%       Ex_1: menor_percurso(183,595,R).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: mais_rapido(Nodo_A, Nodo_B, Caminho/CostTime)
Descrição: Este predicado permite calcular o percurso com menor distancia usando pesquisa informada.
*/

% Query 6) Escolher o percurso mais rápido (usando critério da distância).

mais_rapido(Nodo_A, Nodo_B, Caminho/CostTime) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_astar(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).
        %resolve_greedy(Paragens, Nodo_A, Nodo_B, Caminho/CostTime), format('~w', ['\nCaminho:\n']), displayList(Caminho), displayCost(CostTime).

% Exemplos de aplicação:
   
%       Ex_1: mais_rapido(183,182,C/T).
%       Ex_2: mais_rapido(183,791,C/T).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: com_publicidade(Nodo_A, Nodo_B, Caminhos)
Descrição: Permite filtrar os percursos possiveis naqueles que passam por paragens com publicidade
*/

% Query 7) Escolher o percurso que passe apenas por abrigos com publicidade.

com_publicidade(Nodo_A, Nodo_B, Caminhos) :-
        findall(paragem(A,B,C,D,E,'Yes',G,H,I,J), paragem(A,B,C,D,E,'Yes',G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = 'n/a', format('~w', ['\nCaminho:\n']), displayList(Caminho).
        %resolve_breathFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = 'n/a', format('~w', ['\nCaminho:\n']), displayList(Caminho).

% Exemplos de aplicação:
   
%       Ex_1: com_publicidade(183,595,C).
%       Ex_2: com_publicidade(594,185,C).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: apenas_abrigadas(Nodo_A, Nodo_B, Caminho)
Descrição: Permite filtrar os percursos possiveis naqueles que passam por paragens abrigadas
*/

% Query 8) Escolher o percurso que passe apenas por paragens abrigadas.

apenas_abrigadas(Nodo_A, Nodo_B, Caminho) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        filtrar_com_abrigo(Paragens, [], Filtrado),
        resolve_depthFirst(Filtrado, Nodo_A, Nodo_B, Caminho),
        print(Caminho).%, format('~w', ['\nCaminho:\n']), displayList(Caminho).

filtrar_com_abrigo([],Acc,Res) :- appendToList(Acc, [], Res).
filtrar_com_abrigo([paragem(A,B,C,D,'Sem Abrigo',F,G,H,I,J)|RestoDasParagens], Acc, Res) :-
        filtrar_com_abrigo(RestoDasParagens, Acc, Res), !. 
filtrar_com_abrigo([P|RestoDasParagens], Acc, Res) :-
        appendToList([P], Acc, R1),
        filtrar_com_abrigo(RestoDasParagens, R1, Res).

% Exemplos de aplicação:
  
%       Ex_1: apenas_abrigadas(183,595,C).
%       Ex_2: apenas_abrigadas(628,39,C).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: passar_por(ListaNodos, Nodo_A, Nodo_B)
Descrição: Permite filtrar os percursos possiveis naqueles que passem num conjunto de pontos intermedios (exceto
o nodo inicial e final).
*/

% Query 9) Escolher um ou mais pontos intermédios por onde o percurso deverá passar.

passar_por(ListaNodos, Nodo_A, Nodo_B) :-
        findall(C, calcula_trajeto(Nodo_A, Nodo_B, C), Caminhos),
        map_percursos_nodos(ListaNodos, Caminhos, 0, [], NodosCaminhos),
        first_list(NodosCaminhos, First),
        print(First).

map_percursos_nodos(ListaNodos, [], C, Acc, Res) :- appendToList(Acc, [], Res).
map_percursos_nodos(ListaNodos, [C1|Caminhos], Counter, Acc, Res) :-
        converte_lista_nodos(C1, [], NodosList),
        inverteLista(NodosList, Inv, []),
        remove_head(Inv, HeadRemoved),
        remove_last(HeadRemoved, SubList),
        (
                membros(SubList, ListaNodos) ->
                        appendToList([(Inv, Order)], Acc, R1),
                        Order is Counter + 1,
                        map_percursos_nodos(ListaNodos, Caminhos, Order, R1, Res)
        ;
                map_percursos_nodos(ListaNodos, Caminhos, Counter, Acc, Res)
        ).

converte_lista_nodos([]/_, Acc, Result) :- appendToList(Acc, [], Result).
converte_lista_nodos([(Nodo, Carreira)|Tail]/_, Acc, Result) :-
        appendToList([Nodo], Acc, R1),
        converte_lista_nodos(Tail/_, R1, Result).

% Exemplos de aplicação:
  
%       Ex_1: passar_por([791],183,595). //yes
%       Ex_2: passar_por([999],183,595). //no

%------------------------------------------------------------------------------------------------------------------
                        /* Querys para o Sistema de procura de percursos (extra) */
%------------------------------------------------------------------------------------------------------------------

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: bom_estado(Nodo_A, Nodo_B, Caminhos)
Descrição: Permite filtrar as paragens no calculo de um percurso para aquelas que tem bom estado.
*/

% Query 10) Escolher percurso com paragens com Bom estado de conservação:

bom_estado(Nodo_A, Nodo_B, Caminhos) :-
        findall(paragem(A,B,C,'Bom',E,F,G,H,I,J), paragem(A,B,C,'Bom',E,F,G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), CostTime = 'n/a', format('~w', ['\nCaminho:\n']), displayList(Caminho).

% Exemplos de aplicação:
  
%       Ex_1: bom_estado(183,595,C).
%       Ex_2: bom_estado(939,587,C).

%------------------------------------------------------------------------------------------------------------------

/* 
Predicado: calcula_percurso_informacao(Nodo_A, Nodo_B, Caminho)
Descrição: Calcula um percurso e apresenta as informações relativas a um percurso como a Rua, Freguesia,...
*/

% Query 11) Apresentar as caracteristicas de um percurso (Rua, Freguesia, ...)

calcula_percurso_informacao(Nodo_A, Nodo_B, Caminho) :-
        findall(paragem(A,B,C,D,E,F,G,H,I,J), paragem(A,B,C,D,E,F,G,H,I,J), Paragens),
        resolve_depthFirst(Paragens, Nodo_A, Nodo_B, Caminho), !,
        apresenta_informacoes(Caminho).

apresenta_informacoes([(Node,Carreira)]) :- 
        paragem(Node,B,C,D,E,F,G,CodRua,NomeRua,Freguesia),
        format('\nNodo -> ~w, na rua ~w (codigo:~w) da freguesia ~w,', [Node, NomeRua, CodRua, Freguesia]).
apresenta_informacoes([(Node,Carreira)|Tail]) :-
        paragem(Node,B,C,D,E,F,G,CodRua,NomeRua,Freguesia),
        format('\nNodo -> ~w, na rua ~w (codigo:~w) da freguesia ~w,', [Node, NomeRua, CodRua, Freguesia]),
        apresenta_informacoes(Tail).

% Exemplos de aplicação:
  
% Ex_1: calcula_percurso_informacao(183,791, C).
% Ex_2: calcula_percurso_informacao(183,79, C).