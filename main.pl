%-------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

% ~/bin/sicstus -l main.pl

:- [funcAux].

%-------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%-------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

:- dynamic adjudicante/4.   % #IdAd, Nome, NIF, Morada

:- dynamic adjudicataria/4. % #IdAda, Nome, NIF, Morada

:- dynamic contrato/10.     % #IdC, #IdAd, #IdAda, Tipo de Contrato, 
                            % Tipo de Procedimento, Descricao,
                            % Custo, Preco, Local, Data

% Predicado que permite a representação de conhecimento imperfeito para o adjudicante
-adjudicante(IdAd, Nome, NIF, Morada) :-
    nao(adjudicante(IdAd, Nome, NIF, Morada)),
    nao(excecao(adjudicante(IdAd, Nome, NIF, Morada))).

% Predicado que permite a representação de conhecimento imperfeito para a adjudicataria
-adjudicataria(IdAda, Nome, NIF, Morada) :- 
    nao(adjudicataria(IdAda, Nome, NIF, Morada)),
    nao(excecao(adjudicataria(IdAda, Nome, NIF, Morada))).

% Predicado que permite a representação de conhecimento imperfeito para o contrato
-contrato(IdC, IdAd, IdAda, TipodeContrato, TipodeProcedimento,
            Descricao, Custo, Preco, Local, Data) :-
    nao(contrato(IdC, IdAd, IdAda, TipodeContrato, TipodeProcedimento,
            Descricao, Custo, Preco, Local, Data)),
    nao(excecao(contrato(IdC, IdAd, IdAda, TipodeContrato, TipodeProcedimento,
            Descricao, Custo, Preco, Local, Data))).

%-------------------------------------------------------------------

% Extensão do Predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V,F,D}
adjudicante(1, 'Estado Portugues', 1, 'Portugal').
adjudicante(2, 'Municipio de Guimaraes', 505948605, 'Portugal, Braga, Guimaraes').
adjudicante(3, 'Municipio de Alto de Basto', 705330336, 'Portugal, Braga, Alto de Basto').
adjudicante(4, 'Municipio de Vila Nova de Famalicao', 506663264, 'Portugal, Braga, Vila Nova de Famalicao').
adjudicante(5, 'Municipio de Barcelos', 505584760, 'Portugal, Braga, Barcelos').
adjudicante(6, 'Municipio de Vila Nova de Gaia', 505335018, 'Portugal, Porto, Vila Nova de Gaia').
adjudicante(7, 'Universidade do Minho', 502011378, 'Portugal, Braga, Braga').
adjudicante(8, 'Faculdade de Engenharia da Universidade do Porto', 600027716, 'Portugal, Porto, Porto').
adjudicante(9, 'Universidade Nova de Lisboa', 501559094, 'Portugal, Lisboa, Lisboa').
adjudicante(10, 'Municipio de Braga', 506901173, 'Portugal, Braga, Braga').
adjudicante(11, 'AAUM-Associacao Academica da Universidade do Minho', 500741093, 'Portugal, Braga, Braga').

% Extensão do Predicado adjudicataria: #IdAda, Nome, NIF, Morada -> {V,F,D}
adjudicataria(1, 'SA LIMPA - SOCIEDADE DE LIMPEZAS, LDA.', 504458086, 'Portugal').
adjudicataria(2, 'Vodafone', 506862747, 'Portugal').
adjudicataria(3, 'Universidade do Minho', 502011378, 'Portugal').
adjudicataria(4, 'TUB - Empresa Transportes Urbanos de Braga', 504807684, 'Portugal').
adjudicataria(5, 'Diversey Portugal-Sistemas de Higiene e Limpeza Unipessoal, Lda', 500086753, 'Portugal').
adjudicataria(6, 'Arada - Engenharia e Gestao de Empreitadas, Lda.', 502189150, 'Portugal').
adjudicataria(7, 'ARRIVA Portugal - Transportes Lda.', 504426974, 'Portugal').
adjudicataria(8, 'MEO - Servicos de Comunicacao e multimedia, S. A.', 203755030, 'Portugal').
adjudicataria(9, 'XXX - Associados - Sociedade de Advogados, SP, RL.', 702675112, 'Portugal').

% Extensão do Predicado contrato: #IdC, #IdAd, #IdAda, Tipo de Contrato, 
                                % Tipo de Procedimento, Descriçao,  
                                % Custo, Preço, Local, Data 
                                % -> {V,F,D}
contrato(1, 2, 3, 'Aquisicao de servicos', 'Ajuste Direto', 'Aquisicao de servicos para a elaboracao do estudo urbano sobre a zona norte da Cidade', 
        5000, 112, 'Guimaraes', data(19, 11, 2019)).
contrato(2, 1, 8, 'Aquisicao de servicos', 'Concurso Publico', 'Aquisicao de servicos de telecomunicacoes', 
        20000, 100, 'Lisboa', data(12, 05, 2019)).
contrato(3, 7, 1, 'Aquisicao de bens moveis', 'Consulta Previa', 'Aquisicao de materiais de Limpeza', 
        74000, 400, 'Gualtar', data(01, 08, 2019)).
contrato(4, 3, 6, 'Empreitadas de obras publicas', 'Concurso Publico', 'Obras em salas de aulas do campus de Gualtar', 
        5000000, 1000, 'Alto de Basto', data(7, 03, 2016)).
contrato(5, 11, 4, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos de transporte', 
        50000, 120, 'Braga', data(23, 11, 2003)).
contrato(6, 7, 9, 'Aquisicao de servicos', 'Ajuste Direto', 'Servicos de assessoria', 
        3800, 25, 'Braga', data(03, 12, 2010)).

%-------------------------------------------------------------------

% Invariante para o Adjudicante ------------------------------------

% Verfica se o NIF é um número válido e único;
% Verfica se o ID inserido é válido, ou seja, se não existe nos adjudicantes;
% Verfica se existe uma entidade adjudicataria com o mesmo NIF e,
% caso exista, valida se tem o mesmo nome e morada;
+adjudicante(Id, Nome, NIF, Morada) :: 
    (
        integer(Id),
        solucoes((I, Nif), adjudicante(I, N, Nif, M), Lista),
        isNif(NIF),
        parseIdNif(Lista, [], [], IList, NList),
        nao(pertence(Id, IList)),
        nao(pertence(NIF, NList)),
        solucoes((NOME, Mord), adjudicataria(IdAda, NOME, NIF, Mord), L),
        mesmaEnt((Nome, Morada), L)
    ).

% Verifica se existe uma entidade adjudicataria com o mesmo NIF e,
% caso exista, valida se tem o mesmo nome e morada;
mesmaEnt(N, []).
mesmaEnt(N, [N]).

% Recebe uma lista com pares Id e NIF e faz parse desta, 
% devolvendo duas listas, retirando o último elemento;
parseIdNif([(Id, Nif)], IList, NList, I, N) :- I = IList, N = NList.
parseIdNif([(Id, Nif)|T], IList, NList, I, N) :- 
    parseIdNif(T, [Id|IList], [Nif|NList], I, N).

% Verifica se o Nif é válido;
isNif(X) :- 
    X >= 0,
    X =< 999999999.
    
% evolucao(adjudicante(10, 'Municipio de Alto de Basto', 103, 'Portugal, Braga, Alto de Basto')).


% Invariante para a Adjudicataria ----------------------------------

% Verfica se o NIF é um número válido e único;
% Verfica se o ID inserido é válido, ou seja, se não existe nas adjudicatarias;
% Verfica se existe uma entidade adjudicante com o mesmo NIF e,
% caso exista, valida se tem o mesmo nome e morada;
+adjudicataria(Id, Nome, NIF, Morada) ::
    (
        integer(Id),
        solucoes((I, Nif), adjudicataria(I, N, Nif, M), Lista),
        isNif(NIF),
        parseIdNif(Lista, [], [], IList, NList),
        nao(pertence(Id, IList)),
        nao(pertence(NIF, NList)),
        solucoes((NOME, Mord), adjudicante(IdAd, NOME, NIF, Mord), L),
        mesmaEnt((Nome, Morada), L)
    ).

% evolucao(adjudicataria(100, 'Universidade do Porto', 05, 'Portugal, Porto, Porto')). 


% Invariantes para o Contrato --------------------------------------

% Verifica se os Id's são inteiros;
% Verifica se o IdC é único nos Contratos;
% Verfica se o Tipo de Procedimento é válido;
% Verifica se a Data é válida;
% Verifica se o Valor do contrato é válido, ou seja, não negativo;
% Verifica se o Prazo do contrato é válido, ou seja, positivo;
% Verifica se as entidades dos contratos existem;
+contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, Data) ::
    (
        integer(IdC), integer(IdAd), integer(IdAda),
        solucoes(IdC, contrato(IdC, Id, IdA, Tc, Tp, Desc, V, Prazo, Local, D), Lista),
        tiraUltimo(Lista, [], R),
        nao(pertence(IdC, R)),
        tipoProcedimento(TP),
        Data,
        Va >= 0,
        PR > 0,
        solucoes(IdAd, adjudicante(IdAd, N, Nif, M), ListIdAd),
        comprimento(ListIdAd, 1),
        solucoes(IdAda, adjudicataria(IdAda, N, Nif, M), ListIdAda),
        comprimento(ListIdAda, 1)
    ).

% Retorna a lista sem o último elemento;
tiraUltimo([X], Acc, R) :- R = Acc.
tiraUltimo([H|T], Acc, R) :- tiraUltimo(T, S, R), S = [H|Acc].


% Contratos por Ajuste Direto --------------------------------------

% Se o contrato tiver como Tipo de Procedimento 'Ajuste Direto'
% -> Se o Tipo de Contrato for 'Aquisicao de servicos', 'Contrato de Aquisicao' ou 'Locação de Bens Moveis', então
% Tem de ter valor igual ou inferior a 5 000 euros;
% -> Senão o valor tem de ser igual ou inferior a 10 000 euros ('Empreitadas de obras publicas');
% -> O Prazo de vigência tem de ser até 1 ano (assumimos o valor de 365 dias) a contar da decisão de adjudicação;
% NOTA: pode haver outro tipos de contrato
+contrato(IdC, IdAd, IdAda, TC, 'Ajuste Direto', DESC, Va, PR, L, D) :: 
    (
        PR =< 365,
        (TC == 'Empreitadas de obras publicas') -> Va =< 10000 ;
        (pertence(TC, ['Aquisicao de servicos', 'Aquisicao de Bens Moveis', 'Locacao de Bens Moveis'])) -> Va =< 5000
    ).

% evolucao(contrato(200, 5, 2, 'Empreitadas de obras publicas', 'Ajuste Direto', 'Assessoria juridica', 7000, 300, 'Alto de Basto', data(12, 05, 2009))).


% Contratos por Consulta Previa ------------------------------------

% Se o contrato tiver como Tipo de Procedimento 'Consulta Previa' e
% o Tipo de Contrato ser 'Aquisicao de servicos', 'Contrato de Aquisicao' ou 'Locação de Bens Moveis', então: 
% -> Tem de ter valor inferior a 75 000 euros;
% Se o contrato tiver como Tipo de Procedimento 'Consulta Previa' e
% o Tipo de Contrato ser 'Empreitadas de obras publicas', então: 
% -> Tem de ter valor inferior a 150 000 euros;
% Se o contrato tiver como Tipo de Procedimento 'Consulta Previa' e
% o Tipo de Contrato ser de outro tipo (relativamente aos dois invariantes de cima), então: 
% -> Tem de ter valor inferior a 100 000 euros;
+contrato(IdC, IdAd, IdAda, TC, 'Consulta Previa', DESC, Va, PR, L, D) :: 
    (
        (TC == 'Empreitadas de obras publicas') -> Va < 150000 ;
        (pertence(TC, ['Aquisicao de servicos', 'Aquisicao de Bens Moveis', 'Locacao de Bens Moveis'])) -> Va < 75000 ; Va < 100000
    ).

% evolucao(contrato(200, 5, 2, 'outro tipo', 'Consulta Previa', 'Assessoria juridica', 86000, 300, 'Alto de Basto', data(12, 05, 2009))).


% Contratos por Concurso Publico -----------------------------------

% Se o contrato tiver como Tipo de Procedimento 'Concurso Publico' e
% o Tipo de Contrato ser 'Empreitadas de obras publicas', então: 
% -> Tem de ter valor inferior a 5 350 000 euros;
% Se o contrato tiver como Tipo de Procedimento 'Concurso Publico',
% o Tipo de Contrato ser 'Aquisicao de servicos', 'Aquisicao de Bens Moveis', 'Locacao de Bens Moveis' e
% a entidade adjudicante ser o Estado (Id == 1) 
% -> Tem de ter valor inferior a 139 000 euros;
% Se o contrato tiver como Tipo de Procedimento 'Concurso Publico',
% o Tipo de Contrato ser 'Aquisicao de servicos', 'Aquisicao de Bens Moveis', 'Locacao de Bens Moveis' e
% a entidade adjudicante não ser o Estado (Id \== 1) 
% -> Tem de ter valor inferior a 214 000 euros;
+contrato(IdC, IdAd, IdAda, TC, 'Concurso Publico', DESC, Va, PR, L, D) :: 
    (
        (TC == 'Empreitadas de obras publicas') -> Va < 5350000 ;
        (pertence(TC, ['Aquisicao de servicos', 'Aquisicao de Bens Moveis', 'Locacao de Bens Moveis']) , IdAd == 1)
            -> Va < 139000 ; 
        (pertence(TC, ['Aquisicao de servicos', 'Aquisicao de Bens Moveis', 'Locacao de Bens Moveis']) , IdAd \== 1) 
            -> Va < 214000
    ).


% evolucao(contrato(200, 1, 2, 'Aquisicao de servicos', 'Concurso Publico', 'Assessoria juridica', 100000, 300, 'Alto de Basto', data(12, 05, 2009))).


% Regra dos 3 Anos válida para todos os contratos ------------------

% Uma entidade adjudicante não pode convidar a mesma empresa para um contrato com prestações de serviço (Tipo de Contrato)
% do mesmo tipo às de contratos anteriores no ano económico em curso e nos dois anteriores, sempre que:
% -> O valor das somas dos contrato celebrados seja igual ou superior a 75000;
+contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, data(DIns, MIns, AINS)) ::
    (   
        solucoes((Ano, V), contrato(I, IdAd, IdAda, TC, Tp, Desc, V, Prazo, Local, data(Dia, Mes, Ano)), Lista),
        c3Anos(AINS, Lista, -Va, N),
        N < 75000
    ).

% Verifica se a lista recebida [(Ano, Valor)] tem contratos no mesmo ano ou nos dois anteriores relativamente
% ao ano do contrato recebido (AInser).
% A função recebe no campo de Valor, -Va, pois não queremos somar o valor do contrato que se pretende
% celebrar.
c3Anos(AInser, [], Acc, R) :- R is Acc. 
c3Anos(AInser, [ (Ano, V)|T ], Acc, R) :- 
    ((AInser =:= Ano; AInser =:= Ano+1; AInser =:= Ano+2) -> H is (V+Acc) ; H is Acc),
    c3Anos(AInser, T, H, R).

% evolucao(contrato(102, 7, 1, 'Aquisicao de bens moveis', 'Consulta Previa', 'Assessoria juridica', 3000, 1000, 'Alto de Basto', data(12, 05, 2020))).


% Invariantes para Involução ---------------------------------------

% Só é possível remover um adjudicante caso ele não tenha nenhum contrato
-adjudicante(Id, Nome, NIF, Morada) :: 
    (
        solucoes(IdC, contrato(IdC, Id, IdAda, TC, TP, DESC, Va, PR, L, D), Lista),
        comprimento(Lista, N),
        N == 0
    ).

% Só é possível remover um adjudicataria caso ele não tenha nenhum contrato
-adjudicataria(Id, Nome, NIF, Morada) :: 
    (
        solucoes(IdC, contrato(IdC, IdAd, Id, TC, TP, DESC, Va, PR, L, D), Lista),
        comprimento(Lista, N),
        N == 0
    ).

% involucao(adjudicataria(7, 'ARRIVA Portugal - Transportes Lda.', 504426974, 'Portugal')).
% involucao(adjudicante(200000, 'Municipio de Guimaraes', 505948605, 'Portugal, Braga, Guimaraes')).

%-------------------------------------------------------------------

% Funcionalidades

% Funcionalidade 1 ----------------------------------------------------------

%   O Municipio de Braga indicou que celebrou um contrato por Concurso Publico com uma entidade. Desconhece-se qual
%   a entidade adjucataria que foi celebrado o contrato. O contrato foi de Aquisicao de servicos informaticos no valor de 
%   1000 com um prazo de 50 dias.
%   A data do contrato foi a 3 de Março, embora se desconheça o ano.
contrato(301, 10, ent1, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos Informaticos', 
        1000, 50, 'Braga', data(03, 12, ano1)).
excecao(contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, data(DIns, MIns, AINS))) :- 
    contrato(IdC, IdAd, ent1, TC, TP, DESC, Va, PR, L, data(DIns, MIns, ano1)).


% demo(contrato(301, 10, 1, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos Informaticos', 1000, 50, 'Braga', data(03, 12, 2010)), R).


% Funcionalidade 2 ----------------------------------------------------------

%   A entidade adjucataria diz que celebrou um contrato com o Municipio de Alto de Basto por Concurso Publico. Contudo, a imprensa afirma que 
%   foi por Ajuste Direto. A entidade adjudicante não confirma nem desmente nenhum dos valores.
excecao(contrato(302, 3, 7, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos de transporte', 
        4000, 100, 'Alto de Basto', data(06, 08, 2012))).
excecao(contrato(302, 3, 7, 'Aquisicao de servicos', 'Ajuste Direto', 'Servicos de transporte', 
        4000, 100, 'Alto de Basto', data(06, 08, 2012))).

% demo(contrato(302, 3, 7, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de transporte', 4000, 100, 'Alto de Basto', data(06, 08, 2012)), R).


% Funcionalidade 3 ----------------------------------------------------------

%   No contrato celebrado, desconhece-se se a entidade adjudicante é o Municipio de Barcelos ou o Municipio de Braga. A data do contrato
%   ou é 05/2/2019 ou a 07/02/2019.
excecao(contrato(303, 5, 9, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(05, 02, 2019))).
excecao(contrato(303, 5, 9, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(07, 02, 2019))).
excecao(contrato(303, 10, 9, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(05, 02, 2019))).
excecao(contrato(303, 10, 9, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(07, 02, 2019))).

% demo(contrato(303, 5, 9, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 80000, 500, 'Braga', data(05, 02, 2019)), R).

% Funcionalidade 4 ----------------------------------------------------------

%   O contrato celebrado entre a adjudicante Municipio de Vila Nova de Famalicao e a 
%   adjudicataria Vodafone, tem um valor proximo dos 5000, em Consulta Previa.
excecao(contrato(304, 4, 2, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de telecomonicacoes', 
    Valor, 365, 'Vila Nova de Famalicao', data(13, 04, 2018))) :- 
        proximo(5000, Linf, Lsup),
        Valor >= Linf, Valor =< Lsup.

proximo(X, Linf, Lsup) :-
    Linf is X * 0.90,
    Lsup is X * 1.10.

% demo(contrato(304, 4, 2, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de telecomonicacoes', 50000, 365, 'Vila Nova de Famalicao', data(13, 04, 2018)), R).

% Funcionalidade 5 ----------------------------------------------------------

%   O papel do contrato estabelecido entre a empresa 'Diversey Portugal-Sistemas de Higiene e Limpeza Unipessoal, Lda' 
%   e a entidade adjudicante 'Faculdade de Engenharia da Universidade do Porto' perdeu-se,
%   pelo que é impossível saber qual o tipo de procedimento adotado.
contrato(id5, 8, 5, 'Aquisicao de servicos', tp5, 'Servicos de Limpeza', 
    1000, 500, 'Porto', data(30, 01, 1960)).

excecao(contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, D)) :- 
    contrato(id5,IdAd, IdAda, TC, tp5, DESC, Va, PR, L, D).
nulo(tp5).
+contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, Local, data(DIns, MIns, AINS)) :: 
    (
        solucoes(Tp, (contrato(305, I, IA, Tc, Tp, Desc, V, P, L, D), nao(nulo(Tp))), Lista),
        comprimento(Lista, 0)
    ).

% demo(contrato(305, 8, 5, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de Limpeza', 1000, 500, 'Porto', data(30, 01, 1960)),R).
% evolucao(contrato(305, 8, 5, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de Limpeza', 1000, 500, 'Porto', data(30, 01, 1960))).

% Funcionalidade 6 ----------------------------------------------------------

% Desconhece-se a morada da entidade adjudicante 'XCorp'
adjudicante(306, 'XCorp', 3000, m6).
excecao(adjudicante(Id, N, Nif, Morada)) :-
    adjudicante(Id, N, Nif, m6).

% demo(adjudicante(306, 'XCorp', 3000, 'Famalicao'), R).

% Funcionalidade 7 ----------------------------------------------------------

% Sabe-se que a entidade adjudicataria 'NOS' tem como Morada Portugal, Lisboa. Contudo desconhece-se o NIF desta
% sabendo apenas que não é 10101
-adjudicataria(307, 'NOS', 10101, 'Portugal, Lisboa').
adjudicataria(307, 'NOS', n7, 'Portugal, Lisboa').
excecao(adjudicataria(Id, Nome, NIF, Morada)) :-
    adjudicataria(Id, Nome, n7, Morada).

% demo(adjudicataria(307, 'NOS', 101, 'Portugal, Lisboa'), R).

% Funcionalidade 8 ----------------------------------------------------------

% O contrato entre a entidade adjudicante 'AAUM-Associacao Academica da Universidade do Minho' e a entidade adjudicataria 'Transportes Urbanos 
% de Braga serviu para propocionar viagens aos estudantes dentro da cidade de Braga. Sabe-se que o contrato foi celebrado
% no dia '06-2-2004' e que o prazo deste foi maior que 10 anos, visto que em 2014 ainda se encontrava em exercicio 
% Contudo, o valor exato do prazo é desconhecido. O valor do contrato encontra-se proximo dos 200 000 euros.
-contrato(308, 11, 4, 'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de transportes para estudantes', 
Va, Prazo, 'Braga', data(06,02,2004)) :-
    Prazo > 0, Prazo =< (10*365).

contrato(308, 11, 4, 'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de transportes para estudantes',
Va, p8, 'Braga', data(06,02,2004)) :- 
    proximo(200000, Vinf, Vsup), 
    Va >= Vinf, Va =< Vsup.

excecao(contrato(IdC, IdA, IdAda, TC, TP, Desc, V, Pr, Loc, D)) :-
    contrato(IdC, IdA, IdAda, TC, TP, Desc, V, p8, Loc, D).

% demo(contrato(308, 11, 4, 'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de transportes para estudantes', 200, 20000, 'Braga', data(06,02,2004)), R).

%-------------------------------------------------------------------

% Extensão do predicado que permite a evolução do conhecimento
evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

% Ocorre a inserção do Termo através do "assert". Contudo, se o predicado seguinte falhar, 
% a segunda cláusula é chamada retirando o Termo
insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

% Testa cada elemento da lista, verificando se o elemento falha em algum invariante
teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%-------------------------------------------------------------------

% Extensão do predicado que permite a involução do conhecimento.
% Através da chamada do Termo, verificamos se este existe no conhecimento do sistema.
% Caso não exista, falha o involução e não o acrescenta ao conhecimento.
involucao( Termo ) :-
    Termo,
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

% Predicado simétrico ao insercao. Faz o retract do Termo e, caso falhe o predicado seguinte no involucao,
% chama o assert para voltar a inserir este.
remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.


%-------------------------------------------------------------------
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).
    