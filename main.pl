%-------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

% ~/bin/sicstus -l main.pl

:- [conf].

%-------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% Faz reload ao ficheiro 'main.pl'
reload :- consult('main.pl').

% Faz clear ao terminal
clear :- write('\33\[2J').

%-------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

:- dynamic adjudicante/4.   % #IdAd, Nome, NIF, Morada

:- dynamic adjudicataria/4. % #IdAda, Nome, NIF, Morada

:- dynamic contrato/10.     % #IdC, #IdAd, #IdAda, Tipo de Contrato, 
                            % Tipo de Procedimento, Descriçao,
                            % Custo, Preço, Local, Data

-adjudicante(IdAd, Nome, NIF, Morada) :-
    nao(adjudicante(IdAd, Nome, NIF, Morada)),
    nao(excecao(adjudicante(IdAd, Nome, NIF, Morada))).

-adjudicataria(IdAda, Nome, NIF, Morada) :- 
    nao(adjudicataria(IdAda, Nome, NIF, Morada)),
    nao(excecao(adjudicataria(IdAda, Nome, NIF, Morada))).

-contrato(IdC, IdAd, IdAda, TipodeContrato, TipodeProcedimento,
            Descricao, Custo, Preco, Local, Data) :-
    nao(contrato(IdC, IdAd, IdAda, TipodeContrato, TipodeProcedimento,
            Descricao, Custo, Preco, Local, Data)),
    nao(excecao(contrato(IdC, IdAd, IdAda, TipodeContrato, TipodeProcedimento,
            Descricao, Custo, Preco, Local, Data))).

%-------------------------------------------------------------------

% Extensão do Predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V,F,D}
adjudicante(1, 'Municipio de Alto de Basto', 705330336, 'Portugal, Braga, Alto de Basto').
adjudicante(2, 'Municipio de Vila Nova de Famalicao', 01, 'Portugal, Braga, Vila Nova de Famalicao').
adjudicante(3, 'Municipio de Barcelos', 02, 'Portugal, Braga, Barcelos').
adjudicante(4, 'Municipio de Vila Nova de Gaia', 03, 'Portugal, Porto, Vila Nova de Gaia').
adjudicante(5, 'Universidade do Minho', 04, 'Portugal, Braga, Braga').
adjudicante(6, 'Universidade do Porto', 05, 'Portugal, Porto, Porto').
adjudicante(7, 'Associacao Humanitaria de Bombeiros Voluntarios de Vila Nova de Famalicao', 06, 'Portugal, Braga, Vila Nova de Famalicao').
adjudicante(8, 'Universidade Nova de Lisboa', 07, 'Portugal, Lisboa, Lisboa').
adjudicante(9, 'Municipio de Braga', 08, 'Portugal, Braga, Braga').
adjudicante(10, 'Estabelecimento Prisional Regional de Braga', 09, 'Portugal, Braga, Braga').

% Extensão do Predicado adjudicataria: #IdAda, Nome, NIF, Morada -> {V,F,D}
adjudicataria(1, 'Sa Limpa', 101, 'Portugal').
adjudicataria(2, 'Biofluidos - Climatizacao', 102, 'Portugal').
adjudicataria(3, 'Support Evolution', 103, 'Portugal').
adjudicataria(4, 'SecurNet', 104, 'Portugal').
adjudicataria(5, 'Safira Facility Services', 105, 'Inglaterra').
adjudicataria(6, 'Transportes Urbanos de Braga', 106, 'Portugal').
adjudicataria(7, 'Serviurge', 107, 'Portugal').
adjudicataria(8, 'Uninefro', 108, 'Franca').
adjudicataria(9, 'Universidade do Minho', 4, 'Portugal').
adjudicataria(10, 'Reload - Consultoria Informatica', 110, 'Espanha').
adjudicataria(11, 'XXX - Associados - Sociedade de Advogados, SP, RL.', 702675112, 'Portugal').

% Extensão do Predicado contrato: #IdC, #IdAd, #IdAda, Tipo de Contrato, 
                                % Tipo de Procedimento, Descriçao,  
                                % Custo, Preço, Local, Data 
                                % -> {V,F,D}
contrato(1, 1, 11, 'Aquisicao de servicos', 'Consulta Previa', 'Assessoria juridica', 
        70000, 547, 'Alto de Basto', data(11, 02, 2020)).
contrato(2, 1, 11, 'Aquisicao de servicos', 'Consulta Previa', 'Assessoria juridica', 
        1000, 30, 'Alto de Basto', data(12, 05, 2019)).
contrato(3, 4, 1, 'Aquisicao de servicos', 'Concurso Publico', 'Limpeza de instalacoes', 
        15000, 380, 'Gualtar', data(21, 09, 2015)).
contrato(4, 10, 10, 'Aquisicao de bens', 'Ajuste Direto', 'aquisicao de bens informaticos', 
        4500, 25, 'Braga', data(7, 03, 2016)).
contrato(5, 2, 8, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos terapeuticos', 
        10000, 120, 'Vila Nova de Famalicao', data(23, 11, 2003)).
contrato(6, 9, 6, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos de transporte', 
        100000, 400, 'Braga', data(03, 12, 2010)).


%-------------------------------------------------------------------

% Invariante para o Adjudicante ------------------------------------

% Verfica se o NIF é um número válido e único;
% Verfica se o ID inserido é válido, ou seja, se não existe nos adjudicantes;
% Verfica se existe uma entidade adjudicataria com o mesmo NIF e,
% caso exista, valida se tem o mesmo nome e morada;
+adjudicante(Id, Nome, NIF, Morada) :: 
    (
        solucoes((I, Nif), adjudicante(I, N, Nif, M), Lista),
        isNif(NIF),
        parseIdNif(Lista, [], [], IList, NList),
        nao(pertence(Id, IList)),
        nao(pertence(NIF, NList)),
        solucoes((NOME, Mord), adjudicataria(IdAda, NOME, NIF, Mord), L),
        mesmaEnt((Nome, Morada), L)
    ).

% Verfica se existe uma entidade adjudicataria com o mesmo NIF e,
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
    
% evolucao(adjudicante(100, 'Municipio de Alto de Basto', 103, 'Portugal, Braga, Alto de Basto')).


% Invariante para a Adjudicataria ----------------------------------

% Verfica se o NIF é um número válido e único;
% Verfica se o ID inserido é válido, ou seja, se não existe nas adjudicatarias;
% Verfica se existe uma entidade adjudicante com o mesmo NIF e,
% caso exista, valida se tem o mesmo nome e morada;
+adjudicataria(Id, Nome, NIF, Morada) ::
    (
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

% Verifica se o Id é único nos Contratos;
% Verfica se o Tipo de Procedimento é válido;
% Verifica se a Data é válida;
% Verifica se o Valor do contrato é válido, ou seja, não negativo;
% Verifica se o Prazo do contrato é válido, ou seja, positivo;
% Verifica se as entidades dos contratos existem;
+contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, Data) ::
    (
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

% Se o contrato tiver como Tipo de Procedimento 'Ajuste Direto', então: 
% -> Tem de ter valor igual ou inferior a 5000 euros;
% -> O Tipo de Contrato só pode ser 'Aquisicao de servicos', 'Contrato de Aquisicao' ou 'Locação de Bens Moveis';
% -> O Prazo de vigência tem de ser até 1 ano (assumimos o valor de 365 dias) a contar da decisão de adjudicação;
+contrato(IdC, IdAd, IdAda, TC, 'Ajuste Direto', DESC, Va, PR, L, D) :: 
    (
        Va =< 5000,
        pertence(TC, ['Aquisicao de servicos', 'Contrato de Aquisicao', 'Locacao de Bens Moveis']),
        PR =< 365
    ).

% evolucao(contrato(200, 5, 2, 'Aquisicao de servicos', 'Ajuste Direto', 'Assessoria juridica', 6000, 300, 'Alto de Basto', data(12, 05, 2009))).

% Regra dos 3 Anos válida para todos os contratos:
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

% evolucao(contrato(100, 10, 11, 'Aquisicao de servicos', 'Consulta Previa', 'Assessoria juridica', 3000, 1000, 'Alto de Basto', data(12, 05, 2010))).

%-------------------------------------------------------------------
% Queries

% Query 1 ----------------------------------------------------------

%   O Municipio de Braga indicou que celebrou um contrato por Concurso Publico com uma entidade. Desconhece-se qual
%   a entidade a qual foi celebrado o contrato. O contrato foi de Aquisicao de servicos informaticos no valor de 
%   1000 com um prazo de 50 dias.
%   A data do contrato foi a 3 de Março, embora se desconheça o ano.
contrato(301, 9, ent1, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos Informaticos', 
        1000, 50, 'Braga', data(03, 12, ano1)).
excecao(contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, data(DIns, MIns, AINS))) :- 
    contrato(IdC, IdAd, ent1, TC, TP, DESC, Va, PR, L, data(DIns, MIns, ano1)).


% demo(contrato(301, 9, 1, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos Informaticos', 1000, 50, 'Braga', data(03, 12, 2010)), R).


% Query 2 ----------------------------------------------------------

%   A entidade adjucataria diz que celebrou um contrato com o Municipio de Alto de Basto por Concurso Publico. Contudo, a imprensa afirma que 
%   foi por Ajuste Direto. A entidade adjudicante não confirma nem desmente a imprensa.
excecao(contrato(302, 1, 7, 'Aquisicao de servicos', 'Concurso Publico', 'Servicos Eletricos', 
        4000, 100, 'Alto de Basto', data(06, 08, 2012))).
excecao(contrato(302, 1, 7, 'Aquisicao de servicos', 'Ajuste Direto', 'Servicos Eletricos', 
        4000, 100, 'Alto de Basto', data(06, 08, 2012))).

% demo(contrato(302, 1, 7, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos Eletricos', 4000, 100, 'Alto de Basto', data(06, 08, 2012)), R).


% Query 3 ----------------------------------------------------------

%   No contrato celebrado, desconhece-se se a entidade adjudicante é o Municipio de Barcelos ou o Municipio de Braga. A data do contrato
%   ou é 05/2/2019 ou a 07/02/2019.
excecao(contrato(303, 3, 10, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(05, 02, 2019))).
excecao(contrato(303, 3, 10, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(07, 02, 2019))).
excecao(contrato(303, 8, 10, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(05, 02, 2019))).
excecao(contrato(303, 8, 10, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 
        80000, 500, 'Braga', data(07, 02, 2019))).

% demo(contrato(303, 8, 10, 'Aquisicao de bens', 'Concurso Publico', 'Aquisicao de bens moveis', 80000, 500, 'Braga', data(09, 02, 2019)), R).

% Query 4 ----------------------------------------------------------

%   O contrato celebrado entre a adjudicante Associacao Humanitaria de Bombeiros Voluntarios de Vila Nova de Famalicao e a 
%   adjudicataria Support Evolution, tem um valor proximo dos 5000, em Consulta Previa.
excecao(contrato(304, 7, 3, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de atendimento', 
    Valor, 365, 'Vila Nova de Famalicao', data(13, 04, 2018))) :- 
        proximo(5000, Linf, Lsup),
        Valor >= Linf, Valor =< Lsup.

proximo(X, Linf, Lsup) :-
    Linf is X * 0.90,
    Lsup is X * 1.10.

% demo(contrato(304, 7, 3, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de atendimento', 5000, 365, 'Vila Nova de Famalicao', data(13, 04, 2018)), R).

% Query 5 ----------------------------------------------------------

%   O papel do contrato estabelecido entre a empresa Safira Facility Services e a entidade adjudicante Universidade do Porto perdeu-se,
%   pelo que é impossível saber qual o tipo de procedimento adotado.
contrato(305, 6, 5, 'Aquisicao de servicos', tp5, 'Servicos de Limpeza', 
    130000, 500, 'Porto', data(30, 01, 1960)).

excecao(contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, L, data(DIns, MIns, AINS))) :- 
    contrato(IdC,IdAd, IdAda, TC, tp5, DESC, Va, PR, L, data(DIns, MIns, AINS)).
nulo(tp5).
+contrato(IdC, IdAd, IdAda, TC, TP, DESC, Va, PR, Local, data(DIns, MIns, AINS)) :: 
    (
        solucoes(Tp, (contrato(305, I, IA, Tc, Tp, Desc, V, P, L, D), nao(nulo(Tp))), Lista),
        comprimento(Lista, 0)
    ).

% demo(contrato(305, 6, 5, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de Limpeza', 130000, 500, 'Porto', data(30, 01, 1960)),R).
% evolucao(contrato(305, 6, 5, 'Aquisicao de servicos', 'Consulta Previa', 'Servicos de Limpeza', 130000, 500, 'Porto', data(30, 01, 1960))).

% Query 6 ----------------------------------------------------------

% Desconhece-se a morada da entidade adjudicante 'XCorp'
adjudicante(306, 'XCorp', 3000, m6).
excecao(adjudicante(Id, N, Nif, Morada)) :-
    adjudicante(Id, N, Nif, m6).

% demo(adjudicante(306, 'XCorp', 3000, 'Famalicao'), R).

% Query 7 ----------------------------------------------------------

% Sabe-se que a entidade adjudicataria 'NOS' tem como Morada Portugal, Lisboa. Contudo desconhece-se o NIF desta
% sabendo apenas que não é 10101
-adjudicataria(307, 'NOS', 10101, 'Portugal, Lisboa').

adjudicataria(307, 'NOS', n7, 'Portugal, Lisboa').

excecao(adjudicataria(Id, Nome, NIF, Morada)) :-
    adjudicataria(Id, Nome, n7, Morada).

% demo(adjudicataria(307, 'NOS', 10101, 'Portugal, Lisboa'), R).

% Query 8 ----------------------------------------------------------

% O contrato entre a entidade adjudicante 'Universidade do Minho' e a entidade adjudicataria 'Transportes Urbanos 
% de Braga para propocionar viagens aos estudantes dentro da cidade de Braga. Sabe-se que o contrato foi celebrado
% no dia '06-2-2004' e que o prazo deste foi maior que 10 anos, visto que em 2014 ainda se encontrava em exercicio 
% Contudo, o valor exato é desconhecido. O valor do contrato encontra-se proximo dos 200 000 euros.
-contrato(308, 5, 6, 'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de transportes para estudantes', 
Va, Prazo, 'Braga', data(06,02,2004)) :-
    Prazo > 0, Prazo =< (10*365).

contrato(308, 5, 6, 'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de transportes para estudantes',
Va, p8, 'Braga', data(06,02,2004)) :- 
    proximo(200000, Vinf, Vsup), 
    Va >= Vinf, Va =< Vsup.

excecao(contrato(IdC, IdA, IdAda, TC, TP, Desc, V, Pr, Loc, D)) :-
    contrato(IdC, IdA, IdAda, TC, TP, Desc, V, p8, Loc, D).

% demo(contrato(308, 5, 6, 'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de transportes para estudantes', 200000, 20, 'Braga', data(06,02,2004)), R).

%-------------------------------------------------------------------

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%-------------------------------------------------------------------
% Extensão do predicado que permite a involucao do conhecimento

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

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
    