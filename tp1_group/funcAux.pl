%-------------------------------------------------------------------
% Extensões de Predicados

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% Faz reload ao ficheiro 'main.pl'
reload :- consult('main.pl').

% Faz clear ao terminal
clear :- write('\33\[2J').

% Extensão do predicado data valida: Data -> {V,F}
data(Dia, Mes, Ano) :- anoV(Ano), mesV(Mes), diaV(Dia, Mes).


% Extensão do predicado anoV: Ano -> {V,F}
anoV(Ano) :- integer(Ano), Ano > 1900 , Ano =< 2021.


% Extensão do predicado mesV: Mes -> {V,F}
mesV(Mes) :- integer(Mes), Mes =< 12.


% Extensão do predicado diaV: Dia, Mes, Ano -> {V,F}
% Meses com 31 dias
diaV(Dia, Mes) :- pertence(Mes, [1, 3, 5, 7, 8, 10, 12]),
                  integer(Dia),
                  Dia =< 31.
% Meses com 30 ou menos dias, no caso de Fevereiro
diaV(Dia, Mes) :- pertence(Mes, [4, 6, 9, 11]),
                  integer(Dia),
                  Dia =< 30.
% Fevereiro nunca tem mais do que 29 dias
diaV(Dia, 02) :- Dia =< 29.


% Extensão do predicado tipoProcedimento: TipoProcedimento -> {V,F}
tipoProcedimento(X) :- pertence(X, ['Ajuste Direto', 'Consulta Previa', 'Concurso Publico']).

%-------------------------------------------------------------------
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%-------------------------------------------------------------------
% Funções auxiliares

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).
    