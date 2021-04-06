%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica
% Vacinacao global da populacao portuguesa em contexto COVID

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
%:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/10.
:- dynamic centro_saúde/5.
:- dynamic staff/4.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: Idutente, Nº Segurança_Social,
% Nome, Data_Nasc, Email, Telefone, Morada,
% Profissão, [Doenças_Crónicas], CentroSaúde -> {V,F}

utente(1,123456789,Pedro,19_02_1934,email1,253123451,Almada,Bombeiro,[],1).
utente(2,123123123,Manuel,13_03_1945,email2,253429351,Barcelos,Eng_Civil,[],1).
utente(3,523183123,Carla,02_12_1977,email3,253459320,Coimbra,Jornalista,[],2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}

centro_saúde(1,centro_saude_1,Viana,253456712,emailC1).
centro_saúde(2,centro_saude_2,Viseu,253921733,emailC2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado staff: Idstaff, Idcentro, Nome, email -> {V,F}

staff(1,2,Jose,emailS1).
staff(2,1,Joao,emailS2).
staff(3,1,Maria,emailS3).
staff(4,1,Renata,emailS4).
staff(5,2,Marta,emailS5).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado vacinação_Covid:  Staf, utente, Data, Vacina, Toma -> {V,F}

vacinação_Covid(4,3,23_03_2021,Astrazeneca,1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inserir ou remover predicados

inserir(P) :- assert(P).
remover(P) :- retract(P).
