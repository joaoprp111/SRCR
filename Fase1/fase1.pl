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

utente(1,123456789,pedro,(19,02,1934),email1,253123451,almada,bombeiro,[],1).
utente(2,123123123,manuel,(13,03,1945),email2,253429351,barcelos,eng_civil,[],1).
utente(3,523183123,carla,(02,12,1977),email3,253459320,coimbra,jornalista,[],2).
utente(4,256331909,roberto,(21,01,1955),email4,253919559,guimarães,eng_informático,[hipertensão],2).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}

centro_saúde(1,centro_saude_1,viana,253456712,emailC1).
centro_saúde(2,centro_saude_2,viseu,253921733,emailC2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado staff: Idstaff, Idcentro, Nome, email -> {V,F}

staff(1,2,jose,emailS1).
staff(2,1,joao,emailS2).
staff(3,1,maria,emailS3).
staff(4,1,renata,emailS4).
staff(5,2,marta,emailS5).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado vacinação_Covid:  Staf, utente, Data, Vacina, Toma -> {V,F}

vacinacao_Covid(4,3,(23,03,2021),astrazeneca,1).
vacinacao_Covid(5,3,(06,04,2021),astrazeneca,2).
vacinacao_Covid(2,1,(01,04,2021),astrazeneca,1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inserir predicados

inserir(Termo) :- assert(Termo).
inserir(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes estruturais: nao permitir a insercao de conhecimento
%                         repetido

+utente(Id,Niss,Nome,Data,Email,Telefone,Morada,Prof,Doencas,IdCS) ::
       (solucoes((Id,Niss,Nome,Data,Email,Telefone,Morada,Prof,Doencas,IdCS),
        (utente(Id,Niss,Nome,Data,Email,Telefone,Morada,Prof,Doencas,IdCS)),S),
        comprimento(S,N),
        N == 1).

+centro_saúde(IdCS,Nome,Morada,Telefone,Email) ::
              (solucoes((IdCS,Nome,Morada,Telefone,Email),
               centro_saúde(IdCS,Nome,Morada,Telefone,Email),S),
               comprimento(S,N),
               N == 1).

+staff(Id,IdCentro,Nome,Email) ::
             (solucoes((Id,IdCentro,Nome,Email),
             staff(Id,IdCentro,Nome,Email),S),
             comprimento(S,N),
             N == 1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes referenciais: nao permite relacionar uma entidade a outra
%                           que nao exista (aquando da insercao)

+utente(_,_,_,_,_,_,_,_,_,IdCS) ::
       (solucoes(IdsCS,
        (centro_saúde(IdsCS,_,_,_,_)),S),
        pertence(IdCS,S)).

+staff(_,IdC,_,_) ::
       (solucoes(IdsCS,
       (centro_saúde(IdsCS,_,_,_,_)),S),
        pertence(IdC,S)).

+vacinacao_Covid(Staff,Utente,_,_,_) ::
        (solucoes(Staffs,
        (staff(Staffs,_,_,_)),S),
        solucoes(Utentes,
        (utente(Utentes,_,_,_,_,_,_,_,_,_)),S1),
        pertence(Staff,S),
        pertence(Utente,S1)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :- solucoes(Invariante,+Termo::Invariante,Lista),
                     inserir(Termo),
                     teste(Lista).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar pessoas não vacinadas: Utente -> {V,F}

nao_vacinada(X):- not(vacinada(X)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar pessoas vacinadas: Utente -> {V,F}

vacinada(X):- vacinacao_Covid(_,X,_,_,_).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Data atual
date(Day,Month,Year) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar pessoas não vacinadas e candidatas a serem vacinadas: Utente -> {V,F}
idade((_,M,A),I):- date(_,Y,Z), I is Z-A, M<Y.
idade((D,M,A),I):- date(X,Y,Z), I is Z-A, M==Y, D=<X.
idade((D,M,A),I):- date(X,Y,Z), I is Z-A-1, M==Y, D>X.
idade((_,M,A),I):- date(_,Y,Z), I is Z-A-1, M>=Y.

candidata(X):- nao_vacinada(X), utente(X,_,_,_,_,_,_,_,S,_), length(S,N), N>=1.

candidata(X):- nao_vacinada(X), utente(X,_,_,(D,M,A),_,_,_,_,_,_), idade((D,M,A),R), R>=65.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar pessoas que falta a segunda toma: Utente -> {V,F}

falta_2toma(_):- not(nao_falta_2toma).
nao_falta_2toma(X):- vacinacao_Covid(_,X,_,_,Y), Y==2.

retirar(X.[X|T],T).
retirar(X,[H|T],[H|T1]):- retirar(X,T,T1).

inserir(X,L,R):- retirar(X,R,L).

pessoas_vacinadas_centro(Idcentro,L):- solucoes((Idu,_,Nome,_,_,_,_,_,_),(utente(Idu,Nss,Nome,Data,Email,Tel,Mor,Prof,Doencas,Idcentro),vacinacao_Covid(IdS,Idu,DataV,Vacina,T)),L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado solucoes
solucoes(X,P,S) :- findall(X,P,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Comprimento da Lista
comprimento(S,N) :- length(S,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado teste
teste([]).
teste([R|LR]) :- R, teste(LR).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pertencer a uma Lista
pertence(H,[H|_]).
pertence(X,[H|T]) :-
    X \= H,
    pertence(X,T).
