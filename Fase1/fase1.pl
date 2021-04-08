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
utente(2,123123123,manuel,(13,03,1945),email2,253429351,barcelos,medico,[],1).
utente(3,523183123,carla,(02,12,1977),email3,253459320,coimbra,jornalista,[],2).
utente(4,256331909,roberto,(21,01,1955),email4,253919559,guimarães,eng_informático,[hipertensão],2).
utente(5,436329091,rodrigo,(21,01,2001),email5,253010123,vila_do_conde,eng_materiais,[],1).


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
%                         repetido (com o mesmo id)

+utente(Id,_,_,_,_,_,_,_,_,_) ::
       (solucoes(Id,
        (utente(Id,_,_,_,_,_,_,_,_,_)),S),
        comprimento(S,N),
        N == 1).

+centro_saúde(IdCS,_,_,_,_) ::
              (solucoes(IdCS,
               centro_saúde(IdCS,_,_,_,_),S),
               comprimento(S,N),
               N == 1).

+staff(Id,_,_,_) ::
             (solucoes(Id,
             staff(Id,_,_,_),S),
             comprimento(S,N),
             N == 1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes referenciais: nao permite relacionar uma entidade a outra
%                           que nao exista (aquando da insercao)
% Exemplo: inserir um utente que refere um centro de saude inexistente

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
% 1) Definição de fases:
% Fase1 -> medicos, enfermeiros e pessoas >80 com doenças crónicas
%       -> a partir de __/__/____

% Fase2 -> pessoas > 50 com doenças ou >65
%       -> a partir de __/__/____

% Fase3 -> o resto
%       -> a partir de __/__/____

% Lista de profissoes incluídas na fase 1
profissoesFase1([medico,enfermeiro]).

dataFase1((1,12,2020)).
dataFase2((1,4,2021)).
dataFase3((1,7,2021)).

fase1(Lista) :- solucoes(X,candidata1(X),Lista).
fase2(Lista) :- solucoes(X,candidata2(X),Lista).
fase3(Lista) :- solucoes(X,candidata3(X),Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verificar se uma pessoa é candidata a uma fase de vacinação: Utente -> {V,F}

candidata1(Id):-
      utente(Id,_,_,_,_,_,_,P,_,_),
      profissoesFase1(Ps),
      pertence(P,Ps).
candidata1(Id) :-
      utente(Id,_,_,(D,M,A),_,_,_,P,Ds,_),
      idade((D,M,A),R),
      R >= 80,
      comprimento(Ds,N),
      N >= 1,
      profissoesFase1(Ps),
      nao(pertence(P,Ps)).

candidata2(Id) :-
      utente(Id,_,_,(D,M,A),_,_,_,_,Ds,_),
      idade((D,M,A),R),
      R >= 50,
      R < 65,
      comprimento(Ds,N),
      N >= 1,
      nao(candidata1(Id)).
candidata2(Id) :-
      utente(Id,_,_,(D,M,A),_,_,_,_,_,_),
      idade((D,M,A),R),
      R >= 65,
      nao(candidata1(Id)).

candidata3(Id) :-
      utente(Id,_,_,_,_,_,_,_,_,_),
      nao(candidata1(Id)),
      nao(candidata2(Id)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 2) Identificar pessoas não vacinadas: Utente -> {V,F}

nao_vacinada(X):- nao(vacinada(X)).

nao_vacinadas(Lista) :-
        solucoes(Ids,
        (utente(Ids,_,_,_,_,_,_,_,_,_),nao_vacinada(Ids))
        ,Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 3) Identificar pessoas vacinadas: Utente -> {V,F}

vacinada(X):- vacinacao_Covid(_,X,_,_,_).

vacinadas(Lista) :-
        solucoesSRep(Ids,
        (utente(Ids,_,_,_,_,_,_,_,_,_),vacinada(Ids)),
        Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 4) Identificar pessoas vacinadas indevidamente: Utente -> {V,F}

vacina_indevida(X) :-
        vacinada(X),
        candidata1(X),
        vacinacao_Covid(_,X,(D,M,A),_,_),
        dataFase1((D1,M1,A1)),
        anterior((D,M,A),(D1,M1,A1)).
vacina_indevida(X) :-
        vacinada(X),
        candidata2(X),
        vacinacao_Covid(_,X,(D,M,A),_,_),
        dataFase2((D1,M1,A1)),
        anterior((D,M,A),(D1,M1,A1)).
vacina_indevida(X) :-
        vacinada(X),
        candidata3(X),
        vacinacao_Covid(_,X,(D,M,A),_,_),
        dataFase3((D1,M1,A1)),
        anterior((D,M,A),(D1,M1,A1)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Data atual
date(Day,Month,Year) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verificar se uma data é anterior a outra

anterior((_,_,A1),(_,_,A2)) :- A1 < A2.
anterior((_,M1,A1),(_,M2,A2)) :- A1 == A2, M1 < M2.
anterior((D1,M1,A1),(D2,M2,A2)) :- A1 == A2, M1 == M2, D1 < D2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular a idade de um utente
idade((_,M,A),I):- date(_,Y,Z), I is Z-A, M<Y.
idade((D,M,A),I):- date(X,Y,Z), I is Z-A, M==Y, D=<X.
idade((D,M,A),I):- date(X,Y,Z), I is Z-A-1, M==Y, D>X.
idade((_,M,A),I):- date(_,Y,Z), I is Z-A-1, M>=Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 6) Identificar pessoas que falta a segunda toma: Utente -> {V,F}

falta_2toma(_):- not(nao_falta_2toma).
nao_falta_2toma(X):- vacinacao_Covid(_,X,_,_,Y), Y==2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Lista das pessoas vacinadas num determinado centro de saúde

pessoas_vacinadas_centro(Idcentro,L):- solucoesSRep((Idu,Nome),
                                       (utente(Idu,_,Nome,_,_,_,_,_,_,Idcentro),
                                       vacinacao_Covid(_,Idu,_,_,_)),
                                       L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Lista das diferentes vacinas dadas num determinado centro de saúde

vacinas_centro(Idcentro,L):- solucoesSRep(Vacina,
                             (vacinacao_Covid(_,_,_,Vacina,_),
                             staff(_,Idcentro,_,_)),
                             L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado solucoes
solucoes(X,P,S) :- findall(X,P,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado solucoes sem repetiçoes
solucoesSRep(X,Y,Z1) :-
        findall(X,Y,Z),
        sort(Z,Z1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Comprimento da Lista
comprimento(S,N) :- length(S,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado teste
teste([]).
teste([R|LR]) :- R, teste(LR).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pertencer a uma Lista
pertence(H,[H|_]):-!,true.
pertence(X,[H|T]) :-
    X \= H,
    pertence(X,T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Concatenar uma lista
append([ ], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do sistema de inferencia si: Questao, (Valor -> {V,F}}
si(Questao,verdadeiro) :-
    Questao.
si(Questao,falso) :-
    nao(Questao).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( _ ).
