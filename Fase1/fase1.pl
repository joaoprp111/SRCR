%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica
% Vacinacao global da populacao portuguesa em contexto COVID

% Grupo 20
% Guilherme Martins a89532 - Jaime Oliveira a89598 - João Pereira a89607
% José Costa a89519 - Tiago Freitas a89570

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/10.
:- dynamic centro_saude/5.
:- dynamic staff/4.
:- dynamic vacinacao_Covid/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: Idutente, Nº Segurança_Social,
% Nome, Data_Nasc, Email, Telefone, Morada,
% Profissão, [Doenças_Crónicas], CentroSaúde -> {V,F}

utente(1,123456789,'Pedro Oliveira',(19,02,1934),'po@gmail.com',253123451,'Barcelos','Bombeiro',[],1).
utente(2,123123123,'Manuel Faria',(13,03,1945),'mf@gmail.com',253429351,'Barcelos','Medico',[],1).
utente(3,523183123,'Carla Castro',(02,12,1977),'cc@gmail.com',253459320,'Viana do Castelo','Jornalista',[],2).
utente(4,256331909,'Roberto Carlos',(21,01,1955),'rc@gmail.com',253919559,'Guimarães','Engenheiro Informático',['Hipertensão'],2).
utente(5,436329091,'Rita Neves',(21,01,2001),'rn@gmail.com',253010123,'Viana do Castelo','Engenheira de Materiais',[],1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}

centro_saude(1,'Centro de saúde de Viana','Viana do Castelo',253456712,'csv@gmail.com').
centro_saude(2,'Centro de saúde de Guimarães','Guimarães',253921733,'csg@gmail.com').
centro_saude(3,'Centro de saúde de Barcelos','Barcelos',253004239,'csb@gmail.com').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado staff: Idstaff, Idcentro, Nome, email -> {V,F}

staff(1,2,'Jose Sa','js@gmail.com').
staff(2,1,'Joao Marques','jm@gmail.com').
staff(3,1,'Maria Matos','m&m@gmail.com').
staff(4,3,'Renata Peixoto','rp@gmail.com').
staff(5,2,'Marta Domingues','md@gmail.com').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado vacinação_Covid:  Staf, utente, Data, Vacina, Toma -> {V,F}

vacinacao_Covid(4,2,(23,03,2021),'Astrazeneca',1).
vacinacao_Covid(4,2,(06,04,2021),'Astrazeneca',2).
vacinacao_Covid(2,5,(01,04,2021),'Pfizer',1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes estruturais: nao permitir a insercao de conhecimento
%                         repetido (com o mesmo id)

+utente(Id,_,_,_,_,_,_,_,_,_) ::
       (solucoes(Id,
        (utente(Id,_,_,_,_,_,_,_,_,_)),S),
        comprimento(S,N),
        N == 1).

+centro_saude(IdCS,_,_,_,_) ::
              (solucoes(IdCS,
               centro_saúde(IdCS,_,_,_,_),S),
               comprimento(S,N),
               N == 1).

+staff(Id,_,_,_) ::
             (solucoes(Id,
             staff(Id,_,_,_),S),
             comprimento(S,N),
             N == 1).

%não deixar inserir uma toma > 2
+vacinacao_Covid(_,_,_,_,Toma) ::
        Toma =< 2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes referenciais: nao permite relacionar uma entidade a outra
%                           que nao exista (aquando da insercao)
%não deixar inserir um utente ou staff que refere um centro de saude inexistente

+utente(_,_,_,_,_,_,_,_,_,IdCS) ::
       (solucoes(IdsCS,
       (centro_saude(IdsCS,_,_,_,_)),S),
       pertence(IdCS,S)).

+staff(_,IdC,_,_) ::
       (solucoes(IdsCS,
       (centro_saude(IdsCS,_,_,_,_)),S),
       pertence(IdC,S)).

%não deixar inserir a segunda toma sem existir a primeira
+vacinacao_Covid(Staff,Utente,_,_,2) ::
        (solucoes((Staff,Utente,1),
        (vacinacao_Covid(Staff,Utente,_,_,1)),R),
        comprimento(R,N),
        N == 1).

%não deixar tomar a segunda dose com uma vacina diferente da primeira
+vacinacao_Covid(Staff,Utente,_,Nome,2) ::
        (solucoes((Staff,Utente,Nome,1),
        (vacinacao_Covid(Staff,Utente,_,Nome,1)),R),
        comprimento(R,N),
        N == 1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Utentes

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs) :-
             evolucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Centro de Saúde

registaCentro(Id,Nome,Mor,Tel,Email) :-
             evolucao(centro_saude(Id,Nome,Mor,Tel,Email)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Staff

registaStaff(Id,Idcentro,Nome,Email) :-
            evolucao(staff(Id,Idcentro,Nome,Email)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Vacinação

registaVacinacao(Idstaff,Idutente,Data,Vac,T) :-
                evolucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,T)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 1) Definição de fases:
% Fase1 -> medicos, enfermeiros e pessoas >80 com doenças crónicas
%       -> a partir de __/__/____

% Fase2 -> pessoas > 50 com doenças ou >65
%       -> a partir de __/__/____

% Fase3 -> o resto
%       -> a partir de __/__/____

% Lista de profissoes incluídas na fase 1
profissoesFase1(['Medico','Enfermeiro','Medica','Enfermeira']).

dataFase1((1,12,2020)).
dataFase2((1,4,2021)).
dataFase3((1,7,2021)).

fase1(Lista) :- solucoes((X,Nomes),(utente(X,_,Nomes,_,_,_,_,_,_,_),candidata1(X)),Lista).
fase2(Lista) :- solucoes((X,Nomes),(utente(X,_,Nomes,_,_,_,_,_,_,_),candidata2(X)),Lista).
fase3(Lista) :- solucoes((X,Nomes),(utente(X,_,Nomes,_,_,_,_,_,_,_),candidata3(X)),Lista).

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
        solucoes((Ids,Nomes),
        (utente(Ids,_,Nomes,_,_,_,_,_,_,_),nao_vacinada(Ids))
        ,Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 3) Identificar pessoas vacinadas: Utente -> {V,F}

vacinada(X):- vacinacao_Covid(_,X,_,_,_).

vacinadas(Lista) :-
        (solucoesSRep((Ids,Nomes),
        (utente(Ids,_,Nomes,_,_,_,_,_,_,_),vacinada(Ids)),
        Lista)).

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

vacinas_indevidas(Lista) :-
      (solucoesSRep((Ids,Nomes),
      (utente(Ids,_,Nomes,_,_,_,_,_,_,_),vacina_indevida(Ids)),
      Lista)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 5) Identificar pessoas não vacinadas e que são candidatas (por fases)

% Pessoas nao vacinadas e candidatas a fase 1
nao_vacinada1(X) :-
      nao_vacinada(X),
      candidata1(X).

nao_vacinadas1(Lista) :-
      (solucoes((Ids,Nomes),
      (utente(Ids,_,Nomes,_,_,_,_,_,_,_),nao_vacinada1(Ids)),
      Lista)).

% Pessoas nao vacinadas e candidatas a fase 2
nao_vacinada2(X) :-
      nao_vacinada(X),
      candidata2(X).

nao_vacinadas2(Lista) :-
      (solucoes((Ids,Nomes),
      (utente(Ids,_,Nomes,_,_,_,_,_,_,_),nao_vacinada2(Ids)),
      Lista)).

% Pessoas nao vacinadas e candidatas a fase 3
nao_vacinada3(X) :-
      nao_vacinada(X),
      candidata3(X).

nao_vacinadas3(Lista) :-
      (solucoes((Ids,Nomes),
      (utente(Ids,_,Nomes,_,_,_,_,_,_,_),nao_vacinada3(Ids)),
      Lista)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 6) Identificar pessoas que falta a segunda toma: Utente -> {V,F}

falta_2toma(X):-
      (solucoes(Ys,
      (vacinacao_Covid(_,X,_,_,Ys)),
      Res),
      comprimento(Res,N),
      N == 1,
      [H|_] = Res,
      H == 1).

falta_2tomaLista(Lista) :-
      (solucoes((Ids,Nomes),
      (utente(Ids,_,Nomes,_,_,_,_,_,_,_),falta_2toma(Ids)),
      Lista)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 7) Extensao do sistema de inferencia si: Questao, (Valor -> {V,F}}
si(Questao,verdadeiro) :-
    Questao.
si(Questao,falso) :-
    nao(Questao).

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
% Lista das pessoas que tomaram uma determinada vacina
pessoas_vacina(Vacina,L) :- solucoesSRep(Nome,
                            (vacinacao_Covid(_,Idu,_,Vacina,_),
                            utente(Idu,_,Nome,_,_,_,_,_,_,_)),
                            L).

pessoas_staff(Staff,(NomeStaff,Nomes)):-
  	solucoesSRep(N,(vacinacao_Covid(Staff,U,_,_,_),utente(U,_,N,_,_,_,_,_,_,_)),Nomes),
    staff(Staff,NomeStaff,_,_).

vacinacao_completa(R) :- solucoesSRep((Idu,Nome),
                         (vacinacao_Covid(_,Idu,_,_,2),
                         utente(Idu,_,Nome,_,_,_,_,_,_,_)),
                         R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inserir predicados

inserir(Termo) :- assert(Termo).
inserir(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :- solucoes(Invariante,+Termo::Invariante,Lista),
                     inserir(Termo),
                     teste(Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado teste
teste([]).
teste([R|LR]) :- R, teste(LR).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Mostrar registos
mostrarRegistos(P) :- listing(P).
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
% Predicado solucoes
solucoes(X,P,S) :- findall(X,P,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado solucoes sem repetiçoes
solucoesSRep(X,Y,Z1) :-
        findall(X,Y,Z),
        list_to_set(Z,Z1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Comprimento da Lista
comprimento(S,N) :- length(S,N).

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
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( _ ).
