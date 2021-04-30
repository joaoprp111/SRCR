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
:- dynamic '-'/1.
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
% Negação forte do predicado utente
-utente(Id,Nss,N,D,E,Tel,M,P,Dc,Cs) :- nao(utente(Id,Nss,N,D,E,Tel,M,P,Dc,Cs)),
                                       nao(excecao(utente(Id,Nss,N,D,E,Tel,M,P,Dc,Cs))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inserção de conhecimento negativo para o predicado utente
-utente(8,'Cristiano Ronaldo',(05,02,1985),'cr7@gmail.com',253991955,'Madeira','Futebolista',[],2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto para o predicado utente

utente(9,'Cláudio Coelho',(09,03,2000),'ccgmail.com',253444343,'Braga','Motorista',[],incerto).

excecao(utente(Id,Nss,N,D,E,Tel,M,P,Dc,Cs)) :-
    utente(Id,Nss,N,D,E,Tel,M,P,Dc,incerto).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito impreciso para o predicado utente

excecao(utente(10,'Cristina Ferreira',(12,02,1984),'cfgmail.com',Impreciso,'Lisboa','Apresentadora',[],2)) :-
    Impreciso >= 253777777, Impreciso =< 253888888.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito interdito para o predicado utente




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}

centro_saude(1,'Centro de saúde de Viana','Viana do Castelo',253456712,'csv@gmail.com').
centro_saude(2,'Centro de saúde de Guimarães','Guimarães',253921733,'csg@gmail.com').
centro_saude(3,'Centro de saúde de Barcelos','Barcelos',253004239,'csb@gmail.com').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Negação forte do predicado centro_saúde

-centro_saude(Id,N,M,Tel,E) :- nao(centro_saude(Id,N,M,Tel,E)),
                               nao(excecao(centro_saude(Id,N,M,Tel,E))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado staff: Idstaff, Idcentro, Nome, email -> {V,F}

staff(1,2,'Jose Sa','js@gmail.com').
staff(2,1,'Joao Marques','jm@gmail.com').
staff(3,1,'Maria Matos','m&m@gmail.com').
staff(4,3,'Renata Peixoto','rp@gmail.com').
staff(5,2,'Marta Domingues','md@gmail.com').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Negação forte do predicado staff

-staff(Ids,Idc,N,E) :- nao(staff(Ids,Idc,N,E)),
                       nao(excecao(staff(Ids,Idc,N,E))).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado vacinação_Covid:  Staff, Utente, Data, Vacina, Toma -> {V,F}

vacinacao_Covid(4,2,(23,03,2021),'Astrazeneca',1).
vacinacao_Covid(4,2,(06,04,2021),'Astrazeneca',2).
vacinacao_Covid(2,5,(01,04,2021),'Pfizer',1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Negação forte do predicado vacinação_Covid

-vacinacao_Covid(S,U,D,V,T) :- nao(vacinacao_Covid(S,U,D,V,T)),
                               nao(excecao(vacinacao_Covid(S,U,D,V,T))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes estruturais: nao permitir a insercao de conhecimento
%                          repetido nem inváilido

% Utente - Id
+utente(Id,_,_,_,_,_,_,_,_,_) ::
       (solucoes(Id,
        (utente(Id,_,_,_,_,_,_,_,_,_)),S),
        comprimento(S,N),
        N == 1).

% Utente - Nº Segurança Social
+utente(_,Nss,_,_,_,_,_,_,_,_) ::
       (solucoes(Nss,
       (utente(_,Nss,_,_,_,_,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Id
+centro_saude(IdCS,_,_,_,_) ::
       (solucoes(IdCS,
       (centro_saude(IdCS,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Telefone
+centro_saude(_,_,_,Tel,_) ::
       (solucoes(Tel,
       (centro_saude(_,_,_,Tel,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Email
+centro_saude(_,_,_,_,Email) ::
       (solucoes(Email,
       (centro_saude(_,_,_,_,Email)),S),
       comprimento(S,N),
       N == 1).

% Staff - Id
+staff(Id,_,_,_) ::
      (solucoes(Id,
      staff(Id,_,_,_),S),
      comprimento(S,N),
      N == 1).

% Staff - Email
+staff(_,_,_,Email) ::
      (solucoes(Email,staff(_,_,_,Email),S),
      comprimento(S,N),
      N == 1).

% Vacinação - Toma válida
+vacinacao_Covid(_,_,_,_,T) ::
      (T >=1,T =< 2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes referenciais: nao permite relacionar uma entidade a outra
%                           que nao exista (aquando da insercao)

% Utente exige um Centro de Saúde existente
+utente(_,_,_,_,_,_,_,_,_,IdCS) ::
       (solucoes(IdsCS,
       (centro_saude(IdsCS,_,_,_,_)),S),
       pertence(IdCS,S)).

% Staff exige um Centro de Saúde existente
+staff(_,IdC,_,_) ::
       (solucoes(IdsCS,
       (centro_saude(IdsCS,_,_,_,_)),S),
       pertence(IdC,S)).

% Vacinação exige Staff existente
+vacinacao_Covid(Staff,_,_,_,_) ::
        (solucoes(IdS,
        (staff(IdS,_,_,_)),S),
        pertence(Staff,S)).

% Vacinação exige Utente existente
+vacinacao_Covid(_,U,_,_,_) ::
        (solucoes(IdU,
        (utente(IdU,_,_,_,_,_,_,_,_,_)),S),
        pertence(U,S)).

% Segunda dose de uma vacina exige uma primeira já existente
+vacinacao_Covid(Staff,Utente,_,_,2) ::
        (solucoes((Staff,Utente,1),
        (vacinacao_Covid(Staff,Utente,_,_,1)),R),
        comprimento(R,N),
        N == 1).

% Segunda dose de uma vacina exige uma vacina igual à primeira
+vacinacao_Covid(Staff,Utente,_,Nome,2) ::
        (solucoes((Staff,Utente,Nome,1),
        (vacinacao_Covid(Staff,Utente,_,Nome,1)),R),
        comprimento(R,N),
        N == 1).

% Primeira dose de uma vacina exige que esse Utente nao tenha sido vacinado ainda
+vacinacao_Covid(_,U,_,_,1) ::
        (solucoes(U,
        (vacinacao_Covid(_,U,_,_,_)),R),
        comprimento(R,N),
        N == 1).

% Segunda dose de uma vacina exige que seja depois da primeira (data)
+vacinacao_Covid(_,U,Data2,V,2) ::
        (solucoes((D1,M1,A1),
        (vacinacao_Covid(_,U,(D1,M1,A1),V,1)),R),
        head(R,X),
        anterior(X,Data2)).

% Utente só pode tomar vacina no seu centro de saúde

+vacinacao_Covid(S,U,_,_,_) ::
         (solucoes(Cs,(staff(S,Cs,_,_),utente(U,_,_,_,_,_,_,_,_,Cs)),R),
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
%       -> a partir de 1/12/2020

% Fase2 -> pessoas > 50 com doenças ou >65
%       -> a partir de 1/4/2021

% Fase3 -> o resto
%       -> a partir de 1/7/2021

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
% Extensao do sistema de inferencia si: Questao, (Valor -> {V,F}}
si(Questao,verdadeiro) :- Questao.
si(Questao,falso) :- -Questao.
si(Questao,desconhecido) :- nao(Questao), nao(-Questao).

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

% Lista das pessoas que receberam a vacina por uma determinada pessoa do staff
pessoas_staff(Staff,(NomeStaff,Nomes)) :- solucoesSRep(N,
                                          (vacinacao_Covid(Staff,U,_,_,_),
                                          utente(U,_,N,_,_,_,_,_,_,_)),
                                          Nomes),
                                          staff(Staff,NomeStaff,_,_).


% Lista das pessoas que têm a vacinação completa
vacinacao_completa(R) :- solucoesSRep((Idu,Nome),
                         (vacinacao_Covid(_,Idu,_,_,2),
                         utente(Idu,_,Nome,_,_,_,_,_,_,_)),
                         R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inserir predicados

inserir(Termo) :- assert(Termo).
inserir(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover predicados

remover(Termo) :- retract(Termo).
remover(Termo) :- assert(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :- solucoes(Invariante,+Termo::Invariante,Lista),
                     inserir(Termo),
                     teste(Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento

involucao( Termo ) :- solucoes(Invariante,-Termo::Invariante,Lista),
                      remover(Termo),
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
% Cabeça de uma lista
head([H],H).
head([H|_],H).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Concatenar uma lista
append([ ], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
    Questao, !, fail.
nao( _ ).
