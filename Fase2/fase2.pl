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
:- dynamic excecao/1.
:- discontiguous utente/10.
:- discontiguous excecao/1.
:- discontiguous (-)/1.
:- discontiguous centro_saude/5.
:- discontiguous nulo/1.
:- discontiguous staff/4.
:- discontiguous vacinacao_Covid/5.


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
-utente(8,372666211,'Cristiano Ronaldo',(05,02,1985),'cr7@gmail.com',253991955,'Madeira','Futebolista',[],2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto para o predicado utente

utente(9,nss_incerto,'Cláudio Coelho',(09,03,2000),'cc@gmail.com',253444343,'Braga','Motorista',[],2).
utente(10,225656292,'Fábio Freitas',(12,06,1998),'ff@gmail.com',253111222,'Taipas',profissao_incerto,['Hipertensão'],3).

excecao(utente(Id,_,N,D,E,Tel,M,P,Dc,Cs)) :-
    utente(Id,nss_incerto,N,D,E,Tel,M,P,Dc,Cs).

excecao(utente(Id,Nss,N,D,E,Tel,M,_,Dc,Cs)) :-
    utente(Id,Nss,N,D,E,Tel,M,profissao_incerto,Dc,Cs).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito impreciso para o predicado utente

excecao(utente(11,252787999,'Cristina Ferreira',(12,02,1984),'cf@gmail.com',Impreciso,'Lisboa','Apresentadora',[],2)) :-
    Impreciso >= 253777777, Impreciso =< 253888888.

excecao(utente(12,112989582,'Carlos Faria',(24,05,1994),'cf@gmail.com',253887454,'Beja','Agricultor',[],Impreciso)) :-
    Impreciso >= 2, Impreciso =< 3.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito interdito para o predicado utente

utente( 13,447565666,'Ismael Lopes',(04,11,1998),'il@gmail.com',253997815,morada_interdito,'Economista',[],1 ).
excecao( utente( Id,Nss,N,D,E,Tel,_,P,Dc,Cs ) ) :-
    utente( Id,Nss,N,D,E,Tel,morada_interdito,P,Dc,Cs ).


nulo( morada_interdito ).


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
% Inserção de conhecimento negativo para o predicado centro_saude
-centro_saude(6,'Centro de saúde de Famalicão','Famalicão',253990139,'csf@gmail.com').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto para o predicado centro_saude

centro_saude(7,'Centro de saúde da PVZ',morada_incerto,253994854,'cspvz@gmail.com').

excecao(centro_saude(Id,N,_,Tel,E)) :-
    centro_saude(Id,N,morada_incerto,Tel,E).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito impreciso para o predicado centro_saude

excecao(centro_saude(8,'Centro de saúde de Beja','Beja',Impreciso,'csbeja@gmail.com')) :-
    Impreciso >= 253000000, Impreciso =< 253999999.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito interdito para o predicado centro_saude

centro_saude(9,'Centro de saúde do Porto','Porto',253747252,email_interdito).
excecao( centro_saude(Id,N,M,Tel,_) ) :-
    centro_saude(Id,N,M,Tel,email_interdito).


nulo( email_interdito ).


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
% Inserção de conhecimento negativo para o predicado staff
-staff(8,3,'Joana Santos','js@gmail.com').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto para o predicado staff

staff(9,1,nome_incerto,'jr@gmail.com').

excecao(staff(Ids,Idc,_,E)) :-
    staff(Ids,Idc,nome_incerto,E).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito impreciso para o predicado staff

excecao(staff(10,Impreciso,'Clara Matias','cm@gmail.com')) :-
    Impreciso >= 1, Impreciso =< 3.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito interdito para o predicado staff

staff(11,3,'Jorge Jerónimo',email_interdito).
excecao( staff(Ids,Idc,N,_) ) :-
    staff(Ids,Idc,N,email_interdito).

nulo( email_interdito ).

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
% Inserção de conhecimento negativo para o predicado vacinacao_Covid
-vacinacao_Covid(3,3,(01,05,2021),'Pfizer',1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto para o predicado vacinacao_Covid

vacinacao_Covid(4,1,data_incerto,'Pfizer',1).

excecao(vacinacao_Covid(S,U,_,V,T)) :-
    vacinacao_Covid(S,U,data_incerto,V,T).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito impreciso para o predicado vacinacao_Covid

excecao(vacinacao_Covid(2,4,(25,04,2021),'Astrazeneca',Impreciso)) :-
    Impreciso >= 1, Impreciso =< 2.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito interdito para o predicado vacinacao_Covid

vacinacao_Covid(staff_interdito,6,(01,05,2021),'Astrazeneca',1).
excecao( vacinacao_Covid(_,U,D,V,T) ) :-
    vacinacao_Covid(staff_interdito,U,D,V,T).

nulo( staff_interdito ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes estruturais: nao permitir a insercao de conhecimento
%                          repetido nem inváilido, nem a remoção de conhecimento inexistente

% Utente - Id (Inserção)
+utente(Id,_,_,_,_,_,_,_,_,_) ::
       (solucoes(Id,
       (utente(Id,_,_,_,_,_,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Utente - Id (Remoção)
-utente(Id,_,_,_,_,_,_,_,_,_) ::
       (solucoes(Id,
       (utente(Id,_,_,_,_,_,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Utente - Nº Segurança Social (Inserção)
+utente(_,Nss,_,_,_,_,_,_,_,_) ::
       (solucoes(Nss,
       (utente(_,Nss,_,_,_,_,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Id (Inserção)
+centro_saude(IdCS,_,_,_,_) ::
       (solucoes(IdCS,
       (centro_saude(IdCS,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Id (Remoção)
-centro_saude(IdCS,_,_,_,_) ::
       (solucoes(IdCS,
       (centro_saude(IdCS,_,_,_,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Telefone (Inserção)
+centro_saude(_,_,_,Tel,_) ::
       (solucoes(Tel,
       (centro_saude(_,_,_,Tel,_)),S),
       comprimento(S,N),
       N == 1).

% Centro de Saúde - Email (Inserção)
+centro_saude(_,_,_,_,Email) ::
       (solucoes(Email,
       (centro_saude(_,_,_,_,Email)),S),
       comprimento(S,N),
       N == 1).

% Staff - Id (Inserção)
+staff(Id,_,_,_) ::
      (solucoes(Id,
      staff(Id,_,_,_),S),
      comprimento(S,N),
      N == 1).

% Staff - Id (Remoção)
-staff(Id,_,_,_) ::
      (solucoes(Id,
      staff(Id,_,_,_),S),
      comprimento(S,N),
      N == 1).

% Staff - Email (Inserção)
+staff(_,_,_,Email) ::
      (solucoes(Email,staff(_,_,_,Email),S),
      comprimento(S,N),
      N == 1).

% Vacinação - Toma válida (Inserção)
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
% REGISTAR/REMOVER UTENTE

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs) :-
             evolucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)).

removeUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs) :-
             involucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)).

% Pelo tipo (positivo ou negativo)

registaUtente(Type,Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs) :-
             evolucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs),Type).

removeUtente(Type,Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs) :-
             involucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs),Type).


% Com conhecimento imperfeito incerto
% -> NSS

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Incerto) :-
             Valor == incerto,
             Incerto == nss,
             evolucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)),
             inserir((excecao(utente(I,_,Nm,Dt,Em,Tl,Mr,Pf,Dcr,Csd)) :-
                  utente(I,Nss,Nm,Dt,Em,Tl,Mr,Pf,Dcr,Csd))).

removeUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Incerto) :-
             Valor == incerto,
             Incerto == nss,
             involucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)),
             remover((excecao(utente(I,_,Nm,Dt,Em,Tl,Mr,Pf,Dcr,Csd)) :-
                  utente(I,Nss,Nm,Dt,Em,Tl,Mr,Pf,Dcr,Csd))).

% -> Profissão

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Incerto) :-
             Valor == incerto,
             Incerto == profissao,
             evolucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)),
             inserir((excecao(utente(I,Ns,Nm,Dt,Em,Tl,Mr,_,Dcr,Csd)) :-
                  utente(I,Ns,Nm,Dt,Em,Tl,Mr,Prof,Dcr,Csd))).

removeUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Incerto) :-
             Valor == incerto,
             Incerto == profissao,
             involucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)),
             remover((excecao(utente(I,Ns,Nm,Dt,Em,Tl,Mr,_,Dcr,Csd)) :-
                  utente(I,Ns,Nm,Dt,Em,Tl,Mr,Prof,Dcr,Csd))).

% Com conhecimento imperfeito impreciso
% -> Telefone

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Incerto == telefone,
             inserir((excecao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)) :-
                  Telefone >= Inicio,
                  Telefone =< Fim)).

removeUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Incerto == telefone,
             remover((excecao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)) :-
                  Telefone >= Inicio,
                  Telefone =< Fim)).

% -> Centro de Saúde

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Incerto == centro,
             inserir((excecao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)) :-
                  Cs >= Inicio,
                  Cs =< Fim)).

removeUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Incerto == centro,
             remover((excecao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)) :-
                  Cs >= Inicio,
                  Cs =< Fim)).

% Com conhecimento imperfeito interdito
% -> Morada

registaUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Interdito) :-
             Valor == interdito,
             Incerto == morada,
             evolucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)),
             inserir((excecao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)) :-
                  utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs))),
             inserir(nulo(Mor)).

removeUtente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs,Valor,Interdito) :-
             Valor == interdito,
             Incerto == morada,
             involucao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)),
             remover((excecao(utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs)) :-
                  utente(Id,Nss,Nome,Data,Email,Tel,Mor,Prof,Dc,Cs))),
             remover(nulo(Mor)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% REGISTAR/REMOVER CENTRO DE SAÚDE

registaCentro(Id,Nome,Mor,Tel,Email) :-
             evolucao(centro_saude(Id,Nome,Mor,Tel,Email)).

removeCentro(Id,Nome,Mor,Tel,Email) :-
             involucao(centro_saude(Id,Nome,Mor,Tel,Email)).

% Pelo tipo (positivo ou negativo)

registaCentro(Type,Id,Nome,Mor,Tel,Email) :-
             evolucao(centro_saude(Id,Nome,Mor,Tel,Email),Type).

removeCentro(Type,Id,Nome,Mor,Tel,Email) :-
             involucao(centro_saude(Id,Nome,Mor,Tel,Email),Type).

% Com conhecimento imperfeito incerto
% -> Morada

registaCentro(Id,Nome,Mor,Tel,Email,Valor,Incerto) :-
             Valor == incerto,
             Incerto == morada,
             evolucao(centro_saude(Id,Nome,Mor,Tel,Email)),
             inserir((excecao(centro_saude(I,N,_,T,E)) :-
                    centro_saude(I,N,Mor,T,E))).

removeCentro(Id,Nome,Mor,Tel,Email,Valor,Incerto) :-
              Valor == incerto,
              Incerto == morada,
              involucao(centro_saude(Id,Nome,Mor,Tel,Email)),
              remover((excecao(centro_saude(I,N,_,T,E)) :-
                    centro_saude(I,N,Mor,T,E))).

% Com conhecimento imperfeito impreciso
% -> Telefone

registaCentro(Id,Nome,Mor,Tel,Email,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Impreciso == telefone,
             inserir((excecao(centro_saude(Id,Nome,Mor,Tel,Email)) :-
                    Tel >= Inicio,
                    Tel =< Fim)).

removeCentro(Id,Nome,Mor,Tel,Email,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Impreciso == telefone,
             remover((excecao(centro_saude(Id,Nome,Mor,Tel,Email)) :-
                    Tel >= Inicio,
                    Tel =< Fim)).

% Com conhecimento imperfeito interdito
% -> Email

registaCentro(Id,Nome,Mor,Tel,Email,Valor,Interdito) :-
             Valor == interdito,
             Interdito == email,
             evolucao(centro_saude(Id,Nome,Mor,Tel,Email)),
             inserir((excecao(centro_saude(Id,Nome,Mor,Tel,Email)) :-
                    centro_saude(Id,Nome,Mor,Tel,Email))),
             inserir(nulo(Email)).

removeCentro(Id,Nome,Mor,Tel,Email,Valor,Interdito) :-
             Valor == interdito,
             Interdito == email,
             involucao(centro_saude(Id,Nome,Mor,Tel,Email)),
             remover((excecao(centro_saude(Id,Nome,Mor,Tel,Email)) :-
                    centro_saude(Id,Nome,Mor,Tel,Email))),
             remover(nulo(Email)).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% REGISTAR/REMOVER STAFF

registaStaff(Id,Idcentro,Nome,Email) :-
            evolucao(staff(Id,Idcentro,Nome,Email)).

removeStaff(Id,Idcentro,Nome,Email) :-
            involucao(staff(Id,Idcentro,Nome,Email)).

% Pelo tipo (positivo ou negativo)

registaStaff(Type,Id,Idcentro,Nome,Email) :-
             evolucao(staff(Id,Idcentro,Nome,Email),Type).

removeStaff(Type,Id,Idcentro,Nome,Email) :-
             involucao(staff(Id,Idcentro,Nome,Email),Type).

% Com conhecimento imperfeito incerto
% -> Nome

registaStaff(Id,Idcentro,Nome,Email,Valor,Incerto) :-
             Valor == incerto,
             Incerto == nome,
             evolucao(staff(Id,Idcentro,Nome,Email)),
             inserir((excecao(staff(I,Ic,_,E)) :-
               staff(I,Ic,Nome,E))).

removeStaff(Id,Idcentro,Nome,Email,Valor,Incerto) :-
            Valor == incerto,
            Incerto == nome,
            involucao(staff(Id,Idcentro,Nome,Email)),
            remover((excecao(staff(I,Ic,_,E)) :-
              staff(I,Ic,Nome,E))).

% Com conhecimento imperfeito impreciso
% -> Centro de saúde

registaCentro(Id,Idcentro,Nome,Email,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Impreciso == centro,
             inserir((excecao(staff(Id,Idcentro,Nome,Email)) :-
                    Idcentro >= Inicio,
                    Idcentro =< Fim)).

removeCentro(Id,Idcentro,Nome,Email,Valor,Impreciso,Inicio,Fim) :-
             Valor == impreciso,
             Impreciso == centro,
             remover((excecao(staff(Id,Idcentro,Nome,Email)) :-
                    Idcentro >= Inicio,
                    Idcentro =< Fim)).

% Com conhecimento imperfeito interdito
% -> Email

registaStaff(Id,Idcentro,Nome,Email,Valor,Interdito) :-
             Valor == interdito,
             Interdito == email,
             evolucao(staff(Id,Idcentro,Nome,Email)),
             inserir((excecao(staff(Id,Idcentro,Nome,Email)) :-
               staff(Id,Idcentro,Nome,Email))),
             inserir(nulo(Email)).

removeStaff(Id,Idcentro,Nome,Email,Valor,Interdito) :-
             Valor == interdito,
             Interdito == email,
             involucao(staff(Id,Idcentro,Nome,Email)),
             remover((excecao(staff(Id,Idcentro,Nome,Email)) :-
               staff(Id,Idcentro,Nome,Email))),
             remover(nulo(Email)).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% REGOSTAR/REMOVER VACINAÇÃO

registaVacinacao(Idstaff,Idutente,Data,Vac,T) :-
                evolucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,T)).

removeVacinacao(Idstaff,Idutente,Data,Vac,T) :-
                involucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,T)).

% Pelo tipo (positivo ou negativo)

registaVacinacao(Type,Idstaff,Idutente,Data,Vac,T) :-
                evolucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,T),Type).

removeVacinacao(Type,Idstaff,Idutente,Data,Vac,T) :-
                involucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,T),Type).

% Com conhecimento imperfeito incerto
% -> Data

registaVacinacao(Idstaff,Idutente,Data,Vac,Toma,Valor,Incerto) :-
                Valor == incerto,
                Incerto == data,
                evolucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma)),
                inserir((excecao(vacinacao_Covid(Ids,Idu,_,V,T)) :-
                  vacinacao_Covid(Ids,Idu,Data,V,T))).

removeVacinacao(Idstaff,Idutente,Data,Vac,Toma,Valor,Incerto) :-
                Valor == incerto,
                Incerto == data,
                involucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma)),
                remover((excecao(vacinacao_Covid(Ids,Idu,_,V,T)) :-
                  vacinacao_Covid(Ids,Idu,Data,V,T))).

% Com conhecimento imperfeito interdito
% -> Staff

registaVacinacao(Idstaff,Idutente,Data,Vac,Toma,Valor,Interdito) :-
                Valor == interdito,
                Incerto == staff,
                evolucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma)),
                inserir((excecao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma)) :-
                  vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma))).

removeVacinacao(Idstaff,Idutente,Data,Vac,Toma,Valor,Interdito) :-
                Valor == interdito,
                Incerto == staff,
                involucao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma)),
                remover((excecao(vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma)) :-
                  vacinacao_Covid(Idstaff,Idutente,Data,Vac,Toma))).
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

evolucao(Termo) :- solucoes(Invariante,+Termo::Invariante,Lista),
                     inserir(Termo),
                     teste(Lista).

% com tipo (positivo ou negativo)

evolucao(Termo, Type) :- Type == positivo,
                         solucoes(Invariante,+Termo::Invariante,Lista),
                         inserir(Termo),
                         teste(Lista).

evolucao(Termo, Type) :- Type == negativo,
                         solucoes(Invariante, +(-Termo)::Invariante, Lista),
                         inserir(-Termo),
                         teste(Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a "involução" do conhecimento

involucao( Termo ) :- solucoes(Invariante,-Termo::Invariante,Lista),
                      remover(Termo),
                      teste(Lista).

% com tipo (positivo ou negativo)

involucao(Termo, Type) :- Type == positivo,
                         solucoes(Invariante,-Termo::Invariante,Lista),
                         remover(Termo),
                         teste(Lista).

involucao(Termo, Type) :- Type == negativo,
                         solucoes(Invariante, -(-Termo)::Invariante, Lista),
                         remover(-Termo),
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

solucoesSRep(X,Y,Z1) :- findall(X,Y,Z),
                        list_to_set(Z,Z1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Comprimento da Lista

comprimento(S,N) :- length(S,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pertencer a uma Lista

pertence(H,[H|_]):-!,true.
pertence(X,[H|T]) :- X \= H, pertence(X,T).

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

demo( Questao,verdadeiro ) :- Questao.
demo( Questao,falso ) :- -Questao.
demo( Questao,desconhecido ) :- nao( Questao ), nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :- Questao, !, fail.
nao( _ ).
