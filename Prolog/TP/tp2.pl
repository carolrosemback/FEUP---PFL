fatorial(0,1).
fatorial(N,F):- N>0,
                N1 is N-1,
                fatorial(N1,FN1),
                F is N*FN1.

somaRec(1, 1).
somaRec(N, Sum) :- N>0,
                   N1 is N-1,
                   somaRec(N1, Sum1),
                   Sum is Sum1 +N.

fibbonaci(0,0).
fibbonaci(1,1).
fibbonaci(N,F):- N>0,
                 N1 is N -1, 
                 N2 is N -2,
                 fibbonaci(N1,F1),
                 fibbonaci(N2,F2),
                 F is F1 + F2.    

isPrime(X) :- X1 is X - 1,
              isPrime(X,X1).
 
isPrime(X,1).
isPrime(X,N) :- N > 1,
                X mod N =\= 0,
                N1 is N - 1,
                isPrime(X,N1).

isPrime2(A):- \+((A1 is A-1, between(2,A1,N), 0 is mod(A,N))), \+(A is 1).


% Questão 4:
/* a) True 
b) Syntax Error
c) True
d) H =  pfl, T = [ltw, redes]
e) H =  pfl, T = [ltw]
f) H =  leic, T = []
g) no
h) H = leic, T = [[pfl, ltw, lbaw]]
i) H = leic, T = Two
j) Inst = gram , LEIC = feup
k) One = 1, Two = 2, Tail = [3,4]
l) One = leic, Rest = [Two | Tail] */



% Questão 5:

list_Size([], 0).
list_Size([_|Xs], Size):- list_Size(Xs, Size1),
                          Size is Size1+1.

list_sum([], 0).
list_sum([X|Xs], Sum):- list_sum(Xs, Sum1),
                          Sum is Sum1+X.

list_prod([], 1).
list_prod([X|Xs], Prod):- list_prod(Xs, Prod1),
                          Prod is Prod1*X.                       

inner_prod([],[],0).
inner_prod([X|Xs], [Y|Ys], Result) :- Prod is (X*Y),
                                    inner_prod(Xs, Ys, Remaining),
                                    Result is Prod + Remaining.

count(_,[],0).     
count(Elem, [X|Xs], Result) :- count(Elem, Xs, Result1),
                               (Elem =:= X -> Result is Result1+1; Result is Result1).

% Questão 6:

invert(List, List2) :- reverse(List, List2, []).
reverse([], Acc, Acc). 
reverse( [H|T], List2, Acc) :- reverse(T, List2, [H | Acc]).


/*                               
del_one(Elem,L1,L2) :- append(Result,[Elem|Xs],L1),
                       append(Result,Xs,L2). */
/* 
del_all(_, [], Result).
del_all(Elem, [Elem|Xs], L2) :- del_all(Elem, Xs, L2).
del_all(Elem, [X|Xs], L2) :- append([X], L2, L3),
                             del_all(Elem, Xs, L3).  */    

del_one(_, [],[]).
del_one(Elem, [Elem| Xs], Xs).
del_one(Elem, [X | Xs] , [X | Result]) :- del_one(Elem, Xs, Result).

del_all(_, [],[]).
del_all(Elem, [Elem| Xs], Result):- del_all(Elem, Xs, Result).
del_all(Elem, [X | Xs] , [X | Result]) :- del_all(Elem, Xs, Result).

/* del_all(X,L,L) :- \+member(X,L).
del_all(X,L1,L2) :- del_one(X,L1,L), 
                    del_all(X,L,L2). */

del_all_list([], L,L).
del_all_list([Y | Ys], [Y | Xs], Result):- del_all_list(Ys, Xs, Result).
del_all_list([Y | Ys], [X | Xs] , [X | Result]) :- del_all_list(Ys, Xs, Result).                    


del_dups(L,Res) :- del_dups_aux(L, [], L2),
                 invert(L2, Res).
del_dups_aux([], L,L).
del_dups_aux([H|T],L,L2) :- \+ member(H,L), del_dups_aux(T,[H|L],L2).
del_dups_aux([H|T],L,L2) :- del_dups_aux(T,L,L2).


replicate(N, Elem, List) :- replicate_aux(N, Elem, [] , List).

replicate_aux(0,_, L, L).
replicate_aux(N, Elem,L, List):- N1 is N - 1,
                                 replicate_aux(N1, Elem, [Elem | L], List).


interperse(Elem, List, Res) :- interperse_aux(Elem, List, Aux, List2),
                                invert(List2, Res).

interperse_aux(_, [] , L, L).
interperse_aux(Elem, [X | Xs], Aux, List2) :- interperse_aux(Elem, Xs, [Elem, X | Aux], List2).   

insert_elem(Index, List, Elem, Res):-insert_elem_aux(Index, List, Elem, [], Res).

insert_elem_aux( 1, [X | Xs],Elem,LAux, List2):- append(LAux,[X, Elem|Xs], List2).
insert_elem_aux( Index, [X| Xs],Elem,LAux, List2):- Index1 is Index - 1,
                                                       insert_elem_aux(Index1, Xs, Elem, [X|Aux], List2). 


delete_elem(Index, List, Elem, Res):-delete_elem_aux(Index, List, [], Elem, Res).

delete_elem_aux( 1, [X , Y| Xs],LAux, Elem, List2):- Elem is Y,
                                                     append(LAux,[ X |Xs], List2).
delete_elem_aux( Index, [X| Xs],LAux, Elem, List2):- Index1 is Index - 1,
                                                     delete_elem_aux(Index1, Xs, [X|Aux],Elem, List2). 

replace(List1,Index, Old, New , List2):-replace_aux(List1, Index, Old, New,[], List2).

replace_aux([X, Y|Xs], 1, Old, New, LAux, List2):- Old is Y,
                                                   append(LAux,[X, New|Xs], List2).
replace_aux([X|Xs], Index, Old, New, LAux, List2):- Index1 is Index - 1,
                                                    replace_aux(Xs, Index1 , Old, New, [X | LAux], List2). 
