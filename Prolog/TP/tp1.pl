:-use_module(library(lists)).

female(mary).
male(john).

parent(john, paul).
parent(mary, paul).


%soma dos N numeros 
sumN(N, Sum):- sumN(N, Sum, 0). 
sumN(0, Sum, Sum).
sumN(N, Sum, Acc) :- N>0,
                     N1 is N-1,
                     Acc1 is N+Acc,
                     sumN(N1, Sum, Acc1).

myappend( [ ], L2, L2 ).
myappend( [H|T], L2, [H|T3] ):- myappend( T, L2, T3).


reverse1([], []).
reverse1([X|Xs], Rev):-  reverse1(Xs, Ys),
                        append(Ys, [X], Rev).

reverse2(Xs, Rev):- reverse2(Xs, [], Rev).
reverse2([X|Xs], Acc, Rev):- reverse2(Xs, [X|Acc], Rev).
                            reverse2([], Rev, Rev).


                    female('Grace').
                    male('Frank').
                    female('DeDe').
                    male('Jay').
                    female('Gloria').
                    male('Javier').
                    female('Barb').
                    male('Merle').
                    male('Phill').
                    female('Claire').
                    female('Mitchell').
                    male('Joe').
                    female('Manny').
                    male('Cameron').
                    female('Pameron').
                    male('Bo').
                    male('Dylan').
                    female(haley).
                    female('Alex').
                    male('Luke').
                    female('Lily').
                    male('Rexford').
                    male('Calhoun').
                    male('George').
                    female('Poppy').
                    
                    parent('Grace','Phill').
                    parent('Frank','Phill').
                    parent('DeDe','Claire').
                    parent('Jay','Claire').
                    parent('DeDe','Mitchell').
                    parent('Jay','Mitchell').
                    parent('Gloria','Joe').
                    parent('Jay','Joe').
                    parent('Gloria','Manny').
                    parent('Javier','Manny').
                    parent('Barb','Cameron').
                    parent('Merle','Cameron').
                    parent('Barb','Pameron').
                    parent('Merle','Pameron').
                    parent('Phill','Haley').
                    parent('Claire','Haley').
                    parent('Phill','Alex').
                    parent('Claire','Alex').
                    parent('Phill','Luke').
                    parent('Claire','Luke').
                    parent('Mitchell','Lily').
                    parent('Cameron','Lily').
                    parent('Mitchell','Rexford').
                    parent('Cameron','Rexford').
                    parent('Pameron','Calhoun').
                    parent('Bo','Calhoun').
                    parent('Dylan','George').
                    parent('Haley','George').
                    parent('Dylan','Poppy').
                    parent('Haley','Poppy').                        

father(X,Y) :- parent(X,Y),
               male(X).
        
grandparent(X,Y):- parent(X,Z),
                   parent(Z,Y).

siblings(X,Y) :- parent(P1,X),
                 parent(P2,X),
                 parent(P1,Y),
                 parent(P2,Y),
                 P1\=P2,
                 X\=Y.


cousin(X,Y) :- grandparent(Z,X),
                grandparent(Z, Y),
                \+ siblings(X,Y).