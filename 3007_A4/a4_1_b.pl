countZeroFree([],0).
countZeroFree([H|T], X) :-
              \+checkZeroFree(H,1), countZeroFree(T,Y), X is 1+Y;
              checkZeroFree(H,1), countZeroFree(T,Y), X is Y.


checkZeroFree(X,Y):- 10 is X;
                    X < 10, 0 is X;
                    X > 10, X < 100, checkNum(X);
                    X > 10, X is 10*Y*(X div 10);
                    X >= 100, checkZeroFree((X div 10),Y+1).


checkNum(X) :- X =:= (X div 10)*10 .
