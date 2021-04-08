% human, tiger, neanderthal
boat(1,0,1).
boat(1,1,0).
boat(0,1,1).
boat(2,0,0).
boat(1,0,1).
back(0,0,1).
back(1,0,0).



% human, tiger then neanderthal
moving(state(NH_E,NT_E,NN_E, NH_W,NT_W,NN_W, east), inBoat(H,T,N), state(NH_E1,NT_E1,NN_E1, NH_W1,NT_W1,NN_W1, west) ) :-
boat(H,T,N), NH_E1 is NH_E - H, NH_E1 >= 0,
             NT_E1 is NT_E - T, NT_E1 >= 0,
             NN_E1 is NN_E - N, NN_E1 >= 0,
             NH_W1 is NH_W + H, NT_W1 is NT_W + T, NN_W1 is NN_W + N,
             safe(NH_E1,NT_E1, NH_W1,NT_W1).


 moving(state(NH_E,NT_E,NN_E, NH_W,NT_W,NN_W, west), inBoat(H,T,N), state(NH_E1,NT_E1,NN_E1, NH_W1,NT_W1,NN_W1, east) ) :-
 back(H,T,N), NH_E1 is NH_E + H, NT_E1 is NT_E + T, NN_E1 is NN_E + N,
              NH_W1 is NH_W - H,NH_W1 >= 0,
              NT_W1 is NT_W - T, NT_W1 >= 0,
              NN_W1 is NN_W - N, NN_W1 >= 0,
              safe(NH_E1,NT_E1, NH_W1,NT_W1).



safe(A, B, C, D) :- (A >= B ; A == 0), (C >= D ; C == 0).

doIt(S, S, []).
doIt(S, E, [X | Y]) :- S \= E, moving(S, X, I), doIt(I, E, Y).

isItPossible(X,Y) :- doIt(state(3,2,1,0,0,0,east),state(0,0,0,3,2,1,west),X), length(X,Z), Z =< Y.
