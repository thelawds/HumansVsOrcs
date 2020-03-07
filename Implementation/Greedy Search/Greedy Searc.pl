% Gteedy Search implementation:
:- ['../General_Base'].

% Heuristic Function
h_cost(Curr, Cost):-
	Curr = [X,Y],
	t(Xt, Yt),
	Cost #=  abs(X - Xt) + abs(Y - Yt).

% Search for boxes reachable from current position
expand_nodes(Curr, Queue, Passed):- (
        (
            Passed #= 0, 
            findall(Q, (move(Curr,Q); pass(Curr, Q)), Q1),
            convlist([X,Y] >> (h_cost(X, F), Y = [F, X]), Q1, Queue)
        ); (
            Passed #= 1, 
            findall(Q, (move(Curr,Q)), Q1),
            convlist([X,Y] >> (h_cost(X, F), Y = [F, X]), Q1, Queue)
        )
    ).

% Extract node with min Cost from Queue (~Priority Queue Implementation)
minNode(Queue, MinNode, NewQueue):-
    convlist([X, Y] >> (X = [Xh, [_,_]], Y = Xh), Queue, Q1),
    min_list(Q1, Min),
    member(X, Queue),
    X = [Xval, _],
    Xval #= Min,
    subtract(Queue, [X], NewQueue),
    MinNode = X.

% Greedy Search Algorithm Itself:

greedy(Curr, Moves, _,_):-
	Curr = [X,Y],
    t(X,Y),
    parse([0,0],Moves, 0, []), !.

greedy(Curr, Moves, Passed, Queue):-
	expand_nodes(Curr, Q1, Passed),
    append(Queue, Q1, Q2),
    minNode(Q2, MinNode, NewQueue),
    MinNode = [_, NextPos],
    (
        (
            (
                (
                    (
                        Curr = [Xc,Yc],
                        NextPos = [Xn, Yn],
                        1 #= abs(Xc-Xn) + abs(Yc - Yn),
                        NewPassed = Passed
                    );(
                        Passed #= 0,
                        pass(Curr, NextPos),
                        NewPassed #= 1
                    )
                )
            ),
            append(Moves, [NextPos], NewMoves),
            greedy(NextPos, NewMoves, NewPassed, NewQueue)
        ); (
            greedy(NextPos, Moves, Passed, NewQueue)
        )
    ).

solve():-
	time(greedy([0,0],[],0,[])).
	
% Moves and Passes:

move(Curr, Next):- (
		move_up(Curr, Next),
		Next = [X1, Y1],
		not(o(X1,Y1)),
		Curr ins 0..20,
		Next ins 0..20
	);
	(
		move_down(Curr, Next),
		Next = [X1, Y1],
		not(o(X1,Y1)),
		Curr ins 0..20,
		Next ins 0..20
	);
	(
		move_right(Curr, Next),
		Next = [X1, Y1],
		not(o(X1,Y1)),
		Curr ins 0..20,
		Next ins 0..20
	);
	(
		move_left(Curr, Next),
		Next = [X1, Y1],
		not(o(X1,Y1)),
		Curr ins 0..20,
		Next ins 0..20
	).

pass(Curr, Next) :- (
		pass_up(Curr, Next);
		pass_down(Curr, Next);
		pass_right(Curr,Next);
		pass_left(Curr,Next);
		pass_up_right(Curr, Next);
		pass_down_right(Curr, Next);
		pass_up_left(Curr, Next);
		pass_down_left(Curr, Next)
).