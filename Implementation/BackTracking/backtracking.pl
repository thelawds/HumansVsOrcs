% Libraries:
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
% Random search implementation:
:- dynamic(h/2).
:- dynamic(o/2).
:- dynamic(t/2).
:- dynamic(passed/1).
:- ['../Test_Input/input_13'].

start(0,0).
m(_,_). % Depicts move in the output
p(_,_). % Depicts pass in the output


% Constraints on suolutions set:

% TODO: After everything is done, it is better to delete Visited from satisfy, in order to get rid of redundant
% Debug messages.

% TODO: Get rid of all Debug Messages.

% TODO: Implement Passes in Diagonals.

satisfy(Curr, _, _, _) :-
	Curr = [X0,Y0],
	t(X0,Y0).

satisfy(Curr, Moves, Passed, Visited):- (
	% Debug message:
	% format('solving with Current = ~w, Visited = ~w, passed = ~w~n',[Curr, Visited, Passed]),
	
	% Obtain X_0, X_1, Y_0, Y_1 from Curr and Moves:
	Curr = [X0,Y0],
	Moves = [[X1,Y1]|R],
	
	% Check whether current position is available:
	not(o(X0,Y0)),

	% Len(Moves) <= 400:
	length(Moves, X), X #< 401,
	
	% Check two cases: 
	% If |(X0 + Y0) - (X1 + Y1)| = 1 => move()
	% Otherwise => pass()
	(
		(
			1 #= X1+Y1-X0-Y0,
			move(Curr, [X1,Y1]),
			append(Visited, [[X1,Y1]], Vis),
			satisfy([X1,Y1], R, Passed, Vis)
		); (
			Passed #= 0,
			pass(Curr, [X1,Y1]),
			append(Visited, [[X1,Y1]], Vis),
			satisfy([X1,Y1], R, 1, Vis)
		)
	)
).

% Moves implementation:
move(Curr, Next):-
	% Check whether moves are in the region
	Curr ins 0..20,
	Next ins 0..20,
	% Check whether move is in the Hexagon with an Orc
	Next = [X1, Y1],
	not(o(X1,Y1)),
	% Check whether 
	move_up(Curr, Next);
	move_down(Curr, Next);
	move_right(Curr,Next);
	move_left(Curr, Next).

move_up(Curr, Next):-
	Curr = [X0,Y0],
	Y0 + 1 #\= 21,
	Y1 is Y0 + 1,
	Next = [X0, Y1].

move_down(Curr, Next):-
	Curr = [X0,Y0],
	Y0 - 1 #\= -1,
	Y1 is Y0 - 1,
	Next = [X0, Y1].

move_right(Curr, Next):-
	Curr = [X0,Y0],
	X0 + 1 #\= 21,
	X1 is X0 + 1,
	Next = [X1, Y0].

move_left(Curr, Next):-
	Curr = [X0,Y0],
	X0 - 1 #\= -1,
	X1 is X0 - 1,
	Next = [X1, Y0].

% Passes implementation:
pass(Curr, Next) :- (
	Curr ins 0..20,
	Next ins 0..20,(
		pass_up(Curr, Next);
		pass_down(Curr, Next);
		pass_right(Curr,Next);
		pass_left(Curr,Next);
		pass_up_right(Curr, Next);
		pass_down_right(Curr, Next);
		pass_up_left(Curr, Next);
		pass_down_left(Curr, Next)
	)
).

pass_up(Curr, Next):- (
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	Y1 #= Y0 + DY,
	DY #> 0,
	X1 #= X0,
	not((
		o(X0, Y2),
		Y2 #< Y1,
		Y2 #> Y0
	))
).

pass_down(Curr, Next):- (
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	Y1 #= Y0 - DY,
	DY #> 0,
	X1 #= X0,
	not((
		o(X0, Y2),
		Y2 #> Y1,
		Y2 #< Y0
	))
).

pass_right(Curr, Next):-(
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	X1 #= X0 + DX,
	DX #> 0,
	Y1 #= Y0,
	not((
		o(X2, Y0),
		X2 #< X1,
		X2 #> X0
	))
).

pass_left(Curr, Next):-(
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	X1 #= X0 - DX,
	DX #> 0,
	Y1 #= Y0,
	not((
		o(X2, Y0),
		X2 #> X1,
		X2 #< X0
	))
).

pass_up_right(Curr, Next):-
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	X1 #= X0 + D,
	Y1 #= Y0 + D,
	D > 0,
	not((
		o(X2, Y2),
		X2 #= X0 + D1,
		Y2 #= Y0 + D1,
		D1 > 0,
		D1 < D
	)).

pass_down_right(Curr, Next):-
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	X1 #= X0 + D,
	Y1 #= Y0 - D,
	D > 0,
	not((
		o(X2, Y2),
		X2 #= X0 + D1,
		Y2 #= Y0 - D1,
		D1 > 0,
		D1 < D
	)).

pass_up_left(Curr, Next):-
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	X1 #= X0 + D,
	Y1 #= Y0 - D,
	D < 0,
	not((
		o(X2, Y2),
		X2 #= X0 + D1,
		Y2 #= Y0 - D1,
		D1 < 0,
		D1 > D
	)).

pass_down_left(Curr, Next):-
	Curr = [X0, Y0],
	Next = [X1, Y1],
	h(X1, Y1),
	X1 #= X0 + D,
	Y1 #= Y0 + D,
	D < 0,
	not((
		o(X2, Y2),
		X2 #= X0 + D1,
		Y2 #= Y0 + D1,
		D1 < 0,
		D1 > D
	)).

% No GOD after this line, only shitty prolog code 
% To print the results out:

solve():-
	time(satisfy([0,0],Moves,0,[])), !,
	parse([0,0],Moves, 0, []), !.
	

parse(Curr ,Moves, N, Ans):- (
	Moves = [M1|R],
	Curr = [X0, Y0],
	M1 = [X1, Y1],
	(
		(	
			1 #= X1+Y1-X0-Y0, append(Ans, [m(X1,Y1)], Ans2), (
				(
					h(X1,Y1),
					parse(M1, R, N, Ans2)
				);(
					N2 is N + 1,
					parse(M1, R,N2,Ans2)
				)
			)
		); (
			append(Ans, [p(X1,Y1)], Ans2), 
			N2 is N + 1,
			parse(M1, R,N2,Ans2)
		)
	)
).

parse(_, [], N, Ans):-
	format('Answer: ~w, with the number of Steps: ~w~n',[Ans,N]).
