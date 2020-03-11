% Libraries:
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- dynamic(h/2).
:- dynamic(o/2).
:- dynamic(t/2).

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
	)),
	not((
		h(X0, Y3),
		Y3 #< Y1,
		Y3 #> Y0
	))).

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
	)),
	not((
		h(X0, Y3),
		Y3 #> Y1,
		Y3 #< Y0
	))).

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
	)),
	not((
		h(X3, Y0),
		X3 #< X1,
		X3 #> X0
	))).

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
	)),
	not((
		h(X3, Y0),
		X3 #> X1,
		X3 #< X0
	))).

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
	)),
	not((
		h(X3, Y3),
		X3 #= X0 + D2,
		Y3 #= Y0 + D2,
		D2 > 0,
		D2 < D
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
	)),
	not((
		h(X3, Y3),
		X3 #= X0 + D2,
		Y3 #= Y0 + D2,
		D2 > 0,
		D2 < D
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
	)),
	not((
		h(X3, Y3),
		X3 #= X0 + D2,
		Y3 #= Y0 + D2,
		D2 > 0,
		D2 < D
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
	)),
	not((
		h(X3, Y3),
		X3 #= X0 + D2,
		Y3 #= Y0 + D2,
		D2 > 0,
		D2 < D
	)).



parse(Curr ,Moves, N, Ans):- (
	Moves = [M1|R],
	Curr = [X0, Y0],
	M1 = [X1, Y1],
	(
		(	
			1 #= abs(X1-X0) + abs(Y1-Y0), append(Ans, [m(X1,Y1)], Ans2), (
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
