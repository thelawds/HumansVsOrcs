% Backtracking implementation:
:- ['../General_Base'].

start(0,0).
m(_,_). % Depicts move in the output
p(_,_). % Depicts pass in the output


% Constraints on suolutions set:

% TODO: After everything is done, it is better to delete Visited from satisfy, in order to get rid of redundant
% Debug messages.

satisfy(Curr, _, _, _) :-
	Curr = [X0,Y0],
	t(X0,Y0).

% satisfy(Curr, Moves, Passed, Visited)
satisfy(Curr, Moves, Passed, Visited):- (
	% Debug message:
	% format('solving with Current = ~w, Visited = ~w, passed = ~w~n',[Curr, Visited, Passed]),
	
	% Obtain X_0, X_1, Y_0, Y_1 from Curr and Moves:
	Curr = [X0,Y0],
	Moves = [[X1,Y1]|R],
	
	% Check whether current position is available:
	not(o(X0,Y0)),

	% Len(Moves) <= 50:
	length(Moves, X), X #< 50,
	
	% Check two cases: 
	% If |(X0 + Y0)| - |(X1 + Y1)| = 1 => move()
	% Otherwise => pass()
	(
		(
			1 #= abs(X1-X0) + abs(Y1-Y0),
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
	not(o(X1,Y1)),(
		move_up(Curr, Next);
		move_down(Curr, Next);
		move_right(Curr,Next);
		move_left(Curr, Next)
	).

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
)).

% No GOD after this line, only shitty prolog code 
% To print the results out:

solve():-
	time(satisfy([0,0],Moves,0,[])), !,
	parse([0,0],Moves, 0, []), !.
	