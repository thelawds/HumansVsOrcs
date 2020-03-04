% Libraries:
:- use_module(library(lists)).
:- use_module(library(random)).

% Random search implementation:
:- dynamic(h/2).
:- dynamic(o/2).
:- dynamic(t/2).
:- ['../Test_Input/input_13'].

start(0,0).
m(_,_). % Depicts move in the output
p(_,_). % Depicts pass in the output

% TODO: make here doesn't work. Fix It!
% TODO: Make the algorithm perform 100 tries and find the best(in terms of minimizing number of steps) path.
solve() :- make, solve_game_A_star(0, 0, [], 0, 100).

% TODO: Refactor code. Carry out moves and throws in different file
% TODO: Refactor code. Make all debug outputs be performed only if special flag is turned on.
% Moves:
move_up(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Move_Up'), % Debugging Output
	% Changing coordinates of the Runner:
	not(Pos_y is 20),
	NPos_y is Pos_y + 1,
	NPos_x is Pos_x,
	not(member(m(NPos_x,NPos_y), Steps)),
	% Chack if there is an Orc at new (X,Y) and check whether running play can be initiated:
	not(o(NPos_x, NPos_y)),
	(not(h(NPos_x, NPos_y)) -> (
		NewNsteps is Nsteps + 1,
		NewMax_steps is Max_steps - 1
	); (
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)) % Remove Current runner from the players list
	)),
	% Append new position to the set of moves performed and continue with the solution:
	append(Steps, [m(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps).

move_right(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Move_Right'), % Debugging Output
	% Changing coordinates of the Runner:
	not(Pos_x is 20),
	NPos_y is Pos_y,
	NPos_x is Pos_x + 1,
	not(member(m(NPos_x,NPos_y), Steps)),
	% Chack if there is an Orc at new (X,Y) and check whether running play can be initiated:
	not(o(NPos_x, NPos_y)),
	(not(h(NPos_x, NPos_y)) -> (
		NewNsteps is Nsteps + 1,
		NewMax_steps is Max_steps - 1
	); (
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)) % Remove Current runner from the players list
	)),
	% Append new position to the set of moves performed and continue with the solution:
	append(Steps, [m(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps).

move_down(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Move_Down'), % Debugging Output
	% Changing coordinates of the Runner:
	not(Pos_y is 0),
	NPos_y is Pos_y - 1,
	NPos_x is Pos_x,
	not(member(m(NPos_x,NPos_y), Steps)),
	% Chack if there is an Orc at new (X,Y) and check whether running play can be initiated:
	not(o(NPos_x, NPos_y)),
	(not(h(NPos_x, NPos_y)) -> (
		NewNsteps is Nsteps + 1,
		NewMax_steps is Max_steps - 1
	); (
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)) % Remove Current runner from the players list
	)),
	% Append new position to the set of moves performed and continue with the solution:
	append(Steps, [m(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps).

move_left(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Move_Left'), % Debugging Output
	% Changing coordinates of the Runner:
	not(Pos_x is 0),
	NPos_y is Pos_y,
	NPos_x is Pos_x - 1,
	not(member(m(NPos_x,NPos_y), Steps)),
	% Chack if there is an Orc at new (X,Y) and check whether running play can be initiated:
	not(o(NPos_x, NPos_y)),
	(not(h(NPos_x, NPos_y)) -> (
		NewNsteps is Nsteps + 1,
		NewMax_steps is Max_steps - 1
	); (
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)) % Remove Current runner from the players list
	)),
	% Append new position to the set of moves performed and continue with the solution:
	append(Steps, [m(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps).

% Ball Throws:
throw_up(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Up'), % Debugging Output
	% Checking Prerequisites to make the pass:
	NPos_x is Pos_x,
	h(NPos_x, NPos_y),
	NPos_y > Pos_y,
	% Meeting Orc Position Requirenments:
	((o(Orc_x, Orc_y), Orc_x >= Pos_x, Orc_y >= Pos_y,NPos_x is Orc_x, NPos_y > Orc_y) -> solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps);
	% Meeting the Pass Results Contract:
	NewNsteps is Nsteps,
	NewMax_steps is Max_steps,
	assert(h(Pos_x,Pos_y)), % Add previous runner to players list
	retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
	append(Steps, [p(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)).

throw_up_right(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Up_Right'), % Debugging Output
	% Checking Prerequisites to make the pass:
	% Searching for Human on the same diagonal:
	h(NPos_x, NPos_y),
	Dx1 is NPos_x - Pos_x,
	NPos_y is Pos_y + Dx1,
	Dx1 > 0,
	% Meeting Orc Position Requirenments:
	(o(Orc_x, Orc_y), Dx2 is Orc_x - Pos_x,	Orc_y is Pos_y + Dx2, Dx2 > 0,Dx1 > Dx2 -> 	solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps); (
		% Meeting the Pass Results Contract:
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
		append(Steps, [p(NPos_x,NPos_y)], NewSteps),
		solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)
	)).

throw_right(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Right'), % Debugging Output
	% Checking Prerequisites to make the pass:
	NPos_y is Pos_y,
	h(NPos_x, NPos_y),
	NPos_x > Pos_x,
	% Meeting Orc Position Requirenments:
	((o(Orc_x, Orc_y), Orc_x >= Pos_x, Orc_y >= Pos_y,NPos_y is Orc_y, NPos_x > Orc_x) -> solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps);
	% Meeting the Pass Results Contract:
	NewNsteps is Nsteps,
	NewMax_steps is Max_steps,
	assert(h(Pos_x,Pos_y)), % Add previous runner to players list
	retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
	append(Steps, [p(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)).

throw_down_right(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Down_Right'), % Debugging Output
	% Checking Prerequisites to make the pass:
	% Searching for Human on the same diagonal:
	h(NPos_x, NPos_y),
	Dx1 is NPos_x - Pos_x,
	NPos_y is Pos_y - Dx1,
	Dx1 > 0,
	% Meeting Orc Position Requirenments:
	(o(Orc_x, Orc_y), Dx2 is Orc_x - Pos_x,	Orc_y is Pos_y - Dx2, Dx2 > 0,Dx1 > Dx2 -> 	solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps); (
		% Meeting the Pass Results Contract:
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
		append(Steps, [p(NPos_x,NPos_y)], NewSteps),
		solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)
	)).

throw_down(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Down'), % Debugging Output
	% Checking Prerequisites to make the pass:
	NPos_x is Pos_x,
	h(NPos_x, NPos_y),
	NPos_y < Pos_y,
	% Meeting Orc Position Requirenments:
	((o(Orc_x, Orc_y), Orc_x =< Pos_x, Orc_y =< Pos_y, NPos_x is Orc_x, NPos_y < Orc_y) -> solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps);
	% Meeting the Pass Results Contract:
	NewNsteps is Nsteps,
	NewMax_steps is Max_steps,
	assert(h(Pos_x,Pos_y)), % Add previous runner to players list
	retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
	append(Steps, [p(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)).

throw_down_left(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Down_Left'), % Debugging Output
	% Checking Prerequisites to make the pass:
	% Searching for Human on the same diagonal:
	h(NPos_x, NPos_y),
	Dx1 is Pos_x - NPos_x,
	NPos_y is Pos_y - Dx1,
	Dx1 > 0,
	% Meeting Orc Position Requirenments:
	(o(Orc_x, Orc_y), Dx2 is Pos_x - Orc_x,	Orc_y is Pos_y - Dx2, Dx2 > 0,Dx1 > Dx2 -> 	solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps); (
		% Meeting the Pass Results Contract:
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
		append(Steps, [p(NPos_x,NPos_y)], NewSteps),
		solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)
	)).

throw_left(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Left'), % Debugging Output
	% Checking Prerequisites to make the pass:
	NPos_y is Pos_y,
	h(NPos_x, NPos_y),
	NPos_x < Pos_x,
	% Meeting Orc Position Requirenments:
	((o(Orc_x, Orc_y), Orc_x =< Pos_x, Orc_y =< Pos_y,NPos_y is Orc_y, NPos_x < Orc_x) -> solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps);
	% Meeting the Pass Results Contract:
	NewNsteps is Nsteps,
	NewMax_steps is Max_steps,
	assert(h(Pos_x,Pos_y)), % Add previous runner to players list
	retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
	append(Steps, [p(NPos_x,NPos_y)], NewSteps),
	solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)).

throw_up_left(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :-
	% write('Pass_Up_Left'), % Debugging Output
	% Checking Prerequisites to make the pass:
	% Searching for Human on the same diagonal:
	h(NPos_x, NPos_y),
	Dx1 is Pos_x - NPos_x,
	NPos_y is Pos_y + Dx1,
	Dx1 > 0,
	% Meeting Orc Position Requirenments:
	(o(Orc_x, Orc_y), Dx2 is Pos_x - Orc_x,	Orc_y is Pos_y + Dx2, Dx2 > 0,Dx1 > Dx2 -> 	solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps); (
		% Meeting the Pass Results Contract:
		NewNsteps is Nsteps,
		NewMax_steps is Max_steps,
		assert(h(Pos_x,Pos_y)), % Add previous runner to players list
		retract(h(NPos_x,NPos_y)), % Remove Current runner from the players list
		append(Steps, [p(NPos_x,NPos_y)], NewSteps),
		solve_game_A_star(NPos_x, NPos_y, NewSteps, NewNsteps, NewMax_steps)
	)).



% Main algorithm:
% Works as follows:
% You give the algorithm your initial position as <Pos_x, Pos_y>,
% Steps - [], in which all the moves will be stored,

heuristics(Pos_x, Pos_y, H) :-
    t(X,Y), 
    H is abs(X - Pos_x) + abs(Y - Pos_y).


solve_game_A_star(Pos_x, Pos_y, Steps, Nsteps, Max_steps) :- (
	% Debug output:
	format('------------~nSolving with: ~a ~a~n~w~n~a~n~a~n------------', [Pos_x,Pos_y,Steps,Nsteps, Max_steps]),
	( % Loosing Position
		Max_steps == 0 -> false;
		( % Winning Position:
			t(Pos_x, Pos_y),
			% Print Answer on the screen:
			format('~n~n###### ANSWER: ######~n~w~n~a~n#####################~n~n',[Steps,Nsteps])
		); (
            
        )
	)
).