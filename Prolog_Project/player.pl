:- use_module(library(random)).

:- dynamic player/2.
:- dynamic cpu/1.

cpu('Random').

start_points(0).

% load_player(+Player)
load_player(Player):-
    start_points(Points),
    assertz(player(Player, Points)).

% The load_players predicate loads the players P1 and P2 into the program.
% It first removes any previously loaded players using the retractall/1 predicate,
% then loads each player using the load_player predicate.
load_players(P1, P2) :-
    retractall(player( _ , _ )),

    load_player(P1),
    load_player(P2).


% add_points(+Player, +Points)
add_points(Player, Points) :-

    player(Player, Old),
    New is Old + Points,

    retract(player(Player, _ )),
    assert(player(Player, New)).

% This predicate retrieves the number of points for a given player.
value(Player, Points):-
    % Retrieve the number of points for the given player.
    player(Player, Points).

% This predicate changes the type of the CPU player.
change_cpu :-
    % Retrieve the current CPU player type.
    cpu(Old),

    % If the current CPU player type is 'Random', change it to 'Smart'. Otherwise, change it to 'Random'.
    (   Old == 'Random'
    ->  New = 'Smart'
    ;   New = 'Random'),

    % Retract the current CPU player type and assert the new CPU player type.
    retractall(cpu(_)),
    assert(cpu(New)).
% The random_approach predicate selects a random valid move for the CPU player
% from the list of valid moves stored in the VALID_MOVES variable.
% The selected move is output using the format predicate.
random_approach(X, Y, VALID_MOVES):-
    random_member([X, Y], VALID_MOVES),
    format('\n\n\n\tCPU CHOICE\nColomn : ~d\tRow : ~d\n', [X, Y]).
