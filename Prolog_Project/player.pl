:- use_module(library(random)).

:- dynamic player/2.
:- dynamic cpu/1.

cpu('Random').

start_points(0).

% load_player(+Player)
load_player(Player):-
    start_points(Points),
    assertz(player(Player, Points)).

%
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

value(Player, Points):-
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
%
random_approach(X, Y, VALID_MOVES):-
    random_member([X, Y], VALID_MOVES),
    format('\n\n\n\tCPU CHOICE\nColomn : ~d\tRow : ~d\n', [X, Y]).