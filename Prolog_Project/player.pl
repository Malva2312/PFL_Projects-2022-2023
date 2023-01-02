:- dynamic player/2.
:- dynamic cpu/1.

player_1(1).
player_2(2).
player_cpu('CPU').
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

change_cpu :-
    cpu(Old),
    (   Old == 'Random'
    ->  New = 'Smart'
    ;   New = 'Random'),

    retractall(cpu(_)),
    assert(cpu(New)).