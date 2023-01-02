:- dynamic player/2.

player_1(1).
player_2(2).
start_points(0).


%
load_player(Player):-
    start_points(Points),
    assert(player(Player, Points)).

%
load_players :-
    retractall(player( _ , _ )),

    player_1(P1),
    player_2(P2), 

    load_player(P1),
    load_player(P2).


%
add_points(Player, Points) :-

    player(Player, Old),
    New is Old + Points,

    retract(player(Player, _ )),
    assert(player(Player, New)).