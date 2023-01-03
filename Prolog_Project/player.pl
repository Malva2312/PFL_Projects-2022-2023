:- dynamic player/2.


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

% This predicate retrieves the number of points for a given player.
value(Player, Points):-
    % Retrieve the number of points for the given player.
    player(Player, Points).
