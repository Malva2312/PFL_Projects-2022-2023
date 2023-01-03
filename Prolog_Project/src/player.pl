:- dynamic player/2.

start_points(0).

% load_player(+Player).
load_player(Player):-
    start_points(Points),
    assertz(player(Player, Points)).

/*  The load_players predicate loads the players P1 and P2 into the program.
    It first removes any previously loaded players using the retractall/1 predicate,
    then loads each player using the load_player predicate.*/
% load_players(+P1, +P2).
load_players(P1, P2) :-
    retractall(player( _ , _ )),

    load_player(P1),
    load_player(P2).

% add_points(+Player, +Points).
add_points(Player, Points) :-

    player(Player, Old),
    New is Old + Points,

    retract(player(Player, _ )),
    assert(player(Player, New)).

% This predicate retrieves the number of points for a given player.
% value(+Player, -Points).
value(Player, Points):-
    % Retrieve the number of points for the given player.
    player(Player, Points).
