:- include('board.pl').
:- include('player.pl').
:- include('input.pl').
:- include('display.pl').
:- include('utils.pl').

:- dynamic state/1.
:- dynamic who_turn/1.
:- dynamic next_turn/1.

initial_state(menu).

menu_options(0, 3). %menu_options(Min, Max).
rules_options(0, 1).
game_menu_options(0, 2).
settings_options(0, 2).

request('Choose Option\n').

change_state(NewState) :-
    retractall(state(_)),
    assert(state(NewState)).

menu :-
    display_menu,
    menu_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    menu_handler(Choice).

menu_handler(Choice) :-
    (   Choice == 1
    ->  change_state(game_menu)
    );

    (   Choice == 2
    ->  change_state(settings)
    );

    (   Choice == 3
    ->  change_state(rules)
    );

    (   Choice == 0
    -> change_state(exit)).

game_menu :-
    display_game_menu,
    game_menu_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    game_menu_handler(Choice).

game_menu_handler(Choice) :-
    (   Choice == 1
    ->  change_state(game(1, 2))
    );
    (   Choice == 2
    ->  change_state(game(1, 'CPU'))
    );
    (   Choice == 0
    -> change_state(menu)).

settings :-
    board_size(Size),
    cpu(CPU),
    display_settings(Size, CPU),
    settings_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    settings_handler(Choice).

settings_handler(Choice) :-
    (   Choice == 0
    ->  change_state(menu)
    );
    (   Choice == 1
    ->  change_board_size, change_state(settings)
    );
    (   Choice == 2
    ->  change_cpu, change_state(settings)
    ).

rules :-
    board_size(Size),
    display_rules(Size),
    rules_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    rules_handler(Choice).

rules_handler(Choice) :-
    (   Choice == 0
    ->  change_state(menu)
    );
    (   Choice == 1
    ->  display_authors,change_state(menu)
    ).


% move(+Player, +X, +Y)
move(Player, X, Y) :-
    add_stone(X, Y),
    stones_total(X, Y, V, H, D1, D2),
    how_many_points(V, H, D1, D2, Points),
    add_points(Player, Points).

%
display_game :-
    board(B),
    
    setof([P, Points], player( P, Points), Result),

    nth1(1, Result, [P1, Points1]),
    nth1(2, Result, [P2, Points2]),

    display_game_board(P1, P2, Points1, Points2, B).


switch_turn :-

    who_turn(P1),
    next_turn(P2),

    retract(who_turn(P1)),
    retract(next_turn(P2)),

    assert(who_turn(P2)),
    assert(next_turn(P1)).
    
turn :-
    who_turn(Player), 
    board_size(Size),
    repeat,
        %human interaction
        format('\tPlayer ~d\n', [Player]),
        read_coords(X, Y, 1, Size),
        
        (   spot_available(X, Y)
        ->  move(Player, X, Y),
            !, true
        ;   format('Spot (~d, ~d) is already with a Stone\n', [X, Y]),
            nl, 
            fail
        ),

    display_game,
    !,
    (   board_full
    ->  change_state(game_over)
    ;   switch_turn).


game(P1, P2) :- 
    load_board, 
    load_players(P1, P2),

    display_game, 

    retractall(who_turn( _ )),
    assert(who_turn(P1)),

    retractall(next_turn( _ )),
    assert(next_turn(P2)),
    
    change_state(turn), !.
        
exit :- !.

%
play :-
    initial_state(StartState),
    change_state(StartState), 
        
    repeat,
        state(State),
        call(State),
        (   State == exit
        -> true
        ;  fail)
        .




