:- include('board.pl').
:- include('player.pl').
:- include('input.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('handlers.pl').

:- dynamic state/1.
:- dynamic who_turn/1.
:- dynamic next_turn/1.

initial_state(menu).

menu_options(0, 3). %menu_options(Min, Max).
rules_options(0, 1).
game_menu_options(0, 2).
settings_options(0, 2).

request('Choose Option\n').


menu :-
    display_menu,
    menu_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    menu_handler(Choice).

game_menu :-
    display_game_menu,
    game_menu_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    game_menu_handler(Choice).

settings :-
    board_size(Size),
    cpu(CPU),
    display_settings(Size, CPU),
    settings_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    settings_handler(Choice).

rules :-
    board_size(Size),
    display_rules(Size),
    rules_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    rules_handler(Choice).

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
    
choose_move(Player, X, Y, Size) :-
    (   integer(Player)
    ->  format('\tPlayer ~d\n', [Player]),
        read_coords(X, Y, 1, Size)
    ;   cpu_move(X, Y)
    ).
    
turn :-
    who_turn(Player), 
    board_size(Size),
    repeat,
        choose_move(Player, X, Y, Size),
        
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
    ->  change_state(game_over(Winner))
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
        
game_over(Winner) :-
    setof([P, Points], player( P, Points), Result),

    nth1(1, Result, [P1, Points1]),
    nth1(2, Result, [P2, Points2]),

    (   Points1 > Points2
    ->  Winner is P1,
        display_winner(P1, Points1, fail)
    ; true
    ),
        (   Points1 < Points2
    ->  Winner is P2,
        display_winner(P2, Points2, fail)
    ; true
    ),
        (   Points1 == Points2
    ->  Winner = 'BOTH',
        display_winner(P1, Points1, true)
    ; true
    ),
    
    change_state(menu), !.


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
