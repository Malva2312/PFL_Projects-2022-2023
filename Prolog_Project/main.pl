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

% This predicate changes the current state of the game.
change_state(NewState) :-
    % Remove the old state from the database.
    retractall(state(_)),
    % Assert the new state into the database.
    assert(state(NewState)).

% This predicate displays the main menu of the game and handles user input. The menu consists of a list of options and a prompt for the user to enter their choice. The users choice is then passed to the menu_handler predicate for further processing.
menu :-
    display_menu,
    menu_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    menu_handler(Choice).

% This predicate handles the users choice from the main menu.
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

% This predicate displays the game menu and handles user input. The menu consists of a list of options and a prompt for the user to enter their choice. The users choice is then passed to the game_menu_handler predicate for further processing.
game_menu :-
    display_game_menu,
    game_menu_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    game_menu_handler(Choice).

% This predicate handles the users choice from the game menu.
game_menu_handler(Choice) :-
    (   Choice == 1
    ->  change_state(game(1, 2))
    );
    (   Choice == 2
    ->  change_state(game(1, 'CPU'))
    );
    (   Choice == 0
    -> change_state(menu)).

% This predicate displays the current settings and handles user input. The settings consist of the board size and the CPU player setting. The user is presented with a list of options and a prompt to enter their choice. The users choice is then passed to the settings_handler predicate for further processing.
settings :-
    board_size(Size),
    cpu(CPU),
    display_settings(Size, CPU),
    settings_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    settings_handler(Choice).

% This predicate handles the users choice from the settings menu.
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

% This predicate displays the rules of the game and handles user input. The rules consist of the board size and the rules for winning the game. The user is presented with a list of options and a prompt to enter their choice. The users choice is then passed to the rules_handler predicate for further processing.
rules :-
    board_size(Size),
    display_rules(Size),
    rules_options(Min, Max),
    request(Request),
    read_integer(Choice, Min, Max, Request), nl,
    rules_handler(Choice).

% This predicate handles the users choice from the rules menu.
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

% This predicate displays the current state of the game. The game state includes the board, the players, and their scores.
display_game :-
    board(B),
    
    setof([P, Points], player( P, Points), Result),

    nth1(1, Result, [P1, Points1]),
    nth1(2, Result, [P2, Points2]),

    display_game_board(P1, P2, Points1, Points2, B).


% This predicate switches the current player and the next player.
switch_turn :-

    who_turn(P1),
    next_turn(P2),

    retract(who_turn(P1)),
    retract(next_turn(P2)),

    assert(who_turn(P2)),
    assert(next_turn(P1)).

% This predicate represents a players turn in the game. The players turn consists of making a move on the game board and switching to the next player.
turn :-
    who_turn(Player), 
    board_size(Size),
    repeat,
        %human interaction
        format('\tPlayer ~d\n', [Player]),
        read_coords(X, Y, 1, Size),
        
        % Check if the spot is available and make the move if it is. Otherwise, display an error message and try again.
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


% This predicate starts the game. The game consists of loading the board, loading the players, displaying the game board, and switching to the turn state.
game(P1, P2) :- 
    load_board, 
    load_players(P1, P2),

    display_game, 

    % Set the first player as the current player and the second player as the next player.
    retractall(who_turn( _ )),
    assert(who_turn(P1)),

    retractall(next_turn( _ )),
    assert(next_turn(P2)),
    
    change_state(turn), !.
        
exit :- !.

% This predicate plays the game. The game consists of switching to the initial state and repeating the following block until the exit state is reached. The block consists of retrieving the current state, calling the current state, and either continuing or exiting the game depending on the current state.
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




