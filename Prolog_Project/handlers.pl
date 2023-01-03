:- dynamic state/1.

initial_state(menu).

% This predicate changes the current state of the game.
change_state(NewState) :-
    retractall(state(_)),
    assert(state(NewState)).

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

% This predicate handles the users choice from the game menu.
game_menu_handler(Choice) :-
    (   Choice == 1
    ->  change_state(game(1, 2))
    );
    (   Choice == 2
    ->  change_state(game(1, 'CPU'))
    );
    (   Choice == 3
    ->  change_state(game('CPU_1', 'CPU_2'))
    );
    (   Choice == 4
    ->  change_state(game('CPU_1', 1))
    );
    (   Choice == 0
    -> change_state(menu)).
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
    ),
    !.
% This predicate handles the users choice from the rules menu.
rules_handler(Choice) :-
    (   Choice == 0
    ->  change_state(menu)
    );
    (   Choice == 1
    ->  display_authors,change_state(menu)
    ).
