change_state(NewState) :-
    retractall(state(_)),
    assert(state(NewState)).

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

game_menu_handler(Choice) :-
    (   Choice == 1
    ->  change_state(game(1, 2))
    );
    (   Choice == 2
    ->  change_state(game(1, 'CPU'))
    );
    (   Choice == 0
    -> change_state(menu)).

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

rules_handler(Choice) :-
    (   Choice == 0
    ->  change_state(menu)
    );
    (   Choice == 1
    ->  display_authors,change_state(menu)
    ).
