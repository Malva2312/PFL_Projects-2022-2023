:- include('board.pl').
:- include('player.pl').
:- include('input.pl').
:- include('display.pl').

%:- dynamic state/1.



% action(+Player, +X, +Y)
action(Player, X, Y) :-
    add_stone(X, Y),
    stones_total(X, Y, V, H, D1, D2),
    how_many_points(V, H, D1, D2, Points),
    add_points(Player, Points).

% player_action(+Player, +Size)
player_action(Player, Size) :-
    repeat,
        read_coords(X, Y, 1, Size),
        (   spot_available(X, Y)
        ->  action(Player, X, Y),
            ! , true
        ;   format('Spot (~d, ~d) is already with a Stone\n', [X, Y]),
            nl, 
            fail
        ).

% print_game prints the game board and the points of player 1 and player 2
print_game :-
    board(B),
    player_1(P1),
    player_2(P2),
    
    player(P1, Points1),
    player(P2, Points2),
    print_all(P1, P2, Points1, Points2, B). % print the game board and points of player 1 and player 2


% game starts the game by setting the board size, loading the board, loading the players, and printing the game. It then enters a repeat loop in which each player takes a turn, the game is printed, and the loop continues until the board is full. When the board is full, the winner is determined and displayed.
game :-
    board_size(Size),
    load_board(Size), 
    load_players,
    
    player_1(P1),
    player_2(P2),

    print_game,

    repeat, 

        player_action(P1, Size),
        print_game,

        player_action(P2, Size),
        print_game,

        (   board_full
        ->  player(P1, P1_Points),
            player(P2, P2_Points),
            display_winner(P1, P1_Points, P2, P2_Points),
            !, true
        ;   true
        ).




