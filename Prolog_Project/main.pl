:- include('board.pl').
:- include('player.pl').
:- include('input.pl').
:- include('display.pl').

:- dynamic board_size/1.

board_size(2).




%
action(Player, X, Y) :-
    add_stone(X, Y),
    stones_total(X, Y, V, H, D1, D2),
    how_many_points(V, H, D1, D2, Points),
    add_points(Player, Points).
%
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
%
print_board :-
    board(B),
    print_table(B).

/*%
win_condition :-
    board(B),
    full(B).
*/
%
game :-
    board_size(Size),
    load_board(Size), 
    load_players,
    
    player_1(P1),
    player_2(P2),

    print_board,

    repeat, 

        player_action(P1, Size),

        %display

        %print_board,

        %player_action(P2, Size),

        %display
        print_board,

        (   board_full
        ->  write('\nWorked\n'),
            !, true
        ;   write('Failed'), fail
        ).



        %jogador 1 joga
            %receber input
        
            %ver se o input é valido
            %ver se se pode colocar no tabuleiro
            %colocar no tabuleiro

        %jogador 2 joga

        %verificar se o tabuleiro está cheio



