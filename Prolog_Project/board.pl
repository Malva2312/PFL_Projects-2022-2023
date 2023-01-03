:- include('matrix.pl').

:- dynamic board/1.
:- dynamic board_size/1.
:- dynamic valid_move/2.

board_size(9).

stone_value('X').
free_space('_').

% This predicate changes the size of the board from 9x9 to 6x6 or vice versa.
change_board_size :-
    board_size(Old),
    (   Old == 9
    ->  New is 6
    ;   New is 9),

    retractall(board_size( _ )),
    assert(board_size(New)).


% create_row(+SizeRow, +Element, -List)
create_row(0, _, []) :- !.
create_row(SizeRow, Element, [Element|Tail]) :-
    SizeRow > 0,
    NewSize is SizeRow - 1,
    create_row(NewSize, Element, Tail).

% create_board(+Size, +Element, -List)
create_board(0, _, _ , []):- !.
create_board(Height, Width, Element, [Row | Tail]) :-
    Height > 0,
    NewHeight is Height -1,
    create_row(Width, Element, Row),
    create_board(NewHeight, Width, Element, Tail).

% new_board(+SideSize, -Board)
new_board(SideSize, Board) :-
    free_space(FreeSpot),
    create_board(SideSize, SideSize, FreeSpot, Board).

% This predicate loads a new board into the database.
load_board :-

    board_size(SideSize),
    new_board(SideSize, Board),

    add_valid_moves(SideSize),

    retractall(board( _ )),
    assert(board(Board)).

% spot_available(+X, +Y)
spot_available(X, Y) :-
    valid_move(X, Y).
/*
    board(Board),
    free_space(FreeSpot),

    matrix_bounds(X, Y, Board),

    nth1(Y, Board, Row),
    nth1(X, Row, Element),

    Element == FreeSpot.
*/
% add_stone(+X, +Y)
add_stone(X, Y) :-
    spot_available(X, Y),

    board(Board),
    stone_value(Stone), 
    
    change_value(X, Y, Stone, Board, NewBoard),
    retract(valid_move(X, Y)),
    retract(board(Board)),
    assert(board(NewBoard)).
    
% row_full(-X)
row_full([]).
row_full([X | Rest]) :-
    stone_value(Stone),
    X == Stone,
    row_full(Rest).

% full(-X)
full([]).
full([Row | Rest]) :-
    row_full(Row),
    full(Rest).

% This predicate checks if the board is full. The board is considered full if all of its cells are occupied by pieces.
board_full :-
    board(Board),
    % Check if the board is full.
    full(Board).

% count_x (+List, +X, -Count)
count_x(List, X, Count) :-
    count_x(List, X, 0, Count).

% count_x (+List, +X, +Acc, -Count)
count_x([], _, Acc, Acc).
count_x([H|T], X, Acc, Count) :-
    (   
        H = X ->  Acc1 is Acc + 1;
            Acc1 is Acc
    ),
    count_x(T, X, Acc1, Count).

% stones_horizontal(+Y, -N_Stone)
stones_horizontal(Y, N_Stone) :-
    board(Board),
    stone_value(Stone),

    nth1(Y, Board, Row),
    count_x(Row, Stone, N_Stone).
    
% stones_vertical(+X, -N_Stone)
stones_vertical(X, N_Stone) :- 
    board(Board),
    stone_value(Stone),

    select_col(X, Board, Col),
    count_x(Col, Stone, N_Stone).

% stones_diagonal_1(+X, +Y, -N_Stone)
stones_diagonal_1(X,Y,  N_Stone) :-
    board(Board),
    stone_value(Stone),

    diagonal_1(X, Y, Board, D1),
    count_x(D1, Stone, N_Stone).

% stones_diagonal_2(+X, +Y, -N_Stone)
stones_diagonal_2(X, Y, N_Stone) :-
    board(Board),
    stone_value(Stone), 

    diagonal_2(X, Y, Board, D2),
    count_x(D2, Stone, N_Stone).

% This predicate counts the total number of stones in a given position
stones_total(X, Y, V, H, D1, D2) :-
    % Count the number of stones in the vertical row containing the position.
    stones_vertical(X, V),
    % Count the number of stones in the horizontal row containing the position.
    stones_horizontal(Y, H),
    % Count the number of stones in the first diagonal containing the position.
    stones_diagonal_1(X, Y, D1),
    % Count the number of stones in the second diagonal containing the position.
    stones_diagonal_2(X, Y, D2).

% This predicate generates a list of pairs from two lists of elements.
list_pairs(List1, Pairs) :-
    % Find all pairs of elements from the two lists using a combination of member and findall.
    findall((X,Y), (member(X, List1), member(Y, List1)), Pairs).

% This predicate adds a list of valid moves to the database.
add_valid_move([]).
add_valid_move([(X, Y) | Tail]) :-
    % Add a valid move to the database using assert
    assert(valid_move(X, Y)),
    % Add the rest of the valid moves to the database.
    add_valid_move(Tail).

% This predicate adds all valid moves to the database for a given board size.
add_valid_moves(Size) :-
    % Remove all valid moves from the database using retractall
    retractall(valid_move(_, _)),
    % Generate a list of integers from 1 to Size using range
    range(1, Size, 1, L1),
    % Generate a list of pairs of coordinates using list_pairs
    list_pairs(L1, Coordinates),
    % Add the list of valid moves to the database using add_valid_move
    add_valid_move(Coordinates).
