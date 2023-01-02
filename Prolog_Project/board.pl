:- include('matrix.pl').

:- dynamic board/1.
:- dynamic board_size/1.

board_size(9).

stone_value('X').
free_space('_').

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

% load_board(+SideSize)
load_board(SideSize) :-
    new_board(SideSize, Board),

    retractall(board( _ )),
    assert(board(Board)).

% spot_available(+X, +Y)
spot_available(X, Y) :-
    board(Board),
    free_space(FreeSpot),

    matrix_bounds(X, Y, Board),

    nth1(Y, Board, Row),
    nth1(X, Row, Element),

    Element == FreeSpot.

% add_stone(+X, +Y)
add_stone(X, Y) :-
    spot_available(X, Y),

    board(Board),
    stone_value(Stone), 
    
    change_value(X, Y, Stone, Board, NewBoard),
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


board_full :-
    board(Board),
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


stones_total(X, Y, V, H, D1, D2) :-
    stones_vertical(X, V),
    stones_horizontal(Y, H),
    stones_diagonal_1(X, Y, D1),
    stones_diagonal_2(X, Y, D2).