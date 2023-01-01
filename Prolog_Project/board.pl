:- use_module(library(lists)).

stone_value('X').
free_space('O').

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

%
new_board(SideSize, Board) :-
    free_space(FreeSpot),
    create_board(SideSize, SideSize, FreeSpot, Board).

%
load_board(SideSize) :-
    new_board(SideSize, Board),

    retractall(board( _ )),
    assert(board(Board)).


% between(+N, +Min, +Max)
between(N, Min, Max) :-
    N >= Min,
    N =< Max.

%
board_bounds(X, Y, Board) :-
    length(Board, Len),

    between(X, 1, Len),
    between(Y, 1, Len).

%
spot_available(X, Y) :-
    board(Board),
    free_space(FreeSpot),

    board_bounds(X, Y, Board),

    nth1(Y, Board, Row),
    nth1(X, Row, Element),

    Element == FreeSpot.

%
change_value(X, Y, Elem, Matrix, NewMatrix) :-

    nth1(Y, Matrix, Row, Rest),
    nth1(X, Row, _ , RowRest),

    nth1(X, NewRow, Elem, RowRest),
    nth1(Y, NewMatrix, NewRow , Rest).

%
add_stone(X, Y) :-
    spot_available(X, Y),

    board(Board),
    stone_value(Stone), 
    
    change_value(X, Y, Stone, Board, NewBoard),
    retract(board(Board)),
    assert(board(NewBoard)).
    
%
row_full([]).
row_full([X | Rest]) :-
    stone_value(Stone),
    X == Stone,
    row_full(Rest).

%
full([]).
full([Row | Rest]) :-
    row_full(Row),
    full(Rest).

%
board_full :-
    board(Board),
    full(Board).

%
count_x(List, X, Count) :-
    count_x(List, X, 0, Count).
%
count_x([], _, Acc, Acc).
count_x([H|T], X, Acc, Count) :-
    (   
        H = X ->  Acc1 is Acc + 1;
            Acc1 is Acc
    ),
    count_x(T, X, Acc1, Count).


%
diagonal_1_down( X, Y, Matrix, D1) :-
    length(Matrix, Len),
    diagonal_1_down(X, Y, Matrix, Len, D1).
%
diagonal_1_down( _ , Len , _ , Len, []) :- !.
diagonal_1_down( Len , _ , _ , Len, []) :- !.
diagonal_1_down( X , Y , Matrix, Len, [Elem | Tail]) :-

    X1 is X + 1, Y1 is Y + 1,

    nth1(Y1, Matrix, Row),
    nth1(X1, Row, Elem),
    
    diagonal_1_down(X1, Y1, Matrix, Len, Tail).

%
diagonal_1_up( 0 , _ , _ , []) :- !.
diagonal_1_up( _ , 0 , _ , []) :- !.
diagonal_1_up(X, Y, Matrix , [Elem | Tail]) :-

    nth1(Y, Matrix, Row),
    nth1(X, Row, Elem),

    X1 is X - 1, Y1 is Y - 1,

    diagonal_1_up(X1, Y1, Matrix, Tail).

%
diagonal_1(X, Y, Matrix, D1):-

    board_bounds(X, Y, Matrix),

    diagonal_1_up(X, Y, Matrix, Reverese_UP),
    diagonal_1_down(X, Y, Matrix, DOWN),

    reverse(UP, Reverese_UP),
    append(UP, DOWN, D1).


%
diagonal_2_down(X, Y, Matrix, D_DOWN) :-
    length(Matrix, Len),
    diagonal_2_down(X, Y, Matrix, Len, D_DOWN).
%
diagonal_2_down( _ , Len , _ , Len, []) :- !.
diagonal_2_down( 1 , Y , Matrix, Len, []):- !.
diagonal_2_down(X , Y, Matrix, Len, [Elem | Tail]) :-
    X1 is X -1, Y1 is Y + 1,

    nth1(Y1, Matrix, Row),
    nth1(X1, Row, Elem),

    diagonal_2_down(X1, Y1, Matrix, Len, Tail).

%
diagonal_2_up(X, Y, Matrix, D_UP) :-
    length(Matrix, Len),
    diagonal_2_up(X , Y, Matrix, Len, D_UP).
%
diagonal_2_up( _ , 0 , _ ,  _ , []) :- !.
diagonal_2_up(Len, Y , Matrix , Len, [Elem]) :- 
    nth1(Y, Matrix, Row),
    nth1(Len, Row, Elem), 
    !.
diagonal_2_up( X , Y , Matrix , Len, [Elem | Tail]) :-

    nth1(Y, Matrix, Row),
    nth1(X, Row, Elem),

    X1 is X + 1, Y1 is Y - 1,

    diagonal_2_up(X1, Y1, Matrix, Len, Tail).

%
diagonal_2(X, Y, Matrix, D2):-

    board_bounds(X, Y, Matrix),

    diagonal_2_up(X, Y, Matrix, Reverese_UP),
    diagonal_2_down(X, Y, Matrix, DOWN),

    reverse(UP, Reverese_UP),
    append(UP, DOWN, D2).

%
select_col( _, [], [] ).
select_col(X, [MatrixHead | MatrixTail], [Head | Tail]) :-
    nth1(X, MatrixHead, Head),
    select_col(X, MatrixTail, Tail). 

%
stones_horizontal(Y, N_Stone) :-
    board(Board),
    stone_value(Stone),

    nth1(Y, Board, Row),
    count_x(Row, Stone, N_Stone).
    
%
stones_vertical(X, N_Stone) :- 
    board(Board),
    stone_value(Stone),

    select_col(X, Board, Col),
    count_x(Col, Stone, N_Stone).

%
stones_diagonal_1(X,Y,  N_Stone) :-
    board(Board),
    stone_value(Stone),

    diagonal_1(X, Y, Board, D1),

    count_x(D1, Stone, N_Stone).

%
stones_diagonal_2(X, Y, N_Stone) :-
    board(Board),
    stone_value(Stone), 

    diagonal_2(X, Y, Board, D2),

    count_x(D2, Stone, N_Stone).