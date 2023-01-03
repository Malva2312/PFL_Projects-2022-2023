:- use_module(library(lists)).

:- include('utils.pl').

% matrix_bounds(+X, +Y, +Matrix)
matrix_bounds(X, Y, Matrix) :-
    length(Matrix, Len),

    between(X, 1, Len),
    between(Y, 1, Len).

% change_value(+X, +Y, +Elem, +Matrix, -NewMatrix)
change_value(X, Y, Elem, Matrix, NewMatrix) :-

    nth1(Y, Matrix, Row, Rest),
    nth1(X, Row, _ , RowRest),

    nth1(X, NewRow, Elem, RowRest),
    nth1(Y, NewMatrix, NewRow , Rest).

% select_col(+X, +Matrix, -Col)
select_col( _, [], [] ).
select_col(X, [MatrixHead | MatrixTail], [Head | Tail]) :-
    nth1(X, MatrixHead, Head),
    select_col(X, MatrixTail, Tail). 


% diagonal_1_down(+X, +Y, +Matrix, -D1)
diagonal_1_down( X, Y, Matrix, D1) :-
    length(Matrix, Len),
    diagonal_1_down(X, Y, Matrix, Len, D1).

% diagonal_1_down(+X, +Y, +Matrix, +Len, -D1)
diagonal_1_down( _ , Len , _ , Len, []) :- !.
diagonal_1_down( Len , _ , _ , Len, []) :- !.
diagonal_1_down( X , Y , Matrix, Len, [Elem | Tail]) :-

    X1 is X + 1, Y1 is Y + 1,

    nth1(Y1, Matrix, Row),
    nth1(X1, Row, Elem),
    
    diagonal_1_down(X1, Y1, Matrix, Len, Tail).

% diagonal_1_up(+X, +Y, +Matrix, -D1)
diagonal_1_up( 0 , _ , _ , []) :- !.
diagonal_1_up( _ , 0 , _ , []) :- !.
diagonal_1_up(X, Y, Matrix , [Elem | Tail]) :-

    nth1(Y, Matrix, Row),
    nth1(X, Row, Elem),

    X1 is X - 1, Y1 is Y - 1,

    diagonal_1_up(X1, Y1, Matrix, Tail).

% diagonal_1(+X, +Y, +Matrix, -D1)
diagonal_1(X, Y, Matrix, D1):-

    matrix_bounds(X, Y, Matrix),

    diagonal_1_up(X, Y, Matrix, Reverese_UP),
    diagonal_1_down(X, Y, Matrix, DOWN),

    reverse(UP, Reverese_UP),
    append(UP, DOWN, D1).


% diagonal_2_down(+X, +Y, +Matrix, -D2)
diagonal_2_down(X, Y, Matrix, D_DOWN) :-
    length(Matrix, Len),
    diagonal_2_down(X, Y, Matrix, Len, D_DOWN).

% diagonal_2_down(+X, +Y, +Matrix, +Len, -D2)
diagonal_2_down( _ , Len , _ , Len, []) :- !.
diagonal_2_down( 1 , _ , _ , _ , []):- !.
diagonal_2_down(X , Y, Matrix, Len, [Elem | Tail]) :-
    X1 is X -1, Y1 is Y + 1,

    nth1(Y1, Matrix, Row),
    nth1(X1, Row, Elem),

    diagonal_2_down(X1, Y1, Matrix, Len, Tail).

% diagonal_2_up(+X, +Y, +Matrix, -D2)
diagonal_2_up(X, Y, Matrix, D_UP) :-
    length(Matrix, Len),
    diagonal_2_up(X , Y, Matrix, Len, D_UP).

% diagonal_2_up(+Len, +Y, +Matrix, +X, -Diagonal) 
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

% diagonal_2(+X, +Y, +Matrix, -D2)
diagonal_2(X, Y, Matrix, D2):-

    matrix_bounds(X, Y, Matrix),

    diagonal_2_up(X, Y, Matrix, Reverese_UP),
    diagonal_2_down(X, Y, Matrix, DOWN),

    reverse(UP, Reverese_UP),
    append(UP, DOWN, D2).

