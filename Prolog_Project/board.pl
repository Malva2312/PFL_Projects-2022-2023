stone_value('X').
free_space('O').

/*
start_board(
    [
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O'],
        ['O','O','O','O','O','O','O','O','O']
    ]
).*/

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
    
%
spot_available(X, Y) :-
    board(Board),
    free_space(FreeSpot),
    

    
%
add_stone(X, Y) :-
    board(Board),
    length(Board, Len), 

    X >= 0, Y >= 0,
    X < Len, Y < Len,


