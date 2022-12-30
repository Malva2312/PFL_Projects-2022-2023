:- dynamic board/1.
:- dynamic player/2.
% :- dynamic player_1/1.
% :- dynamic player_2/1.


stone_value('X').
board_value('O').
board_side_size(9).

player_1(1). %player 1 get '1' as a identifier
player_2(2). %player 2 get '2' as a identifier
start_points(0). %start_points is zero

points(InRow, Points) :-
    (InRow == 3 ; InRow == 6 ; InRow == 9 ) -> Points is InRow // 3 ;
    Points is 0.
    
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
).

/* loads player points (0) */
load_players:-

    retractall(player(_, _)), 

    player_1(P1),
    player_2(P2),
    start_points(Points),

    asserta(player(P1, Points)),
    asserta(player(P2, Points)).


/* updates player n points */
update_points(NewPoints, Player):-
    retract(player(Player, _)),
    asserta(player(Player, NewPoints)).

add_points(Player, InRow):-
    points(InRow, Add),
    Add =\= 0,
    player(Player, Points),
    NewPoints is Points + Add,
    update_points(NewPoints, Player).

/* loads the initial board to board() */
load_board :-
   retractall(board(_)), 
   start_board(Board),
   asserta(board(Board)).

/* checks if the board is full*/
row([]).
row([X|Rest]):-
   stone_value(Stone),
   X == Stone,
   row(Rest).

full([]).
full([Row|Rest]) :-
   row(Row),
   full(Rest).

/* read postion to put the stone */
read_integer(X) :-
   read(X),
   (X >= 1, X =< 9 -> true ; 
   write("\nInvalid input. Please enter a value between 1 and 9.\n"),
   read_integer(X)).

get_position(X, Y):-
    read_integer(X),
    read_integer(Y),
    write(X), write(', '), write(Y), nl.

/* get value in some position of the board */
get_value_in_row([Head | _ ], 0 , Value):-
    Value is Head.
get_value_in_row([ _ | Tail], Idx, Value):-
    NextIdx is Idx - 1,
    get_value_in_row(Tail, NextIdx, Value).

get_value_in_position([Head | _ ], 0, Col, Value):-
    get_value_in_row( Head, Col, Value).
get_value_in_position([ _ | Tail], Row, Col, Value):-
    NextRow is Row -1,
    get_value_in_postion(Tail, NextRow, Col, Value).


/* return lsit with updated values on the board */
replace_in_row([_|T],0,NewVal,[NewVal|T]).
replace_in_row([H|T],Col,NewVal,[H|R]) :-
    Col > 0,
    NextCol is Col-1,
    replace_in_row(T,NextCol,NewVal,R).

replace_in_board([ Head | Tail ], 0 , Col, NewVal, [Update | Tail]):- 
    !, 
    replace_in_row(Head, Col, NewVal, Update).
replace_in_board([ Head | Tail], Row, Col, NewVal, [ Head | Rest ]):-
    Row > 0,
    NextRow is Row -1,
    replace_in_board( Tail, NextRow , Col, NewVal, Rest).


/* update board values */
update_board(Board, Pos_y, Pos_x, Stone, NewBoard):-
    stone_value(Stone), 
    % get_position(Pos_x, Pos_y),
    X is Pos_x - 1,
    Y is Pos_y - 1, 
    replace_in_board(Board, Y, X, Stone, NewBoard),
    retract(board(B)),
    asserta(board(NewBoard)).


/* outputs the board */
print_row([], _).
print_row([Col|Rest], NumColumns) :-
   write(Col),
   print_row(Rest, NumColumns).

print_table([]).
print_table([Row|Rest]) :-
   length(Row, NumColumns),
   print_row(Row, NumColumns),
   nl,
   print_table(Rest).

print_board :-
    board(B),
    print_table(B).

main :-
    load_players,
    load_board, 
    board(Board),
    
    print_table(Board).


