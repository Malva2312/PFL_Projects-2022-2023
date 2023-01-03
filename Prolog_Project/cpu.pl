:- use_module(library(random)).
:- use_module(library(lists)).

:- include('board.pl').

:- dynamic cpu/1.

cpu('Random').


% The cpu_move predicate selects a random valid move for the CPU player. 
% A valid move is one that satisfies the valid_move predicate.
cpu_move(X, Y) :-
    cpu(CPU),
    bagof([X, Y], valid_move(X, Y), VALID_MOVES),
    random_permutation(VALID_MOVES, Random),
    (   CPU == 'Random'
    ->  random_approach(X, Y, Random), !
    ;   true),

    (   CPU == 'Greedy'
    ->  greedy_approach(X, Y, Random), !
    ;   true),

    format('\n\n\n\tCPU CHOICE\nColomn : ~w\tRow : ~w\n', [X, Y]).




% This predicate changes the type of the CPU player.
change_cpu :-
    % Retrieve the current CPU player type.
    cpu(Old),

    % If the current CPU player type is 'Random', change it to 'Smart'. Otherwise, change it to 'Random'.
    (   Old == 'Random'
    ->  New = 'Smart'
    ;   New = 'Random'),

    % Retract the current CPU player type and assert the new CPU player type.
    retractall(cpu(_)),
    assert(cpu(New)).
% The random_approach predicate selects a random valid move for the CPU player
% from the list of valid moves stored in the VALID_MOVES variable.
% The selected move is output using the format predicate.
random_approach(X, Y, VALID_MOVES):-
    random_member([X, Y], VALID_MOVES),
    format('\n\n\n\tCPU CHOICE\nColomn : ~d\tRow : ~d\n', [X, Y]).

greedy_approach(X, Y, VALID_MOVES):-
   
    greedy(VALID_MOVES, Points),
    max_member(Elem, Points),
    nth1(Idx, Points, Elem),
    nth1(Idx, VALID_MOVES , [X, Y]).

greedy([], []):- !.
greedy([ Coords | Tail], [Points | Rest]) :-
    thinking( Coords, Points),
    greedy(Tail, Rest).

thinking( [X, Y], Points):-
    board(Board),
    stone_value(Stone),
    change_value(X, Y, Stone, Board, NewBoard),
    
    nth1(Y, NewBoard, Row),
    count_x(Row, Stone, N_Horizontal),
    select_col(X, NewBoard, Col),
    count_x(Col, Stone, N_Vertical),
    diagonal_1(X, Y, NewBoard, D1),
    count_x(D1, Stone, N_Diagonal1),
    diagonal_2(X, Y, NewBoard, D2),
    count_x(D2, Stone, N_Diagonal2),
    how_many_points(N_Vertical, N_Horizontal, N_Diagonal1, N_Diagonal2, Points),
    !.

