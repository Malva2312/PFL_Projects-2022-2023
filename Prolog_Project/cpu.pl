:- use_module(library(random)).
:- use_module(library(lists)).

:- include('board.pl').

:- dynamic cpu/1.
:- dynamic thinking_board/1.

cpu('Random').


% The cpu_move predicate selects a random valid move for the CPU player. 
% A valid move is one that satisfies the valid_move predicate.
cpu_move(X, Y) :-
    cpu(CPU),
    bagof([X, Y], valid_move(X, Y), VALID_MOVES),
    random_permutation(VALID_MOVES, Random).
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

% greedy_approach(X, Y, VALID_MOVES) is true if (X, Y) is a greedy move in VALID_MOVES.
greedy_approach(X, Y, VALID_MOVES):-
   % Find the points of all the valid moves.
    greedy(VALID_MOVES, Points),
    % Find the maximum point value.
    max_member(Elem, Points),
    % Find the index of the maximum point value.
    nth1(Idx, Points, Elem),
    % Find the move with the maximum point value.
    nth1(Idx, VALID_MOVES , [X, Y]).

% greedy(COORDS_LIST, POINTS_LIST) is true if POINTS_LIST is a list of points corresponding to the moves in COORDS_LIST.
% The first element of COORDS_LIST is paired with the first element of POINTS_LIST, and so on.
greedy([], []):- !.
greedy([ Coords | Tail], [Points | Rest]) :-
    % Find the points for the current move.
    thinking( Coords, Points),
    % Find the points for the remaining moves.
    greedy(Tail, Rest).

% thinking(COORDS, POINTS) is true if POINTS is the number of points that would be gained by making a move at COORDS.
thinking( [X, Y], Points):-
     % Retrieve the current board.
    board(Board),

    % Retrieve the value of the current players stone.
    stone_value(Stone),
    
    % Make the move and get the resulting board.
    change_value(X, Y, Stone, Board, NewBoard),
    
    % Count the number of stones in a row horizontally.
    nth1(Y, NewBoard, Row),
    count_x(Row, Stone, N_Horizontal),

    % Select the column at position X.
    select_col(X, NewBoard, Col),

    % Count the number of stones in the column.
    count_x(Col, Stone, N_Vertical),

    % Select the elements on the first diagonal.
    diagonal_1(X, Y, NewBoard, D1),

    % Count the number of stones on the first diagonal.
    count_x(D1, Stone, N_Diagonal1),

    % Select the elements on the second diagonal.
    diagonal_2(X, Y, NewBoard, D2),

    % Count the number of stones on the second diagonal.
    count_x(D2, Stone, N_Diagonal2),

    % Calculate the total number of points.
    how_many_points(N_Vertical, N_Horizontal, N_Diagonal1, N_Diagonal2, Points),

    % Cut to prevent backtracking.
    !.

