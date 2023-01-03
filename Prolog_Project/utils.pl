% between(+N, +Min, +Max)
between(N, Min, Max) :-
    N >= Min,
    N =< Max.

% points(+InARow, -Points)
points(InARow, Points) :-
    (InARow == 3 ; InARow == 6 ; InARow == 9 ) -> Points is InARow // 3 ;
    Points is 0.

% how_many_points(+N_Vertical, +N_Horizontal, +N_Diagonal1, +N_Diagonal2, -Total_Points)
how_many_points(N_Vertical, N_Horizontal, N_Diagonal1, N_Diagonal2, Total_Points) :-
    
    points(N_Vertical, V),
    points(N_Horizontal, H),
    points(N_Diagonal1, D1),
    points(N_Diagonal2, D2),

    Total_Points is V + H + D1 + D2.

% range(+Start, +End, +Step, -L)
range(Start, End, Step, L) :-
    integer(Start),
    integer(End),
    integer(Step),
    Step > 0,

    (   End - Start > 0
    ->  S1 is Step
    ;   S1 is Step * (-1)
    ),
    range_(Start, End, S1, L, []).

% This predicate generates a list of integers in a given range, with a given step between each integer.
range_(Int, End, Step, L, Acc) :-
    % Calculate the next integer in the range.
    Next is Int + Step, 

    % If the next integer is less than or equal to the ending integer, add it to the list and generate the rest of the list. Otherwise, return the list.
    (   Next =< End
    ->  range_(Next, End, Step, L, [Int|Acc])
    ;   L = [Int|Acc]
    ).

