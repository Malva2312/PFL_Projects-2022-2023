% between(+N, +Min, +Max)
between(N, Min, Max) :-
    N >= Min, N =< Max.
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
    !, 
    range_(Start, End, S1, L, []).

% This predicate generates a list of integers in a given range, with a given step between each integer.
range_(Int, End, Step, L, Acc) :-
    !,
    % Calculate the next integer in the range.
    Next is Int + Step, 

    % If the next integer is less than or equal to the ending integer, add it to the list and generate the rest of the list. Otherwise, return the list.
    (   Next =< End
    ->  !, range_(Next, End, Step, L, [Int|Acc])
    ;   L = [Int|Acc]
    ).

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

% idx(ELEMENT, LIST, INDEX) is true if INDEX is the index of ELEMENT in LIST.
% The first element of the list has index 0.
% idx(ELEMENT, LIST, INDEX) is true if ELEMENT is in the tail of the list and INDEX is the index of ELEMENT in the tail.
idx(E,[E|_],0).
idx(E,[_|T],Res) :-
    % Find the index of ELEMENT in the tail.
    idx(E,T,Cur_rest),

    % Adjust the index to account for the elements that were skipped.
    Res is Cur_rest + 1.