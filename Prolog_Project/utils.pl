% between(+N, +Min, +Max)
between(N, Min, Max) :-
    N >= Min,
    N =< Max.

%
points(InARow, Points) :-
    (InARow == 3 ; InARow == 6 ; InARow == 9 ) -> Points is InARow // 3 ;
    Points is 0.

%
how_many_points(N_Vertical, N_Horizontal, N_Diagonal1, N_Diagonal2, Total_Points) :-

    points(N_Vertical, V),
    points(N_Horizontal, H),
    points(N_Diagonal1, D1),
    points(N_Diagonal2, D2),

    Total_Points is V + H + D1 + D2.
