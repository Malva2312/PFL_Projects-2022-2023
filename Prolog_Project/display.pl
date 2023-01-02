:- include('utils.pl').

%
print_row([], _).
print_row([Col|Rest], NumColumns) :-
   format('~w  ', [Col]),
   print_row(Rest, NumColumns).

%
print_table(Matrix) :-
   print_table(Matrix, 1).
%
print_table([], _ ).
print_table([Row|Rest], Counter) :-
   length(Row, NumColumns),
   format('~d |  ', [Counter]), % rows number
   print_row(Row, NumColumns),
   nl,
   C1 is Counter +1,
   print_table(Rest, C1).


%
numeric_header([]).
numeric_header([Elem | Tail]) :-
   format('~d  ', [Elem]),
   numeric_header(Tail).
%
stuff_header([]).
stuff_header([Elem | Tail]) :-
   write('---'),
   stuff_header(Tail).
%
header(P1, P2, P1_Points, P2_Points, Size) :-
   format('\n\n  Player ~d: ~d\tPlayer ~d: ~d\n\n', [P1, P1_Points, P2, P2_Points]),

   write('  |  '),
   range(1, Size, 1, ReverseHeader),
   reverse(Header, ReverseHeader),
   numeric_header(Header), nl,
   write('--+--'), 
   stuff_header(Header), nl.

%
print_all(P1, P2, P1_Points, P2_Points, Matrix) :-
   length(Matrix, Size),
   header(P1, P2, P1_Points, P2_Points, Size),
   
   print_table(Matrix).

%
winner_msg(P, Points) :-
   format('\n\n\tPlayer ~d WON THIS MATCH : ~d POINTS\n\n', [P, Points]).
%
display_winner(P1, P1_Points, P2, P2_Points) :-
   (  P1_Points > P2_Points
   -> winner_msg(P1, P1_Points)
   )
   ;
   (  P2_Points > P1_Points
   -> winner_msg(P2, P2_Points)
   )
   ;
   (  P1_Points == P2_Points
   -> format('\n\n\tDRAW, BOTH PLAYERS SCORED ~d POINTS\n\n', [P1_Points])
   ).

