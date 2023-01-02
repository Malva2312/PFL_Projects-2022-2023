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




