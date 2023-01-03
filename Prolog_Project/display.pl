:- include('utils.pl').

%print_row(+Row, +NumColumns)
print_row([], _).
print_row([Col|Rest], NumColumns) :-
   format('~w  ', [Col]),
   print_row(Rest, NumColumns).

%print_table(+Matrix)
print_table(Matrix) :-
   print_table(Matrix, 1).

%print_table(+Matrix, +Counter)
print_table([], _ ).
print_table([Row|Rest], Counter) :-
   length(Row, NumColumns),
   format('~d |  ', [Counter]), % rows number
   print_row(Row, NumColumns),
   nl,
   C1 is Counter +1,
   print_table(Rest, C1).

% numeric_header(+Header)
numeric_header([]).
numeric_header([Elem | Tail]) :-
   format('~d  ', [Elem]),
   numeric_header(Tail).

% stuff_header(+Header)
stuff_header([]).
stuff_header([Elem | Tail]) :-
   write('---'),
   stuff_header(Tail).

% header(+P1, +P2, +P1_Points, +P2_Points, +Size)
header(P1, P2, P1_Points, P2_Points, Size) :-
   format('\n\n  Player ~d: ~d\tPlayer ~d: ~d\n\n', [P1, P1_Points, P2, P2_Points]),

   write('  |  '),
   range(1, Size, 1, ReverseHeader),
   reverse(Header, ReverseHeader),
   numeric_header(Header), nl,
   write('--+--'), 
   stuff_header(Header), nl.

% print_all(+P1, +P2, +P1_Points, +P2_Points, +Matrix)
print_all(P1, P2, P1_Points, P2_Points, Matrix) :-
   length(Matrix, Size),
   header(P1, P2, P1_Points, P2_Points, Size),
   
   print_table(Matrix).

% winner_msg(+P, +Points)
winner_msg(P, Points) :-
   format('\n\n\tPlayer ~d WON THIS MATCH : ~d POINTS\n\n', [P, Points]).

% display_winner(+P1, +P1_Points, +P2, +P2_Points)
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



display_menu :-
   write('\n  3 - 6 - 9 \n'),
   write('1 -> Play\n'),
   write('2 -> Settings\n'),
   write('3 -> Rules\n'),
   write('0 -> Exit\n').
   
display_authors :-
   write('Created by : Paul Townsend, 1995\n'),
   write('Implemented in PROLOG by: Joao Malva && Joao Felix , 2023\n').

display_rules(Size) :-
   
   format('O 369 joga-se em um tabuleiro ~dx~d.\n',[Size, Size] ),
   write('Os jogadores jogam um de cada vez colocando uma pedra numa coordenada do tabuleiro.\n'),
   
   write('Colocando as pedras alinhadas em 3, 6 ou 9 pedras (vertical, horizontal ou diagonais).\n'),
   write('3 pedras numa linha -> 1 ponto;\n6 pedras numa linha -> 2 pontos;\n9 pedras numa linha -> 3 pontos;\n'),
   write('Vence o jogador com o maior numero de pontos quando o tabuleiro estiver preenchido.\n'), nl,
   
   write('0 -> Return\n'),
   write('1 -> Authors\n').

display_game_menu :-
   write('\n  GAME ON \n'),
   write('1 -> Player1 Vs Player2\n'),
   write('2 -> Player  Vs CPU\n'),
   write('0 -> Return\n').

display_settings(Size, CPU) :-
   write('\n  SETTINGS\n'),
   format('1 -> Change Board (6x6 or 9x9)\t\t\tBoard\t:\t~dx~d\n', [Size, Size]),
   format('2 -> Change CPU Tatics (Random or Smart)\tCPU\t:\t~w\n', [CPU]),
   write('0 -> Return'), nl.
