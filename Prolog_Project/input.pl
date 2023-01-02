:- include('utils.pl').

% read_integer(?X, +Min, +Max, +Request)
read_integer(X, Min, Max, Request) :-
    repeat,
        write(Request),
        read(Y),
        (  (integer(Y), between(Y, Min, Max))
        ->  X = Y,
            !, true
        ;   format('\nOnly values between ~d and ~d\n', [Min, Max]),
            nl,
            fail
        ).

% read_coords(-X, -Y, +Min, +Max)
read_coords(X, Y, Min, Max) :-
    
    read_integer(X, Min, Max, '\tChose Colomn\n'),
    read_integer(Y, Min, Max, '\tChose Row\n'), 

    format('\nColomn : ~d\tRow : ~d\n', [X, Y]).