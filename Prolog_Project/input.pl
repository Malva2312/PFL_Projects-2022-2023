:- include('utils.pl').

%
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

%
read_coords(X, Y, Min, Max) :-
    
    read_integer(X, Min, Max, 'Chose Colomn\n'),
    read_integer(Y, Min, Max, '\nChose Row\n'), 

    format('\nColomn : ~d\tRow : ~d\n', [X, Y]).