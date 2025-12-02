% logic.pl

% Dynamic predicates to be loaded from C++ generated file
:- dynamic start/2.
:- dynamic goal/2.
:- dynamic wall/2.
:- dynamic bounds/2.

% 1. The Entry Point
solve :-
    catch(
        (
            consult('current_map.pl'),
            start(SX, SY),
            goal(GX, GY),
            astar([[0, SX, SY, []]], [GX, GY], ReversedPath),
            reverse(ReversedPath, Path),
            save_path_to_file(Path)
        ),
        Error,
        (
            open('path_output.txt', write, Stream),
            write(Stream, 'ERROR'), % Signal C++ that pathing failed
            close(Stream),
            print_message(error, Error)
        )
    ),
    halt.

% 2. The File Manager
save_path_to_file(Path) :-
    open('path_output.txt', write, Stream),
    write_lines(Path, Stream), % Call the recursive writer
    close(Stream).

% 3. The Recursive Writer (The part you asked about)
% Base case: Empty list, do nothing
write_lines([], _).

% Recursive case: Write X and Y, new line, then process the rest (T)
write_lines([move(X,Y) | T], Stream) :-
    write(Stream, X), 
    write(Stream, ' '), 
    write(Stream, Y), 
    nl(Stream),         % Creates a new line
    write_lines(T, Stream). % Pass Stream to the next iteration

% A* Algorithm Skeleton
% queue: [[F_Score, X, Y, Path_So_Far], ...]
astar([[_, X, Y, Path] | _], [X, Y], Path) :- !. % Goal Reached

astar([[_F, X, Y, Path] | RestQueue], Goal, FinalPath) :-
    findall(
        [NewF, NX, NY, [move(NX, NY) | Path]],
        (
            move(X, Y, NX, NY),
            \+ member(move(NX, NY), Path), % Avoid cycles
            heuristic(NX, NY, Goal, H),
            length(Path, G),
            NewF is G + 1 + H
        ),
        Children
    ),
    append(RestQueue, Children, NewQueue),
    sort(NewQueue, SortedQueue), % Sort by F-Score (Simple Priority Queue)
    astar(SortedQueue, Goal, FinalPath).

% Allowable moves (Up, Down, Left, Right)
move(X, Y, NX, NY) :- NX is X, NY is Y - 1, legal(NX, NY). % Up
move(X, Y, NX, NY) :- NX is X, NY is Y + 1, legal(NX, NY). % Down
move(X, Y, NX, NY) :- NX is X - 1, NY is Y, legal(NX, NY). % Left
move(X, Y, NX, NY) :- NX is X + 1, NY is Y, legal(NX, NY). % Right

% Check if a cell is within bounds and not a wall
legal(X, Y) :-
    bounds(W, H),
    X >= 0, X < W,
    Y >= 0, Y < H,
    \+ wall(X, Y).

% Manhattan Distance Heuristic
heuristic(X, Y, [GX, GY], H) :-
    H is abs(X - GX) + abs(Y - GY).