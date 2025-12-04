% dynamic predicates to be loaded from the current_map.pl file from the C++
:- dynamic start/2.
:- dynamic goal/2.
:- dynamic wall/2.
:- dynamic bounds/2.

% this executes the main logic for pathfinding
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
            write(Stream, 'ERROR'),
            close(Stream),
            print_message(error, Error)
        )
    ),
    halt.

% this writes the path to a specified file (path_output.txt)
% uses the write_lines helper
save_path_to_file(Path) :-
    open('path_output.txt', write, Stream),
    write_lines(Path, Stream), % Call the recursive writer
    close(Stream).

% this takes the path and writes the coordinate pairs to a file
% base case: empty list = do nothing
write_lines([], _).

% recursive case: Write X and Y, new line, then process the rest
write_lines([move(X,Y) | T], Stream) :-
    write(Stream, X), 
    write(Stream, ' '), 
    write(Stream, Y), 
    nl(Stream),
    write_lines(T, Stream).

% A* algorithm implementation
astar([[_, X, Y, Path] | _], [X, Y], Path) :- !. % Goal Reached

astar([[_F, X, Y, Path] | RestQueue], Goal, FinalPath) :-
    % finds all solutions for a goal and puts them into the Children list
    findall(
        [NewF, NX, NY, [move(NX, NY) | Path]],
        (
            % finds valid neighboring coordinate
            move(X, Y, NX, NY),
            % checks if the new move is not already in the Path
            \+ member(move(NX, NY), Path),
            % calculate heuristic value
            heuristic(NX, NY, Goal, H),
            % calculates current path length
            length(Path, G),
            % the F score
            NewF is G + 1 + H
        ),
        Children
    ),
    % combines all of the unprocessed nodes and the Children to NewQueue
    append(RestQueue, Children, NewQueue),
    % implements a priority queue sorted F score
    sort(NewQueue, SortedQueue),
    % process the newly sorted queue
    astar(SortedQueue, Goal, FinalPath).

% defines the legal moves
move(X, Y, NX, NY) :- NX is X, NY is Y - 1, legal(NX, NY). % up
move(X, Y, NX, NY) :- NX is X, NY is Y + 1, legal(NX, NY). % down
move(X, Y, NX, NY) :- NX is X - 1, NY is Y, legal(NX, NY). % left
move(X, Y, NX, NY) :- NX is X + 1, NY is Y, legal(NX, NY). % right

% for checking if the cell is within grid bounds
legal(X, Y) :-
    bounds(W, H),
    X >= 0, X < W,
    Y >= 0, Y < H,
    \+ wall(X, Y).

% calculates the heuristic value sing Manhattan distance formula
heuristic(X, Y, [GX, GY], H) :-
    H is abs(X - GX) + abs(Y - GY).