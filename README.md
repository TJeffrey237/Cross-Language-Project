# Hybrid C++/Prolog A* Pathfinding System
## Overview
This project demonstrates a hybrid programming architecture that solves the grid-based pathfinding problem using two distinct paradigms: C++ for managing the environment and control flow, and Prolog (specifically SWI-Prolog) for executing the core search algorithm.

The system uses a simple Blackboard Integration model, communicating between the processes via file I/O.

## System Components
The system is split into three main logical parts across five files:
### C++ Host (Control & Data Manager )
- <b>grid.h:</b>
  - Encapsulates the map (dimensions, start/goal, walls). Includes exportToProlog() to translate map data into Prolog facts and write them to current_map.pl.
- <b>PrologInterface.h:
  - Manages the synchronous execution of the Prolog solver (runSolver()) via a std::system call and retrieves the computed path from path_output.txt (readPath()).
- <b>main.cpp:</b>
  - Initializes the map, calls the export function, launches the solver, reads the result, and prints the path execution.
### Prolog Slave (Reasoning & Algorithm)
- <b>logic.pl:</b>
  - Defines the A* search logic (astar/3), movement rules (move/4), constraints (legal/2), and the Manhattan distance heuristic (heuristic/4). It contains the solve predicate, which is the entry point from C++.
### Blackboard (Communication)
- <b>current_map.pl:</b>
  - Input: C++ writes map facts (e.g., wall(x, y).).
- <b>path_output.txtL:</b>
  - Output: Prolog writes the computed path as space-separated coordinates, one per line.

## Execution Flow
The system operates in a sequential cycle orchestrated by main.cpp:
1. <b>Map Setup (C++):</b> main.cpp initializes the Grid object and defines its size, start, goal, and walls.
2. <b>Data Translation (C++):</b> gameMap.exportToProlog("current_map.pl") is called. The C++ data is converted into Prolog facts and written to the input file.
3. <b>Control Handover (C++ -> Prolog):</b> ai.runSolver() executes the command swipl -s logic.pl -g solve -t halt. C++ blocks (pauses).
4. <b>A* Execution (Prolog):</b> The solve predicate loads the map facts (current_map.pl), executes the astar search, and computes the optimal path.
5. <b>Result Output (Prolog -> C++):</b> The resulting path coordinates are written to path_output.txt via save_path_to_file/1.
6. <b>Control Return:</b> Prolog exits (halt). C++ unblocks.
7. <b>Path Consumption (C++):</b> ai.readPath("path_output.txt") reads the coordinates from the file, converts them back to a std::vector<Point>, and prints the final execution sequence.

## Paradigm Integration Rationale
The design is motivated by utilizing the core strengths of each language:
- <b>C++ (Object-Oriented):</b>
  - Used for: System Control, File I/O, Data Structure Management.
  - Benefit: Efficient low-level control and integration with the operating system.
- <b>Prolog (Logic):</b>
  - Used for: A* Search, Rule-based Constraints, Heuristics.
  - Benefit: High-level, concise implementation of complex search logic using built-in unification, pattern matching, and backtracking.
