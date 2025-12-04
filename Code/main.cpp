#include <iostream>
#include <thread>
#include "Grid.h"
#include "PrologInterface.h"

int main() {
    // setup the grid bounds
    Grid gameMap(5, 5); 
    
    // setting up a map/maze
    gameMap.setStart(0, 0);
    gameMap.setGoal(4, 4);
    gameMap.setWall(2, 0);
    gameMap.setWall(2, 1);
    gameMap.setWall(2, 2);
    gameMap.setWall(2, 4);

    // print the maze
    gameMap.printMap();

    // exporting to prolog
    std::cout << "Exporting map to 'current_map.pl'...\n";
    if (!gameMap.exportToProlog("current_map.pl")) {
        std::cerr << "Issue writing map file.\n";
        return 1;
    }

    // running the AI solver
    PrologInterface ai;
    std::cout << "Launching SWI-Prolog...\n";
    
    // freezes the program until Prolog finishes (blocking call)
    if (ai.runSolver()) {
        std::cout << "Solver completed successfully.\n";
    } else {
        std::cerr << "Solver returned an error code.\n";
        return 1;
    }

    // read and execute the moves from the prolog generated file
    std::vector<Point> path = ai.readPath("path_output.txt");
    
    if (path.empty()) {
        std::cout << "No path found or output file empty.\n";
    } else {
        std::cout << "Path received. Executing moves:\n";
        // reads the X,Y coordinates
        for (const auto& p : path) {
            std::cout << "Player moved to: (" << p.x << ", " << p.y << ")\n";
        }
    }
    
    std::cout << "--- GOAL REACHED ---\n";
    return 0;
}