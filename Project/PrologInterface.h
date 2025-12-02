#pragma once
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <cstdlib>

struct Point { int x, y; };

class PrologInterface {
public:
    // Runs the SWI Prolog interpreter:
        // -s logic.pl : Loads your logic file
        // -g solve    : Executes the 'solve' predicate immediately
        // -t halt     : Exits after 'solve' finishes
    bool runSolver() {
        std::string command = "swipl -s logic.pl -g solve -t halt";
        int result = std::system(command.c_str());
        // the OS will return a 0 if it was successful
        return result == 0;
    }

    // reads the path_output.txt file left by the Prolog interpreter
    std::vector<Point> readPath(const std::string& filename) {
        std::vector<Point> path;
        std::ifstream inFile(filename);
        int x, y;
        // reads each line containing two integers and assigns them to x, y
        while (inFile >> x >> y) {
            // adds the points to the path
            path.push_back({x, y});
        }
        // send the path back to main.cpp
        return path;
    }
};