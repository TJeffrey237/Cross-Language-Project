#pragma once
#include <vector>
#include <string>
#include <fstream>
#include <iostream>

enum CellType { EMPTY, WALL, START, GOAL };

class Grid {
private:
    int width, height;
    std::vector<std::vector<CellType>> map;
    int startX, startY, goalX, goalY;

public:
    // the constructor, initializes a grid of a given width & height with EMPTY spaces
    Grid(int w, int h) : width(w), height(h) {
        map.resize(h, std::vector<CellType>(w, EMPTY));
    }

    // setter functions to update the cells in the grid
    void setWall(int x, int y) { map[y][x] = WALL; }
    void setStart(int x, int y) { startX = x; startY = y; map[y][x] = START; }
    void setGoal(int x, int y) { goalX = x; goalY = y; map[y][x] = GOAL; }

    // converts the C++ data into Prolog facts
    bool exportToProlog(const std::string& filename) {
        // creates the current_map.pl file
        std::ofstream outFile(filename);
        if (!outFile.is_open()) return false;

        // pushing the static facts into the file (start, goal, grid size)
        outFile << "bounds(" << width << ", " << height << ").\n";
        outFile << "start(" << startX << ", " << startY << ").\n";
        outFile << "goal(" << goalX << ", " << goalY << ").\n";

        // writes all the wall facts
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                if (map[y][x] == WALL) {
                    outFile << "wall(" << x << ", " << y << ").\n";
                }
            }
        }
        // close file
        outFile.close();
        return true;
    }

    // visualize the grid to the console
    void printMap() {
        std::cout << "--- MAZE ---\n";
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                char symbol;
                // prints character based on cell type
                switch (map[y][x]) {
                    case WALL:  symbol = '#'; break;
                    case START: symbol = 'S'; break;
                    case GOAL:  symbol = 'G'; break;
                    default:    symbol = '.'; break;
                }
                
                std::cout << symbol << " ";
            }
            std::cout << "\n";
        }
        std::cout << "-------------------------\n";
    }
};