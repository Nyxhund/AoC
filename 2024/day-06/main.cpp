#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>

int part1(std::vector<std::string> lines, int X, int Y)
{
    unsigned int lineLen = lines[0].size();
    unsigned int count = 1;

    int directionX = 0;   //  0 -> 1 -> 0 -> -1
    int directionY = -1;  // -1 -> 0 -> 1 -> 0

    while (X + directionX >= 0 && X + directionX < lineLen && Y + directionY >= 0 && Y + directionY < lines.size())
    {
        char next = lines[Y + directionY][X + directionX];

        if (next == '#')
        {
            int tmp = directionX;
            directionX = directionY * -1;
            directionY = tmp;
        }

        lines[Y][X] = 'X';
        X += directionX;
        Y += directionY;

        if (lines[Y][X] != 'X')
            count++;
    }

    return count;
}

bool testLoop(std::vector<std::string> lines, int X, int Y, int directionX, int directionY)
{
    int startX = X;
    int startY = Y;
    int startDirectionX = directionX;
    int startDirectionY = directionY;
    unsigned int lineLen = lines[0].size();
    unsigned int count = 0;
    unsigned int limit = 100000000;

    while (X + directionX >= 0 && X + directionX < lineLen && Y + directionY >= 0 && Y + directionY < lines.size() && count < limit)
    {
        char next = lines[Y + directionY][X + directionX];

        if (next == '#')
        {
            int tmp = directionX;
            directionX = directionY * -1;
            directionY = tmp;
        }

        X += directionX;
        Y += directionY;

        if (X == startX && Y == startY && startDirectionX == directionX && startDirectionY == directionY)
            return true;

        count ++;
    }

    return count > limit;
}

int part2(std::vector<std::string> lines, int X, int Y)
{
    unsigned int lineLen = lines[0].size();
    unsigned int count = 0;

    int directionX = 0;   //  0 -> 1 -> 0 -> -1
    int directionY = -1;  // -1 -> 0 -> 1 -> 0

    while (X + directionX >= 0 && X + directionX < lineLen && Y + directionY >= 0 && Y + directionY < lines.size())
    {
        char next = lines[Y + directionY][X + directionX];

        if (next == '#')
        {
            int tmp = directionX;
            directionX = directionY * -1;
            directionY = tmp;
        }
        else if (next != 'X')
        {
            lines[Y + directionY][X + directionX] = '#';

            std::cout << "Testing new " << X << " " << Y << std::endl;
            if (testLoop(lines, X, Y, directionX, directionY))
                count++;

            lines[Y + directionY][X + directionX] = '.';
        }

        lines[Y][X] = 'X';
        X += directionX;
        Y += directionY;
    }

    return count;
}


int main(int argc, char* argv[])
{
    // std::string path("example.txt");
    std::string path("input.txt");

    std::ifstream stream(path);

    std::string line;
    std::vector<std::string> lines{};

    int X = 0;
    int Y = 0;

    while (stream >> line)
    {
        lines.push_back(line);
        // std::cout << line << std::endl;

        std::string::size_type n = line.find('^');
        if (n != std::string::npos)
        {
            Y = lines.size() - 1;
            X = n;
        }
    }

    unsigned int count = part1(lines, X, Y);
    std::cout << "Part 1: " << count << std::endl;

    unsigned int count2 = part2(lines, X, Y);

    // for (auto& line : lines)
    //      std::cout << line << std::endl;

    std::cout << "Part 2: " << count2 << std::endl;
}
