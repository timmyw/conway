#include "board.h"

#include <tuple>
#include <iostream>

void Conway::Board::generateRandom(int c)
{
    while (c-- > 0) {
        auto x = rand() % m_width;
        auto y = rand() % m_height;

        set(x, y, 1);
    }
    
             
}

void Conway::Board::show() const
{
    for (int y = 0; y <= m_height; y++)
    {
        for (int x = 0; x <= m_width; x++)
            std::cout << get(x,y);
        std::cout << endl;
    }
}
