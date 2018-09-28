#include "board.h"
#include <tuple>

void Conway::Board::generateRandom(int c)
{
    while (c-- > 0) {
        auto x = rand() % m_width;
        auto y = rand() % m_height;

        set(x, y, 1);
    }
    
             
}

