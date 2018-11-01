#include <iostream>

#include "board.h"

int main(int argc, char* argv[])
{
    Conway::Board<20, 20> board;

    board.generateRandom(40);
    board.show();
    
    return 0;
}
