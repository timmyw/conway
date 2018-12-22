/*
 */

#if !defined(_CONWAY_BOARD_H_)

#include <vector>
#include <memory>

namespace Conway
{
    
    class Board
    {
    public:

        Board(int width, int height) : m_board(std::make_unique<int>(width * height)), m_width(width), m_height(height) {
            ::memset(m_board.get(), 0, width * height);
            // for (int x = 0; x < m_width; x++)
            //     for (int y = 0; y < m_height; y++)
            //         m_board[x][y] = 0;
        }

        void generateRandom();

        void set(int x, int y, int v) {
            m_board[x+y*m_width] = v;
        }

        int get(int x, int y) {
            return m_board[x+y*m_width];
        }

        void show() const;
        
    private:
        std::unique_ptr<int> m_board;
        int m_width;
        int m_height;
    };

};

#endif // _CONWAY_BOARD_H_
