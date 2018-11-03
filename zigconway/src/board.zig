/// A struct for storing a conway board

const bb = []u8.{0} ** 10;

pub const Board = struct. {

    const height = 20;
    const width = 20;

    const boardSize = height * width;
    
    cells:[boardSize]u8,

    pub fn init() Board {
        var b = Board.{
            .cells = []u8.{0} ** boardSize
        };

        return b;
    }
};

pub fn displayBoard(board: Board) {
}
