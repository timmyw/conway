/// A struct for storing a conway board

pub const Board = struct. {
    width: u32,
    height: u32,

    cells: []u8,

    pub fn init(w: u32, h: u32) Board {
        var b = Board.{
            .width = w,
            .height = h, 
            .cells = undefined
        };

        return b;
    }
};
