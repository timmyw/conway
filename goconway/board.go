package goconway

import (
	"fmt"
	"time"
	"math/rand"
)

// Board is a struct that contains a complete conway board
type Board struct {
	height int
	width int
	cells []int
}

// NewBoard creates a new (initialised) board of the specified size
func NewBoard(w int, h int) *Board {
	b := new(Board)
	b.width = w
	b.height = h
	b.cells = make([]int, h*w)
	return b
}

// GenerateRandom randomly sets cells on or off
func (b *Board) GenerateRandom() {
	r := rand.New(rand.NewSource(time.Now().UnixNano() / int64(time.Millisecond)))
	for i := 0; i < b.height; i++ {
		for j := 0; j < b.width; j++ {
			if r.Intn(10) >= 9 {
				b.setCell(i, j, 1)
			}
		}
	}
}

func (b *Board) setCell(i int, j int, v int) {
	b.cells[i*b.width+j] = v
}

func (b *Board) getCell(i int, j int) int {
	return b.cells[i*b.width+j]
}

func (b *Board) countForRow(i int, j int) int {
	cnt := 0
	cnt += b.getCell(i, j)
	if j >= 1 { cnt += b.getCell(i, j-1) }
	if j < b.width { cnt += b.getCell(i, j+1) }
	return cnt
}

func (b *Board) getNeighbours(i int, j int) int {
	cnt := 0

	// Previous row
	if i >= 1 {
		cnt += b.countForRow(i-1, j)
	}

	// Current row
	cnt += b.countForRow(i, j)

	// Following row
	if i < b.height {
		cnt += b.countForRow(i+1, j)
	}
	
	return cnt
}

// Display will dump the current state of the board to stdout
func (b *Board) Display() {
	// fmt.Printf("%d by %d\n", b.height, b.width)
	for i := 0; i < b.height; i++ {
		for j := 0; j < b.width; j++ {
			fmt.Printf("%d", b.cells[i*b.width+j])
		}
		fmt.Printf("\n")
	}
}
