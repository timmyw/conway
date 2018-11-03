package goconway

import (
	"fmt"
	"time"
	"math/rand"
)

type Board struct {
	height int
	width int
	cells []int
}

func NewBoard(w int, h int) *Board {
	b := new(Board)
	b.width = w
	b.height = h
	b.cells = make([]int, h*w)
	return b
}

func (b *Board) GenerateRandom() {
	r := rand.New(rand.NewSource(time.Now().UnixNano() / int64(time.Millisecond)))
	for i := 0; i < b.height; i++ {
		for j := 0; j < b.width; j++ {
			if r.Intn(10) >= 9 {
				b.set_cell(i, j, 1)
			}
		}
	}
}

func (b *Board) set_cell(i int, j int, v int) {
	b.cells[i*b.width+j] = v
}

func (b *Board) get_cell(i int, j int) int {
	return b.cells[i*b.width+j]
}

func (b *Board) count_for_row(i int, j int) int {
	cnt := 0
	cnt += b.get_cell(i, j)
	if j >= 1 { cnt += b.get_cell(i, j-1) }
	if j < b.width { cnt += b.get_cell(i, j+1) }
	return cnt
}

func (b *Board) get_neighbours(i int, j int) int {
	cnt := 0

	// Previous row
	if i >= 1 {
		cnt += b.count_for_row(i-1, j)
	}

	// Current row
	cnt += b.count_for_row(i, j)

	// Following row
	if i < b.height {
		cnt += b.count_for_row(i+1, j)
	}
	
	return cnt
}

func (b *Board) Display() {
	fmt.Printf("%d by %d\n", b.height, b.width)
	for i := 0; i < b.height; i++ {
		for j := 0; j < b.width; j++ {
			fmt.Printf("%d", b.cells[i*b.width+j])
		}
		fmt.Printf("\n")
	}
}
