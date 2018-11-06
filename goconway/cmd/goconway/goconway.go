package main

import (
	"fmt"
	conway "gitea.zipt.co/timmy/goconway"
)

func main() {
	fmt.Printf("conway\n")

	b := conway.NewBoard(20, 20)
	b.GenerateRandom()
	b.Display()
}
