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

	s := b.Dump()
	fmt.Printf("%s", s)

	b2 := conway.NewBoard(0, 0)
	b2.Load(s)

	fmt.Printf("Loaded:\n")
	b2.Display()
}
