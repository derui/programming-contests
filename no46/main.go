package main
import "fmt"

func main() {
	var pitch, block int
	fmt.Scanf("%d %d", &pitch, &block)

	fmt.Println(solve(pitch, block))
}

func solve(pitch int, block int) int {
	result := block / pitch

	if (result * pitch) < block {
		return result + 1
	}

	return result
}
