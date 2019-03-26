package main
import "fmt"

func main() {
	var dices int
	fmt.Scanf("%dd", &dices)

	fmt.Println(solve(dices))
}

func solve(dices int) float32 {
	return float32(dices) * 3.5
}
