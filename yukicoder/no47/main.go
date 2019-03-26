package main
import "fmt"

func main() {
	var toEat int
	fmt.Scanf("%d", &toEat)

	fmt.Println(solve(toEat))
}

func solve(toEat int) int {
	cookies := 1
	count := 0

	for cookies < toEat {
		count = count + 1

		if cookies * 2 > toEat {
			cookies += (toEat - cookies)
		} else {
			cookies *= 2
		}
	}

	return count
}
