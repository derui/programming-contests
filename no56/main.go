ackage main
import "fmt"

func main() {
	var price, tax int
	fmt.Scanf("%d %d", &price, &tax)

	fmt.Println(solve(price, tax))
}

func solve(price int, taxPercent int) int {
	tax := (price * 100) * taxPercent
	return price + (tax / 10000)
}
