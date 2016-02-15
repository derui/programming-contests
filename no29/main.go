package main
import "fmt"
import "bufio"
import "os"

func loadItems(numOfDefeats int, sc *bufio.Scanner) []int {
	// 0-9で表す。
	items := make([]int, 10)

	for i := 0;i < numOfDefeats; i++ {
		var r1, r2, r3 int
		sc.Scan()

		text := sc.Text()
		fmt.Sscanf(text, "%d %d %d", &r1, &r2, &r3)
		for _, v := range []int{r1, r2, r3} {
			items[v - 1]++
		}
	}

	return items
}

func main() {
	var defeats int
	fmt.Scan(&defeats)

	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanLines)
	items := loadItems(defeats, sc)

	fmt.Println(solve(items))
}

func solve(defeats []int) int {
	result := 0

	// それぞれのアイテムについて、2で割った商だけパワーアップできる。
	for ind, v := range defeats {
		quotient := v / 2
		rem := v % 2
		result += quotient
		defeats[ind] = rem
	}

	// 残ったアイテムの個数を4で割った商の回数だけパワーアップできる
	sum := 0
	for _, v := range defeats {
		sum += v
	}
	result += sum / 4

	return result
}
