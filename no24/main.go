package main
import "fmt"
import "bufio"
import "os"
import "math"

type turn struct {
	Numbers []int

	Contains bool
}

func loadTurns(numOfTurn int, sc *bufio.Scanner) []turn {
	turns := make([]turn, 0)

	for i := 0;i < numOfTurn; i++ {
		var r1, r2, r3, r4 int
		var yesno string
		sc.Scan()

		cur := turn{}
		text := sc.Text()
		fmt.Sscanf(text, "%d %d %d %d %s", &r1, &r2, &r3, &r4, &yesno)
		if yesno == "YES" {
			cur.Contains = true
		} else {
			cur.Contains = false
		}
		cur.Numbers = []int{r1, r2, r3,r4}

		turns = append(turns, cur)
	}

	return turns
}

func main() {
	var turn int
	fmt.Scan(&turn)

	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanLines)
	turns := loadTurns(turn, sc)

	fmt.Println(solve(turn, turns))
}

func solve(turn int, turns []turn) int {
	results := make([]int, 10);
	for i := range results {
		results[i] = 0
	}

	// それぞれのturnをすべてチェックして、含まれていれば＋、含まれていなければマイナス
	// として扱う。
	// 複数回Containsであるとされたデータは、必ず最も大きい値となる。
	for _, v := range turns {
		for _, num := range v.Numbers {
			if v.Contains {
				results[num] += 1
			} else {
				results[num] -= 1
			}
		}
	}

	target := math.MinInt64
	resultNumber := -1
	for ind, v := range results {
		if v > target {
			target = v
			resultNumber = ind
		}
	}

	return resultNumber
}
