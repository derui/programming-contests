package main
import "fmt"
import "bufio"
import "os"

type game struct {
	Time int
	Characters string
}

func loadGames(games int, sc *bufio.Scanner) []game {
	// 0-9で表す。
	items := make([]game, 0)

	for i := 0;i < games; i++ {
		var time int
		var chars string
		sc.Scan()

		text := sc.Text()
		fmt.Sscanf(text, "%d %s", &time, &chars)

		items = append(items, game{time, chars})
	}

	return items
}

func main() {
	var games int
	fmt.Scan(&games)

	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanLines)
	items := loadGames(games, sc)

	typed, untyped := solve(items)
	fmt.Println(typed, untyped)
}

const (
	typePerSec = 12.0
)

func solve(games []game) (int, int) {
	result := 0
	untyped := 0

	// それぞれのアイテムについて、2で割った商だけパワーアップできる。
	for _, v := range games {
		typed := int(float64(v.Time * typePerSec / 1000))
		if typed >= len(v.Characters) {
			result += len(v.Characters)
		} else {
			result += typed
			untyped += len(v.Characters) - typed
		}
	}

	return result, untyped
}
