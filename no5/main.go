package main
import "fmt"
import "bufio"
import "os"
import "sort"
import "strconv"

func loadBlocks(count int, sc *bufio.Scanner) []int {
	blocks := make([]int, 0)

	for i := 0; i < count; i++ {
		sc.Scan()
		v, e := strconv.Atoi(sc.Text())
		if  e != nil {
			panic(e)
		}
		blocks = append(blocks, v)
	}

	return blocks
}

func main() {
	var width int
	var numberOfBlock int
	fmt.Scan(&width)
	fmt.Scan(&numberOfBlock)

	var sc = bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	blocks := loadBlocks(numberOfBlock, sc)

	fmt.Println(solve(width, blocks))
}

func solve(width int, blocks []int) int {
	sort.Ints(blocks)

	current := 0
	count := 0
	for _, v := range blocks {
		if current + v > width {
			break
		}
		count++
		current += v
	}

	return count
}
