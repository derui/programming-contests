package main
import "fmt"

func main() {
	var work, limit int
	fmt.Scanf("%d", &work)
	fmt.Scanf("%d", &limit)

	fmt.Println(solve(work, limit))
}

func solve(work int, limit int) int {
	current := limit
	restWork := work
	
	for current > 1 {
		todayWork := current * current
		current = current - 1
		
		if todayWork > restWork {
			continue
		}

		restWork = restWork - (restWork / todayWork)
	}

	return restWork
}
