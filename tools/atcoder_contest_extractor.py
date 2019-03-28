
import sys

from pathlib import Path
from bs4 import BeautifulSoup
import requests

if __name__ == '__main__':
    contest: str = sys.argv[1].split("/")[1]
    base_path = sys.argv[2]
    path: Path = Path(base_path) / contest

    if not path.exists():
        path.mkdir(parents=True, mode=0o755)

    contest_tasks: str = "https://atcoder.jp/contests/{}/tasks".format(contest)

    r: requests.Response = requests.get(contest_tasks)

    soup = BeautifulSoup(r.text, 'html.parser')

    print(soup.prettify())
