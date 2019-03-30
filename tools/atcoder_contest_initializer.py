
import sys

import re
from pathlib import Path
from bs4 import BeautifulSoup
import requests

from jinja2 import Environment, FileSystemLoader


def ignore_part(part):

    header = part.find("h3")
    if header is None:
        return True

    text = header.string
    if text == "問題文":
        return True
    elif text == "入力":
        return True
    elif text == "出力":
        return True
    else:
        return False


def is_input_example(part):
    header = part.find("h3")
    if header is None:
        return True

    text = header.string
    return text.find("入力例") != -1


def is_output_example(part):
    header = part.find("h3")
    if header is None:
        return True

    text = header.string
    return text.find("出力例") != -1


def extract_example(part, path):
    """
    Get example from the part and write it to path.
    """
    with open(path, "w") as f:
        f.write(re.sub(r"\r\n", "\n", part.find("section").pre.get_text(strip=True)))
        f.write("\n")


def scrape_problem(url, problem_name, base_path):
    """
    scrape url to get examples.
    """
    problem_path = Path(base_path, problem_name)

    if not problem_path.exists():
        problem_path.mkdir(parents=True, mode=0o755)

    r = requests.get("https://atcoder.jp{}".format(url))

    soup = BeautifulSoup(r.text, 'html.parser')

    input_count = 1
    output_count = 1
    input_examples = []
    output_examples = []

    for part in soup.find_all(class_="part"):
        in_file = None
        out_file = None

        if ignore_part(part):
            continue

        if is_input_example(part):
            in_file = Path(problem_path, "in{}".format(input_count))
            extract_example(part, in_file)
            input_count = input_count + 1
            input_examples.append(in_file)

        if is_output_example(part):
            out_file = Path(problem_path, "out{}".format(output_count))
            extract_example(part, out_file)
            output_count = output_count + 1
            output_examples.append(out_file)

    examples = []
    for i in range(min(input_count - 1, output_count - 1)):
        examples.append({"in_file_name": input_examples[i].name,
                         "out_file_name": output_examples[i].name})

    return problem_path, examples


def generate_initial_program(template_env, lang, problem_path, scraped_examples):
    if lang == "ocaml":
        main_tmpl = template_env.get_template("{}/main.ml.j2".format(lang))
        makefile_tmpl = template_env.get_template("{}/Makefile.j2".format(lang))

        print("Generating main.ml for {}...".format(problem_path))
        if not Path(problem_path, "main.ml").exists():
            with open(Path(problem_path, "main.ml"), "w") as f:
                f.write(main_tmpl.render(examples=scraped_examples))

        print("Generating Makefile for {}...".format(problem_path))
        if not Path(problem_path, "Makefile").exists():
            with open(Path(problem_path, "Makefile"), "w") as f:
                f.write(makefile_tmpl.render(examples=scraped_examples))
    else:
        print("Unknown language: {}".format(lang))
        exit(1)


if __name__ == '__main__':
    template_env = Environment(loader=FileSystemLoader('./template', encoding='utf8'))

    contest: str = sys.argv[1].split(":")[1]
    lang = sys.argv[1].split(":")[2]
    base_path = sys.argv[2]
    path: Path = Path(base_path, contest)

    if not path.exists():
        path.mkdir(parents=True, mode=0o755)

    contest_tasks: str = "https://atcoder.jp/contests/{}/tasks".format(contest)

    r: requests.Response = requests.get(contest_tasks)

    soup = BeautifulSoup(r.text, 'html.parser')

    container = soup.find(id="main-container")
    for problem in container.find('tbody').find_all("tr"):
        problem_path, scraped_examples = scrape_problem(problem.a.get("href"), problem.a.string, path)
        generate_initial_program(template_env, lang, problem_path, scraped_examples)
