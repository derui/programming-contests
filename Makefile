.PHONY: setup

setup:  ## Setup libraries into current python env
	pip install -r tools/requirements.txt

atcoder/%:
	cd tools && \
	python -m atcoder_contest_extractor $@ "../atcoder"
