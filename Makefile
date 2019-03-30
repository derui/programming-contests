.PHONY: setup

setup:  ## Setup libraries into current python env
	pip install -r tools/requirements.txt

atcoder\:%:
	cd tools && \
	python -m atcoder_contest_initializer "$@" "../atcoder"

test\:atcoder:
	find atcoder -mindepth 2 -maxdepth 2 -type d | xargs -I{} make -C {} test
