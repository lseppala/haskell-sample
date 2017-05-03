.PHONY: all compile lint test

all: compile lint

compile:
	@stack build --fast --trace --no-run-tests

lint: 
	@stack exec hlint -- .

test: all
	@stack test

run: all
	@stack exec meatbar

ghcid:
	@stack exec ghcid -- \
		--command 'stack ghci meatbar:lib meatbar:meatbar-test' \
		--test ':main'
