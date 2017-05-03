.PHONY: all compile lint test run

all: compile lint

compile:
	@stack build --fast --no-run-tests

lint: 
	@stack exec hlint -- .

test: compile
	@stack test

run: compile
	@stack exec meatbar

ghcid:
	@stack exec ghcid -- \
		--command 'stack ghci meatbar:lib meatbar:meatbar-test' \
		--test ':main'
