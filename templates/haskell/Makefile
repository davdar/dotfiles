DEV  := Template.dev
ROOT := src/Template.hs
NAME := template

$(NAME): build
	rm -f $(NAME)
	ln -s `stack path --dist-dir`/build/$(NAME)/$(NAME) ./

.PHONY: run
run: .stack-work
	stack run

.PHONY: build
build: .stack-work
	stack build

.PHONY: dev
dev: .stack-work
	ghcid --warnings --test=$(DEV)

.PHONY: trace
trace: .stack-work
	stack run --trace -- $(ARGS) +RTS -xc

.PHONY: profile
profile: .stack-work
	stack run --profile -- $(ARGS) +RTS -p

.PHONY: ghci
ghci: .stack-work
	stack ghci

.PHONY: docs
docs: .stack-work
	stack haddock
	rm -rf ./docs
	cp -r `stack path --local-doc-root` ./docs

.PHONY: clean
clean:
	stack clean --full
	rm -f $(NAME)
	rm -f $(NAME).cabal
	rm -rf docs

.stack-work:
	stack setup

.PHONY: re
re:
	touch $(ROOT)

.PHONY: hoogle
hoogle:
	stack hoogle -- generate --local
	(sleep 1 && open http://localhost:8080/?scope=package%3A$(NAME)) &
	stack hoogle -- server --local --port=8080

