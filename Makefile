STACK = stack

.PHONY: build
build:
	$(STACK) build

.PHONY: update
update:
	$(STACK) build --only-dependencies

.PHONY: run
run:
	$(STACK) run

.PHONY: test
test:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make test FILE=<test-file>"; \
		exit 1; \
	fi
	$(STACK) runhaskell src/Main.hs $(FILE) main

.PHONY: test-all
test-all:
	$(STACK) runhaskell tests/Tests.hs