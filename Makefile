STACK = stack
STEP ?= false

.PHONY: build
build:
	$(STACK) build

.PHONY: update
update:
	$(STACK) build --only-dependencies

.PHONY: run
run:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make run FILE=<program-file> [STEP=true]"; \
		exit 1; \
	fi
	$(STACK) runhaskell src/Main.hs $(FILE) main $(STEP)

.PHONY: test-all
test-all:
	$(STACK) runhaskell tests/Tests.hs