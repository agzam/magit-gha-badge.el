# Every emacs invocation sandboxes user-emacs-directory: neither -Q nor
# --batch relocates it, and stray runs would litter the real ~/.emacs.d
SANDBOX = $(CURDIR)/.sandbox
ELPA_DIR = $(SANDBOX)/elpa

EMACS_BATCH = emacs -Q --batch --init-directory $(SANDBOX) \
	--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	--eval "(package-initialize)"

.PHONY: help deps test lint check-compile clean

help:
	@echo "Available commands:"
	@echo "  make deps           Install dependencies into $(ELPA_DIR)"
	@echo "  make test           Run unit tests"
	@echo "  make lint           package-lint the package files"
	@echo "  make check-compile  Check for clean byte-compilation"
	@echo "  make clean          Remove sandbox and compiled files"

$(ELPA_DIR):
	@echo "Installing dependencies..."
	$(EMACS_BATCH) \
	--eval "(package-refresh-contents)" \
	--eval "(dolist (p '(magit ghub buttercup package-lint)) (package-install p))"

deps: $(ELPA_DIR)

test: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . \
	--eval "(setq buttercup-stack-frame-style 'omit)" \
	-l test/magit-gha-badge-tests.el \
	-l test/magit-gha-badge-runs-tests.el \
	--funcall buttercup-run

lint: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit magit-gha-badge.el magit-gha-badge-runs.el

# the .elc are dropped again: `load' prefers one over a newer .el, so a
# leftover would have `make test' run yesterday's code
check-compile: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(dolist (file '(\"magit-gha-badge.el\" \"magit-gha-badge-runs.el\")) \
	          (unless (byte-compile-file file) (kill-emacs 1)))"
	rm -f magit-gha-badge.elc magit-gha-badge-runs.elc

clean:
	rm -f *.elc test/*.elc
	rm -rf $(SANDBOX)
