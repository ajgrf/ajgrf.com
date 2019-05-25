all: preview

build:
	@ikiwiki --setup ikiwiki.setup
	@./acme-challenge.sh

clean:
	@echo "Cleaning ./public destdir..."
	@rm -rf ./public

preview:
	@mkdir -p public/css public/fonts public/js
	@ikiwiki --setup ikiwiki.setup --no-usedirs --set bootstrap_local=1
	@echo "Open `readlink -e ./public/index.html` to view the wiki".

.PHONY: all build clean preview
