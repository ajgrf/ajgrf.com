all: build

build:
	@emacs --quick --script publish.el --funcall=org-publish-all

clean:
	@rm -rf ./public ./cache ./content/index.org ./content/post/index.org

preview: build
	@cd ./public && python3 -m http.server 8000

.PHONY: all build clean preview
