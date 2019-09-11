all: build

build:
	@emacs --quick --script publish.el --funcall=ajgrf/publish
	@sed -i 's/index.html//g' ./public/index.html ./public/post/index.html ./public/atom.xml

clean:
	@rm -rf ./public ./cache ./content/index.org ./content/post/index.org

preview: clean build
	@cd ./public && python3 -m http.server 8000

.PHONY: all build clean preview
