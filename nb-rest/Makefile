TARGET=nb-rest

build:
	@rm -f .stack-work/logs/*
	stack $@
	@if [ `find .stack-work/logs/ -type f | wc -l` -gt 0 ]; then \
	  ls -tr .stack-work/logs/* | xargs cat; \
	fi

.PHONY: test
test:
	stack $@

it:
	./test.sh

its:
	./test.sh 2>/dev/null


clean:
	stack $@

run: build
	stack exec $(TARGET)-exe


repl:
	stack ghci --main-is $(TARGET):exe:$(TARGET)-exe

tags:
	hasktags --etags src app

