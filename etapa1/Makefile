all: build

main: main.c
	gcc main.c -c
	gcc utils.c -c

scanner: scanner.l
	flex scanner.l
	gcc lex.yy.c -c

build: main scanner
	gcc lex.yy.o main.o utils.o -o etapa1

run: build
	./etapa1

test: build
	./test.sh

clean:
	rm *.o
	rm etapa1
	rm lex.yy.c