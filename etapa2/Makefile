all: build

main: main.c
	gcc main.c -c
	gcc utils.c -c -Wno-discarded-qualifiers

scanner: parser scanner.l
	flex scanner.l
	gcc lex.yy.c -c

parser: parser.y
	bison -d parser.y
	gcc parser.tab.c -c

build: parser scanner main 
	gcc parser.tab.o lex.yy.o main.o utils.o -o etapa2

run: build
	./etapa2

test: build
	./test.sh

clean:
	rm parser.tab.*
	rm etapa2
	rm *.o
	rm lex.yy.c


entrega: 
	tar cvzf etapa2.tgz .