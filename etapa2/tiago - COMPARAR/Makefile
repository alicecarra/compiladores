all: build

main: main.c
	gcc main.c -c
	gcc utils.c -c 

scanner: scanner.l
	flex scanner.l
	gcc -c lex.yy.c 
	
bison: parser.y
	bison -d parser.y 
	gcc -c parser.tab.c 

build: bison scanner main  
	gcc lex.yy.o parser.tab.o main.o utils.o -o etapa2

run: build
	./etapa2

test: build
	./test.sh

clean:
	rm *.o
	rm etapa2
	rm lex.yy.c
	rm parser.tab.*
		
entrega: 
	tar cvzf etapa2.tgz .
