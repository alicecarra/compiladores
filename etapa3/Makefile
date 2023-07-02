all: build

build:
	cargo build --release
	cp target/release/etapa3 . 

run: build
	./etapa3

clean:
	cargo clean
	rm -f etapa3 Cargo.lock
	rm -rf target
		
entrega: 
	tar cvzf etapa3.tgz .
