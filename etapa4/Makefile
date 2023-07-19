all: build

build:
	cargo build --release
	cp target/release/etapa4 . 

run: build
	./etapa4

clean:
	cargo clean
	rm -f etapa4 Cargo.lock
	rm -rf target
		
entrega: 
	tar cvzf etapa4.tgz .
