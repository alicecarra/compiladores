all: build

build:
	cargo build --release
	cp target/release/etapa5 . 

run: build
	./etapa5

clean:
	cargo clean
	rm -f etapa5 Cargo.lock
	rm -rf target
		
entrega: 
	tar cvzf etapa5.tgz .
