all: build

build:
	cargo build --release
	cp target/release/etapa6 . 

run: build
	./etapa6

clean:
	cargo clean
	rm -f etapa6 Cargo.lock
	rm -rf target
		
entrega: 
	tar cvzf etapa6.tgz .
