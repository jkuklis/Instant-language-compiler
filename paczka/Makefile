all: clean instantM compiler rights	

instantM:
	cp src/Instant.cf instant/Instant.cf
	cd instant && bnfc -m Instant.cf
	$(MAKE) -C instant

compiler:
	ghc src/Main.hs -o build/Compiler -odir build -isrc -iinstant
	mv src/*.hi build

rights:
	chmod +x insc_jvm
	chmod +x insc_llvm
	
clean:
	rm -f instant/*
	rm -f build/*
	rm -f output/*
