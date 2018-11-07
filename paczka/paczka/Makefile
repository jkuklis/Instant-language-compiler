all: clean instantM compiler rights	

instantM:
	bnfc -o instant -m src/Instant.cf
	make -C instant

compiler:
	ghc src/Main.hs -o build/Compiler -odir build -isrc -iinstant
	mv src/*.hi build
	cd -

rights:
	chmod +x insc_jvm
	chmod +x insc_llvm
	
clean:
	rm -f instant/*
	rm -f build/*
	rm -f output/*
