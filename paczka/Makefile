all: clean instantM compiler rights	

instantM:
	bnfc -o instant -m src/Instant.cf
	make -C instant

compiler:
	ghc -o build/Compiler -odir build src/Compiler.hs -iinstant
	mv src/Compiler.hi build/Compiler.hi

rights:
	chmod +x insc_jvm
	chmod +x insc_llvm

clean:
	rm -f instant/*
	rm -f build/*
