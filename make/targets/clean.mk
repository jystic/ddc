
# -- clean up everything
.PHONY : clean
clean  : cleanWar cleanRuntime cleanLibrary
	@echo "* Cleaning leftovers"
	@find . \
			-name "*.o" \
		-o      -name "*.o-boot" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.hi" \
		-o	-name "*.hi-boot" \
		-o	-name "*.hcr" \
		-o	-name "*.td" \
		-o	-name "*.ti" \
		-o	-name "Makefile.deps" \
		-follow | xargs -n 1 rm -f

	@rm -f doc/haddock/*
	@rm -f src/Config/Config.hs
	@rm -f make/Makefile.deps.inc
	@rm -f 	bin/* \
		make/Makefile.deps.bak
