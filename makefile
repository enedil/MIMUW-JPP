
build_dir = build
exe = cerber

$(exe):
	mkdir -p $(build_dir)
	ghc -Wall --make -outputdir $(build_dir) -o $@ -isrc/ -isrc/generated Main.hs

clean:
	rm -rf $(build_dir) $(exe)

.PHONY: clean
