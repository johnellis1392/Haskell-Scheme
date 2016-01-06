

# Gather all files of a particular extension 
all_files=$(wildcard $(1)/$(2)) $(foreach i,$(wildcard $(1)/*),$(call all_files,$(i),$(2))) 
clean_files=$(call all_files,.,*.o) $(call all_files,.,*.hi) 

scheme_files:=$(call all_files,Scheme,*.hs) 
output:=parser 
main:=main.hs

ghc:=ghc
ghc_flags:=--make -package parsec -fglasgow-exts -XExistentialQuantification 


build: $(scheme_files) 
	@echo 'Compiling Main...' 
	@$(ghc) $(ghc_flags) -o $(output) $(scheme_files) $(main) 

clean:
	@echo 'Removing extra files...' 
	@rm -rvf $(output) $(call clean_files) 

all: build 
.PHONY: all

