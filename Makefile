

ghc:=ghc
ghc_flags:=--make -package parsec 
#main:=main.hs
files:=*.hs 
output:=parser 

build:
	@echo 'Compiling...' 
	@$(ghc) $(ghc_flags) -o $(output) $(files) 

clean:
	@echo 'Removing extra files...' 
	@rm $(output) *.o *.hi 

all: build 
.PHONY: all

