CONJ=../../conj

#CONJ += -O0
#CONJ += -O1 -fno-rename-registers -ftree-tail-merge -fno-copy-elision
CONJ += -O2
#CONJ += -fno-tree-tail-merge
#CONJ += -fno-thread-jumps
CONJ += -fno-optimize-tail-calls
#CONJ += -foptimize-tail-calls
CONJ += -fdebug-optimization-stats
CONJ += -fdebug-ir-dump

OBJS=
OBJS += conjugator.o
OBJS += booleans.o
#OBJS += stpcpyn.o

CXXFLAGS=-Og -g -Wall -Wextra -fno-builtin

##########################################

test: test.o $(OBJS)
	g++ -o "$@" $^ -std=c++17 $(CXXFLAGS)

# Generic rule to create .o out from .cc
%.o: %.asm
	g++ -o "$@" "$<" -std=c++17 -c $(CXXFLAGS)

# Generic rule to create .o out from .asm
%.o: %.asm
	yasm "$<" -f elf64 -o"$@"

# Generic rule to create .asm out from .code2
%.asm: ../%.code2
	$(CONJ) -fdebug-codegen -march=x86_64 "$<" -o "$@"

clean: ;
	rm -f {conjugator,stpcpyn,booleans,test}.{o,asm} test
