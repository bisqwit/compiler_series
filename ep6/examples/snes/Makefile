CONJ=../../conj

#CONJ += -O0
#CONJ += -O1 -fno-rename-registers -ftree-tail-merge -fno-copy-elision
#CONJ += -O1 -ftree-tail-merge
CONJ += -O2
#CONJ += -fno-thread-jumps
CONJ += -fno-optimize-tail-calls
#CONJ += -fno-rename-registers
#CONJ += -foptimize-tail-calls
#CONJ += -fdebug-ir-dump
CONJ += -fdebug-optimization-stats

INCS = registers.inc common.inc zero-macros.inc
OBJS = header.ips fill.ips \
       main.o65 reset.o65 interrupts.o65 dma.o65 text.o65 input-dialog.o65 \
       conjugator.o65 resources.o65 tail.o65
RESS = font-8x16.2bpp.inc


##########################################

demo.smc: cksum/fixchecksum_raw temp.smc
	"$<" temp.smc "$@"
temp.smc: $(OBJS)
	sneslink -fsmc -o "$@" $^
demo.ips: $(OBJS)
	sneslink -fips -o "$@" $^
demo.o65: $(OBJS)
	sneslink -fo65 -o "$@" $^

# Generic rule to create .o65 out from .a65
%.o65: %.a65
	snescom -J -Wall -o $@ "$<"
# Generic rule to create .ips out from .a65
%.ips: %.a65
	snescom -I -J -Wall -o "$@" "$<"
# Generic rule to create .a65 out from .code2
%.a65: ../%.code2
	$(CONJ) -fdebug-codegen -march=snes "$<" -o "$@"

resources.o65: $(RESS)

$(RESS): font/makefont
	./"$<" > "$@"

font/makefont: font/makefont.cc font/8x16.inc
	$(CXX) -o "$@" "$<" -Wall -Wextra -Os -pedantic -std=c++14

cksum/fixchecksum_raw: cksum/fixchecksum_raw.cc
	$(CXX) -o "$@" "$<" -Wall -Wextra -Os -pedantic -std=c++14

clean: ;
	rm -f $(OBJS) demo.smc temp.smc cksum/fixchecksum_raw font/makefont
