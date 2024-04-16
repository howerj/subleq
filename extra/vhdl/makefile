CC:=gcc
CFLAGS:=-Wall -Wextra -std=c99 -O2 -pedantic
GHDL:=ghdl
GOPTS:=--max-stack-alloc=16384 --ieee-asserts=disable
USB:=/dev/ttyUSB0
BAUD:=115200
DIFF:=vimdiff
PROGRAM=subleq.dec
BITS:=16
DEBUG:=0
FAST:=false
CONFIG:=tb.cfg
TOP:=top
GHW:=$(basename ${CONFIG}).ghw

.PHONY: all run diff simulation viewer clean documentation synthesis implementation bitfile

.PRECIOUS: ${GHW}

all: subleq simulation

run: subleq ${PROGRAM}
	./subleq ${PROGRAM}

talk:
	picocom --omap delbs -e b -b ${BAUD} ${USB}

simulation: ${GHW}

viewer: ${GHW} signals.tcl
	gtkwave -S signals.tcl -f $< > /dev/null 2>&1 &

documentation: readme.htm

%.htm: %.md
	pandoc $< -o $@

subleq: subleq.c
	${CC} ${CFLAGS} $< -o $@

%.an: %.vhd
	${GHDL} -a -g $<
	touch $@

uart.an: uart.vhd util.an

top.an: top.vhd subleq.an system.an util.an uart.an

tb.an: tb.vhd top.an

tb: tb.an top.an
	${GHDL} -e $@
	touch $@

system.an: system.vhd subleq.an util.an

gforth.dec: eforth.txt
	gforth $< > $@

gforth: subleq gforth.dec
	./subleq gforth.dec

${GHW}: tb ${CONFIG} ${PROGRAM}
	${GHDL} -r $< --wave=$@ ${GOPTS} '-gbaud=${BAUD}' '-gprogram=${PROGRAM}' '-gN=${BITS}' '-gconfig=${CONFIG}' '-gdebug=${DEBUG}' '-gen_non_io_tb=${FAST}'

SOURCES=top.vhd subleq.vhd uart.vhd system.vhd util.vhd

bitfile: design.bit

reports:
	@[ -d reports    ]    || mkdir reports
tmp:
	@[ -d tmp        ]    || mkdir tmp
tmp/_xmsgs:
	@[ -d tmp/_xmsgs ]    || mkdir tmp/_xmsgs

tmp/${TOP}.prj: tmp
	@rm -f tmp/${TOP}.prj
	@( \
	    for f in ${SOURCES}; do \
	        echo "vhdl work \"$$f\""; \
	    done; \
	    echo "vhdl work \"${TOP}.vhd\"" \
	) > tmp/${TOP}.prj

tmp/${TOP}.lso: tmp
	@echo "work" > tmp/${TOP}.lso

tmp/${TOP}.xst: tmp tmp/_xmsgs tmp/${TOP}.lso tmp/${TOP}.lso
	@( \
	    echo "set -tmpdir \"tmp\""; \
	    echo "set -xsthdpdir \"tmp\""; \
	    echo "run"; \
	    echo "-lso tmp/${TOP}.lso"; \
	    echo "-ifn tmp/${TOP}.prj"; \
	    echo "-ofn ${TOP}"; \
	    echo "-p xc6slx16-csg324-3"; \
	    echo "-top ${TOP}"; \
	    echo "-opt_mode area"; \
	    echo "-opt_level 2" \
	) > tmp/top.xst

synthesis: subleq.dec reports tmp tmp/_xmsgs tmp/${TOP}.prj tmp/${TOP}.xst
	@echo "Synthesis running..."
	@${TIME} xst -intstyle silent -ifn tmp/${TOP}.xst -ofn reports/xst.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv ${TOP}_xst.xrpt tmp
	@grep "ERROR\|WARNING" reports/xst.log | \
	 grep -v "WARNING.*has a constant value.*This FF/Latch will be trimmed during the optimization process." | \
	 cat
	@grep ns reports/xst.log | grep 'Clock period'

implementation: reports tmp
	@echo "Implementation running..."

	@[ -d tmp/xlnx_auto_0_xdb ] || mkdir tmp/xlnx_auto_0_xdb

	@${TIME} ngdbuild -intstyle silent -quiet -dd tmp -uc ${TOP}.ucf -p xc6slx16-csg324-3 ${TOP}.ngc ${TOP}.ngd
	@mv ${TOP}.bld reports/ngdbuild.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv xlnx_auto_0_xdb/* tmp
	@rmdir xlnx_auto_0_xdb
	@mv ${TOP}_ngdbuild.xrpt tmp

	@${TIME} map -intstyle silent -detail -p xc6slx16-csg324-3 -convert_bram8 -pr b -c 100 -w -o ${TOP}_map.ncd ${TOP}.ngd ${TOP}.pcf
	@mv ${TOP}_map.mrp reports/map.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv ${TOP}_usage.xml ${TOP}_summary.xml ${TOP}_map.map ${TOP}_map.xrpt tmp

	@${TIME} par -intstyle silent -w -ol std ${TOP}_map.ncd ${TOP}.ncd ${TOP}.pcf
	@mv ${TOP}.par reports/par.log
	@mv ${TOP}_pad.txt reports/par_pad.txt
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv par_usage_statistics.html ${TOP}.ptwx ${TOP}.pad ${TOP}_pad.csv ${TOP}.unroutes ${TOP}.xpi ${TOP}_par.xrpt tmp

design.bit: reports tmp/_xmsgs
	@echo "Generate bitfile running..."
	@touch webtalk.log
	@${TIME} bitgen -intstyle silent -w ${TOP}.ncd
	@mv ${TOP}.bit $@
	@mv ${TOP}.bgn reports/bitgen.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@sleep 5
	@mv ${TOP}.drc ${TOP}_bitgen.xwbt ${TOP}_usage.xml ${TOP}_summary.xml webtalk.log tmp
	@grep -i '\(warning\|clock period\)' reports/xst.log

upload:
	djtgcfg prog -d Nexys3 -i 0 -f design.bit

design: clean simulation synthesis implementation bitfile

postsyn:
	@netgen -w -ofmt vhdl -sim ${TOP}.ngc post_synthesis.vhd
	@netgen -w -ofmt vhdl -sim ${TOP}.ngd post_translate.vhd
	@netgen  -pcf ${TOP}.pcf -w -ofmt vhdl -sim ${TOP}.ncd post_map.vhd

clean:
	git clean -fddx .


