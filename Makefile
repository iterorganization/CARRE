#  VERSION : 27.07.97 22:41

OBJDIR = $(OBJECTCODE)

SHELL	= /bin/sh
CPP	= /usr/lib/cpp

ifdef NCARG
VPATH	= src/carre:src/cntour:src/graphe:src/trans:src/fcrr:src/dummy
EXCLUDELIS = carre.o tradui.o bidon.o fcrr.o fcrblkd.o
else
VPATH	= src/carre:src/trans:src/fcrr:src/dummy
EXCLUDELIS = carre.o tradui.o fcrr.o fcrblkd.o
endif

INCLUDE = -Isrc/include

include config/compiler.${OBJECTCODE}
ifeq ($(shell [ -e config.local/compiler.${OBJECTCODE} ] && echo yes || echo no ),yes)
include config.local/compiler.${OBJECTCODE}
endif

include LISTOBJ

DEST = $(OBJS:%.o=$(OBJDIR)/%.o)
MAINLIST = $(EXCLUDELIS:.=\.)
LIBRARIES = $(LDFLAGS:-l%=${LIBSOLDIR}/lib%.a)

$(OBJDIR)/%.o : %.F
	- /bin/rm -f ${OBJDIR}/$*.f
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f; \
	case $< in \
		src/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

all: ${OBJDIR}/carre ${OBJDIR}/traduit ${OBJDIR}/fcrr

standalone: ${OBJDIR}/carre ${OBJDIR}/traduit

${OBJDIR}/carre: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a
	rm -f ${OBJDIR}/carre 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/carre ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/traduit: ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a
	rm -f ${OBJDIR}/traduit 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/traduit ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/fcrr: ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a ${SOLPS_LIB}/libmscl.a Makefile
	rm -f ${OBJDIR}/fcrr 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/fcrr ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a ${SOLPS_LIB}/libmscl.a  ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/libcarre.a: ${DEST}
	ar r $@ $?
	ranlib $@

clean:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f ${OBJDIR}/libcarre.a ${OBJDIR}/carre ${OBJDIR}/traduit ${OBJDIR}/fcrr

neat:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

TAGS:	tags

tags:
	rm -f TAGS ; etags src/*/*.F

depend: ${OBJS:.o=.F} ${EXCLUDELIS:.o=.F}
	makedepend -f ${OBJDIR}/dependencies.${OBJECTCODE} ${INCLUDE} $^
	mv ${OBJDIR}/dependencies.${OBJECTCODE} ${OBJDIR}/dependencies.${OBJECTCODE}.bak
	sed -e '3,$$s/^\.\.\/src\/[^\/]*\///' ${OBJDIR}/dependencies.${OBJECTCODE}.bak | \
	sed -e '3,$$s|^|${OBJDIR}/|' > ${OBJDIR}/dependencies.${OBJECTCODE}

listobj:
	@rm -f LISTOBJ; touch LISTOBJ; l="OBJS ="; \
	for d in `echo "$(VPATH)" | tr : \ `; do \
		l="$$l `(cd $$d > /dev/null; echo *.F)`"; \
	done; \
	E="-e 's/\.F/\.o/g'" ; for f in $(MAINLIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$l" | eval sed "$$E" > LISTOBJ

LISTOBJ: listobj

${OBJDIR}/dependencies.${OBJECTCODE}:
	-mkdir -p ${OBJDIR}
	touch ${OBJDIR}/dependencies.${OBJECTCODE}
	${MAKE} listobj
	${MAKE} depend

include ${OBJDIR}/dependencies.${OBJECTCODE}

