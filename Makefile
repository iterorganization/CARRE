#  VERSION : 27.07.97 22:41

PROG      = carre.exe
PROG_TRA  = traduit.exe
PROG_FCRR = fcrr.exe


# Test whether necessary environment variables are defined; if not, exit
ifndef HOST_NAME
$(error HOST_NAME not defined)
endif
ifndef COMPILER
$(error COMPILER not defined)
endif
ifdef SOLPS_DEBUG
EXT_DEBUG = .debug
endif


OBJDIR = $(PWD)/builds/$(HOST_NAME).$(COMPILER)$(EXT_DEBUG)

SHELL	= /bin/sh
CPP	= /usr/lib/cpp

ifeq ($(shell [ -e config/config.${HOST_NAME}.${COMPILER} ] && echo yes || echo no ),yes)
include config/config.${HOST_NAME}.${COMPILER}
else
$(error config/config.${HOST_NAME}.${COMPILER} not found.)
endif

ifeq ($(shell [ -e config/config.${HOST_NAME}.${COMPILER}.local ] && echo yes || echo no ),yes)
include config/config.${HOST_NAME}.${COMPILER}.local
endif

ifdef NCARG_ROOT
VPATH	= src/carre:src/cntour:src/graphe:src/trans:src/fcrr:src/dummy
EXCLUDELIS = carre.o tradui.o bidon.o fcrr.o fcrblkd.o
else
VPATH	= src/carre:src/trans:src/fcrr:src/dummy
EXCLUDELIS = carre.o tradui.o fcrr.o fcrblkd.o
endif

INCLUDE = -Isrc/include


include ${OBJDIR}/LISTOBJ

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

all: VERSION ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR}

.PHONY: VERSION clean neat standalone all local depend listobj force

standalone: ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA}

${OBJDIR}/${PROG}: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a
	rm -f ${OBJDIR}/${PROG} 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/${PROG_TRA}: ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a
	rm -f ${OBJDIR}/${PROG_TRA} 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG_TRA} ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/${PROG_FCRR}: ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a
	rm -f ${OBJDIR}/${PROG_FCRR} 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a  ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/libcarre.a: ${DEST}
	ar rcv $@ ${DEST}
	ranlib $@

clean:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f ${OBJDIR}/libcarre.a ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR} src/include/git_version.h ${OBJDIR}/dependencies* ${OBJDIR}/LISTOBJ

neat:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

TAGS:	tags

tags:
	rm -f TAGS ; etags src/*/*.F

depend: ${OBJS:.o=.F} ${EXCLUDELIS:.o=.F}
	makedepend -f ${OBJDIR}/dependencies.${COMPILER} ${INCLUDE} $^
	mv ${OBJDIR}/dependencies.${COMPILER} ${OBJDIR}/dependencies.${COMPILER}.bak
	sed -e 's|src/[^ ]*/|${OBJDIR}/|' ${OBJDIR}/dependencies.${COMPILER}.bak > ${OBJDIR}/dependencies.${COMPILER}


listobj:
	@rm -f ${OBJDIR}/LISTOBJ; touch ${OBJDIR}/LISTOBJ; l="OBJS ="; \
	for d in `echo "$(VPATH)" | tr : \ `; do \
		l="$$l `(cd $$d > /dev/null; echo *.F)`"; \
	done; \
	E="-e 's/\.F/\.o/g'" ; for f in $(MAINLIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$l" | eval sed "$$E" > ${OBJDIR}/LISTOBJ

${OBJDIR}/LISTOBJ: listobj

VERSION: src/include/git_version.h

src/include/git_version.h: force
	@echo "      character*15 :: gitversion ='`git describe --dirty --always`'" > src/include/git_version_new.h
	@if cmp -s src/include/git_version_new.h src/include/git_version.h; then rm src/include/git_version_new.h; else mv src/include/git_version_new.h src/include/git_version.h; fi

${OBJDIR}/dependencies.${COMPILER}:
	-mkdir -p ${OBJDIR}
	touch ${OBJDIR}/dependencies.${COMPILER}
	${MAKE} VERSION
	${MAKE} tags
	${MAKE} listobj
	${MAKE} depend

include ${OBJDIR}/dependencies.${COMPILER}

