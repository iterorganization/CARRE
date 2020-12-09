# VERSION : 27.07.97 22:41

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

MAKES = Makefile
DEFINES = ${CARRE_DEFINES} ${SOLPS_CPP}
# Include global SOLPS compiler settings
ifndef SOLPS_CPP
NODENAME = $(shell echo `hostname`)
ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER} ] && echo yes || echo no ),yes)
  include ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
  MAKES += ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER} ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
else
  $(warning ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER} not found.)
endif
ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/config.common.${COMPILER} ] && echo yes || echo no ),yes)
  include ${SOLPSTOP}/SETUP/config.common.${COMPILER}
  MAKES += ${SOLPSTOP}/SETUP/config.common.${COMPILER}
endif
else
  MAKES += ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER} ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/config.common.${COMPILER} ] && echo yes || echo no ),yes)
  MAKES += ${SOLPSTOP}/SETUP/config.common.${COMPILER}
endif
endif
ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER}.local ] && echo yes || echo no ),yes)
  MAKES += ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER}.local
endif
ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}.local ] && echo yes || echo no ),yes)
  include ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}.local
  MAKES += ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}.local
endif

# Extensions for object directories when various options are used
ifdef SOLPS_OPENMP
EXT_OPENMP = .openmp
endif
ifdef USE_IMPGYRO
EXT_IMPGYRO = .ig
else
ifdef SOLPS_MPI
EXT_MPI = .mpi
endif
endif
ifdef SOLPS_DEBUG
EXT_DEBUG = .debug
endif

OBJDIR = $(PWD)/builds/$(HOST_NAME).$(COMPILER)$(EXT_DEBUG)
SRCLOCAL = $(PWD)/src.local

SHELL = /bin/sh
CPP = /usr/lib/cpp

ifeq ($(shell [ -e config/config.${HOST_NAME}.${COMPILER} ] && echo yes || echo no ),yes)
include config/config.${HOST_NAME}.${COMPILER}
MAKES+= config/config.${HOST_NAME}.${COMPILER}
else
$(error config/config.${HOST_NAME}.${COMPILER} not found.)
endif

ifeq ($(shell [ -e config/config.common.${COMPILER} ] && echo yes || echo no ),yes)
include config/config.common.${COMPILER}
MAKES+= config/config.common.${COMPILER}
endif

ifeq ($(shell [ -e config/config.${HOST_NAME}.${COMPILER}.local ] && echo yes || echo no ),yes)
include config/config.${HOST_NAME}.${COMPILER}.local
MAKES+= config/config.${HOST_NAME}.${COMPILER}.local
endif

ifdef SOLPS_DEBUG
DEFINES  += -DDBG
endif

# Source form: src is old fixed form source (used by Carre), src90 is current free form source (used by Carre2)
SRCDIR = src

VHEAD   =
ifeq ($(shell [ -d ${SRCLOCAL} ] && echo yes || echo no ),yes)
VHEAD   =${SRCLOCAL}:
endif
FPATH   = ${VHEAD}${SRCDIR}/carre:${SRCDIR}/trans:${SRCDIR}/fcrr:${SRCDIR}/dummy
GPATH   = ${SRCDIR}/cntour:${SRCDIR}/graphe
VPATH   = ${VHEAD}${SRCDIR}/carre:${SRCDIR}/trans:${SRCDIR}/fcrr:${SRCDIR}/cntour:${SRCDIR}/dummy:${SRCDIR}/graphe

ALLTARGETS = ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA}
# We can only build the dg-to-Carre converter fcrr when we have the SOLPS environment available
ifdef SOLPS_CPP
ALLTARGETS += ${OBJDIR}/${PROG_FCRR}
endif

MAINLIST = carre.o tradui.o fcrr.o bidon.o fcrblkd.o

INCLUDE = -I${SRCDIR}/include

include ${OBJDIR}/LISTOBJ

DEST = $(OBJS:%.o=$(OBJDIR)/%.o)
GDEST = $(GOBJS:%.o=$(OBJDIR)/%.o)
EXCLUDELIST = $(MAINLIST:.o=\\.o)
LIBRARIES = $(LDFLAGS:-l%=${LIBSOLDIR}/lib%.a)

$(OBJDIR)/%.o : %.F
	- /bin/rm -f ${OBJDIR}/$*.f
	${CPP} ${DEFINES} -P ${INCLUDE} $< ${OBJDIR}/$*.f; \
	case $< in \
		${SRCDIR}/trans/* ) $(COMPILE) ${FFLAGSEXTRA} $(DBLPAD) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
		       *    ) $(COMPILE) ${FFLAGSEXTRA} $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

ifdef LD_NCARG
ifeq ($(strip ${GLI_HOME}),)
$(warning Carre graphics may not work because GLI_HOME is not defined.)
endif
all: VERSION ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/.x
else
ifndef NCARG_ROOT
$(warning Carre graphics are turned off as NCARG_ROOT is not defined.)
endif
all: VERSION ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/.nox
endif

.PHONY: VERSION clean neat standalone all local depend listobj force

standalone: ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA}

all: ${ALLTARGETS}

ifdef LD_NCARG
${OBJDIR}/${PROG} ${OBJDIR}/.x: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG} 2> /dev/null; \
	rm -f ${OBJDIR}/.nox 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a $(LDFLAGS) $(LDEXTRA) && touch ${OBJDIR}/.x
else
${OBJDIR}/${PROG} ${OBJDIR}/.nox: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/bidon.o $(MAKES)
	rm -f ${OBJDIR}/${PROG} 2> /dev/null; \
	rm -f ${OBJDIR}/.x 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/bidon.o $(LDFLAGS) $(LDEXTRA) && touch ${OBJDIR}/.nox
endif

${OBJDIR}/${PROG_TRA}: ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_TRA} 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG_TRA} ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/${PROG_FCRR}: ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_FCRR} 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJDIR}/libcarre.a: ${DEST} ${MAKES}
	[[ -e ${OBJDIR}/.x && -e ${OBJDIR}/.nox ]] || ( rm -f $@ || echo 2> /dev/null )
	@ar rucv $@ ${DEST}
	ranlib $@

${OBJDIR}/libgcarre.a: ${GDEST} ${MAKES}
	@ar rucv $@ ${GDEST}
	ranlib $@

clean:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR} ${SRCDIR}/include/git_version_Carre.h ${OBJDIR}/dependencies* ${OBJDIR}/LISTOBJ

neat:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

TAGS:	tags

tags:
	rm -f TAGS ; etags ${SRCDIR}/*/*.F || touch TAGS

depend: ${OBJS:.o=.F} ${GOBJS:.o=.F} ${MAINLIST:.o=.F}
	@makedepend -f- ${INCLUDE} $^ | \
	sed -e 's|${SRCDIR}/[^ ]*/|${OBJDIR}/|' | \
	sed -e 's,^${OBJDIR}/,\$${OBJDIR}/,' | \
	sed -e 's,: ${SOLPSTOP},: $${SOLPSTOP},' > ${OBJDIR}/dependencies.${COMPILER}

listobj:
	@rm -f ${OBJDIR}/LISTOBJ; touch ${OBJDIR}/LISTOBJ; \
	l="OBJS ="; \
	for d in `echo "${FPATH}" | tr : \ `; do \
		l="$$l `find $$d -name '*.F' -printf "%f "`"; \
	done; \
	E="-e 's/\.F/\.o/g'" ; for f in $(EXCLUDELIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$l" | eval sed "$$E" > ${OBJDIR}/LISTOBJ
	@ll="GOBJS ="; \
	for d in `echo "$(GPATH)" | tr : \ `; do \
		ll="$$ll `find $$d -name '*.F' -printf "%f "`"; \
	done; \
	E="-e 's/\.F/\.o/g'" ; for f in $(EXCLUDELIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$ll" | eval sed "$$E" >> ${OBJDIR}/LISTOBJ

${OBJDIR}/LISTOBJ: listobj

VERSION: ${SRCDIR}/include/git_version_Carre.h

${SRCDIR}/include/git_version_Carre.h: force
	@echo "      character*33 ::" > ${SRCDIR}/include/git_version_new.h
	@echo "     . git_version_Carre = '`git describe --dirty --always`'" >> ${SRCDIR}/include/git_version_new.h
	@if cmp -s ${SRCDIR}/include/git_version_new.h ${SRCDIR}/include/git_version_Carre.h; then rm ${SRCDIR}/include/git_version_new.h; else mv ${SRCDIR}/include/git_version_new.h ${SRCDIR}/include/git_version_Carre.h; fi

${OBJDIR}/dependencies.${COMPILER}:
	-mkdir -p ${OBJDIR}
	touch ${OBJDIR}/dependencies.${COMPILER}
	${MAKE} VERSION
	${MAKE} tags
	${MAKE} listobj
	${MAKE} depend

include ${OBJDIR}/dependencies.${COMPILER}

echo:
	@echo INCLUDE=${INCLUDE}
	@echo DEFINES=${DEFINES}
	@echo GOBJS=${GOBJS}
	@echo DEST =${DEST}
