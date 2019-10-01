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
# Include global SOLPS compiler settings
ifndef SOLPS_CPP
NODENAME = $(shell echo `hostname`)
ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER} ] && echo yes || echo no ),yes)
  include ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
  MAKES += ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER} ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
else
  $(warning ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER} not found.)
endif
else
  MAKES += ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER} ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
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
ifdef SOLPS_MPI
EXT_MPI = .mpi
endif
ifdef USE_IMPGYRO
EXT_IMPGYRO = .ig
endif
ifdef SOLPS_DEBUG
EXT_DEBUG = .debug
endif

OBJDIR = $(PWD)/builds/$(HOST_NAME).$(COMPILER)$(EXT_DEBUG)
SRCLOCAL = $(PWD)/src.local

CARRE_NONINTERACTIVE = -DCARRE_NONINTERACTIVE

SHELL = /bin/sh
CPP = /usr/lib/cpp

ifeq ($(shell [ -e config/config.${HOST_NAME}.${COMPILER} ] && echo yes || echo no ),yes)
include config/config.${HOST_NAME}.${COMPILER}
MAKES+= config/config.${HOST_NAME}.${COMPILER}
else
$(error config/config.${HOST_NAME}.${COMPILER} not found.)
endif

ifeq ($(shell [ -e config/config.${HOST_NAME}.${COMPILER}.local ] && echo yes || echo no ),yes)
include config/config.${HOST_NAME}.${COMPILER}.local
MAKES+= config/config.${HOST_NAME}.${COMPILER}.local
endif

ifdef SOLPS_DEBUG
DEFINES  += -DDBG
endif

# Source form: src is old fixed form source (used by Carre), src90 is current free form source (used by Carre2)
SRCDIR = src90

VHEAD   =
ifeq ($(shell [ -d ${SRCLOCAL} ] && echo yes || echo no ),yes)
VHEAD   =${SRCLOCAL}:
endif
FPATH   = ${VHEAD}${SRCDIR}/carre_mod:${SRCDIR}/carre:${SRCDIR}/dummy:${SRCDIR}/usol:${SRCDIR}/itm_shared
GPATH   = ${SRCDIR}/cntour:${SRCDIR}/graphe
VPATH   = ${VHEAD}${SRCDIR}/carre_mod:${SRCDIR}/carre:${SRCDIR}/dummy:${SRCDIR}/usol:${SRCDIR}/carre_shared:${SRCDIR}/itm_shared:${SRCDIR}/cntour:${SRCDIR}/graphe

ALLTARGETS = ${OBJDIR}/${PROG}
EXCLUDELIS = carre.o bidon.o
# We can only build the dg-to-Carre converter fcrr when we have the SOLPS environment available
ifdef SOLPS_CPP
FPATH      += :${SRCDIR}/trans:${SRCDIR}/fcrr
VPATH      += :${SRCDIR}/trans:${SRCDIR}/fcrr
ALLTARGETS += ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR}
EXCLUDELIS += tradui.o fcrr.o fcrblkd.o
endif

MODLIST = ${SRCDIR}/carre_mod/*.F90 ${SRCDIR}/usol/*.f90

DEFINES = -DBUILDING_CARRE ${CARRE_NONINTERACTIVE}

INCLUDE = -I ${SRCDIR}/include

include ${OBJDIR}/LISTOBJ

DEST = $(OBJS:%.o=$(OBJDIR)/%.o) $(OBJSL90:%.o=$(OBJDIR)/%.o) $(OBJSU90:%.o=$(OBJDIR)/%.o)
GDEST = $(GOBJS:%.o=$(OBJDIR)/%.o)
MAINLIST = $(EXCLUDELIS:.=\.)
LIBRARIES = $(LDFLAGS:-l%=${LIBSOLDIR}/lib%.a)

$(OBJDIR)/%.o : %.F
	- /bin/rm -f ${OBJDIR}/$*.f
	${CPP} ${SOLPS_CPP} ${DEFINES} -P ${INCLUDE} $< ${OBJDIR}/$*.f; \
	case $< in \
		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

all: VERSION ${OBJDIR}/${PROG}
ifdef LD_NCARG
all: ${OBJDIR}/.x
else
all: ${OBJDIR}/.nox
endif
ifdef SOLPS_CPP
all: ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR}
endif

.PHONY: VERSION clean neat standalone all local depend listobj force

standalone: ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA}

$(OBJDIR)/%.o : %.F90
	@- /bin/rm -f ${OBJDIR}/$*.F90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

ifneq (${MOD},o)
$(OBJDIR)/%.${MOD}: %.F90
	@- /bin/rm -f ${OBJDIR}/$*.F90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi
	if [ -f $*.${MOD} ]; then /bin/mv $*.${MOD} ${OBJDIR}; fi
endif

$(OBJDIR)/%.o : %.f90
	@- /bin/rm -f ${OBJDIR}/$*.f90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

ifneq (${MOD},o)
$(OBJDIR)/%.${MOD}: %.f90
	@- /bin/rm -f ${OBJDIR}/$*.f90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi
	if [ -f $*.${MOD} ]; then /bin/mv $*.${MOD} ${OBJDIR}; fi
endif

# compile an executable
${OBJDIR}/%.exe : ${OBJDIR}/%.o ${MAKES}
	${FC} $(FFLAGS) -o ${OBJDIR}/$*.exe ${OBJDIR}/%.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

all: ${ALLTARGETS}

ifdef LD_NCARG
${OBJDIR}/${PROG} ${OBJDIR}/.x: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG} 2> /dev/null; \
	rm -f ${OBJDIR}/.nox 2> /dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS} && touch ${OBJDIR}/.x
else
${OBJDIR}/${PROG} ${OBJDIR}/.nox: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/bidon.o $(MAKES)
	rm -f ${OBJDIR}/${PROG} 2> /dev/null; \
	rm -f ${OBJDIR}/.x 2> /dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/bidon.o ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS} && touch ${OBJDIR}/.nox
endif

${OBJDIR}/${PROG_TRA}: ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_TRA} 2> /dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG_TRA} ${OBJDIR}/tradui.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJDIR}/${PROG_FCRR}: ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_FCRR} 2> /dev/null; \
	${FC} $(FFLAGS) -o ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJDIR}/libcarre.a: ${DEST} ${MAKES}
	[[ -e ${OBJDIR}/.x && -e ${OBJDIR}/.nox ]] || ( rm -f $@ || echo 2> /dev/null )
	@ar rucv $@ ${DEST}
	ranlib $@

${OBJDIR}/libgcarre.a: ${GDEST} ${MAKES}
	@ar rucv $@ ${GDEST}
	ranlib $@

clean:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f ${OBJDIR}/*.f90 ${OBJDIR}/*.mod ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR} ${SRCDIR}/include/git_version_Carre.h ${OBJDIR}/dependencies* ${OBJDIR}/LISTOBJ

neat:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f

runclean: 
	@rm carre0*

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

TAGS:	tags

tags:
	rm -f TAGS ; etags ${SRCDIR}/*/*.F `find -L ${SRCDIR}/ -name '*.[Ff]90' -not -name ".*"` || touch TAGS

depend: ${OBJS:.o=.F} ${GOBJS:.o=.F90} ${EXCLUDELIS:.o=.F90} ${OBJSL90:.o=.f90} ${OBJSU90:.o=.F90} 
	@makedepend ${DEFINES} -f- ${INCLUDE} $^ | \
	sed -e 's|${SRCDIR}/[^ ]*/|${OBJDIR}/|' | \
	sed -e 's,^${OBJDIR}/,\$${OBJDIR}/,' | \
	sed -e 's,: ${SOLPSTOP},: $${SOLPSTOP},' > ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 1' >> ${OBJDIR}/dependencies.${COMPILER}
ifneq (${MOD},o)
	@makedepend -p'$${OBJDIR}/' ${DEFINES} -f- ${INCLUDE} ${MODLIST} -o.${MOD} | \
	sed 's,^$${OBJDIR}/[^ ][^ ]*/,\$${OBJDIR}/,' | \
        sed 's,: ${SOLPSTOP},: $${SOLPSTOP},' >> ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 2' >> ${OBJDIR}/dependencies.${COMPILER}
endif
	@egrep -aiH '^ {0,}use ' $^ | grep -v 'IGNORE' | tr , ' ' | awk '{sub("\\.F:",".o:",$$1);sub("\\.F90:",".o:",$$1);sub("\\.f90:",".o:",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"tolower($$3)".${MOD}"}' >> ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 3' >> ${OBJDIR}/dependencies.${COMPILER}
ifneq (${MOD},o)
	@egrep -aiH '^ {0,}use ' ${MODLIST} | grep -v 'IGNORE' | tr , ' ' | awk '{sub("\\.F:",".${MOD}:",$$1);sub("\\.f:",".${MOD}:",$$1);sub("\\.F90:",".${MOD}:",$$1);sub("\\.f90:",".${MOD}:",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"tolower($$3)".${MOD}"}' >> ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 4' >> ${OBJDIR}/dependencies.${COMPILER}
endif

listobj:
	@rm -f ${OBJDIR}/LISTOBJ; touch ${OBJDIR}/LISTOBJ; \
	l="OBJS ="; ll90="OBJSL90 = "; lu90="OBJSU90 = "; \
	for d in `echo "${FPATH}" | tr : \ `; do \
		l="$$l `find $$d -name '*.F' -printf "%f "`"; \
		ll90="$$ll90 `find -L $$d -name '*.f90' -printf "%f "`"; \
		lu90="$$lu90 `find -L $$d -name '*.F90' -printf "%f "`"; \
	done; \
	E="-e 's/\*.F//g' -e 's/\.F/\.o/g'" ; \
	EL90="-e 's/\*.f90//g' -e 's/\.f90/\.o/g'" ; \
	EU90="-e 's/\*.F90//g' -e 's/\.F90/\.o/g'" ; \
	for f in $(MAINLIST); do \
		E="$$E -e 's/ $$f//'"; \
		EL90="$$EL90 -e 's/ $$f//'"; \
		EU90="$$EU90 -e 's/ $$f//'"; \
	done; \
	E="$$E -e 's/[ ]*//'"; \
	EL90="$$EL90 -e 's/[ ]*//'"; \
	EU90="$$EU90 -e 's/[ ]*//'"; \
	echo "$$l" | eval sed "$$E" > ${OBJDIR}/LISTOBJ; \
	echo "$$ll90" | eval sed "$$EL90" >> ${OBJDIR}/LISTOBJ; \
	echo "$$lu90" | eval sed "$$EU90" >> ${OBJDIR}/LISTOBJ
	@ll="GOBJS ="; \
	for d in `echo "$(GPATH)" | tr : \ `; do \
		ll="$$ll `find $$d -name '*.F90' -printf "%f "`"; \
	done; \
	E="-e 's/\.F90/\.o/g'" ; for f in $(MAINLIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$ll" | eval sed "$$E" >> ${OBJDIR}/LISTOBJ

${OBJDIR}/LISTOBJ: listobj ${ITM_SRC_PREREQS}

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
	@echo VPATH=${VPATH}
	@echo SOLPS_LIB=${SOLPS_LIB}
	@echo GOBJS=${GOBJS}
	@echo DEST =${DEST}
	@echo OBJS=$(OBJS)
	@echo OBJSL90=$(OBJSL90)
	@echo OBJSU90=$(OBJSU90)
