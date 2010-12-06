#  VERSION : --> see SVN

SHELL	= /bin/sh
CPP	= /usr/lib/cpp

# Source form: src is old fixed form source (no longer maintained), src90 is current free form source
SRCDIR = src90

VPATH	= ${SRCDIR}/carre:${SRCDIR}/cntour:${SRCDIR}/graphe:${SRCDIR}/trans:${SRCDIR}/fcrr
INCLUDE = -I${SRCDIR}/include

ALLTARGETS = ${OBJECTCODE}/carre ${OBJECTCODE}/traduit ${OBJECTCODE}/fcrr
EXCLUDELIST = carre.o tradui.o fcrr.o fcrblkd.o 
MAINLIST = carre.o tradui.o fcrr.o fcrblkd.o 

# *************************************************************
# ITM-CARRE
# If we're compiling for the ITM, we don't want any utility executables and graphics output
# But we want the UAL library
ifeq ($(USE_ITMCARRE),-DUSE_ITMCARRE)
NOUSE_MSCL = -DNOUSE_MSCL
USE_NCARG = 
USE_SILO = 

EXCLUDELIST +=
ALLTARGETS = ${OBJECTCODE}/libcarre.a ${OBJECTCODE}/carre
VPATH = ${SRCDIR}/carre:${SRCDIR}/itmcarre:${SRCDIR}/usol

#SCHEMAS_O = ${filter-out ${EXCLUDELIST},\
#  ${addsuffix .o,\
#    ${addprefix ${OBJECTCODE}/,\
#      ${filter-out *,\
#        ${basename \
#	  ${notdir \
#            ${shell echo src/schemas/*.[fF] src/schemas/*.[fF]90}}}}}}}

endif
# *************************************************************
# NCAR graphics, used for runtime plotting
ifeq ($(USE_NCARG),-DUSE_NCARG)
EXCLUDELIST += bidon.o
else
VPATH +=:${SRCDIR}/dummy
endif
# *************************************************************
# SILO/VisIt graphics output
# Set up HDF5 and SILO libraries only if specifically requested
ifeq ($(USE_SILO),-DUSE_SILO)
# Libraries from usol
USOLLIBDIR = ./usol/lib/${OBJECTCODE}

# SILO
INCLUDE += -I${USOLLIBDIR}/silo/include
USOLLIBS += -L${USOLLIBDIR}/silo/lib -lsiloh5

# HDF5
INCLUDE += -I${USOLLIBDIR}/hdf5/include
USOLLIBS += -L${USOLLIBDIR}/hdf5/lib -lhdf5hl_fortran -lhdf5_fortran -lhdf5_hl -lhdf5 -lz -lstdc++
VPATH += :${SRCDIR}/usol
else
EXCLUDELIST += SiloIO.o
endif
# *************************************************************


include LISTOBJ

# Bring in compiler-specific configuration (moved here because we need the *USE_* variables set up properly)
include config/compiler.${OBJECTCODE}
ifeq ($(shell [ -e config.local/compiler.${OBJECTCODE} ] && echo yes || echo no ),yes)
include config.local/compiler.${OBJECTCODE}
endif

DEST = $(OBJS:%.o=$(OBJECTCODE)/%.o) $(OBJSL90:%.o=$(OBJECTCODE)/%.o) $(OBJSU90:%.o=$(OBJECTCODE)/%.o)
#LIBRARIES = $(LDFLAGS:-l%=${LIBSOLDIR}/lib%.a)

$(OBJECTCODE)/%.o : %.F
	- /bin/rm -f ${OBJECTCODE}/$*.f
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJECTCODE}/$*.f; \
	case $< in \
		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

$(OBJECTCODE)/%.o : %.F90
	- /bin/rm -f ${OBJECTCODE}/$*.f
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJECTCODE}/$*.f90; \
	case $< in \
		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

$(OBJECTCODE)/%.o : %.f90
	case $< in \
		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< ;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< ; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

all: ${ALLTARGETS}

${OBJECTCODE}/carre: ${OBJECTCODE}/carre.o ${OBJECTCODE}/libcarre.a
	rm -f ${OBJECTCODE}/carre 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/carre ${OBJECTCODE}/carre.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJECTCODE}/itmcarre: ${OBJECTCODE}/itmcarre.o ${OBJECTCODE}/libcarre.a
	rm -f ${OBJECTCODE}/carre 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/itmcarre ${OBJECTCODE}/itmcarre.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJECTCODE}/silotest: ${OBJECTCODE}/silotest.o ${OBJECTCODE}/libcarre.a
	rm -f ${OBJECTCODE}/silotest 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/silotest ${OBJECTCODE}/silotest.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJECTCODE}/traduit: ${OBJECTCODE}/tradui.o ${OBJECTCODE}/libcarre.a
	rm -f ${OBJECTCODE}/traduit 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/traduit ${OBJECTCODE}/tradui.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJECTCODE}/fcrr: ${OBJECTCODE}/fcrr.o ${OBJECTCODE}/fcrblkd.o ${OBJECTCODE}/libcarre.a ${SOLPS_LIB}/libmscl.a Makefile
	rm -f ${OBJECTCODE}/fcrr 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/fcrr ${OBJECTCODE}/fcrr.o ${OBJECTCODE}/fcrblkd.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJECTCODE}/libcarre.a: ${DEST}
	ar r $@ $?
	ranlib $@

clean:
	rm -rf ${OBJECTCODE}/*.o ${OBJECTCODE}/*.f ${OBJECTCODE}/*.f90 ${OBJECTCODE}/*.mod ${OBJECTCODE}/libcarre.a ${OBJECTCODE}/carre ${OBJECTCODE}/traduit ${OBJECTCODE}/fcrr

neat:
	rm -rf ${OBJECTCODE}/*.o ${OBJECTCODE}/*.f

runclean: 
	@rm carre0*

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

tags:
	rm TAGS ; etags ${SRCDIR}/*/*.F `find -L ${SRCDIR}/ -name '*.[Ff]90' -not -name ".*"` 

depend: ${OBJS:.o=.F} ${OBJSL90:.o=.f90} ${OBJSU90:.o=.F90} 
	makedepend -f ${OBJECTCODE}/dependencies.${OBJECTCODE} ${INCLUDE} $^
	mv ${OBJECTCODE}/dependencies.${OBJECTCODE} ${OBJECTCODE}/dependencies.${OBJECTCODE}.bak
	sed -e '3,$$s/^\.\.\/${SRCDIR}\/[^\/]*\///' ${OBJECTCODE}/dependencies.${OBJECTCODE}.bak | \
	sed -e '3,$$s/^/${OBJECTCODE}\//' > ${OBJECTCODE}/dependencies.${OBJECTCODE}
	@for d in `echo "${VPATH}" | tr : \ `; do \
		dirfiles=`find -L $$d/ -name '*.f90'` ; \
		if [ -n "$$dirfiles" ] ; then \
			for f in `find -L $$d -name '*.f90' -printf "%f "`; do \
				${CPP} ${DEFINES} -P -C ${INCLUDE} $$d/$$f ${OBJECTCODE}/$$f; \
				grep -i '^[[:space:]]*use ' ${OBJECTCODE}/$$f /dev/null | grep -v 'IGNORE' | awk '{sub(".f90",".o",$$1);sub("^.*/","$${OBJECTCODE}/",$$1); print $$1,"$${OBJECTCODE}/"$$3".o"}' >> ${OBJECTCODE}/dependencies.${OBJECTCODE} ;\
				line='$${OBJECTCODE}/'"`echo $$f | tr A-Z a-z | sed -e 's/.f90/.mod/'`:"'$${OBJECTCODE}/'"`echo $$f | sed -e 's/.f90/.o/'`";\
				echo $$line >> ${OBJECTCODE}/dependencies.${OBJECTCODE} ;\
			done \
		fi ; \
		dirfiles=`find -L $$d/ -name '*.F90'` ; \
		if [ -n "$$dirfiles" ] ; then \
			for f in `find -L $$d -name '*.F90' -printf "%f "`; do \
				${CPP} ${DEFINES} -P -C ${INCLUDE} $$d/$$f ${OBJECTCODE}/$$f; \
				grep -i '^[[:space:]]*use ' ${OBJECTCODE}/$$f /dev/null | grep -v 'IGNORE' | awk '{sub(".F90",".o",$$1);sub("^.*/","$${OBJECTCODE}/",$$1); print $$1,"$${OBJECTCODE}/"$$3".o"}' >> ${OBJECTCODE}/dependencies.${OBJECTCODE} ;\
				line='$${OBJECTCODE}/'"`echo $$f | tr A-Z a-z | sed -e 's/.f90/.mod/'`:"'$${OBJECTCODE}/'"`echo $$f | sed -e 's/.F90/.o/'`";\
				echo $$line >> ${OBJECTCODE}/dependencies.${OBJECTCODE} ;\
			done \
		fi \
	done;


update:
	cd $(SOLPSTOP); gmake update

listobj:
	@rm -f LISTOBJ; touch LISTOBJ ; \
	l="OBJS ="; ll90="OBJSL90 = "; lu90="OBJSU90 = "; \
	for d in `echo "${VPATH}" | tr : \ `; do \
		l="$$l `find $$d -name '*.F' -printf "%f "`"; \
		ll90="$$ll90 `find -L $$d -name '*.f90' -printf "%f "`"; \
		lu90="$$lu90 `find -L $$d -name '*.F90' -printf "%f "`"; \
	done; \
	E="-e 's/\*.F//g' -e 's/\.F/\.o/g'" ; \
	EL90="-e 's/\*.f90//g' -e 's/\.f90/\.o/g'" ; \
	EU90="-e 's/\*.F90//g' -e 's/\.F90/\.o/g'" ; \
	for f in ${EXCLUDELIST}; do \
		E="$$E -e 's/ $$f//'"; \
		EL90="$$EL90 -e 's/ $$f//'"; \
		EU90="$$EU90 -e 's/ $$f//'"; \
	done; \
	E="$$E -e 's/[ ]*//'"; \
	EL90="$$EL90 -e 's/[ ]*//'"; \
	EU90="$$EU90 -e 's/[ ]*//'"; \
	echo "$$l" | eval sed "$$E" > LISTOBJ ; \
	echo "$$ll90" | eval sed "$$EL90" >> LISTOBJ ; \
	echo "$$lu90" | eval sed "$$EU90" >> LISTOBJ

LISTOBJ: listobj

${OBJECTCODE}/dependencies.${OBJECTCODE}:
	-mkdir ${OBJECTCODE}
	touch ${OBJECTCODE}/dependencies.${OBJECTCODE}
	${MAKE} depend

include ${OBJECTCODE}/dependencies.${OBJECTCODE}

# DO NOT DELETE
