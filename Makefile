# VERSION : --> see SVN

# Compile-time switches

# FIXME: explicitly test for OBJECTCODE

CARRE_DEBUG = yes

USE_SILO = '-DUSE_SILO'

CARRE_NONINTERACTIVE = "-DCARRE_NONINTERACTIVE"

#USE_ITMCARRE = "-DUSE_ITMCARRE -DEUITM"
#USE_UAL = "-DUSE_UAL"

#USE_NCARG = '-DUSE_NCARG'
#NOUSE_MSCL = "True"


#SOLPSTOP = '/scratch/hmk/svn/solps5.0/'
#SOLPS_LIB = "$SOLPSTOP/src/lib/$OBJECTCODE"

SHELL	= /bin/sh
CPP	= /usr/lib/cpp

# Source form: src is old fixed form source (no longer maintained), src90 is current free form source
SRCDIR = src90

SVN_B2SRC_PATH = https://solps-mdsplus.aug.ipp.mpg.de/repos/SOLPS/branches/ITM/4.10a/solps5.0/src/Braams/b2/src_xpb

VPATH	= ${SRCDIR}/carre:${SRCDIR}/trans:${SRCDIR}/fcrr:${SRCDIR}/usol:${SRCDIR}/carre_shared:${SRCDIR}/b2_shared ${SRCDIR}/itm_shared
INCLUDE = -I ${SRCDIR}/include

ALLTARGETS = ${OBJECTCODE}/carre ${OBJECTCODE}/traduit ${OBJECTCODE}/fcrr
EXCLUDELIST = carre.o fcrr.o tradui.o euitm_routines.o itmcarre_wrapper.o

DEFINES = ${USE_ITMCARRE} ${USE_NCARG} ${USE_SILO} ${USE_UAL} ${CARRE_NONINTERACTIVE}

# Include object lists
include LISTOBJ

# *************************************************************
# ITM-CARRE
# If we're compiling for the ITM, we don't want graphics output and most utilities
# But we want the UAL library
ifdef USE_ITMCARRE 

ITM_SRC_PREREQS=src90/b2_shared 

EXCLUDELIST +=
ALLTARGETS += ${OBJECTCODE}/itmcarre_wrapper
VPATH = ${SRCDIR}/carre:${SRCDIR}/trans:${SRCDIR}/fcrr:${SRCDIR}/usol:${SRCDIR}/carre_shared:${SRCDIR}/itmcarre:${SRCDIR}/b2_shared

# Some variables are expected to be set up properly by the ITMv1 script
# -${UAL} is the path to the UAL library files
# -${DATAVERSION} is the currently selected UAL version (e.g. 4.09a)

endif
# *************************************************************
# NCAR graphics, used for runtime plotting
ifdef USE_NCARG
EXCLUDELIST += bidon.o
VPATH +=:${SRCDIR}/cntour:${SRCDIR}/graphe
else
VPATH +=:${SRCDIR}/dummy
endif
# *************************************************************
# SILO/VisIt graphics output
# Set up HDF5 and SILO libraries only if specifically requested
ifdef USE_SILO

else
EXCLUDELIST += SiloIO.o
endif
# *************************************************************


# Bring in system-specific configuration (moved here because we need the *USE_* variables set up properly)
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
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJECTCODE}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

# 	case $< in \
# 		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90;; \
# 		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90;; \
# 	esac; \


$(OBJECTCODE)/%.o : %.f90
	$(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< 
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi
# 	case $< in \
# 		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< ;; \
# 		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< ; \
# 	esac; \

# compile an executable
${OBJECTCODE}/%.exe : ${OBJECTCODE}/%.o
	${FC} $(FFLAGS) -o ${OBJECTCODE}/$*.exe $^ ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

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

${OBJECTCODE}/itmcarre_wrapper: ${OBJECTCODE}/itmcarre_wrapper.o ${OBJECTCODE}/libcarre.a Makefile
	rm -f ${OBJECTCODE}/itmcarre_wrapper 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/itmcarre_wrapper ${OBJECTCODE}/itmcarre_wrapper.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

${OBJECTCODE}/libcarre.a: ${DEST}
	ar r $@ $?
	ranlib $@

clean:
	rm -rf ${OBJECTCODE}/*.o ${OBJECTCODE}/*.f ${OBJECTCODE}/*.f90 ${OBJECTCODE}/*.mod ${OBJECTCODE}/libcarre.a ${OBJECTCODE}/carre ${OBJECTCODE}/traduit ${OBJECTCODE}/fcrr ${OBJECTCODE}/itmcarre_wrapper

neat:
	rm -rf ${OBJECTCODE}/*.o ${OBJECTCODE}/*.f

runclean: 
	@rm carre0*

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

tags:
	rm TAGS ; etags ${SRCDIR}/*/*.F `find -L ${SRCDIR}/ -name '*.[Ff]90' -not -name ".*"` 

echo:
	echo INCLUDE ${INCLUDE}

olddepend: ${OBJS:.o=.F} ${OBJSL90:.o=.f90} ${OBJSU90:.o=.F90} 
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

simpledepend: 
	bin/sfmakedepend -d -p '$${OBJECTCODE}/' ${INCLUDE} \
	    -f ${OBJECTCODE}/dependencies.${OBJECTCODE} src90/*/*.[fF] src90/*/*.[fF]90 

depend:
# 	First do preprocessor #includes dependencies (-u disables USE dependency output)	
	bin/sfmakedepend -d -u -p '$${OBJECTCODE}/' -I ${SRCDIR}/include \
	    -f ${OBJECTCODE}/dependencies.${OBJECTCODE}.include src90/*/*.[fF] src90/*/*.[fF]90 
#	Now preprocess all files and do dependencies of preprocessed files to get correct USE dependencies
	-rm ${OBJECTCODE}/*.f90 ${OBJECTCODE}/*.f
	@for d in `echo "${VPATH}" | tr : \ `; do \
		dirfiles=`find -L $$d/ -name '*.F90'` ; \
		if [ -n "$$dirfiles" ] ; then \
			for f in `find -L $$d -name '*.F90' -printf "%f "`; do \
				basename=`basename $$f .F90`; \
				${CPP} ${DEFINES} -P -C ${INCLUDE} $$d/$$f ${OBJECTCODE}/$$basename.f90; \
			done \
		fi ; \
		dirfiles=`find -L $$d/ -name '*.F'` ; \
		if [ -n "$$dirfiles" ] ; then \
			for f in `find -L $$d -name '*.F' -printf "%f "`; do \
				basename=`basename $$f .F`; \
				${CPP} ${DEFINES} -P -C ${INCLUDE} $$d/$$f ${OBJECTCODE}/$$basename.f90; \
			done \
		fi ; \
		dirfiles=`find -L $$d/ -name '*.f90'` ; \
		if [ -n "$$dirfiles" ] ; then \
			cp $$dirfiles ${OBJECTCODE} ; \
		fi ; \
	done ; \
	bin/sfmakedepend -d -p '$${OBJECTCODE}/' \
	    -f ${OBJECTCODE}/dependencies.${OBJECTCODE}.use ${OBJECTCODE}/*.[fF] ${OBJECTCODE}/*.[fF]90 


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

LISTOBJ: listobj ${ITM_SRC_PREREQS}

${OBJECTCODE}/dependencies.${OBJECTCODE}.include:
	-mkdir -p ${OBJECTCODE}
	touch ${OBJECTCODE}/dependencies.${OBJECTCODE}.include
	touch ${OBJECTCODE}/dependencies.${OBJECTCODE}.use
	${MAKE} listobj
	${MAKE} depend
${OBJECTCODE}/dependencies.${OBJECTCODE}.use: ${OBJECTCODE}/dependencies.${OBJECTCODE}.include

src90/b2_shared:
	mkdir src90/b2_shared
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2ag_ghostcells.F90 src90/b2_shared/b2ag_ghostcells.F90
	svn export ${SVN_B2SRC_PATH}/modules/b2mod_constants.F src90/b2_shared/b2mod_constants.F
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2ITMMapping.f90 src90/b2_shared/b2ITMMapping.f90
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_cellhelper.f90 src90/b2_shared/b2mod_cellhelper.f90 
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_indirect.F src90/b2_shared/b2mod_indirect.F
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_types.F src90/b2_shared/b2mod_types.F
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_connectivity.F90 src90/b2_shared/b2mod_connectivity.F90
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_indirect.F src90/b2_shared/b2mod_indirect.F
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_ual.F90 src90/b2_shared/b2mod_ual.F90
	svn export ${SVN_B2SRC_PATH}/b2_shared/b2mod_silo.F90 src90/b2_shared/b2mod_silo.F90

#TODO: export target for b2_shared_itm

include ${OBJECTCODE}/dependencies.${OBJECTCODE}.include
include ${OBJECTCODE}/dependencies.${OBJECTCODE}.use

# DO NOT DELETE
