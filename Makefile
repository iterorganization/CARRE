#  VERSION : 27.07.97 22:41

SHELL	= /bin/sh
CPP	= /usr/lib/cpp

VPATH	= src/carre:src/cntour:src/graphe:src/trans:src/fcrr:src/dummy:src/usol
INCLUDE = -Isrc/include

include config/compiler.${OBJECTCODE}
ifeq ($(shell [ -e config.local/compiler.${OBJECTCODE} ] && echo yes || echo no ),yes)
include config.local/compiler.${OBJECTCODE}
endif

EXCLUDELIS = carre.o tradui.o bidon.o fcrr.o fcrblkd.o

include LISTOBJ

DEST = $(OBJS:%.o=$(OBJECTCODE)/%.o)
MAINLIST = $(EXCLUDELIS:.=\.)
LIBRARIES = $(LDFLAGS:-l%=${LIBSOLDIR}/lib%.a)

$(OBJECTCODE)/%.o : %.F
	- /bin/rm -f ${OBJECTCODE}/$*.f
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJECTCODE}/$*.f; \
	case $< in \
		src/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

$(OBJECTCODE)/%.o : %.F90
	- /bin/rm -f ${OBJECTCODE}/$*.f
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJECTCODE}/$*.f90; \
	case $< in \
		src/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o ${OBJECTCODE}/$*.f90;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

$(OBJECTCODE)/%.o : %.f90
	case $< in \
		src/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< ; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJECTCODE}/$*.o $< ; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJECTCODE}; fi

all: ${OBJECTCODE}/carre ${OBJECTCODE}/traduit ${OBJECTCODE}/fcrr

${OBJECTCODE}/carre: ${OBJECTCODE}/carre.o ${OBJECTCODE}/libcarre.a
	rm -f ${OBJECTCODE}/carre 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/carre ${OBJECTCODE}/carre.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJECTCODE}/traduit: ${OBJECTCODE}/tradui.o ${OBJECTCODE}/libcarre.a
	rm -f ${OBJECTCODE}/traduit 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/traduit ${OBJECTCODE}/tradui.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJECTCODE}/fcrr: ${OBJECTCODE}/fcrr.o ${OBJECTCODE}/fcrblkd.o ${OBJECTCODE}/libcarre.a ${SOLPS_LIB}/libmscl.a Makefile
	rm -f ${OBJECTCODE}/fcrr 2>/dev/null; \
	${FC} $(FFLAGS) -o ${OBJECTCODE}/fcrr ${OBJECTCODE}/fcrr.o ${OBJECTCODE}/fcrblkd.o ${OBJECTCODE}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA)

${OBJECTCODE}/libcarre.a: ${DEST}
	ar r $@ $?
	ranlib $@

clean:
	rm -rf ${OBJECTCODE}/*.o ${OBJECTCODE}/*.f ${OBJECTCODE}/libcarre.a ${OBJECTCODE}/carre ${OBJECTCODE}/traduit ${OBJECTCODE}/fcrr

neat:
	rm -rf ${OBJECTCODE}/*.o ${OBJECTCODE}/*.f

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

tags:
	rm TAGS ; etags src/*/*.F `find -L src/ -name '*.[Ff]90' -not -name ".*" -follow` 

depend: ${OBJS:.o=.F} ${EXCLUDELIS:.o=.F}
	makedepend -f	${OBJECTCODE}/dependencies.${OBJECTCODE} ${INCLUDE} $^
	mv ${OBJECTCODE}/dependencies.${OBJECTCODE} ${OBJECTCODE}/dependencies.${OBJECTCODE}.bak
	sed -e '3,$$s/^\.\.\/src\/[^\/]*\///' ${OBJECTCODE}/dependencies.${OBJECTCODE}.bak | \
	sed -e '3,$$s/^/${OBJECTCODE}\//' > ${OBJECTCODE}/dependencies.${OBJECTCODE}
	@egrep '^      use ' src/carre/*.F | grep -v 'IGNORE' | awk '{sub(".F",".o",$$1);sub("^.*/","$${OBJECTCODE}/",$$1); print $$1,"$${OBJECTCODE}/"$$3".o"}' >> ${OBJECTCODE}/dependencies
	@for d in `echo "${VPATH}" | tr : \ `; do \
		echo $$d ; \
		dirfiles=`find -L $$d/ -name '*.f90'` ; \
		if [ -n "$$dirfiles" ] ; then \
			grep -i '^[[:space:]]*use ' $$d/*.f90 /dev/null | grep -v 'IGNORE' | awk '{sub(".f90",".o",$$1);sub("^.*/","$${OBJECTCODE}/",$$1); print $$1,"$${OBJECTCODE}/"$$3".o"}' >> ${OBJECTCODE}/dependencies ;\
			for f in `find -L $$d -name '*.f90' -printf "%f "`; do \
				line='$${OBJECTCODE}/'"`echo $$f | tr A-Z a-z | sed -e 's/.f90/.mod/'`:"'$${OBJECTCODE}/'"`echo $$f | sed -e 's/.f90/.o/'`";\
				echo $$line >> ${OBJECTCODE}/dependencies.${OBJECTCODE} ;\
			done \
		fi ; \
		dirfiles=`find -L $$d/ -name '*.F90'` ; \
		if [ -n "$$dirfiles" ] ; then \
			grep -i '^[[:space:]]*use ' $$d/*.F90 /dev/null | grep -v 'IGNORE' | awk '{sub(".F90",".o",$$1);sub("^.*/","$${OBJECTCODE}/",$$1); print $$1,"$${OBJECTCODE}/"$$3".o"}' >> ${OBJECTCODE}/dependencies ;\
			for f in `find -L $$d -name '*.F90' -printf "%f "`; do \
				line='$${OBJECTCODE}/'"`echo $$f | tr A-Z a-z | sed -e 's/.f90/.mod/'`:"'$${OBJECTCODE}/'"`echo $$f | sed -e 's/.F90/.o/'`";\
				echo $$line >> ${OBJECTCODE}/dependencies.${OBJECTCODE} ;\
			done \
		fi \
	done;


update:
	cd $(SOLPSTOP); gmake update

listobj:
	@rm -f LISTOBJ; touch LISTOBJ; l="OBJS ="; \
	for d in `echo "$(VPATH)" | tr : \ `; do \
		l="$$l `find $$d -name '*.F' -printf "%f "`"; \
		l="$$l `find -L $$d -name '*.F90' -printf "%f "`"; \
		l="$$l `find -L $$d -name '*.f90' -printf "%f "`"; \
	done; \
	E="-e 's/\.F/\.o/g'" ; for f in $(MAINLIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$l" | eval sed "$$E" > LISTOBJ

LISTOBJ: listobj

${OBJECTCODE}/dependencies.${OBJECTCODE}:
	-mkdir ${OBJECTCODE}
	touch ${OBJECTCODE}/dependencies.${OBJECTCODE}
	${MAKE} depend

include ${OBJECTCODE}/dependencies.${OBJECTCODE}

