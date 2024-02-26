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

MAKETAGS ?= ctags -e -f

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
  ifeq ($(shell [ -e ${SOLPSTOP}/SETUP/config.common.${COMPILER} ] && echo yes || echo no ),yes)
    include ${SOLPSTOP}/SETUP/config.common.${COMPILER}
    MAKES += ${SOLPSTOP}/SETUP/config.common.${COMPILER}
  endif
else
  MAKES += ${SOLPSTOP}/Makefile ${SOLPSTOP}/SETUP/setup.csh.${HOST_NAME}.${COMPILER} ${SOLPSTOP}/SETUP/config.${HOST_NAME}.${COMPILER}
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
B2OBJ  = $(SOLPSTOP)/modules/B2.5/builds/couple_SOLPS-ITER.$(HOST_NAME).$(COMPILER)$(EXT_OPENMP)$(EXT_MPI)$(EXT_IMPGYRO)$(EXT_DEBUG)
B2SRC  = $(SOLPSTOP)/modules/B2.5/src
SRCLOCAL = $(PWD)/src.local
B2INCLUDE = -I${B2SRC}/include.local -I${B2SRC}/include -I${B2SRC}/common
ifdef LD_NETCDF
B2INCLUDE += -I${NCDIR}/include
endif

CARRE_NONINTERACTIVE = -DCARRE_NONINTERACTIVE

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

DEFINES  += ${SOLPS_CPP}
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
B2PATH  = ${SRCDIR}/b2_shared_itm:${SRCDIR}/carre_shared
VPATH   = ${VHEAD}${FPATH}:${GPATH}:${B2PATH}

ALLTARGETS = ${OBJDIR}/${PROG}
EXCLUDELIS = carre.o bidon.o
# We can only build the dg-to-Carre converter fcrr when we have the SOLPS environment available
USE_DIMENSIONS = 0
ifdef SOLPS_CPP
FPATH      += :${SRCDIR}/trans:${SRCDIR}/fcrr
VPATH      += :${SRCDIR}/trans:${SRCDIR}/fcrr
ALLTARGETS += ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR}
EXCLUDELIS += tradui.o fcrr.o fcrblkd.o
DIMMOD =
DIMOBJ =
ifeq ($(shell [ -s ${B2SRC}/modules/b2mod_dimensions.F ] && echo yes || echo no ),yes)
USE_DIMENSIONS = 1
DIMMOD = ${OBJDIR}/b2mod_dimensions.${MOD}
DIMOBJ = ${OBJDIR}/b2mod_dimensions.o
DIMSDIR = ${B2SRC}/modules
ifeq ($(shell [ -s ${B2SRC}/modules.local/b2mod_dimensions.F ] && echo yes || echo no ),yes)
DIMSDIR = ${B2SRC}/modules.local
endif
endif
endif

MODLIST = ${SRCDIR}/carre_mod/*.F90 ${SRCDIR}/carre_shared/*.F90 ${SRCDIR}/usol/*.f90 ${SRCDIR}/itm_shared/*.f90 ${SRCDIR}/b2_shared_itm/*.F90

DEFINES += -DBUILDING_CARRE ${CARRE_NONINTERACTIVE}
COMPILE += ${OBJDEST}

INCLUDE = -I${SRCDIR}/include

include ${OBJDIR}/LISTOBJ

DEST = $(OBJS:%.o=$(OBJDIR)/%.o) $(OBJSL90:%.o=$(OBJDIR)/%.o) $(OBJSU90:%.o=$(OBJDIR)/%.o)
GDEST = $(GOBJS:%.o=$(OBJDIR)/%.o)
B2DEST = $(B2OBJS:%.o=$(OBJDIR)/%.o)
MAINLIST = $(EXCLUDELIS:.=\.)
LIBRARIES = $(LDFLAGS:-l%=${LIBSOLDIR}/lib%.a)

$(OBJDIR)/%.o : %.F
	- /bin/rm -f ${OBJDIR}/$*.f
ifeq ($(strip ${DBLPAD}),)
	${CPP} ${SOLPS_CPP} ${DEFINES} -P ${INCLUDE} $< ${OBJDIR}/$*.f; \
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi
else
	${CPP} ${SOLPS_CPP} ${DEFINES} -P ${INCLUDE} $< ${OBJDIR}/$*.f; \
	case $< in \
		${SRCDIR}/trans/* ) $(COMPILE) $(DBLPAD) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
		       *    ) $(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f;; \
	esac; \
	if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi
endif

all: VERSION ${OBJDIR}/${PROG}
ifdef LD_NCARG
ifeq ($(strip ${GLI_HOME}),)
$(warning Carre2 graphics may not work because GLI_HOME is not defined.)
endif
all: ${OBJDIR}/.x
else
ifndef NCARG_ROOT
$(warning Carre2 graphics are turned off as NCARG_ROOT is not defined.)
endif
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
	@if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

ifneq (${MOD},o)
$(OBJDIR)/%.${MOD}: %.F90
	@- /bin/rm -f ${OBJDIR}/$*.F90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	@if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi
	@if [ -f $*.${MOD} ]; then /bin/mv $*.${MOD} ${OBJDIR}; fi
endif

$(OBJDIR)/%.o : %.f90
	@- /bin/rm -f ${OBJDIR}/$*.f90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	@if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi

ifneq (${MOD},o)
$(OBJDIR)/%.${MOD}: %.f90
	@- /bin/rm -f ${OBJDIR}/$*.f90 ${OBJDIR}/$*.o ${OBJDIR}/$*.${MOD}
	${CPP} ${DEFINES} -P -C ${INCLUDE} $< ${OBJDIR}/$*.f90
	$(COMPILE) $(INCLUDE) -o ${OBJDIR}/$*.o ${OBJDIR}/$*.f90
	@if [ -f $*.o ]; then /bin/mv $*.o ${OBJDIR}; fi
	@if [ -f $*.${MOD} ]; then /bin/mv $*.${MOD} ${OBJDIR}; fi
endif

# compile an executable
${OBJDIR}/%.exe : ${OBJDIR}/%.o ${MAKES}
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/$*.exe ${OBJDIR}/%.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

all: ${ALLTARGETS}

ifdef LD_NCARG
${OBJDIR}/${PROG} ${OBJDIR}/.x: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG} 2> /dev/null; \
	rm -f ${OBJDIR}/.nox 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}
	touch ${OBJDIR}/.x
else
${OBJDIR}/${PROG} ${OBJDIR}/.nox: ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/bidon.o $(MAKES)
	rm -f ${OBJDIR}/${PROG} 2> /dev/null; \
	rm -f ${OBJDIR}/.x 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG} ${OBJDIR}/carre.o ${OBJDIR}/libcarre.a ${OBJDIR}/bidon.o ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}
	touch ${OBJDIR}/.nox
endif

${OBJDIR}/${PROG_TRA}: ${OBJDIR}/tradui.o ${OBJDIR}/bidon.o ${OBJDIR}/libcarre.a ${OBJDIR}/libb25.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_TRA} 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG_TRA} ${OBJDIR}/tradui.o ${OBJDIR}/bidon.o ${OBJDIR}/libb25.a ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}

ifdef ITM_ENVIRONMENT_LOADED
${OBJDIR}/${PROG_FCRR}: ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/b2mod_ual.${MOD} ${OBJDIR}/libcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_FCRR} 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}
else
${OBJDIR}/${PROG_FCRR}: ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a $(MAKES)
	rm -f ${OBJDIR}/${PROG_FCRR} 2> /dev/null; \
	${FC} $(FFLAGS) ${FFLAGSEXTRA} -o ${OBJDIR}/${PROG_FCRR} ${OBJDIR}/fcrr.o ${OBJDIR}/fcrblkd.o ${OBJDIR}/libcarre.a ${LDLIBS} $(LDFLAGS) $(LDEXTRA) ${USOLLIBS}
endif

${OBJDIR}/libcarre.a: ${DEST} ${MAKES}
	@ar rucv $@ ${DEST}
	ranlib $@

${OBJDIR}/libgcarre.a: ${GDEST} ${MAKES}
	@ar rucv $@ ${GDEST}
	ranlib $@

ifdef ITM_ENVIRONMENT_LOADED
${OBJDIR}/libb25.a: ${B2DEST} ${MAKES} ${DIMMOD} ${OBJDIR}/b2mod_ad.${MOD} ${OBJDIR}/b2mod_anomalous_transport.${MOD} ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_b2cmpa.${MOD} ${OBJDIR}/b2mod_b2cmpb.${MOD} ${OBJDIR}/b2mod_b2cmrc.${MOD} ${OBJDIR}/b2mod_b2plot.${MOD} ${OBJDIR}/b2mod_b2plot_wall_loading.${MOD} ${OBJDIR}/b2mod_balance.${MOD} ${OBJDIR}/b2mod_boundary_namelist.${MOD} ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_constants.${MOD} ${OBJDIR}/b2mod_diag.${MOD} ${OBJDIR}/b2mod_eirdiag.${MOD} ${OBJDIR}/b2mod_eirene_globals.${MOD} ${OBJDIR}/b2mod_elements.${MOD} ${OBJDIR}/b2mod_external.${MOD} ${OBJDIR}/b2mod_feedback.${MOD} ${OBJDIR}/b2mod_geo.${MOD} ${OBJDIR}/b2mod_geo2.${MOD} ${OBJDIR}/b2mod_geo_corner.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/b2mod_indirect.${MOD} ${OBJDIR}/b2mod_interp.${MOD} ${OBJDIR}/b2mod_ipmain.${MOD} ${OBJDIR}/b2mod_layer.${MOD} ${OBJDIR}/b2mod_lwimai.${MOD} ${OBJDIR}/b2mod_lwmain.${MOD} ${OBJDIR}/b2mod_math.${MOD} ${OBJDIR}/b2mod_mwti.${MOD} ${OBJDIR}/b2mod_neutr_src_scaling.${MOD} ${OBJDIR}/b2mod_neutrals_namelist.${MOD} ${OBJDIR}/b2mod_openmp.${MOD} ${OBJDIR}/b2mod_plasma.${MOD} ${OBJDIR}/b2mod_ppout.${MOD} ${OBJDIR}/b2mod_rates.${MOD} ${OBJDIR}/b2mod_residuals.${MOD} ${OBJDIR}/b2mod_running_average.${MOD} ${OBJDIR}/b2mod_sources.${MOD} ${OBJDIR}/b2mod_switches.${MOD} ${OBJDIR}/b2mod_subsys.${MOD} ${OBJDIR}/b2mod_tallies.${MOD} ${OBJDIR}/b2mod_transport.${MOD} ${OBJDIR}/b2mod_transport_nspecies.${MOD} ${OBJDIR}/b2mod_types.${MOD} ${OBJDIR}/b2mod_ual.${MOD} ${OBJDIR}/b2mod_ual_io.${MOD} ${OBJDIR}/b2mod_ual_io_data.${MOD} ${OBJDIR}/b2mod_ual_io_grid.${MOD} ${OBJDIR}/b2mod_user_namelist.${MOD} ${OBJDIR}/b2mod_version.${MOD} ${OBJDIR}/b2mod_wall.${MOD} ${OBJDIR}/b2mod_work.${MOD} ${OBJDIR}/b2mod_xerset.${MOD} ${OBJDIR}/b2us_geo.${MOD} ${OBJDIR}/b2us_map.${MOD} ${OBJDIR}/b2xbzb.o ${OBJDIR}/b2xvsg.o ${OBJDIR}/cfruin.o ${OBJDIR}/cfrure.o ${OBJDIR}/cfvers.o ${OBJDIR}/cfwuin.o ${OBJDIR}/cfwure.o ${OBJDIR}/chcase.o ${OBJDIR}/ifill.o ${OBJDIR}/intface.o ${OBJDIR}/intvertex.o ${OBJDIR}/ipgeti.o ${OBJDIR}/ipgetr.o ${OBJDIR}/machsfr.o ${OBJDIR}/my_out_us.o ${OBJDIR}/nagsubst.o ${OBJDIR}/open_file.o ${OBJDIR}/prvrt.o ${OBJDIR}/prvrti.o ${OBJDIR}/sfill.o ${OBJDIR}/smax.o ${OBJDIR}/smin.o ${OBJDIR}/streql.o ${OBJDIR}/xertst.o ${OBJDIR}/xerrab.o
	@ar rucv $@ ${B2DEST} ${DIMOBJ} ${OBJDIR}/b2mod_ad.o ${OBJDIR}/b2mod_anomalous_transport.o ${OBJDIR}/b2mod_b2cmfs.o ${OBJDIR}/b2mod_b2cmpa.o ${OBJDIR}/b2mod_b2cmpb.o ${OBJDIR}/b2mod_b2cmrc.o ${OBJDIR}/b2mod_b2plot.o ${OBJDIR}/b2mod_b2plot_wall_loading.o ${OBJDIR}/b2mod_balance.o ${OBJDIR}/b2mod_boundary_namelist.o ${OBJDIR}/b2mod_cellhelper.o ${OBJDIR}/b2mod_connectivity.o ${OBJDIR}/b2mod_constants.o ${OBJDIR}/b2mod_diag.o ${OBJDIR}/b2mod_eirdiag.o ${OBJDIR}/b2mod_eirene_globals.o ${OBJDIR}/b2mod_elements.o ${OBJDIR}/b2mod_feedback.o ${OBJDIR}/b2mod_geo.o ${OBJDIR}/b2mod_geo2.o ${OBJDIR}/b2mod_geo_corner.o ${OBJDIR}/b2mod_geometry.o ${OBJDIR}/b2mod_grid_mapping.o ${OBJDIR}/b2mod_indirect.o ${OBJDIR}/b2mod_interp.o ${OBJDIR}/b2mod_ipmain.o ${OBJDIR}/b2mod_layer.o ${OBJDIR}/b2mod_lwimai.o ${OBJDIR}/b2mod_lwmain.o ${OBJDIR}/b2mod_math.o ${OBJDIR}/b2mod_mwti.o ${OBJDIR}/b2mod_neutr_src_scaling.o ${OBJDIR}/b2mod_neutrals_namelist.o ${OBJDIR}/b2mod_openmp.o ${OBJDIR}/b2mod_plasma.o ${OBJDIR}/b2mod_ppout.o ${OBJDIR}/b2mod_rates.o ${OBJDIR}/b2mod_residuals.o ${OBJDIR}/b2mod_running_average.o ${OBJDIR}/b2mod_sources.o ${OBJDIR}/b2mod_switches.o ${OBJDIR}/b2mod_subsys.o ${OBJDIR}/b2mod_tallies.o ${OBJDIR}/b2mod_transport.o ${OBJDIR}/b2mod_transport_nspecies.o ${OBJDIR}/b2mod_types.o ${OBJDIR}/b2mod_ual.o ${OBJDIR}/b2mod_ual_io.o ${OBJDIR}/b2mod_ual_io_data.o ${OBJDIR}/b2mod_ual_io_grid.o ${OBJDIR}/b2mod_user_namelist.o ${OBJDIR}/b2mod_version.o ${OBJDIR}/b2mod_wall.o ${OBJDIR}/b2mod_work.o ${OBJDIR}/b2mod_xerset.o ${OBJDIR}/b2us_geo.o ${OBJDIR}/b2us_map.o ${OBJDIR}/b2xbzb.o ${OBJDIR}/b2xvsg.o ${OBJDIR}/cfruin.o ${OBJDIR}/cfrure.o ${OBJDIR}/cfvers.o ${OBJDIR}/cfwuin.o ${OBJDIR}/cfwure.o ${OBJDIR}/chcase.o ${OBJDIR}/ifill.o ${OBJDIR}/intface.o ${OBJDIR}/intvertex.o ${OBJDIR}/ipgeti.o ${OBJDIR}/ipgetr.o ${OBJDIR}/machsfr.o ${OBJDIR}/my_out_us.o ${OBJDIR}/nagsubst.o ${OBJDIR}/open_file.o ${OBJDIR}/prvrt.o ${OBJDIR}/prvrti.o ${OBJDIR}/sfill.o ${OBJDIR}/smax.o ${OBJDIR}/smin.o ${OBJDIR}/streql.o ${OBJDIR}/xertst.o ${OBJDIR}/xerrab.o
else
${OBJDIR}/libb25.a: ${B2DEST} ${MAKES} ${DIMMOD} ${OBJDIR}/b2mod_ad.${MOD} ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_constants.${MOD} ${OBJDIR}/b2mod_geo.${MOD} ${OBJDIR}/b2mod_geo2.${MOD} ${OBJDIR}/b2mod_geo_corner.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/b2mod_indirect.${MOD} ${OBJDIR}/b2mod_ipmain.${MOD} ${OBJDIR}/b2mod_lwimai.${MOD} ${OBJDIR}/b2mod_lwmain.${MOD} ${OBJDIR}/b2mod_math.${MOD} ${OBJDIR}/b2mod_openmp.${MOD} ${OBJDIR}/b2mod_switches.${MOD} ${OBJDIR}/b2mod_subsys.${MOD} ${OBJDIR}/b2mod_types.${MOD} ${OBJDIR}/b2mod_version.${MOD} ${OBJDIR}/b2mod_xerset.${MOD} ${OBJDIR}/b2us_geo.${MOD} ${OBJDIR}/b2us_map.${MOD} ${OBJDIR}/b2xbzb.o ${OBJDIR}/b2xvsg.o ${OBJDIR}/cfruin.o ${OBJDIR}/cfrure.o ${OBJDIR}/cfvers.o ${OBJDIR}/cfwuin.o ${OBJDIR}/cfwure.o ${OBJDIR}/chcase.o ${OBJDIR}/ifill.o ${OBJDIR}/intface.o ${OBJDIR}/intvertex.o ${OBJDIR}/ipgeti.o ${OBJDIR}/machsfr.o ${OBJDIR}/my_out_us.o ${OBJDIR}/nagsubst.o ${OBJDIR}/open_file.o ${OBJDIR}/prvrt.o ${OBJDIR}/prvrti.o ${OBJDIR}/sfill.o ${OBJDIR}/streql.o ${OBJDIR}/xerrab.o ${OBJDIR}/xertst.o
	@ar rucv $@ ${B2DEST} ${DIMOBJ} ${OBJDIR}/b2mod_ad.o ${OBJDIR}/b2mod_b2cmfs.o ${OBJDIR}/b2mod_cellhelper.o ${OBJDIR}/b2mod_connectivity.o ${OBJDIR}/b2mod_constants.o ${OBJDIR}/b2mod_geo.o ${OBJDIR}/b2mod_geo2.o ${OBJDIR}/b2mod_geo_corner.o ${OBJDIR}/b2mod_geometry.o ${OBJDIR}/b2mod_grid_mapping.o ${OBJDIR}/b2mod_indirect.o ${OBJDIR}/b2mod_ipmain.o ${OBJDIR}/b2mod_lwimai.o ${OBJDIR}/b2mod_lwmain.o ${OBJDIR}/b2mod_math.o ${OBJDIR}/b2mod_openmp.o ${OBJDIR}/b2mod_switches.o ${OBJDIR}/b2mod_subsys.o ${OBJDIR}/b2mod_types.o ${OBJDIR}/b2mod_version.o ${OBJDIR}/b2mod_xerset.o ${OBJDIR}/b2us_geo.o ${OBJDIR}/b2us_map.o ${OBJDIR}/b2xbzb.o ${OBJDIR}/b2xvsg.o ${OBJDIR}/cfruin.o ${OBJDIR}/cfrure.o ${OBJDIR}/cfvers.o ${OBJDIR}/cfwuin.o ${OBJDIR}/cfwure.o ${OBJDIR}/chcase.o ${OBJDIR}/ifill.o ${OBJDIR}/intface.o ${OBJDIR}/intvertex.o ${OBJDIR}/ipgeti.o ${OBJDIR}/machsfr.o ${OBJDIR}/my_out_us.o ${OBJDIR}/nagsubst.o ${OBJDIR}/open_file.o ${OBJDIR}/prvrt.o ${OBJDIR}/prvrti.o ${OBJDIR}/sfill.o ${OBJDIR}/streql.o ${OBJDIR}/xerrab.o ${OBJDIR}/xertst.o
endif
	ranlib $@

clean:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f ${OBJDIR}/*.f90 ${OBJDIR}/*.mod ${OBJDIR}/libcarre.a ${OBJDIR}/libgcarre.a ${OBJDIR}/libb25.a ${OBJDIR}/${PROG} ${OBJDIR}/${PROG_TRA} ${OBJDIR}/${PROG_FCRR} ${SRCDIR}/include/git_version_Carre.h ${OBJDIR}/dependencies* ${OBJDIR}/LISTOBJ ${SRCDIR}/b25_links/*.*

neat:
	rm -rf ${OBJDIR}/*.o ${OBJDIR}/*.f

runclean: 
	@rm carre0*

local:
	-rm rzpsi.mtv rzpsi.ps map loadmap gnuplot.data gnuplot.cmd
	-gtfl btor.dat structure.dat rzpsi.dat ncar.cfg gmeta fort.11 carre.out carre.log carre.dat warnings.dat traduit.log selptx.inf traduit.out 

TAGS:	tags

tags:
	rm -f TAGS ; ${MAKETAGS} TAGS ${SRCDIR}/*/*.F `find -L ${SRCDIR}/ -name '*.[Ff]90' -not -name ".*"` || touch TAGS

depend: ${OBJS:.o=.F} ${GOBJS:.o=.F90} ${B2OBJS:.o=.F90} ${EXCLUDELIS:.o=.F90} ${OBJSL90:.o=.f90} ${OBJSU90:.o=.F90}
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
	@egrep -aiH '^ {0,}use ' $^ | grep -v 'IGNORE' | tr , ' ' | awk '{sub("\\.F:",".o:",$$1);sub("\\.F90:",".o:",$$1);sub("\\.f90:",".o:",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"tolower($$3)".o"}' >> ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 4' >> ${OBJDIR}/dependencies.${COMPILER}
	@egrep -aiH '^ {0,}use ' ${MODLIST} | grep -v 'IGNORE' | tr , ' ' | awk '{sub("\\.F:",".${MOD}:",$$1);sub("\\.f:",".${MOD}:",$$1);sub("\\.F90:",".${MOD}:",$$1);sub("\\.f90:",".${MOD}:",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"tolower($$3)".${MOD}"}' >> ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 5' >> ${OBJDIR}/dependencies.${COMPILER}
ifeq (${COMPILER},pgf90)
	@egrep -aiH '^ {0,}use ' ${MODLIST} | grep -v 'IGNORE' | tr , ' ' | awk '{sub("\\.F:",".${MOD}:",$$1);sub("\\.f:",".${MOD}:",$$1);sub("\\.F90:",".${MOD}:",$$1);sub("\\.f90:",".${MOD}:",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"tolower($$3)".o"}' >> ${OBJDIR}/dependencies.${COMPILER}
	@echo '# 6' >> ${OBJDIR}/dependencies.${COMPILER}
endif
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
	@lll="B2OBJS ="; \
	for d in `echo "$(B2PATH)" | tr : \ `; do \
		lll="$$lll `find $$d -name '*.F90' -printf "%f "`"; \
	done; \
	E="-e 's/\.F90/\.o/g'" ; for f in $(MAINLIST); do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$lll" | eval sed "$$E" >> ${OBJDIR}/LISTOBJ

${OBJDIR}/LISTOBJ: listobj ${ITM_SRC_PREREQS}

VERSION: ${SRCDIR}/include/git_version_Carre.h

${SRCDIR}/include/git_version_Carre.h: force
	@echo "      character*32 :: git_version_Carre =" > ${SRCDIR}/include/git_version_new.h
	@echo "     . '`git describe --tags --dirty --always | cut -c 1-32`'" >> ${SRCDIR}/include/git_version_new.h
	@if cmp -s ${SRCDIR}/include/git_version_new.h ${SRCDIR}/include/git_version_Carre.h; then rm ${SRCDIR}/include/git_version_new.h; else mv ${SRCDIR}/include/git_version_new.h ${SRCDIR}/include/git_version_Carre.h; fi

${OBJDIR}/dependencies.${COMPILER}:
	-mkdir -p ${OBJDIR}
	touch ${OBJDIR}/dependencies.${COMPILER}
	${MAKE} VERSION
	${MAKE} tags
	${MAKE} listobj
	${MAKE} depend

ifdef SOLPS_CPP
ifneq (${MOD},o)

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_dimensions.${MOD}: ${DIMSDIR}/b2mod_dimensions.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${DIMSDIR}/b2mod_dimensions.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_dimensions.F ${OBJDIR}/b2mod_dimensions.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_dimensions.o ${OBJDIR}/b2mod_dimensions.f
else
#${OBJDIR}/b2mod_dimensions.${MOD}:
#	@touch ${OBJDIR}/b2mod_dimensions.o
#	@touch ${OBJDIR}/b2mod_dimensions.{MOD}
endif

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_ad.${MOD}: ${B2SRC}/modules/b2mod_ad.F ${OBJDIR}/b2mod_types.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_ad.${MOD}: ${B2SRC}/modules/b2mod_ad.F ${OBJDIR}/b2mod_types.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_ad.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ad.F ${OBJDIR}/b2mod_ad.f
	@rm -f ${OBJDIR}/b2mod_ad.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ad.o ${OBJDIR}/b2mod_ad.f

${OBJDIR}/b2mod_b2cmfs.${MOD}: ${B2SRC}/modules/b2mod_b2cmfs.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2cmfs.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2cmfs.F ${OBJDIR}/b2mod_b2cmfs.f
	@rm -f ${OBJDIR}/b2mod_b2cmfs.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2cmfs.o ${OBJDIR}/b2mod_b2cmfs.f

${OBJDIR}/b2mod_cellhelper.${MOD}: ${B2SRC}/ids/b2mod_cellhelper.F90 ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_cellhelper.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_cellhelper.F90 ${OBJDIR}/b2mod_cellhelper.f90
	@rm -f ${OBJDIR}/b2mod_cellhelper.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_cellhelper.o ${OBJDIR}/b2mod_cellhelper.f90

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_connectivity.${MOD}: ${B2SRC}/ids/b2mod_connectivity.F90 ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_types.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_connectivity.${MOD}: ${B2SRC}/ids/b2mod_connectivity.F90 ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_types.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_connectivity.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_connectivity.F90 ${OBJDIR}/b2mod_connectivity.f90
	@rm -f ${OBJDIR}/b2mod_connectivity.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_connectivity.o ${OBJDIR}/b2mod_connectivity.f90

${OBJDIR}/b2mod_constants.${MOD}: ${B2SRC}/ids/b2mod_constants.F90
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_constants.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_constants.F90 ${OBJDIR}/b2mod_constants.f90
	@rm -f ${OBJDIR}/b2mod_constants.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_constants.o ${OBJDIR}/b2mod_constants.f90

${OBJDIR}/b2mod_geo.${MOD}: ${B2SRC}/modules/b2mod_geo.F ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_geo_corner.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/b2mod_indirect.${MOD} ${OBJDIR}/b2mod_math.${MOD} ${OBJDIR}/b2mod_types.${MOD} ${OBJDIR}/b2mod_version.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_geo.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geo.F ${OBJDIR}/b2mod_geo.f
	@rm -f ${OBJDIR}/b2mod_geo.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geo.o ${OBJDIR}/b2mod_geo.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_geo2.${MOD}: ${B2SRC}/modules/b2mod_geo2.F ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_constants.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/carre_constants.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_geo2.${MOD}: ${B2SRC}/modules/b2mod_geo2.F ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_constants.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/carre_constants.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_geo2.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geo2.F ${OBJDIR}/b2mod_geo2.f
	@rm -f ${OBJDIR}/b2mod_geo2.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geo2.o ${OBJDIR}/b2mod_geo2.f

${OBJDIR}/b2mod_geo_corner.${MOD}: ${B2SRC}/modules/b2mod_geo_corner.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_geo_corner.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geo_corner.F ${OBJDIR}/b2mod_geo_corner.f
	@rm -f ${OBJDIR}/b2mod_geo_corner.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geo_corner.o ${OBJDIR}/b2mod_geo_corner.f

${OBJDIR}/b2mod_geometry.${MOD}: ${B2SRC}/ids/b2mod_geometry.F90 ${OBJDIR}/b2us_geo.${MOD} ${OBJDIR}/b2us_map.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_geometry.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geometry.F90 ${OBJDIR}/b2mod_geometry.f90
	@rm -f ${OBJDIR}/b2mod_geometry.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geometry.o ${OBJDIR}/b2mod_geometry.f90

${OBJDIR}/b2mod_grid_mapping.${MOD}: ${B2SRC}/ids/b2mod_grid_mapping.F90 ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_grid_mapping.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_grid_mapping.F90 ${OBJDIR}/b2mod_grid_mapping.f90
	@rm -f ${OBJDIR}/b2mod_grid_mapping.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_grid_mapping.o ${OBJDIR}/b2mod_grid_mapping.f90

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_indirect.${MOD}: ${B2SRC}/modules/b2mod_indirect.F ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD} ${OBJDIR}/b2mod_math.${MOD}
else
${OBJDIR}/b2mod_indirect.${MOD}: ${B2SRC}/modules/b2mod_indirect.F ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_math.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_indirect.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_indirect.F ${OBJDIR}/b2mod_indirect.f
	@rm -f ${OBJDIR}/b2mod_indirect.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_indirect.o ${OBJDIR}/b2mod_indirect.f

${OBJDIR}/b2mod_ipmain.${MOD}: ${B2SRC}/utility/b2mod_ipmain.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_ipmain.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ipmain.F ${OBJDIR}/b2mod_ipmain.f
	@rm -f ${OBJDIR}/b2mod_ipmain.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ipmain.o ${OBJDIR}/b2mod_ipmain.f

${OBJDIR}/b2mod_lwimai.${MOD}: ${B2SRC}/utility/b2mod_lwimai.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_lwimai.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_lwimai.F ${OBJDIR}/b2mod_lwimai.f
	@rm -f ${OBJDIR}/b2mod_lwimai.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_lwimai.o ${OBJDIR}/b2mod_lwimai.f

${OBJDIR}/b2mod_lwmain.${MOD}: ${B2SRC}/utility/b2mod_lwmain.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_lwmain.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_lwmain.F ${OBJDIR}/b2mod_lwmain.f
	@rm -f ${OBJDIR}/b2mod_lwmain.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_lwmain.o ${OBJDIR}/b2mod_lwmain.f

${OBJDIR}/b2mod_math.${MOD}: ${B2SRC}/modules/b2mod_math.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_math.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_math.F ${OBJDIR}/b2mod_math.f
	@rm -f ${OBJDIR}/b2mod_math.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_math.o ${OBJDIR}/b2mod_math.f

${OBJDIR}/b2mod_openmp.${MOD}: ${B2SRC}/modules/b2mod_openmp.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_openmp.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_openmp.F ${OBJDIR}/b2mod_openmp.f
	@rm -f ${OBJDIR}/b2mod_openmp.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_openmp.o ${OBJDIR}/b2mod_openmp.f

${OBJDIR}/b2mod_switches.${MOD}: ${B2SRC}/modules/b2mod_switches.F ${OBJDIR}/b2mod_constants.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_switches.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_switches.F ${OBJDIR}/b2mod_switches.f
	@rm -f ${OBJDIR}/b2mod_switches.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_switches.o ${OBJDIR}/b2mod_switches.f

${OBJDIR}/b2mod_subsys.${MOD}: ${B2SRC}/utility/b2mod_subsys.F ${OBJDIR}/b2mod_lwimai.${MOD} ${OBJDIR}/b2mod_lwmain.${MOD} ${OBJDIR}/b2mod_openmp.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_subsys.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_subsys.F ${OBJDIR}/b2mod_subsys.f
	@rm -f ${OBJDIR}/b2mod_subsys.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_subsys.o ${OBJDIR}/b2mod_subsys.f

${OBJDIR}/b2mod_types.${MOD}: ${B2SRC}/modules/b2mod_types.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_types.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_types.F ${OBJDIR}/b2mod_types.f
	@rm -f ${OBJDIR}/b2mod_types.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_types.o ${OBJDIR}/b2mod_types.f

${OBJDIR}/b2mod_version.${MOD}: ${B2SRC}/modules/b2mod_version.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_version.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_version.F ${OBJDIR}/b2mod_version.f
	@rm -f ${OBJDIR}/b2mod_version.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_version.o ${OBJDIR}/b2mod_version.f

${OBJDIR}/b2mod_xerset.${MOD}: ${B2SRC}/utility/b2mod_xerset.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_xerset.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_xerset.F ${OBJDIR}/b2mod_xerset.f
	@rm -f ${OBJDIR}/b2mod_xerset.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_xerset.o ${OBJDIR}/b2mod_xerset.f

${OBJDIR}/b2us_geo.${MOD}: ${B2SRC}/modules/b2us_geo.F ${OBJDIR}/b2mod_switches.${MOD} ${OBJDIR}/b2us_map.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2us_geo.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2us_geo.F ${OBJDIR}/b2us_geo.f
	@rm -f ${OBJDIR}/b2us_geo.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2us_geo.o ${OBJDIR}/b2us_geo.f

${OBJDIR}/b2us_map.${MOD}: ${B2SRC}/modules/b2us_map.F ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_version.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2us_map.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2us_map.F ${OBJDIR}/b2us_map.f
	@rm -f ${OBJDIR}/b2us_map.${MOD}
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2us_map.o ${OBJDIR}/b2us_map.f
endif

${OBJDIR}/b2mod_ad.o: ${B2SRC}/modules/b2mod_ad.F ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_ad.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ad.F ${OBJDIR}/b2mod_ad.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ad.o ${OBJDIR}/b2mod_ad.f
	@touch ${OBJDIR}/b2mod_ad.${MOD}

${OBJDIR}/b2mod_b2cmfs.o: ${B2SRC}/modules/b2mod_b2cmfs.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2cmfs.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2cmfs.F ${OBJDIR}/b2mod_b2cmfs.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2cmfs.o ${OBJDIR}/b2mod_b2cmfs.f
	@touch ${OBJDIR}/b2mod_b2cmfs.${MOD}

${OBJDIR}/b2mod_cellhelper.o: ${B2SRC}/ids/b2mod_cellhelper.F90 ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_cellhelper.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_cellhelper.F90 ${OBJDIR}/b2mod_cellhelper.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_cellhelper.o ${OBJDIR}/b2mod_cellhelper.f90
	@touch ${OBJDIR}/b2mod_cellhelper.${MOD}

${OBJDIR}/b2mod_connectivity.o: ${B2SRC}/ids/b2mod_connectivity.F90 ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_connectivity.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_connectivity.F90 ${OBJDIR}/b2mod_connectivity.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_connectivity.o ${OBJDIR}/b2mod_connectivity.f90
	@touch ${OBJDIR}/b2mod_connectivity.${MOD}

${OBJDIR}/b2mod_constants.o: ${B2SRC}/ids/b2mod_constants.F90
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_constants.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_constants.F90 ${OBJDIR}/b2mod_constants.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_constants.o ${OBJDIR}/b2mod_constants.f90
	@touch ${OBJDIR}/b2mod_constants.${MOD}

${OBJDIR}/b2mod_geo.o: ${B2SRC}/modules/b2mod_geo.F ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_geo_corner.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/b2mod_indirect.${MOD} ${OBJDIR}/b2mod_math.${MOD} ${OBJDIR}/b2mod_types.${MOD} ${OBJDIR}/b2mod_version.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_geo.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geo.F ${OBJDIR}/b2mod_geo.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geo.o ${OBJDIR}/b2mod_geo.f
	@touch ${OBJDIR}/b2mod_geo.${MOD}

${OBJDIR}/b2mod_geo2.o: ${B2SRC}/modules/b2mod_geo2.F ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_constants.${MOD} ${OBJDIR}/b2mod_geometry.${MOD} ${OBJDIR}/b2mod_grid_mapping.${MOD} ${OBJDIR}/carre_constants.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_geo2.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geo2.F ${OBJDIR}/b2mod_geo2.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geo2.o ${OBJDIR}/b2mod_geo2.f
	@touch ${OBJDIR}/b2mod_geo2.${MOD}

${OBJDIR}/b2mod_geo_corner.o: ${B2SRC}/modules/b2mod_geo_corner.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_geo_corner.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geo_corner.F ${OBJDIR}/b2mod_geo_corner.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geo_corner.o ${OBJDIR}/b2mod_geo_corner.f
	@touch ${OBJDIR}/b2mod_geo_corner.${MOD}

${OBJDIR}/b2mod_geometry.o: ${B2SRC}/ids/b2mod_geometry.F90 ${OBJDIR}/b2us_geo.${MOD} ${OBJDIR}/b2us_map.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_geometry.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_geometry.F90 ${OBJDIR}/b2mod_geometry.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_geometry.o ${OBJDIR}/b2mod_geometry.f90
	@touch ${OBJDIR}/b2mod_geometry.${MOD}

${OBJDIR}/b2mod_grid_mapping.o: ${B2SRC}/ids/b2mod_grid_mapping.F90 ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_grid_mapping.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_grid_mapping.F90 ${OBJDIR}/b2mod_grid_mapping.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_grid_mapping.o ${OBJDIR}/b2mod_grid_mapping.f90
	@touch ${OBJDIR}/b2mod_grid_mapping.${MOD}

${OBJDIR}/b2mod_indirect.o: ${B2SRC}/modules/b2mod_indirect.F ${OBJDIR}/b2mod_cellhelper.${MOD} ${OBJDIR}/b2mod_connectivity.${MOD} ${OBJDIR}/b2mod_math.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_indirect.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_indirect.F ${OBJDIR}/b2mod_indirect.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_indirect.o ${OBJDIR}/b2mod_indirect.f
	@touch ${OBJDIR}/b2mod_indirect.${MOD}

${OBJDIR}/b2mod_ipmain.o: ${B2SRC}/utility/b2mod_ipmain.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_ipmain.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ipmain.F ${OBJDIR}/b2mod_ipmain.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ipmain.o ${OBJDIR}/b2mod_ipmain.f
	@touch ${OBJDIR}/b2mod_ipmain.${MOD}

${OBJDIR}/b2mod_lwimai.o: ${B2SRC}/utility/b2mod_lwimai.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_lwimai.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_lwimai.F ${OBJDIR}/b2mod_lwimai.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_lwimai.o ${OBJDIR}/b2mod_lwimai.f
	@touch ${OBJDIR}/b2mod_lwimai.${MOD}

${OBJDIR}/b2mod_lwmain.o: ${B2SRC}/utility/b2mod_lwmain.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_lwmain.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_lwmain.F ${OBJDIR}/b2mod_lwmain.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_lwmain.o ${OBJDIR}/b2mod_lwmain.f
	@touch ${OBJDIR}/b2mod_lwmain.${MOD}

${OBJDIR}/b2mod_math.o: ${B2SRC}/modules/b2mod_math.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_math.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_math.F ${OBJDIR}/b2mod_math.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_math.o ${OBJDIR}/b2mod_math.f
	@touch ${OBJDIR}/b2mod_math.${MOD}

${OBJDIR}/b2mod_openmp.o: ${B2SRC}/modules/b2mod_openmp.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_openmp.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_openmp.F ${OBJDIR}/b2mod_openmp.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_openmp.o ${OBJDIR}/b2mod_openmp.f
	@touch ${OBJDIR}/b2mod_openmp.${MOD}

${OBJDIR}/b2mod_switches.o: ${B2SRC}/modules/b2mod_switches.F ${OBJDIR}/b2mod_constants.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_switches.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_switches.F ${OBJDIR}/b2mod_switches.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_switches.o ${OBJDIR}/b2mod_switches.f
	@touch ${OBJDIR}/b2mod_switches.${MOD}

${OBJDIR}/b2mod_subsys.o: ${B2SRC}/utility/b2mod_subsys.F ${OBJDIR}/b2mod_lwimai.${MOD} ${OBJDIR}/b2mod_lwmain.${MOD} ${OBJDIR}/b2mod_openmp.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_subsys.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_subsys.F ${OBJDIR}/b2mod_subsys.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_subsys.o ${OBJDIR}/b2mod_subsys.f
	@touch ${OBJDIR}/b2mod_subsys.${MOD}

${OBJDIR}/b2mod_types.o: ${B2SRC}/modules/b2mod_types.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_types.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_types.F ${OBJDIR}/b2mod_types.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_types.o ${OBJDIR}/b2mod_types.f
	@touch ${OBJDIR}/b2mod_types.${MOD}

${OBJDIR}/b2mod_version.o: ${B2SRC}/modules/b2mod_version.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_version.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_version.F ${OBJDIR}/b2mod_version.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_version.o ${OBJDIR}/b2mod_version.f
	@touch ${OBJDIR}/b2mod_version.${MOD}

${OBJDIR}/b2mod_xerset.o: ${B2SRC}/utility/b2mod_xerset.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/b2mod_xerset.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_xerset.F ${OBJDIR}/b2mod_xerset.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_xerset.o ${OBJDIR}/b2mod_xerset.f
	@touch ${OBJDIR}/b2mod_xerset.${MOD}

${OBJDIR}/b2us_geo.o: ${B2SRC}/modules/b2us_geo.F ${OBJDIR}/b2mod_switches.${MOD} ${OBJDIR}/b2us_map.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2us_geo.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2us_geo.F ${OBJDIR}/b2us_geo.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2us_geo.o ${OBJDIR}/b2us_geo.f
	@touch ${OBJDIR}/b2us_geo.${MOD}

${OBJDIR}/b2us_map.o: ${B2SRC}/modules/b2us_map.F ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_version.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2us_map.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2us_map.F ${OBJDIR}/b2us_map.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2us_map.o ${OBJDIR}/b2us_map.f
	@touch ${OBJDIR}/b2us_map.${MOD}

${OBJDIR}/b2xbzb.o: ${B2SRC}/b2aux/b2xbzb.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/b2aux/b2xbzb.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2xbzb.F ${OBJDIR}/b2xbzb.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2xbzb.o ${OBJDIR}/b2xbzb.f

${OBJDIR}/b2xvsg.o: ${B2SRC}/b2aux/b2xvsg.F ${OBJDIR}/b2mod_types.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/b2aux/b2xvsg.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2xvsg.F ${OBJDIR}/b2xvsg.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2xvsg.o ${OBJDIR}/b2xvsg.f

${OBJDIR}/cfruin.o: ${B2SRC}/utility/cfruin.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/cfruin.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/cfruin.F ${OBJDIR}/cfruin.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/cfruin.o ${OBJDIR}/cfruin.f

${OBJDIR}/cfrure.o: ${B2SRC}/utility/cfrure.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/cfrure.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/cfrure.F ${OBJDIR}/cfrure.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/cfrure.o ${OBJDIR}/cfrure.f

${OBJDIR}/cfvers.o: ${B2SRC}/utility/cfvers.F ${B2SRC}/include/git_version_B25.h
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/cfvers.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/cfvers.F ${OBJDIR}/cfvers.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/cfvers.o ${OBJDIR}/cfvers.f

${OBJDIR}/cfwuin.o: ${B2SRC}/utility/cfwuin.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/cfwuin.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/cfwuin.F ${OBJDIR}/cfwuin.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/cfwuin.o ${OBJDIR}/cfwuin.f

${OBJDIR}/cfwure.o: ${B2SRC}/utility/cfwure.F ${OBJDIR}/b2mod_ad.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/cfwure.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/cfwure.F ${OBJDIR}/cfwure.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/cfwure.o ${OBJDIR}/cfwure.f

${OBJDIR}/chcase.o: ${B2SRC}/utility/chcase.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/chcase.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/chcase.F ${OBJDIR}/chcase.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/chcase.o ${OBJDIR}/chcase.f

${OBJDIR}/ifill.o: ${B2SRC}/utility/ifill.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/ifill.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/ifill.F ${OBJDIR}/ifill.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/ifill.o ${OBJDIR}/ifill.f

${OBJDIR}/intface.o: ${B2SRC}/utility/intface.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/intface.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/intface.F ${OBJDIR}/intface.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/intface.o ${OBJDIR}/intface.f

${OBJDIR}/intvertex.o: ${B2SRC}/utility/intvertex.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/intvertex.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/intvertex.F ${OBJDIR}/intvertex.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/intvertex.o ${OBJDIR}/intvertex.f

${OBJDIR}/ipgeti.o: ${B2SRC}/utility/ipgeti.F ${OBJDIR}/b2mod_ipmain.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/ipgeti.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/ipgeti.F ${OBJDIR}/ipgeti.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/ipgeti.o ${OBJDIR}/ipgeti.f

${OBJDIR}/machsfr.o: ${B2SRC}/utility/machsfr.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/machsfr.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/machsfr.F ${OBJDIR}/machsfr.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/machsfr.o ${OBJDIR}/machsfr.f

${OBJDIR}/my_out_us.o: ${B2SRC}/utility/my_out_us.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/my_out_us.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/my_out_us.F ${OBJDIR}/my_out_us.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/my_out_us.o ${OBJDIR}/my_out_us.f

${OBJDIR}/nagsubst.o: ${B2SRC}/utility/nagsubst.F ${OBJDIR}/b2mod_subsys.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/nagsubst.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/nagsubst.F ${OBJDIR}/nagsubst.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/nagsubst.o ${OBJDIR}/nagsubst.f

${OBJDIR}/open_file.o: ${B2SRC}/utility/open_file.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/open_file.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/open_file.F ${OBJDIR}/open_file.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/open_file.o ${OBJDIR}/open_file.f

${OBJDIR}/prvrt.o: ${B2SRC}/utility/prvrt.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/prvrt.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/prvrt.F ${OBJDIR}/prvrt.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/prvrt.o ${OBJDIR}/prvrt.f

${OBJDIR}/prvrti.o: ${B2SRC}/utility/prvrti.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/prvrti.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/prvrti.F ${OBJDIR}/prvrti.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/prvrti.o ${OBJDIR}/prvrti.f

${OBJDIR}/sfill.o: ${B2SRC}/utility/sfill.F ${OBJDIR}/b2mod_openmp.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/sfill.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/sfill.F ${OBJDIR}/sfill.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/sfill.o ${OBJDIR}/sfill.f

${OBJDIR}/streql.o: ${B2SRC}/utility/streql.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/streql.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/streql.F ${OBJDIR}/streql.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/streql.o ${OBJDIR}/streql.f

${OBJDIR}/xerrab.o: ${B2SRC}/utility/xerrab.F ${OBJDIR}/b2mod_subsys.${MOD} ${OBJDIR}/b2mod_xerset.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/xerrab.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/xerrab.F ${OBJDIR}/xerrab.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/xerrab.o ${OBJDIR}/xerrab.f

${OBJDIR}/xertst.o: ${B2SRC}/utility/xertst.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/xertst.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/xertst.F ${OBJDIR}/xertst.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/xertst.o ${OBJDIR}/xertst.f

${B2SRC}/include/git_version_B25.h:
	@echo "      character*32 :: git_version_B25 =" > ${B2SRC}/include/git_version_new.h
	@echo "     . '`git describe --dirty --always | cut -c 1-32`'" >> ${B2SRC}/include/git_version_new.h
	@echo "      character*32 :: git_version_ADAS =" >> ${B2SRC}/include/git_version_new.h
	@echo "     . '`( cd $${SOLPSTOP}/modules/adas ; git describe --dirty --always | cut -c 1-32 )`'" >> ${B2SRC}/include/git_version_new.h
	@echo "      character*32 :: git_version_SOLPS =" >> ${B2SRC}/include/git_version_new.h
	@echo "     . '`( cd $${SOLPSTOP} ; git describe --dirty --always | cut -c 1-32 )`'" >> ${B2SRC}/include/git_version_new.h
	@if cmp -s ${B2SRC}/include/git_version_new.h ${B2SRC}/include/git_version_B25.h; then rm ${B2SRC}/include/git_version_new.h; else mv ${B2SRC}/include/git_version_new.h ${B2SRC}/include/git_version_B25.h; fi

ifdef ITM_ENVIRONMENT_LOADED
${OBJDIR}/ipgetr.o: ${B2SRC}/utility/ipgetr.F ${OBJDIR}/b2mod_ipmain.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/ipgetr.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/ipgetr.F ${OBJDIR}/ipgetr.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/ipgetr.o ${OBJDIR}/ipgetr.f

${OBJDIR}/smax.o: ${B2SRC}/utility/smax.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/smax.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/smax.F ${OBJDIR}/smax.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/smax.o ${OBJDIR}/smax.f

${OBJDIR}/smin.o: ${B2SRC}/utility/smin.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/utility/smin.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/smin.F ${OBJDIR}/smin.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/smin.o ${OBJDIR}/smin.f

${OBJDIR}/b2mod_anomalous_transport.${MOD}: ${B2SRC}/modules/b2mod_anomalous_transport.F ${OBJDIR}/b2mod_version.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_anomalous_transport.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_anomalous_transport.F ${OBJDIR}/b2mod_anomalous_transport.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_anomalous_transport.o ${OBJDIR}/b2mod_anomalous_transport.f

${OBJDIR}/b2mod_average.${MOD}: ${B2SRC}/user/b2mod_average.F ${OBJDIR}/b2mod_b2cmpa.${MOD} ${OBJDIR}/b2mod_plasma.${MOD} ${OBJDIR}/b2mod_rates.${MOD} ${OBJDIR}/b2mod_sources.${MOD} ${OBJDIR}/b2mod_subsys.${MOD} ${OBJDIR}/b2mod_version.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/user/b2mod_average.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_average.F ${OBJDIR}/b2mod_average.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_average.o ${OBJDIR}/b2mod_average.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_b2plot_wall_loading.${MOD}: ${B2SRC}/modules/b2mod_b2plot_wall_loading.F ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_b2plot_wall_loading.${MOD}: ${B2SRC}/modules/b2mod_b2plot_wall_loading.F
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2plot_wall_loading.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2plot_wall_loading.F ${OBJDIR}/b2mod_b2plot_wall_loading.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2plot_wall_loading.o ${OBJDIR}/b2mod_b2plot_wall_loading.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_boundary_namelist.${MOD}: ${B2SRC}/modules/b2mod_boundary_namelist.F ${OBJDIR}/b2mod_indirect.${MOD} ${OBJDIR}/b2mod_plasma.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_boundary_namelist.${MOD}: ${B2SRC}/modules/b2mod_boundary_namelist.F ${OBJDIR}/b2mod_indirect.${MOD} ${OBJDIR}/b2mod_plasma.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_boundary_namelist.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_boundary_namelist.F ${OBJDIR}/b2mod_boundary_namelist.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_boundary_namelist.o ${OBJDIR}/b2mod_boundary_namelist.f

${OBJDIR}/b2mod_diag.${MOD}: ${B2SRC}/modules/b2mod_diag.F ${OBJDIR}/b2mod_elements.${MOD} ${OBJDIR}/b2mod_residuals.${MOD} ${OBJDIR}/b2mod_eirene_globals.${MOD} ${OBJDIR}/b2mod_neutr_src_scaling.${MOD} ${OBJDIR}/b2mod_boundary_namelist.${MOD} ${OBJDIR}/b2mod_neutrals_namelist.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_diag.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_diag.F ${OBJDIR}/b2mod_diag.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_diag.o ${OBJDIR}/b2mod_diag.f

${OBJDIR}/b2mod_eirdiag.${MOD}: ${B2SRC}/modules/b2mod_eirdiag.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_eirdiag.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_eirdiag.F ${OBJDIR}/b2mod_eirdiag.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_eirdiag.o ${OBJDIR}/b2mod_eirdiag.f

${OBJDIR}/b2mod_eirene_globals.${MOD}: ${B2SRC}/modules/b2mod_eirene_globals.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_eirene_globals.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_eirene_globals.F ${OBJDIR}/b2mod_eirene_globals.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_eirene_globals.o ${OBJDIR}/b2mod_eirene_globals.f

${OBJDIR}/b2mod_elements.${MOD}: ${B2SRC}/modules/b2mod_elements.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_elements.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_elements.F ${OBJDIR}/b2mod_elements.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_elements.o ${OBJDIR}/b2mod_elements.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_feedback.${MOD}: ${B2SRC}/modules/b2mod_feedback.F ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_feedback.${MOD}: ${B2SRC}/modules/b2mod_feedback.F
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_feedback.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_feedback.F ${OBJDIR}/b2mod_feedback.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_feedback.o ${OBJDIR}/b2mod_feedback.f

${OBJDIR}/b2mod_interp.${MOD}: ${B2SRC}/ids/b2mod_interp.F90
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_interp.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_interp.F90 ${OBJDIR}/b2mod_interp.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_interp.o ${OBJDIR}/b2mod_interp.f90

${OBJDIR}/b2mod_layer.${MOD}: ${B2SRC}/modules/b2mod_layer.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_layer.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_layer.F ${OBJDIR}/b2mod_layer.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_layer.o ${OBJDIR}/b2mod_layer.f

${OBJDIR}/b2mod_mwti.${MOD}: ${B2SRC}/modules/b2mod_mwti.F90 ${OBJDIR}/b2mod_transport.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_mwti.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_mwti.F90 ${OBJDIR}/b2mod_mwti.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_mwti.o ${OBJDIR}/b2mod_mwti.f90

${OBJDIR}/b2mod_b2cmfs.${MOD}: ${B2SRC}/modules/b2mod_b2cmfs.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2cmfs.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2cmfs.F ${OBJDIR}/b2mod_b2cmfs.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2cmfs.o ${OBJDIR}/b2mod_b2cmfs.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_b2cmpa.${MOD}: ${B2SRC}/modules/b2mod_b2cmpa.F ${OBJDIR}/b2mod_constants.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_b2cmpa.${MOD}: ${B2SRC}/modules/b2mod_b2cmpa.F ${OBJDIR}/b2mod_constants.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2cmpa.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2cmpa.F ${OBJDIR}/b2mod_b2cmpa.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2cmpa.o ${OBJDIR}/b2mod_b2cmpa.f

${OBJDIR}/b2mod_b2cmpb.${MOD}: ${B2SRC}/modules/b2mod_b2cmpb.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2cmpb.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2cmpb.F ${OBJDIR}/b2mod_b2cmpb.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2cmpb.o ${OBJDIR}/b2mod_b2cmpb.f

${OBJDIR}/b2mod_b2cmrc.${MOD}: ${B2SRC}/modules/b2mod_b2cmrc.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2cmrc.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2cmrc.F ${OBJDIR}/b2mod_b2cmrc.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2cmrc.o ${OBJDIR}/b2mod_b2cmrc.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_b2plot.${MOD}: ${B2SRC}/modules/b2mod_b2plot.F ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_b2plot.${MOD}: ${B2SRC}/modules/b2mod_b2plot.F
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_b2plot.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_b2plot.F ${OBJDIR}/b2mod_b2plot.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_b2plot.o ${OBJDIR}/b2mod_b2plot.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_balance.${MOD}: ${B2SRC}/modules/b2mod_balance.F ${OBJDIR}/b2mod_rates.${MOD} ${OBJDIR}/b2mod_geo.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_balance.${MOD}: ${B2SRC}/modules/b2mod_balance.F ${OBJDIR}/b2mod_rates.${MOD} ${OBJDIR}/b2mod_geo.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_balance.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_balance.F ${OBJDIR}/b2mod_balance.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_balance.o ${OBJDIR}/b2mod_balance.f

${OBJDIR}/b2mod_external.${MOD}: ${B2SRC}/modules/b2mod_external.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_external.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_external.F ${OBJDIR}/b2mod_external.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_external.o ${OBJDIR}/b2mod_external.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_neutr_src_scaling.${MOD}: ${B2SRC}/modules/b2mod_neutr_src_scaling.F ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_neutr_src_scaling.${MOD}: ${B2SRC}/modules/b2mod_neutr_src_scaling.F
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_neutr_src_scaling.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_neutr_src_scaling.F ${OBJDIR}/b2mod_neutr_src_scaling.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_neutr_src_scaling.o ${OBJDIR}/b2mod_neutr_src_scaling.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_neutrals_namelist.${MOD}: ${B2SRC}/modules/b2mod_neutrals_namelist.F ${OBJDIR}/b2mod_wall.${MOD} ${OBJDIR}/b2mod_eirdiag.${MOD} ${OBJDIR}/b2mod_b2cmfs.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_neutrals_namelist.${MOD}: ${B2SRC}/modules/b2mod_neutrals_namelist.F ${OBJDIR}/b2mod_wall.${MOD} ${OBJDIR}/b2mod_eirdiag.${MOD} ${OBJDIR}/b2mod_b2cmfs.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_neutrals_namelist.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_neutrals_namelist.F ${OBJDIR}/b2mod_neutrals_namelist.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_neutrals_namelist.o ${OBJDIR}/b2mod_neutrals_namelist.f

${OBJDIR}/b2mod_plasma.${MOD}: ${B2SRC}/modules/b2mod_plasma.F ${OBJDIR}/b2mod_b2cmpa.${MOD} ${OBJDIR}/b2mod_work.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_plasma.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_plasma.F ${OBJDIR}/b2mod_plasma.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_plasma.o ${OBJDIR}/b2mod_plasma.f

${OBJDIR}/b2mod_ppout.${MOD}: ${B2SRC}/modules/b2mod_ppout.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_ppout.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ppout.F ${OBJDIR}/b2mod_ppout.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ppout.o ${OBJDIR}/b2mod_ppout.f

${OBJDIR}/b2mod_rates.${MOD}: ${B2SRC}/modules/b2mod_rates.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_rates.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_rates.F ${OBJDIR}/b2mod_rates.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_rates.o ${OBJDIR}/b2mod_rates.f

${OBJDIR}/b2mod_residuals.${MOD}: ${B2SRC}/modules/b2mod_residuals.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_residuals.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_residuals.F ${OBJDIR}/b2mod_residuals.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_residuals.o ${OBJDIR}/b2mod_residuals.f

${OBJDIR}/b2mod_sources.${MOD}: ${B2SRC}/modules/b2mod_sources.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_sources.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_sources.F ${OBJDIR}/b2mod_sources.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_sources.o ${OBJDIR}/b2mod_sources.f

${OBJDIR}/b2mod_tallies.${MOD}: ${B2SRC}/modules/b2mod_tallies.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_tallies.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_tallies.F ${OBJDIR}/b2mod_tallies.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_tallies.o ${OBJDIR}/b2mod_tallies.f

${OBJDIR}/b2mod_transport.${MOD}: ${B2SRC}/modules/b2mod_transport.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_transport.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_transport.F ${OBJDIR}/b2mod_transport.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_transport.o ${OBJDIR}/b2mod_transport.f

${OBJDIR}/b2mod_transport_nspecies.${MOD}: ${B2SRC}/modules/b2mod_transport_nspecies.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_transport_nspecies.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_transport_nspecies.F ${OBJDIR}/b2mod_transport_nspecies.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_transport_nspecies.o ${OBJDIR}/b2mod_transport_nspecies.f

${OBJDIR}/b2mod_ual.${MOD}: ${B2SRC}/ids/b2mod_ual.F90 ${OBJDIR}/b2mod_ual_io.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_ual.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ual.F90 ${OBJDIR}/b2mod_ual.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ual.o ${OBJDIR}/b2mod_ual.f90

${OBJDIR}/b2mod_ual_io.${MOD}: ${B2SRC}/ids/b2mod_ual_io.F90 ${OBJDIR}/b2mod_average.${MOD} ${OBJDIR}/b2mod_b2cmpa.${MOD} ${OBJDIR}/b2mod_b2cmpb.${MOD} ${OBJDIR}/b2mod_b2plot_wall_loading.${MOD} ${OBJDIR}/b2mod_feedback.${MOD} ${OBJDIR}/b2mod_geo.${MOD} ${OBJDIR}/b2mod_diag.${MOD} ${OBJDIR}/b2mod_plasma.${MOD} ${OBJDIR}/b2mod_sources.${MOD} ${OBJDIR}/b2mod_transport.${MOD} ${OBJDIR}/b2mod_transport_nspecies.${MOD} ${OBJDIR}/b2mod_anomalous_transport.${MOD} ${OBJDIR}/b2mod_interp.${MOD} ${OBJDIR}/b2mod_b2cmrc.${MOD} ${OBJDIR}/b2mod_mwti.${MOD} ${OBJDIR}/b2mod_version.${MOD} ${OBJDIR}/b2mod_ual_io_data.${MOD} ${OBJDIR}/b2mod_ual_io_grid.${MOD} ${OBJDIR}/b2mod_rates.${MOD} ${OBJDIR}/b2mod_user_namelist.${MOD} ${OBJDIR}/b2mod_external.${MOD} ${OBJDIR}/b2mod_balance.${MOD} ${OBJDIR}/b2mod_b2plot.${MOD} ${OBJDIR}/b2mod_ipmain.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_ual_io.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ual_io.F90 ${OBJDIR}/b2mod_ual_io.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ual_io.o ${OBJDIR}/b2mod_ual_io.f90

${OBJDIR}/b2mod_ual_io_data.${MOD}: ${B2SRC}/ids/b2mod_ual_io_data.F90 ${OBJDIR}/b2mod_ual_io_grid.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_ual_io_data.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ual_io_data.F90 ${OBJDIR}/b2mod_ual_io_data.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ual_io_data.o ${OBJDIR}/b2mod_ual_io_data.f90

${OBJDIR}/b2mod_ual_io_grid.${MOD}: ${B2SRC}/ids/b2mod_ual_io_grid.F90 ${OBJDIR}/b2mod_ppout.${MOD} ${OBJDIR}/b2mod_user_namelist.${MOD}
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/ids/b2mod_ual_io_grid.F90 ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_ual_io_grid.F90 ${OBJDIR}/b2mod_ual_io_grid.f90
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_ual_io_grid.o ${OBJDIR}/b2mod_ual_io_grid.f90

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_user_namelist.${MOD}: ${B2SRC}/modules/b2mod_user_namelist.F ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_user_namelist.${MOD}: ${B2SRC}/modules/b2mod_user_namelist.F
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_user_namelist.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_user_namelist.F ${OBJDIR}/b2mod_user_namelist.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_user_namelist.o ${OBJDIR}/b2mod_user_namelist.f

ifeq (${USE_DIMENSIONS},1)
${OBJDIR}/b2mod_wall.${MOD}: ${B2SRC}/modules/b2mod_wall.F ${OBJDIR}/b2mod_layer.${MOD} ${OBJDIR}/b2mod_tallies.${MOD} ${OBJDIR}/b2mod_subsys.${MOD} ${OBJDIR}/b2mod_dimensions.${MOD}
else
${OBJDIR}/b2mod_wall.${MOD}: ${B2SRC}/modules/b2mod_wall.F ${OBJDIR}/b2mod_layer.${MOD} ${OBJDIR}/b2mod_tallies.${MOD} ${OBJDIR}/b2mod_subsys.${MOD}
endif
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_wall.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_wall.F ${OBJDIR}/b2mod_wall.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_wall.o ${OBJDIR}/b2mod_wall.f

${OBJDIR}/b2mod_work.${MOD}: ${B2SRC}/modules/b2mod_work.F
	@mkdir -p ${SRCDIR}/b25_links/
	ln -sf ${B2SRC}/modules/b2mod_work.F ${SRCDIR}/b25_links/
	${CPP} ${DEFINES} -P -C ${INCLUDE} ${B2INCLUDE} ${SRCDIR}/b25_links/b2mod_work.F ${OBJDIR}/b2mod_work.f
	$(COMPILE) $(INCLUDE) $(B2INCLUDE) -o ${OBJDIR}/b2mod_work.o ${OBJDIR}/b2mod_work.f
endif
endif

include ${OBJDIR}/dependencies.${COMPILER}

echo:
	@echo INCLUDE=${INCLUDE}
	@echo DEFINES=${DEFINES}
	@echo FFLAGSEXTRA=${FFLAGSEXTRA}
	@echo VPATH=${VPATH}
	@echo SOLPS_LIB=${SOLPS_LIB}
	@echo GOBJS=${GOBJS}
	@echo DEST=${DEST}
	@echo OBJS=$(OBJS)
	@echo OBJSL90=$(OBJSL90)
	@echo OBJSU90=$(OBJSU90)
