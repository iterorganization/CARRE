#! /bin/tcsh -f

if($1 == "") then
  if (-e setup.csh.OBJECTCODE) source setup.csh.OBJECTCODE
else
  setenv OBJECTCODE $1
endif
if(! $?OBJECTCODE) then
  echo "OBJECTCODE not defined !"
endif

setenv USE_ITMCARRE '-DUSE_ITMCARRE'
setenv SOLPSTOP '/scratch//hmk/svn/solps5.0/'
setenv SOLPS_LIB "$SOLPSTOP/src/lib/$OBJECTCODE"

#setenv SOLPS_LIB '/efda1/s1/pfs/scratch/imp3/klingshi/solps5.0/src/lib/'$OBJECTCODE
#setenv SOLPS_LIB '/home/hajo/svn/solps5.0/src/lib/'$OBJECTCODE
#setenv NCARG_ROOT "${SOLPSTOP}/../solps_other/NCARG/linux.ifort64"
#setenv NCARG '-L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c -L/usr/X11R6/lib64 -lX11 -lm'

setenv CARRE_DEBUG yes


# "normal" Carre

setenv USE_NCARG '-DUSE_NCARG'
setenv USE_SILO '-DUSE_SILO'

#unsetenv USE_SILO
unsetenv USE_NCARG


# ITM Carre: Override above settings for ITM Carre

setenv USE_ITMCARRE "-DUSE_ITMCARRE -DEUITM"
setenv USE_UAL -DUSE_UAL

unsetenv USE_ITMCARRE
unsetenv USE_UAL

setenv NOUSE_MSCL True


# Interactive Carre

setenv CARRE_NONINTERACTIVE "-DCARRE_NONINTERACTIVE"
#unsetenv CARRE_NONINTERACTIVE

