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

#setenv SOLPS_LIB '/afs/efda-itm.eu/user/k/klingshi/SCRATCH/solps5.0/src/lib/linux_pgf90_GW/'
setenv SOLPS_LIB '/home/hajo/svn/solps5.0/src/lib/linux.ifort64/'
setenv NCARG_ROOT '/home/hajo/workspace-local/solps5.0/src/NCARG/linux.ifort64'
setenv NCARG '-L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c -L/usr/X11R6/lib64 -lX11 -lm'

setenv CARRE_DEBUG yes


# "normal" Carre

setenv USE_NCARG '-DUSE_NCARG'
setenv USE_SILO '-DUSE_SILO'

#unsetenv USE_SILO
unsetenv USE_NCARG


# ITM Carre: Override above settings for ITM Carre

#setenv USE_ITMCARRE "-DUSE_ITMCARRE -DEUITM"
#setenv USE_UAL -DUSE_UAL

unsetenv USE_ITMCARRE
unsetenv USE_UAL
