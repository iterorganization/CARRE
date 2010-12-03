#! /bin/tcsh -f

if($1 == "") then
  if (-e setup.csh.OBJECTCODE) source setup.csh.OBJECTCODE
else
  setenv OBJECTCODE $1
endif
if(! $?OBJECTCODE) then
  echo "OBJECTCODE not defined !"
endif

setenv USE_NCARG '-DUSE_NCARG'
setenv USE_SILO '-DUSE_SILO'

setenv SOLPS_LIB '/home/hajo/workspace-local/svn/solps5.0-trunk/src/lib/$(OBJECTCODE)/'
setenv NCARG_ROOT '/home/hajo/workspace-local/solps5.0/src/NCARG/linux.ifort64'
setenv NCARG '-L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c -L/usr/X11R6/lib64 -lX11 -lm'
