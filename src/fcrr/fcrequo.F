      subroutine fcrequo(ofile)
c
c  version : 29.04.97 22:01
c
c======================================================================
c*** Create the equilibrium file for Carre
c======================================================================
      implicit none
      character ofile*(*)
#include <CARRETYPES.F>
#include <FCRCOM.F>
      integer i
c======================================================================
c
      open(2,file=ofile)
      call wreqvr(2,ngpr,i,nr,nz,rgr,zgr,pfm)
      if(i.ne.0) then
        write(*,*) 'fcrequo: error writing the Carre equilibrium file'
        stop
      end if
      close(2)
c======================================================================
      end
