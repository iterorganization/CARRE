      subroutine fcrequo(ofile)
!
!  version : 29.04.97 22:01
!
!======================================================================
!*** Create the equilibrium file for Carre
!======================================================================
      implicit none
      character ofile*(*)
#include <FCRCOM.F>
      integer i
!======================================================================
!
      open(2,file=ofile)
      call wreqvr(2,ngpr,i,nr,nz,rgr,zgr,pfm)
      if(i.ne.0) then
        write(*,*) 'fcrequo: error writing the Carre equilibrium file'
        stop
      end if
      close(2)
!======================================================================
      end
