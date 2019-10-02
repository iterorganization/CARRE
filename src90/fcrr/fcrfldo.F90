      subroutine fcrfldo(ofile)
!
!  version : 22.04.97 18:06
!
!======================================================================
!*** Create the toroidal field file for Carre
!======================================================================
      use KindDefinitions
      implicit none
      character ofile*(*)
#include <FCRCOM.F>
!======================================================================
!
      open(2,file=ofile)
      rewind(2)
      write(2,*) rbtor
      close(2)
!======================================================================
      end
