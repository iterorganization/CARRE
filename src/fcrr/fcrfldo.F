      subroutine fcrfldo(ofile)
c
c  version : 22.04.97 18:06
c
c======================================================================
c*** Create the toroidal field file for Carre
c======================================================================
      use carre_types
      use fcrcom
      implicit none
      character ofile*(*)
c======================================================================
c
      open(2,file=ofile)
      rewind(2)
      write(2,*) rbtor
      close(2)
c======================================================================
      end
