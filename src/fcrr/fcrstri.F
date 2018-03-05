      subroutine fcrstri(lun)
c
c  version : 27.04.97 16:36
c
c======================================================================
c*** Read the structure file from dg
c======================================================================
      implicit none
      integer lun
#include <CARRETYPES.F>
#include <FCRCOM.F>
      integer i,j,k,l,n
c======================================================================
c
      rewind(lun)
      read(lun,*) n
      nstr=0
      l=0
      do i=1,n
        read(lun,*,err=900) k
        do j=1,k
          l=l+1
          read(lun,*,err=900) xstr(l),ystr(l)
          xstr(l)=1.e-3*xstr(l)
          ystr(l)=1.e-3*ystr(l)
        end do
        nstr=nstr+1
        lstr(nstr)=k
      end do
      return
c======================================================================
 900  write(*,*) 'fcrstri: error in the structure',i
      stop
c======================================================================
      end
