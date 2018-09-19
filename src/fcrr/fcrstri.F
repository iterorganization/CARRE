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
      real(kind=R4) x, y
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
          read(lun,*,err=900) x,y
          xstr(l)=1.e-3_R8*real(x,R8)
          ystr(l)=1.e-3_R8*real(y,R8)
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
