      subroutine rdfrre(iunit,vari,rr,ierror)
c  version : 03.07.2000 23:01
      implicit none
#include <CARRETYPES.F>
c  read a real number from a chain of characters vari
c
c  arguments
      integer iunit,ierror
      real(kind=R8) rr
      character vari*(*)
c  iunit: unit of temporary file
c  vari: chain of characters from which to read
c  rr: real number read from vari (output)
c  ierror: error flag: 0 for no error, 1 when there is an error.
c
c  calculations
c      rewind iunit
c      write(iunit,100)vari
c100   format(a)
c      rewind iunit
c      read(iunit,*,err=99)rr
#ifdef READOPT
      read(vari,err=99) rr
#else
#ifdef GFORTRAN
      read(vari,*,err=99) rr
#else
#ifdef NAGFOR
      read(vari,*,err=99) rr
#else
      read(vari,'(e)',err=99) rr
#endif
#endif
#endif
      ierror=0
      return
99    ierror=1
      return
      end
