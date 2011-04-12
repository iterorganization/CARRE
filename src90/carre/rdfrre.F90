      subroutine rdfrre(iunit,vari,rr,ierror)
!  version : 03.07.2000 23:01
      implicit none
!  read a real number from a chain of characters vari
!
!  arguments
      integer iunit,ierror
      real*8 rr
      character vari*(*)
!  iunit: unit of temporary file
!  vari: chain of characters from which to read
!  rr: real number read from vari (output)
!  ierror: error flag: 0 for no error, 1 when there is an error.
!
!  calculations
!      rewind iunit
!      write(iunit,100)vari
!100   format(a)
!      rewind iunit
!      read(iunit,*,err=99)rr
!      write (*,*) 'rdrfre: vari', vari
#ifdef READOPT
      read(vari,err=99) rr
#else
!      read(vari,'(e20.8)',err=99) rr
      read(vari,*,err=99) rr
#endif
      ierror=0
      return
99    ierror=1
      return
      end
