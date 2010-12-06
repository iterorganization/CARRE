      subroutine rdfrin(iunit,vari,ii,ierror)
!  version : 03.07.2000 22:59
      implicit none
!  read a integer number from a chain of characters vari
!
!  arguments
      integer iunit,ii,ierror
      character vari*(*)
!  iunit: unit of temporary file
!  vari: chain of characters from which to read
!  ii: integer read from vari (output)
!  ierror: error flag: 0 for no error, 1 when there is an error.
!
!  calculations
!      rewind iunit
!      write(iunit,100)vari
!100   format(a)
!      rewind iunit
!      read(iunit,*,err=99)ii
      write (*,*) 'rdrfin: vari', vari
#ifdef READOPT
      read(vari,err=99)ii
#else
!      read(vari,'(i10)',err=99)ii
      read(vari,*,err=99)ii
#endif
      ierror=0
      return
99    ierror=1
      return
      end
