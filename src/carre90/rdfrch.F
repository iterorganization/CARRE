      subroutine rdfrch(iunit,vari,ch,ierror)
!  version : 31.10.2007 12:43
      implicit none
!  read a character string from a chain of characters vari
!
!  arguments
      integer iunit,ierror
      character vari*(*), ch*(*)
!  local variables
      integer i, j, l
!  iunit: unit of temporary file
!  vari: chain of characters from which to read
!  ch: string read from vari (output)
!  ierror: error flag: 0 for no error, 1 when there is an error.
!
!  calculations
!      rewind iunit
!      write(iunit,100)vari
!100   format(a)
!      rewind iunit
!      read(iunit,*,err=99)rr
      l = len(vari)
      i = index(vari,'''')
      if (i.gt.0) then
        j = index(vari(i+1:l),'''')+i
      else
        i = index(vari,'"')
        if (i.gt.0) j = index(vari(i+1:l),'"')+i
      endif
      if (i.eq.0) j=l+1
#ifdef READOPT
      read(vari(i+1:j-1),err=99) ch
#else
      read(vari(i+1:j-1),'(a)',err=99) ch
#endif
      ierror=0
      return
99    ierror=1
      return
      end
