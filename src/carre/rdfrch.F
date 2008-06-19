      subroutine rdfrch(iunit,vari,ch,ierror)
c  version : 31.10.2007 12:43
      implicit none
c  read a character string from a chain of characters vari
c
c  arguments
      integer iunit,ierror
      character vari*(*), ch*(*)
c  local variables
      integer i, j, l
c  iunit: unit of temporary file
c  vari: chain of characters from which to read
c  ch: string read from vari (output)
c  ierror: error flag: 0 for no error, 1 when there is an error.
c
c  calculations
c      rewind iunit
c      write(iunit,100)vari
c100   format(a)
c      rewind iunit
c      read(iunit,*,err=99)rr
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
