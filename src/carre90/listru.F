      subroutine listru(iunit,nstr,npstru,nomstr,xstruc,ystruc,npstmx, & 
     &  nstrmx)
!
!  version : 05.04.97 16:19
!
!======================================================================
      implicit none
!  read data concerning structures
!
!  arguments

      integer nstrmx,npstmx,nstr,npstru(nstrmx),iunit
      real*8 xstruc(npstmx,nstrmx),ystruc(npstmx,nstrmx)
      character nomstr(nstrmx)*80
!
!  iunit: unit of file containing structure data
!  nstr: number of structures
!  npstru(i): number of points in structure i
!             a positive (negative) number indicates a closed (open)
!             structure. When this number is zero, the code counts
!             the number of points and the structure must be closed.
!             When -1, the code counts the number of points and the
!             structure is understood to be closed.
!  nomstr(i): name of structure i
!  xstruc(j,i): j-th x coordinate of structure i
!  ystruc(j,i): j-th y coordinate of structure i
!  nstrmx: maximum number of structures allowed
!  npstmx: maximum number of coordinates per structure
!
!  local variables
      integer i,ifail
      character line*80
!
!  procedures
      external entete
!======================================================================
!  calculation
!
      ifail=1
      call entete(iunit,'$structures',ifail)
      if(ifail.eq.1) then
        write(6,*)'unable to find structure header in file: stop'
        stop
      endif
 1    continue
      read(iunit,100,end=99)line
100   format(a)
      if(index(line,'$fin')+index(line,'$end').eq.0) then
        nstr=nstr+1
        nomstr(nstr)=line
!       call rdfrin(11,line,npstru(nstr),ifail)
        read(iunit,*)npstru(nstr)
        if(abs(npstru(nstr)).le.1) then
          i=0
 3        continue
          i=i+1
          read(iunit,*,err=5)xstruc(i,nstr),ystruc(i,nstr)
          go to 3
 5        continue
          if(npstru(nstr).eq.-1) then
            npstru(nstr)=-(i-1)
          else
            npstru(nstr)=i-1
          endif
          backspace iunit
        else
          do i=1,abs(npstru(nstr))
            read(iunit,*,end=98)xstruc(i,nstr),ystruc(i,nstr)
          enddo
        endif
!
!  structure names containing $n are suppressed
        if(index(nomstr(nstr),'$n').gt.0) nstr=nstr-1
!
        go to 1
      endif
 99   continue
!
!  close input unit
      close(unit=iunit)
      return
!
 98   continue
      write(6,*)'error in structure file'
      stop
      end
