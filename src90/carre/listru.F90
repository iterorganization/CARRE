      subroutine listru(iunit,struct)

        use carre_types
!
!  version : 05.04.97 16:19
!
!======================================================================
      implicit none
!  read data concerning structures
!
!  arguments
      type(CarreStructures), intent(out) :: struct

      integer iunit
!!$      real*8 xstruc(npstmx,nstrmx),ystruc(npstmx,nstrmx)
!!$      character nomstr(nstrmx)*80
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
      struct%nstruc=0
 1    continue
      read(iunit,100,end=99)line
100   format(a)
      if(index(line,'$fin')+index(line,'$end').eq.0) then
        struct%nstruc=struct%nstruc+1
        struct%nomstr(struct%nstruc)=line
!       call rdfrin(line,npstru(struct%nstruc),ifail)
        read(iunit,*)struct%npstru(struct%nstruc)
        if(abs(struct%npstru(struct%nstruc)).le.1) then
          i=0
 3        continue
          i=i+1
          read(iunit,*,err=5)struct%xstruc(i,struct%nstruc),struct%ystruc(i,struct%nstruc)
          go to 3
 5        continue
          if(struct%npstru(struct%nstruc).eq.-1) then
            struct%npstru(struct%nstruc)=-(i-1)
          else
            struct%npstru(struct%nstruc)=i-1
          endif
          backspace iunit
        else
          do i=1,abs(struct%npstru(struct%nstruc))
            read(iunit,*,end=98)struct%xstruc(i,struct%nstruc),struct%ystruc(i,struct%nstruc)
          enddo
        endif
!
!  structure names containing $n are suppressed
        if(index(struct%nomstr(struct%nstruc),'$n').gt.0) struct%nstruc=struct%nstruc-1
!
        go to 1
      endif
 99   continue
!
!  close input unit
      close(unit=iunit)

      ! compute closed flag...
      struct%closed = (struct%npstru > 0)
      ! ...and change sign of npstru to positive for all structures
      struct%npstru = abs(struct%npstru)

      return
!
 98   continue
      write(6,*)'error in structure file'
      stop
      end
