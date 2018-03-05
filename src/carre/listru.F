      subroutine listru(iunit,nstr,npstru,nomstr,xstruc,ystruc,npstmx,
     .  nstrmx)
c
c  version : 05.04.97 16:19
c
c======================================================================
      implicit none
#include <CARRETYPES.F>
#include <COMLAN.F>
c  read data concerning structures
c
c  arguments

      integer nstrmx,npstmx,nstr,npstru(nstrmx),iunit
      real(kind=R8) xstruc(npstmx,nstrmx),ystruc(npstmx,nstrmx)
      character nomstr(nstrmx)*80
c
c  iunit: unit of file containing structure data
c  nstr: number of structures
c  npstru(i): number of points in structure i
c             a positive (negative) number indicates a closed (open)
c             structure. When this number is zero, the code counts
c             the number of points and the structure must be closed.
c             When -1, the code counts the number of points and the
c             structure is understood to be closed.
c  nomstr(i): name of structure i
c  xstruc(j,i): j-th x coordinate of structure i
c  ystruc(j,i): j-th y coordinate of structure i
c  nstrmx: maximum number of structures allowed
c  npstmx: maximum number of coordinates per structure
c
c  local variables
      integer i,ifail
      character line*80
c
c  procedures
      external entete
c======================================================================
c  calculation
c
      ifail=1
      call entete(iunit,'$structures',ifail)
      if(ifail.eq.1) then
        if(sellan(1:8).eq.'francais') then
          write(6,*)'Incapable de trouver l''entete structure '//
     &     'dans le fichier: stop'
        elseif(sellan(1:7).eq.'english') then
          write(6,*)'unable to find structure header in file: stop'
        endif
        stop
      endif
 1    continue
      read(iunit,100,end=99)line
100   format(a)
      if(index(line,'$fin')+index(line,'$end').eq.0) then
        nstr=nstr+1
        nomstr(nstr)=line
c       call rdfrin(11,line,npstru(nstr),ifail)
        read(iunit,*)npstru(nstr)
        if(abs(npstru(nstr)).gt.npstmx) then
          if(sellan(1:8).eq.'francais') then
            write(6,'(a,i2,a)') 
     .       'Trop de points pour la structure ',nstr,' !'
            write(6,'(a,i6)') 'Nombre de points declares : ',
     .       abs(npstru(nstr))
            write(6,'(a,i4)') 'Nombre maximum de points prevu : ',
     .       npstmx
            write(6,'(a)') 'Veuillez augmenter la valeur de npstmx '//
     &       'dans CARREDIM.F et recompiler !'
          elseif(sellan(1:7).eq.'english') then
            write(6,'(a,i2,a)') 
     .       'Too many points in structure ',nstr,' !'
            write(6,'(a,i6)') 'Number of points declared : ',
     .       abs(npstru(nstr))
            write(6,'(a,i4)') 'Maximum number of points allowed : ',
     .       npstmx
            write(6,'(a)') 'Please increase the value of npstmx '//
     &       'in CARREDIM.F and recompile !'
          endif
          stop
        else if(abs(npstru(nstr)).le.1) then
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
c
c  structure names containing $n are suppressed
        if(index(nomstr(nstr),'$n').gt.0) nstr=nstr-1
c
        go to 1
      endif
 99   continue
c
c  close input unit
      close(unit=iunit)
      return
c
 98   continue
      if(sellan(1:8).eq.'francais') then
        write(6,*)'erreur dans le fichier structure'
      elseif(sellan(1:7).eq.'english') then
        write(6,*)'error in structure file'
      endif
      stop
      end
