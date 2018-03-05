      subroutine fcrstro(chcstr)
c
c  version : 05.02.99 13:37
c
c======================================================================
c*** Create the structure file for Carre
c======================================================================
      implicit none
      character chcstr*(*)
#include <CARRETYPES.F>
#include <FCRCOM.F>
      integer i,j,k,l
c======================================================================
c
      open(2,file=chcstr)
      rewind(2)
      write(2,*) nstr
      write(2,'(a)') '$structures'
      k=0
      do j=1,nstr
        write(2,'(a,i4)') 'Structure ',j
        if(j.eq.1) then
c*** This is for structures in "Sonnet" format:
c*** the first one contains the closing box which should be removed
          do i=2,lstr(j)
            if(xstr(k+i).eq.xstr(k+1).and.ystr(k+i).eq.ystr(k+1)) then
              l=i-1
              go to 10
            end if
          end do
        end if
        l=lstr(j)
 10     if(j.le.nclstr) then
          write(2,*) l+1
        else
          write(2,*) -l
        end if
        do i=1,l
          write(2,*) xstr(k+i),ystr(k+i)
        end do
        if(j.le.nclstr) then
          write(2,*) xstr(k+1),ystr(k+1)
        end if
        k=k+lstr(j)
      end do
      write(2,'(a)') '$end'
      close(2)
c======================================================================
      end
