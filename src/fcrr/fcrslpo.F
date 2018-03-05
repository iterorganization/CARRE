      subroutine fcrslpo(ofile)
c
c  version : 08.02.99 13:23
c
c======================================================================
c*** Write the coordinates of the x- and o-points for Carre
c*** At present, always a single-null configuration
c======================================================================
      implicit none
      integer i
      character ofile*(*)
#include <CARRETYPES.F>
#include <FCRCOM.F>
c======================================================================
c
      if(xptcntr(1,1).ge.0. .and. xlpcntr(1).ge.0.) then
        open(2,file=ofile)
        rewind(2)
        write(2,*) xpttol
        write(2,*) nxpt
        do i=1,nxpt
          write(2,*) xptcntr(1,i),xptcntr(2,i)
        end do
        write(2,*) xlpcntr(1),xlpcntr(2)
        if(nxpt.eq.2) then
          if(nrgn.eq.6) then
            write(2,*) 0
          else if(nrgn.eq.5) then
            write(2,*) 1
          end if
        end if
        close(2)
      end if
c======================================================================
      end
