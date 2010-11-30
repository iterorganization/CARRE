      subroutine fcrdgi(nam)
!
!  version : 27.07.99 23:28
!
!======================================================================
!*** Input of dg data for Carre
!======================================================================
      implicit none
#include <FCRCOM.F>
      character*(8) nam
!
      integer nnms
      parameter (nnms=4)
      integer i, j, lutrg
      character*8 unm(nnms)
!
!*** List of the valid input keywords
      data unm / & 
     &    'xptcntr ','xlpcntr ','trg_spcf','tgtgrd  '/
      data lutrg / 0 /
!======================================================================
!
      do i=1,nnms
          if(nam.eq.unm(i)) go to (10,20,30,40),i
      end do
      call skipit
      return
!-----------------------------------------------------------------------
!*** xptcntr
 10   call rearre(xptcntr,3*nxptm,i)
      nxpt=0
      if(i.lt.2) then
        write(*,*) '*** fcrtrn: too few values for xptcntr'
        xptcntr(1,1)=-1.
      else
       nxpt=max(i/3,1)
      do j=1,nxpt
        do i=1,3
            xptcntr(i,j)=0.001*xptcntr(i,j)
        end do
      end do
      end if
      return
!*** xlpcntr
 20   call rearre(xlpcntr,3,i)
      if(i.lt.2) then
        write(*,*) '*** fcrtrn: too few values for xlpcntr'
        xlpcntr(1)=-1.
      else
        xlpcntr(1)=0.001*xlpcntr(1)
        xlpcntr(2)=0.001*xlpcntr(2)
      end if
      return
!*** trg_spcf
 30   call resimi(lutrg)
      ntrg=max0(ntrg,lutrg)
      return
!*** tgtgrd
 40   if(lutrg.le.0 .or. lutrg.gt.ntrgx) then !{
        write(*,*) 'Target index outside limits: ',lutrg
        stop ' ==> Corrupted DG output files?'
      else !}{
        call resime(tgarde(lutrg))
      end if !}
      return
!======================================================================
      end
