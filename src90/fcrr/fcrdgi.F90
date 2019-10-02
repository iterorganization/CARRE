      subroutine fcrdgi(nam)
!
!  version : 27.07.99 23:28
!
!======================================================================
!*** Input of dg data for Carre
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      character*(8) nam
!
      integer nnms
      parameter (nnms=4)
      integer(Short) :: i, j, lutrg
      character*8 unm(nnms)
!
!*** List of the valid input keywords
      data unm / & 
     &    'xptcntr ','xlpcntr ','trg_spcf','tgtgrd  '/
      data lutrg / 0 /
!======================================================================
!
      do i=1,nnms
          if(nam.eq.unm(i)) exit
      end do
      select case (i)
!-----------------------------------------------------------------------
!*** xptcntr
      case (1)
        call rearre(xptcntr,3*nxptm,i)
        nxpt=0
        if(i.lt.2) then
          write(*,*) '*** fcrtrn: too few values for xptcntr'
          xptcntr(1,1)=-1.
        else
         nxpt=max(i/3_Short,1_Short)
         do j=1,nxpt
          do i=1,3
            xptcntr(i,j)=0.001*xptcntr(i,j)
          end do
         end do
        end if
!*** xlpcntr
      case (2)
        call rearre(xlpcntr,3,i)
        if(i.lt.2) then
          write(*,*) '*** fcrtrn: too few values for xlpcntr'
          xlpcntr(1)=-1.
        else
          xlpcntr(1)=0.001*xlpcntr(1)
          xlpcntr(2)=0.001*xlpcntr(2)
        end if
!*** trg_spcf
      case (3)
        call resimi(lutrg)
        ntrg=max(ntrg,lutrg)
!*** tgtgrd
      case(4)
        if(lutrg.le.0 .or. lutrg.gt.ntrgx) then !{
          write(*,*) 'Target index outside limits: ',lutrg
          stop ' ==> Corrupted DG output files?'
        else !}{
          call resime(tgarde(lutrg))
        end if !}
      case default
        call skipit
      end select
      return
!======================================================================
      end
