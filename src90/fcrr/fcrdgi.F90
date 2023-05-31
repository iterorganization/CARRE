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
      parameter (nnms=7)
      integer(Short) :: i, j, lutrg
      real(Single) x(3), xarray(3,nxptm), rdummy
      character*8 name, unm(nnms)
      logical carre_streql
      external carre_streql, rearre, resimi, resime, skipit
!
!*** List of the valid input keywords
      data unm / &
     &    'xptcntr ','xlpcntr ','trg_spcf','tgtgrd  ','lm_cnfg ', &
     &    'lm_pntrt','lm_grcln'/
      data lutrg / 0 /
!======================================================================
!
      call locase(nam,name,8)
      do i=1,nnms
        if(carre_streql(name,unm(i))) exit
      end do
      select case (i)
!-----------------------------------------------------------------------
!*** xptcntr
      case (1)
        call rearre(xarray,3*nxptm,i)
        nxpt=0
        if(i.lt.2) then
          write(*,*) '*** fcrtrn: too few values for xptcntr'
          xptcntr(1,1)=-1.0_rKind
        else
         nxpt=max(i/3_Short,1_Short)
         do j=1,nxpt
          do i=1,3
            xptcntr(i,j)=0.001_rKind*real(xarray(i,j),rKind)
          end do
         end do
        end if
!*** xlpcntr
      case (2)
        call rearre(x,3,i)
        if(i.lt.2) then
          write(*,*) '*** fcrtrn: too few values for xlpcntr'
          xlpcntr(1)=-1.0_rKind
        else
          xlpcntr(1)=0.001_rKind*real(x(1),rKind)
          xlpcntr(2)=0.001_rKind*real(x(2),rKind)
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
          call resime(rdummy)
          tgarde(lutrg) = real(rdummy,rKind)
        end if !}
      case (5)
!***  lm_cnfg
        call resimi(lm_cnfg)
      case (6)
!***  lm_pntrt
        call resime(lm_pntrt)
      case (7)
!***  lm_grcln
        call resime(lm_grcln)
      case default
        call skipit
      end select
      return
!======================================================================
      end
