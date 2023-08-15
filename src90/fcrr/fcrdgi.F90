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
      parameter (nnms=16)
      integer(Short) :: i, j, lutrg
      real(Single) x(3), xarray(3,nxptm), rdummy, rarray(3,nmstr)
      character*8 name, unm(nnms)
      logical carre_streql
      external carre_streql, locase, rearre, rearri, resimi, resime, skipit
!
!*** List of the valid input keywords
      data unm / &
     &    'xptcntr ','xlpcntr ','trg_spcf','tgtgrd  ','lm_cnfg ', &
     &    'lm_pntrt','lm_grcln','crr_mode','grd_mode','equ_mode', &
     &    'trgt_res','vess_elm','p1      ','p2      ','fclbl   ', &
     &    'pasmin  ' /
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
          write(*,*) '*** fcrdgi: too few values for xptcntr'
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
          write(*,*) '*** fcrdgi: too few values for xlpcntr'
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
        call resime(rdummy)
        lm_pntrt = real(rdummy,rKind)
      case (7)
!***  lm_grcln
        call resime(rdummy)
        lm_grcln = real(rdummy,rKind)
      case (8)
!***  crr_mode
        call resimi(carre_mode)
      case (9)
!***  grd_mode
        call resimi(grid_ext_mode)
      case (10)
!***  equ_mode
        call resimi(equ_ext_mode)
      case (11)
!***  trgt_res
        call resime(rdummy)
        target_res = real(rdummy,rKind)
      case (12)
!***  vess_elm
        call rearri(vess_elm,nmstr,i)
        nvess = i
      case (13)
!***  p1
        call rearre(rarray,3*nmstr,i)
        npp=max(i/3_Short,1_Short)
        do j=1,npp
          do i=1,3
            p1(i,j)=0.001_rKind*real(rarray(i,j),rKind)
          end do
        end do
      case (14)
!***  p2
        call rearre(rarray,3*nmstr,i)
        j=max(i/3_Short,1_Short)
        if (j.ne.npp) then
          write(*,*) '*** fcrdgi: inconsistent p1 and p2'
          stop ' ==> Corrupted DG output files?'
        endif
        do j=1,npp
          do i=1,3
            p2(i,j)=0.001_rKind*real(rarray(i,j),rKind)
          end do
        end do
      case (15)
!***  fclbl
        call rearri(fclbl,nmstr,i)
      case (16)
!***  pasmin
        call resime(rdummy)
        pasmin = real(rdummy,rKind)
      case default
        call skipit
      end select
      return
!======================================================================
      end
