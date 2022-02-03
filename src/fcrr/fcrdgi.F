      subroutine fcrdgi(nam)
c
c  version : 27.07.99 23:28
c
c======================================================================
c*** Input of dg data for Carre
c======================================================================
      implicit none
#include <CARRETYPES.F>
#include <FCRCOM.F>
      character*(*) nam
c
      integer nnms
      parameter (nnms=7)
      integer i, j, lutrg
      real(kind=R4) x(3), xarray(3,nxptm), rdummy
      character*8 name, unm(nnms)
      logical streql
      external streql,rearre,resime,skipit
c
c*** List of the valid input keywords
      data unm /
     1    'xptcntr ','xlpcntr ','trg_spcf','tgtgrd  ','lm_cnfg ',
     2    'lm_pntrt','lm_grcln'/
      data lutrg / 0 /
c======================================================================
c
      call locase(nam,name,8)
      do i=1,nnms
        if(streql(name,unm(i))) exit
      end do
c-----------------------------------------------------------------------
      select case (i)
      case (1)
c*** xptcntr
        call rearre(xarray,3*nxptm,i)
        nxpt=0
        if(i.lt.2) then
          write(*,*) '*** fcrtrn: too few values for xptcntr'
          xptcntr(1,1)=-1.0_R8
        else
          nxpt=max(i/3,1)
          do j=1,nxpt
            do i=1,3
              xptcntr(i,j)=0.001_R8*real(xarray(i,j),R8)
            end do
          end do
        end if
      case(2)
c*** xlpcntr
        call rearre(x,3,i)
        if(i.lt.2) then
          write(*,*) '*** fcrtrn: too few values for xlpcntr'
          xlpcntr(1)=-1.0_R8
        else
          xlpcntr(1)=0.001_R8*real(x(1),R8)
          xlpcntr(2)=0.001_R8*real(x(2),R8)
        end if
      case (3)
c*** trg_spcf
        call resimi(lutrg)
        ntrg=max0(ntrg,lutrg)
      case (4)
c*** tgtgrd
        if(lutrg.le.0 .or. lutrg.gt.ntrgx) then !{
          write(*,*) 'Target index outside limits: ',lutrg
          stop ' ==> Corrupted DG output files?'
        else !}{
          call resime(rdummy)
          tgarde(lutrg) = real(rdummy,R8)
        end if !}
      case (5)
c***  lm_cnfg
        call resimi(lm_cnfg)
      case (6)
c***  lm_pntrt
        call resime(lm_pntrt)
      case (7)
c***  lm_grcln
        call resime(lm_grcln)
      case default
        call skipit
      end select
      return
c======================================================================
      end
