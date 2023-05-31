      subroutine fcrtrn(nam)
!
!  version : 18.02.99 17:27
!
!======================================================================
!*** Input of target data for Carre
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      character*(8) nam
!
      integer nnms
      parameter (nnms=8)
      integer(Short) :: i
      real(Single) :: rdummy(1), rarray(nrgnx), parray(nsgmx)
      character*8 name, unm(nnms)
      logical carre_streql
      external carre_streql, rearre, rearri, resime, resimi, scipit
!
!*** List of the valid input keywords
      data unm / & 
     &    'npr     ','nptseg  ','dltr1   ','dltrn   ','dltp1   ', & 
     &    'dltpn   ','pntrat  ','clstruct'/
!======================================================================
!
      call locase(nam,name,8)
      do i=1,nnms
        if(carre_streql(name,unm(i))) exit
      end do
!----------------------------------------------------------------------
      select case (i)
      case (1)
       call rearri(npr,nrgnx,i)
       if(i.gt.0) then
        if(nrgn.gt.0) then
         if(nrgn.ne.i) then
          write(0,*) 'Inconsistent number of regions found for npr:', & 
     &               i,'  instead of ',nrgn
         end if
         nrgn=max(nrgn,i)
        else
         nrgn=i
        end if
       end if
      case (2)
       call rearri(nptseg,nsgmx,i)
       if(i.gt.0) then
        if(nsgm.gt.0) then
         if(nsgm.ne.i) then
          write(0,*) 'Inconsistent number of sections found for ', & 
     &               'nptseg:',i,'  instead of ',nsgm
         end if
         nsgm=max(nsgm,i)
        else
         nsgm=i
        end if
       end if
      case (3)
       call rearre(rarray,nrgnx,i)
       if(i.gt.0) then
        if(nrgn.gt.0) then
         if(nrgn.ne.i) then
          write(0,*) 'Inconsistent number of regions found for ', & 
     &               'deltr1:',i,'  instead of ',nrgn
         end if
         nrgn=max(nrgn,i)
        else
         nrgn=i
        end if
        deltr1(1:i) = real(rarray(1:i),rKind)
       end if
      case (4)
       call rearre(rarray,nrgnx,i)
       if(i.gt.0) then
        if(nrgn.gt.0) then
         if(nrgn.ne.i) then
          write(0,*) 'Inconsistent number of regions found for ', & 
     &               'deltrn:',i,'  instead of ',nrgn
         end if
         nrgn=max(nrgn,i)
        else
         nrgn=i
        end if
        deltrn(1:i) = real(rarray(1:i),rKind)
       end if
      case (5)
       call rearre(parray,nsgmx,i)
       if(i.gt.0) then
        if(nsgm.gt.0) then
         if(nsgm.ne.i) then
          write(0,*) 'Inconsistent number of sections found for ', & 
     &               'deltp1:',i,'  instead of ',nsgm
         end if
         nsgm=max(nsgm,i)
        else
         nsgm=i
        end if
        deltp1(1:i) = real(parray(1:i),rKind)
       end if
      case (6)
       call rearre(parray,nsgmx,i)
       if(i.gt.0) then
        if(nsgm.gt.0) then
         if(nsgm.ne.i) then
          write(0,*) 'Inconsistent number of sections found for ', & 
     &               'deltpn:',i,'  instead of ',nsgm
         end if
         nsgm=max(nsgm,i)
        else
         nsgm=i
        end if
        deltpn(1:i) = real(parray(1:i),rKind)
       end if
      case (7)
        call rearre(rdummy,1,i)
        pntrat = real(rdummy(1),rKind)
      case (8)
       call resimi(nclstr)
      case default
       call scipit
      end select
      return
      end subroutine fcrtrn

!======================================================================
      subroutine fcrchktp(lun)
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      integer lun
      character*80 uline

!*** Check whether "# topo" line is present in the target file (new DG)

      ldgv2=.false.
      rewind lun
 9000 read(lun,'(a)',end=9010) uline
      if(index(uline,'#').eq.0) go to 9010
      ldgv2=index(uline,'# topo ').gt.0
      if(.not.ldgv2) go to 9000
 9010 rewind(lun)
      return
!======================================================================
      end subroutine fcrchktp
