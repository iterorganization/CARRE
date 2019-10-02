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
      integer lun
!
      integer nnms
      parameter (nnms=8)
      integer(Short) i,j
      character*8 unm(nnms), uline*80
!
!*** List of the valid input keywords
      data unm / & 
     &    'npr     ','nptseg  ','dltr1   ','dltrn   ','dltp1   ', & 
     &    'dltpn   ','pntrat  ','clstruct'/
!======================================================================
!
      do j=1,nnms
          if(nam.eq.unm(j)) exit
      end do
!----------------------------------------------------------------------
      select case (j)
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
       call rearre(deltr1,nrgnx,i)
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
       end if
      case (4)
       call rearre(deltrn,nrgnx,i)
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
       end if
      case (5)
       call rearre(deltp1,nsgmx,i)
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
       end if
      case (6)
       call rearre(deltpn,nsgmx,i)
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
       end if
      case (7)
       call resime(pntrat)
      case (8)
       call resimi(nclstr)
      case default
       call scipit
      end select
      return

!======================================================================
      entry fcrchktp(lun)
!======================================================================

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
      end
