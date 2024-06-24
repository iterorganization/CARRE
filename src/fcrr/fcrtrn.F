      subroutine fcrtrn(nam)
c
c  version : 18.02.99 17:27
c
c======================================================================
c*** Input of target data for Carre
c======================================================================
      use carre_types
      use fcrcom
      implicit none
      character*8 nam
c
      integer nnms
      parameter (nnms=8)
      integer i
      real(kind=R4) rdummy(1), rarray(nrgnx), parray(nsgmx)
      character*8 name, unm(nnms)
      logical streql
      external streql, rearri, rearre, resimi, resime, scipit
c
c*** List of the valid input keywords
      data unm /
     1    'npr     ','nptseg  ','dltr1   ','dltrn   ','dltp1   ',
     2    'dltpn   ','pntrat  ','clstruct'/
c======================================================================
c
      call locase(nam,name,8)
      do i=1,nnms
        if(streql(name,unm(i))) exit
      end do
c----------------------------------------------------------------------
      select case (i)
      case (1)
        call rearri(npr,nrgnx,i)
        if(i.gt.0) then
          if(nrgn.gt.0) then
            if(nrgn.ne.i) then
              write(0,*) 'Inconsistent number of regions found for npr:'
     ,                   ,i,' instead of ',nrgn
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
              write(0,*) 'Inconsistent number of sections found for ',
     ,                    'nptseg:',i,' instead of ',nsgm
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
               write(0,*) 'Inconsistent number of regions found for ',
     ,                    'deltr1:',i,' instead of ',nrgn
            end if
            nrgn=max(nrgn,i)
          else
            nrgn=i
          end if
          deltr1(1:i) = real(rarray(1:i),R8)
        end if
      case (4)
        call rearre(rarray,nrgnx,i)
        if(i.gt.0) then
          if(nrgn.gt.0) then
            if(nrgn.ne.i) then
              write(0,*) 'Inconsistent number of regions found for ',
     ,                    'deltrn:',i,' instead of ',nrgn
            end if
            nrgn=max(nrgn,i)
          else
            nrgn=i
          end if
          deltrn(1:i) = real(rarray(1:i),R8)
        end if
      case (5)
        call rearre(parray,nsgmx,i)
        if(i.gt.0) then
          if(nsgm.gt.0) then
            if(nsgm.ne.i) then
              write(0,*) 'Inconsistent number of sections found for ',
     ,                   'deltp1:',i,' instead of ',nsgm
            end  if
            nsgm=max(nsgm,i)
          else
            nsgm=i
          end if
          deltp1(1:i) = real(parray(1:i),R8)
        end if
      case (6)
        call rearre(parray,nsgmx,i)
        if(i.gt.0) then
          if(nsgm.gt.0) then
            if(nsgm.ne.i) then
              write(0,*) 'Inconsistent number of sections found for ',
     ,                    'deltpn:',i,' instead of ',nsgm
            end if
            nsgm=max(nsgm,i)
          else
            nsgm=i
          end if
          deltpn(1:i) = real(parray(1:i),R8)
        end if
      case (7)
        call rearre(rdummy,1,i)
        pntrat = real(rdummy(1),R8)
      case (8)
        call resimi(nclstr)
      case default
        call scipit
      end select
      return
      end

c======================================================================
      subroutine fcrchktp(lun)
c======================================================================
      use carre_types
      use fcrcom
      implicit none
      integer lun
      character*80 uline

c*** Check whether "# topo" line is present in the target file (new DG)

      ldgv2=.false.
      rewind lun
 9000 read(lun,'(a)',end=9010) uline
      if(index(uline,'#').eq.0) go to 9010
      ldgv2=index(uline,'# topo ').gt.0
      if(.not.ldgv2) go to 9000
 9010 rewind(lun)
      return
c======================================================================
      end
