      subroutine fcrprp
c
c  version : 05.02.99 14:04
c
c======================================================================
c*** Data transformation from DG to Carre
c======================================================================
      implicit none
#include <CARRETYPES.F>
#include <FCRCOM.F>
      integer i, j
      logical ex,uex
      real(kind=R8) u, dpi
      real(kind=R8) tgtgrd(4)
      parameter (dpi=2.*3.14159 26535 89793 23846 26433 83280_R8)
c======================================================================
c*** (lower single-null configuration)
c
c      nsgm=3
c      nrgn=3
c
c*** Convert data for limiter configurations
      if (lm_cnfg.eq.1) then
        write (*,*) 'Limiter configuration'
        npr(2)=npr(nrgn)
        nptseg(1)=nptseg(nsgm)
        deltr1(2)=deltr1(nrgn)
        deltrn(2)=deltrn(nrgn)
        nrgn=2
        pntrat = real(lm_pntrt,R8)
      end if
c*** Check the data
c
      ex=.false.
      if(repart.ne.1 .and. repart.ne.2) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of repart',repart
      end if
      if(pntrat.le.0.) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of pntrat',pntrat
      end if
      if(nrelax.le.0) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of nrelax',nrelax
      end if
      if(relax.le.0.0_R8) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of relax',relax
      end if
      if(pasmin.le.0.0_R8) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of pasmin',pasmin
      end if
      if(rlcept.le.0.0_R8) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of rlcept',rlcept
      end if
      uex=.false.
      do i=1,nrgn
        uex=uex .or. npr(i).le.1
      end do
      if(uex) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of npr'
        write (*,*) (npr(i),i=1,nrgn)
      end if
      uex=.false.
      do i=1,nsgm
        uex=uex .or. nptseg(i).le.1
      end do
      if(uex) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of nptseg'
        write (*,*) (nptseg(i),i=1,nsgm)
      end if
      uex=.false.
      do i=1,nrgn
        uex=uex .or. deltr1(i).eq.0.0_R8
      end do
      if(uex) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of deltr1'
        write (*,*) (deltr1(i),i=1,nrgn)
      end if
      uex=.false.
      do i=1,nrgn
        uex=uex .or. deltrn(i).eq.0.0_R8
      end do
      if(uex) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of deltrn'
        write (*,*) (deltrn(i),i=1,nrgn)
      end if
      uex=.false.
      do i=1,nsgm
        uex=uex .or. deltp1(i).eq.0.0_R8
      end do
      if(uex) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of deltp1'
        write (*,*) (deltp1(i),i=1,nsgm)
      end if
      uex=.false.
      do i=1,nsgm
        uex=uex .or. deltpn(i).eq.0.0_R8
      end do
      if(uex) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of deltpn'
        write (*,*) (deltpn(i),i=1,nsgm)
      end if
c
      if(nsgm.gt.nsgmx) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of nsgm > nsgmx ',nsgm,nsgmx
      end if
      if(nrgn.gt.nrgnx) then
        ex=.true.
        write (*,*) 'fcrprp: wrong value of nrgn > nrgnx ',nrgn,nrgnx
      end if
c
      if(nclstr.lt.0) nclstr=nstr
      if(nclstr.lt.max(nxpt*2,1)) then
        ex=.true.
        write (*,*) 'fcrprp: too few closed structures, nclstr=',nclstr
      end if
c
      if(ex) then
        write (*,*) 'fcrprp: errors detected in the input data.'
        write (*,*) 'Check DG output (must be in the Carre mode) ',
     ,                            'and the auxiliary input file dg.aux'
        stop
      end if

#ifdef DBG
      write(0,*) 'fcrprp: ldgv2 = ',ldgv2
#endif
      if(.not.ldgv2.and.lm_cnfg.eq.0) then
c
c*** Re-arrange the input data to fit the Carre conventions
c*** (lower single-null configuration created with old DG)
c
        u=deltr1(1)
        deltr1(1)=deltr1(2)
        deltr1(2)=deltr1(3)
        deltr1(3)=u
        u=deltrn(1)
        deltrn(1)=deltrn(2)
        deltrn(2)=deltrn(3)
        deltrn(3)=u
        i=npr(1)
        npr(1)=npr(2)
        npr(2)=npr(3)
        npr(3)=i
        u=deltp1(1)
        deltp1(1)=deltp1(3)
        deltp1(3)=u
        u=deltpn(1)
        deltpn(1)=deltpn(3)
        deltpn(3)=u
        i=nptseg(1)
        nptseg(1)=nptseg(3)
        nptseg(3)=i
c
        u=deltpn(2)
        deltpn(2)=deltp1(2)
        deltp1(2)=u
        u=deltpn(3)
        deltpn(3)=deltp1(3)
        deltp1(3)=u
      end if
c
c*** ... re-scale the poloidal dimensions from mm (DG) to m (Carre)
c
      do i=1,nsgm
        deltp1(i)=0.001_R8*deltp1(i)
        deltpn(i)=0.001_R8*deltpn(i)
      end do
      if (lm_cnfg.eq.0) pntrat=0.001_R8*pntrat
c
c*** ... and the poloidal flux from wb/rad (DG) to wb (Carre)
c
      do i=1,nr
        do j=1,nz
          pfm(i,j)=dpi*pfm(i,j)
        end do
      end do
      do i=1,nrgn
        deltr1(i)=deltr1(i)*dpi
        deltrn(i)=deltrn(i)*dpi
      end do
c
c*** ... for DN cases, re-order the tgarde array to Carre ordering
c
      if (ntrg.eq.4) then
        do i=1,ntrg ! storing the values in DG order
          tgtgrd(i)=tgarde(i)
        end do
        tgarde(1)=tgtgrd(3)
        tgarde(2)=tgtgrd(2)
        tgarde(3)=tgtgrd(4)
        tgarde(4)=tgtgrd(1)
      end if
c======================================================================
      end
