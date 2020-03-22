      subroutine b2agfz(nx,ny,crx,cry,fpsi,ffbz,nxmax,nymax,
     ,                  r,z,nreg,nppol,nprad,npolmx,nradmx,
     ,                  nptseg,psidx,psidy,psi,psidxm,psidym,b0r0,
     ,                  ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso)
c
c  version : 12.01.99 13:10
c
c======================================================================
c*** This routine converts the grid data stored in carre format - that
c*** is, surface by surface, region by region, - in the Sonnet-B2 form,
c*** that is, cell by cell.
c***
c*** It is not universal, and it only recognises several pre-programmed
c*** cases of the grid topology.
c***
c*** This routine was apparently cloned from an old B2 grid generator
c***
c*** ank-19980626:
c***  - disconnected double null added
c***  - exotic cases (quarters and halves of the grid) removed
c*** ank-19990111:
c***  - connected double null fixed
c***  - guard cells for disconnected double fixed
c***  - data on grid size and cuts added to the grid header
c======================================================================
      implicit none
#include <CARRETYPES.F>

      integer npolmx,nradmx,nxmax,nymax,ncutmx,nisomx
      integer nx,ny,nreg,nppol(nreg),nprad(nreg),nptseg(10),
     ,  ncut,nxcut(ncutmx),nycut(ncutmx),niso,nxiso(nisomx)
      real(kind=R8)
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  fpsi(-1:nxmax,-1:nymax,0:3),ffbz(-1:nxmax,-1:nymax,0:3),
     .  r(npolmx,nradmx,nreg),z(npolmx,nradmx,nreg),
     .  psidx(-1:nxmax,-1:nymax,0:3),psidy(-1:nxmax,-1:nymax,0:3),
     .  psi(npolmx,nradmx,nreg),psidxm(npolmx,nradmx,nreg),
     .  psidym(npolmx,nradmx,nreg),b0r0
c======================================================================
c*** Input:
c***  nxmax,nymax: array dimensions for B2 grid (poloidal & radial)
c***  r,z   : coordinates of the carre grid points
c***  nreg  : actual number of regions in the carre grid
c***  nppol,nprad : number of poloidal and radial grid points in each
c***                region
c***  npolmx,nradmx: corresponding dimensions
c***  nptseg: number of the grid points on each part of the separatrix
c***          as was used in carre
c***  psi   : psi values at the points of the carre grid
c***  psidxm,psidym: psi derivatives in x and y on the carre grid
c***  ncutmx: maximum number of regular cuts
c***  nisomx: maximum number of isolating cuts
c***
c*** Output:
c***  nx,ny : actual dimensions of the B2 grid
c***  crx,cry: coordinates of the cell corners
c***  fpsi  : psi values at the cell corners
c***  ffbz  : "flux function" = 2*pi*B0*R0 for the toroidal geometry
c***  ncut  : actual number of cuts
c***  nxcut : poloidal location of the cuts (left edges)
c***  nycut : radial depth of the cuts
c***  niso  : actual number of isolating cuts
c***  nxiso : poloidal location of the isolating cuts (left edges)
c======================================================================

*   ..local variables

      integer ipol,irad,npol2g,npol2d,
     .  ireg,ix,iy,npol1,npol2,npol3,i
      real(kind=R8) twopi,del,zero,one,tempo
      parameter(del=1.e-2_R8,zero=0._R8,one=1._R8)
      logical ex
      character equtype*20

      logical errenc
      integer j,k,kk,iip,iir,irg,irz,ipz,ihg,lp,lr,mp,mr,ixx,iyy,nxx(2)
      character*8 znstxt(2)*5, znptxt(3), znrtxt(3)
      data znstxt / 'left', 'right' /
      data znptxt / 'bottom', 'center', 'top' /
      data znrtxt / 'inner', 'inside', 'outer' /
      integer kq(2,3,3),nqp(2,3,3),
     ,  nqr(2,3,3),lqp(2,3,3),lqr(2,3,3),mqp(2,3,3),mqr(2,3,3),
     ,  ka(2),kb(2),kc(2),kd(2),ke(2),kf(2),kg(2),kh(2),ki(2),
     ,  lap(2),lbp(2),lcp(2),ldp(2),lep(2),lfp(2),lgp(2),lhp(2),lip(2),
     ,  lar(2),lbr(2),lcr(2),ldr(2),ler(2),lfr(2),lgr(2),lhr(2),lir(2),
     ,  map(2),mbp(2),mcp(2),mdp(2),mep(2),mfp(2),mgp(2),mhp(2),mip(2),
     ,  mar(2),mbr(2),mcr(2),mdr(2),mer(2),mfr(2),mgr(2),mhr(2),mir(2)
      equivalence
     ,    (ka,kq(1,1,1)),    (kb,kq(1,1,2)),     (kc,kq(1,1,3)),
     ,    (kd,kq(1,2,1)),    (ke,kq(1,2,2)),     (kf,kq(1,2,3)),
     ,    (kg,kq(1,3,1)),    (kh,kq(1,3,2)),     (ki,kq(1,3,3)),
     ,  (lap,lqp(1,1,1)),  (lbp,lqp(1,1,2)),   (lcp,lqp(1,1,3)),
     ,  (ldp,lqp(1,2,1)),  (lep,lqp(1,2,2)),   (lfp,lqp(1,2,3)),
     ,  (lgp,lqp(1,3,1)),  (lhp,lqp(1,3,2)),   (lip,lqp(1,3,3)),
     ,  (lar,lqr(1,1,1)),  (lbr,lqr(1,1,2)),   (lcr,lqr(1,1,3)),
     ,  (ldr,lqr(1,2,1)),  (ler,lqr(1,2,2)),   (lfr,lqr(1,2,3)),
     ,  (lgr,lqr(1,3,1)),  (lhr,lqr(1,3,2)),   (lir,lqr(1,3,3)),
     ,  (map,mqp(1,1,1)),  (mbp,mqp(1,1,2)),   (mcp,mqp(1,1,3)),
     ,  (mdp,mqp(1,2,1)),  (mep,mqp(1,2,2)),   (mfp,mqp(1,2,3)),
     ,  (mgp,mqp(1,3,1)),  (mhp,mqp(1,3,2)),   (mip,mqp(1,3,3)),
     ,  (mar,mqr(1,1,1)),  (mbr,mqr(1,1,2)),   (mcr,mqr(1,1,3)),
     ,  (mdr,mqr(1,2,1)),  (mer,mqr(1,2,2)),   (mfr,mqr(1,2,3)),
     ,  (mgr,mqr(1,3,1)),  (mhr,mqr(1,3,2)),   (mir,mqr(1,3,3))

*   ..procedures
      external periph
c======================================================================
c*** The computational grid looks then like the following:
c***
c***      *******************|*******************
c***      * c4 :  f4   : i4 *|* c2 :  f2   : i2 *
c***      *....:............*|*....:............*
c***      * b1 :  e1   | h3 *|* b3 |  e1   : h1 *     ddn-down
c***      *....|.......|....*|*....|.......|....*
c***      * a5 |  d6   | g3 *|* a3 |  d6   | g5 *
c***      *******************|*******************
c***             left                right
c***
c***
c***      *******************|*******************
c***      * c4 :  f4   : i4 *|* c2 :  f2   : i2 *
c***      *....:............*|*....:............*
c***      * b5 |  e1   : h1 *|* b1 :  e1   | h5 *     ddn-up
c***      *....|.......|....*|*....|.......|....*
c***      * a5 |  d6   | g3 *|* a3 |  d6   | g5 *
c***      *******************|*******************
c***             left                right
c***
c***
c***      *******************|*******************
c***      * c3 :  f3   : i3 *|* c1 :  f1   : i1 *
c***      *....|.......|....*|*....|.......|....*     cdn-full
c***      * a4 |  d5   | g2 *|* a2 |  d5   | g4 *
c***      *******************|*******************
c***             left                right
c***
c***
c***      *******************
c***      * c1 :  f1   : i1 *
c***      *....|.......|....*                         sn-...
c***      * a2 |  d3   | g2 *
c***      *******************
c***
c***  Here * denotes a guard cell, | is a cut, and . and : separate
c***  different zones of the grid.
c***  Letters a to i identify the zones of the B2 grid, and numbers
c***  1 to 6 identify the carre regions which are projected onto these
c***  zones.
c***
c*** Correspondence between the B2 and carre grids is set up for each
c*** B2 zone specifying the region, starting indices, number of points,
c*** and scanning direction in both poloidal and radial direction over
c*** the carre grid.
c***
c*** These data are assigned to the variables:
c***
c***  k[a-i]  : carre region projected onto the B2 zone [a-i]
c***  l[a-i][pr]: starting index in poloidal or radial direction
c***  m[a-i][pr]: ending index in poloidal or radial direction
c***  n[a-i][pr]: number of cells in poloidal or radial direction
c======================================================================
*.computation

c<<<
      write(0,*) '===> Entering b2agfz. nreg = ',nreg
c      write(0,*) 'nppol: ',nppol
c      write(0,*) 'nprad: ',nprad
c      write(0,*) 'nptseg: ',nptseg
c      do k=1,nreg     ! {
c       write(0,*) 'Region ',k
c       do j=1,nprad(k)     ! {
c         write(0,'(4h r: ,1p,12e10.2/(4x,12e10.2))')
c     -            (r(i,j,k),i=1,nppol(k))
c         write(0,'(4h z: ,1p,12e10.2/(4x,12e10.2))')
c     -            (z(i,j,k),i=1,nppol(k))     ! }
c       end do      ! }
c      end do
c>>>
      twopi=2.* 4.*atan(one)
      niso=0
      ncut=0
c
c*** nominal magnetic field and major radius

      inquire(file='btor.dat',exist=ex)
      if(ex) then     ! {
        ex=.false.
        open(17,file='btor.dat',status='old',err=10)
        rewind(17)
        read(17,*,err=10) b0r0
        close(17)
        ex= b0r0 .ne. 0.                                   ! DPC
        if(.not.ex) write(6,*) 'Zero value of Btor*R "in btor.dat"'     ! }
      else      ! {
        write(*,*) 'No file btor.dat found.'      ! }
      end if
 10   if(.not.ex) then      ! {
        write(6,*) 'Unable to get data from "btor.dat"'
        write(6,*) 'Please enter the value of Btor*R (0 to quit):'
        read *,b0r0
        if(b0r0.eq.0.) stop     ! }
      end if
c<<<
      write(0,*) 'Input: b0r0 = ',b0r0
c>>>
c======================================================================
c*** Define the equilibrium type

c!!! Change the criteria (O-point is not necessarily around z=0) !!!
c-ank19990108: done. To be honest, ssn-up and ssn-down are treated equally...

c*** Now compare the vertical position of the x-point with that of the mid-SOL
c      if(nreg.eq.3 .and. z(nptseg(1)-1,1,1).gt.zero) then
      if(nreg.eq.2) then
        equtype='limiter'
      else if(nreg.eq.3 .and. z(nptseg(1)-1,1,1).gt.z(nptseg(3)/2,1,3))
     . then
        equtype='sn-up'
c      else if(nreg.eq.3 .and. z(nptseg(1)-1,1,1).lt.zero) then
      else if(nreg.eq.3) then
        equtype='sn-down'
      else if(nreg.eq.5) then
        equtype='cdn-full'
      else if(nreg.eq.6 .and. r(1,1,1).eq.r(1,1,3) .and.
     .                                      z(1,1,1).eq.z(1,1,3) ) then
        equtype='ddn-up'
      else if(nreg.eq.6 .and. r(1,1,1).eq.r(1,1,5) .and.
     .                                      z(1,1,1).eq.z(1,1,5) ) then
        equtype='ddn-down'
      else
        write(6,*) 'b2agfz: the equilibrium is of an unforeseen type. ',
     ,                                                   'nreg = ',nreg
        stop
      end if
c<<<
      write(0,*) 'topology: ',equtype
c>>>

c======================================================================
c*** Create the required mesh

      if(index(equtype,'dn-').gt.0) then     ! { ###
        if(index(equtype,'ddn-').gt.0) then     ! {
c----------------------------------------------------------------------
c*** Construct the full mesh for the disconnected double-null geometry,
c*** inner x-point either below (down) or above (up) the axis
c***
c*** The grid is constructed of two adjacent parts corresponding to the
c*** inner and outer halves of the edge region.
c***
c*** Calculate the indices, increments, and counting for each zone

c----------------------------------------------------------------------
          if(index(equtype,'ddn-down').gt.0) then      ! {  #C
c----------------------------------------------------------------------

cc<<<
c          write(0,'(1x,a8,9a4)') 'ddn-down',
c     ,                              'a','b','c','d','e','f','g','h','i'
cc>>>
c*** Check some grid dimensions for consistency

            errenc=.false.
            if(nprad(5).ne.nprad(6)) then
              write(0,*) 'You must have nprad(5) .eq. nprad(6)'
              write(*,*) 'You must have nprad(5) .eq. nprad(6)'
              errenc=.true.
            end if
            if(nprad(2).ne.nprad(4)) then
              write(0,*) 'You must have nprad(2) .eq. nprad(4)'
              write(*,*) 'You must have nprad(2) .eq. nprad(4)'
              errenc=.true.
            end if
            if(nprad(5)+nprad(1)-1.ne.nprad(3)) then
              write(0,*)
     .         'You must have nprad(5)+nprad(1)-1 .eq. nprad(3)'
              write(*,*)
     .         'You must have nprad(5)+nprad(1)-1 .eq. nprad(3)'
              errenc=.true.
            end if

            if(errenc) then
              write(0,*) 'nprad: ',nprad
              write(*,*) 'nprad: ',nprad
              write(*,*) 'b2agfz: inconsistent grid dimensions.'
              write(*,*) '==> check the npr(i) values in carre'
              stop
            end if

c*** Regions

            ka(1)=5
            ka(2)=3
            kb(1)=1
            kb(2)=3
            kc(1)=4
            kc(2)=2
            kd(1)=6
            kd(2)=6
            ke(1)=1
            ke(2)=1
            kf(1)=4
            kf(2)=2
            kg(1)=3
            kg(2)=5
            kh(1)=3
            kh(2)=1
            ki(1)=4
            ki(2)=2

c*** Starting/ending points

            lap(1)=nppol(ka(1))
            map(1)=nptseg(5)
            lap(2)=1
            map(2)=nptseg(1)

            lbp(1)=nppol(kb(1))
            mbp(1)=nppol(kb(1))-nptseg(6)+1
            lbp(2)=1
            mbp(2)=nptseg(1)

            lcp(1)=1
            mcp(1)=nptseg(6)
            lcp(2)=nppol(kc(2))
            mcp(2)=lcp(2)-nptseg(1)+1

            ldp(1)=nppol(kd(1))
            mdp(1)=ldp(1)-nptseg(4)+1
            ldp(2)=mdp(1)
            mdp(2)=1

            lep(1)=mbp(1)
            mep(1)=lep(1)-nptseg(4)+1
            lep(2)=mep(1)
            mep(2)=lep(2)-nptseg(3)+1

            lfp(1)=mcp(1)
            mfp(1)=lfp(1)+nptseg(4)-1
            lfp(2)=mcp(2)
            mfp(2)=lfp(2)-nptseg(3)+1

            lgp(1)=nptseg(1)
            mgp(1)=nppol(kg(1))
            lgp(2)=nptseg(5)
            mgp(2)=1

            lhp(1)=nptseg(1)
            mhp(1)=nppol(kh(1))
            lhp(2)=mep(2)
            mhp(2)=1

            lip(1)=mfp(1)
            mip(1)=nppol(ki(1))
            lip(2)=mfp(2)
            mip(2)=1

            lar(1)=nprad(ka(1))
            mar(1)=1
            lar(2)=nprad(ka(2))
            mar(2)=lar(2)-lar(1)+1

            lbr(1)=1
            mbr(1)=nprad(kb(1))
            lbr(2)=mar(2)
            mbr(2)=1

            lcr(1)=1
            mcr(1)=nprad(kc(1))
            lcr(2)=1
            mcr(2)=nprad(kc(2))

            ldr(1)=nprad(kd(1))
            mdr(1)=1
            ldr(2)=nprad(kd(2))
            mdr(2)=1

            ler(1)=1
            mer(1)=nprad(ke(1))
            ler(2)=1
            mer(2)=nprad(ke(2))

            lfr(1)=1
            mfr(1)=nprad(kf(1))
            lfr(2)=1
            mfr(2)=nprad(kf(2))

            lgr(1)=nprad(kg(1))
            mgr(1)=lgr(1)-lar(1)+1
            lgr(2)=nprad(kg(2))
            mgr(2)=1

            lhr(1)=mgr(1)
            mhr(1)=1
            lhr(2)=1
            mhr(2)=nprad(kh(2))

            lir(1)=1
            mir(1)=nprad(ki(1))
            lir(2)=1
            mir(2)=nprad(ki(2))

c----------------------------------------------------------------------     ! }
          else if(index(equtype,'ddn-up').gt.0) then     ! {
c----------------------------------------------------------------------

cc<<<
c          write(0,'(1x,a8,9a4)') 'ddn-up',
c     ,                              'a','b','c','d','e','f','g','h','i'
cc>>>

c*** Check some grid dimensions for consistency

            errenc=.false.
            if(nprad(3).ne.nprad(6)) then
              write(0,*) 'You must have nprad(3) .eq. nprad(6)'
              write(*,*) 'You must have nprad(3) .eq. nprad(6)'
              errenc=.true.
            end if
            if(nprad(2).ne.nprad(4)) then
              write(0,*) 'You must have nprad(2) .eq. nprad(4)'
              write(*,*) 'You must have nprad(2) .eq. nprad(4)'
              errenc=.true.
            end if
            if(nprad(3)+nprad(1)-1.ne.nprad(5)) then
              write(0,*)
     .         'You must have nprad(3)+nprad(1)-1 .eq. nprad(5)'
              write(*,*)
     .         'You must have nprad(3)+nprad(1)-1 .eq. nprad(5)'
              errenc=.true.
            end if

            if(errenc) then
              write(0,*) 'nprad: ',nprad
              write(*,*) 'nprad: ',nprad
              write(*,*) 'b2agfz: inconsistent grid dimensions.'
              write(*,*) '==> check the npr(i) values in carre'
              stop
            end if

c*** Regions

            ka(1)=5
            ka(2)=3
            kb(1)=5
            kb(2)=1
            kc(1)=4
            kc(2)=2
            kd(1)=6
            kd(2)=6
            ke(1)=1
            ke(2)=1
            kf(1)=4
            kf(2)=2
            kg(1)=3
            kg(2)=5
            kh(1)=1
            kh(2)=5
            ki(1)=4
            ki(2)=2

c*** Starting/ending points

            lap(1)=nppol(ka(1))
            map(1)=nptseg(5)
            lap(2)=1
            map(2)=nptseg(1)

            lbp(1)=lap(1)
            mbp(1)=map(1)
            lbp(2)=lap(2)
            mbp(2)=map(2)

            lcp(1)=nppol(kc(1))
            mcp(1)=lcp(1)-nptseg(6)+1
            lcp(2)=1
            mcp(2)=nptseg(1)

            ldp(1)=nptseg(3)
            mdp(1)=nppol(kd(1))
            ldp(2)=1
            mdp(2)=ldp(1)

            lep(1)=nptseg(1)+nptseg(3)-1
            mep(1)=lep(1)+nptseg(4)-1
            lep(2)=nptseg(1)
            mep(2)=lep(1)

            lfp(1)=mcp(1)
            mfp(1)=lfp(1)-nptseg(4)+1
            lfp(2)=mcp(2)
            mfp(2)=lfp(2)+nptseg(3)-1

            lgp(1)=nptseg(1)
            mgp(1)=nppol(kg(1))
            lgp(2)=nptseg(5)
            mgp(2)=1

            lhp(1)=mep(1)
            mhp(1)=nppol(kh(1))
            lhp(2)=lgp(2)
            mhp(2)=mgp(2)

            lip(1)=mfp(1)
            mip(1)=1
            lip(2)=mfp(2)
            mip(2)=nppol(ki(2))

            lar(1)=nprad(ka(1))
            mar(1)=lar(1)-nprad(kd(1))+1
            lar(2)=nprad(ka(2))
            mar(2)=1

            lbr(1)=mar(1)
            mbr(1)=1
            lbr(2)=1
            mbr(2)=nprad(kb(2))

            lcr(1)=1
            mcr(1)=nprad(kc(1))
            lcr(2)=1
            mcr(2)=nprad(kc(2))

            ldr(1)=nprad(kd(1))
            mdr(1)=1
            ldr(2)=nprad(kd(2))
            mdr(2)=1

            ler(1)=1
            mer(1)=nprad(ke(1))
            ler(2)=1
            mer(2)=nprad(ke(2))

            lfr(1)=1
            mfr(1)=nprad(kf(1))
            lfr(2)=1
            mfr(2)=nprad(kf(2))

            lgr(1)=nprad(kg(1))
            mgr(1)=1
            lgr(2)=nprad(kg(2))
            mgr(2)=lgr(2)-nprad(kd(2))+1

            lhr(1)=1
            mhr(1)=nprad(kh(1))
            lhr(2)=mgr(2)
            mhr(2)=1

            lir(1)=1
            mir(1)=nprad(ki(1))
            lir(2)=1
            mir(2)=nprad(ki(2))
        ! }
          else      ! {

            write(*,*) 'Non-recognisable option for disconnected ',
     ,              'double null -- internal error in b2agfz: ',equtype
            stop
        ! }
          end if

cc<<<
c        write(0,'(1x,a8,9i4/9x,9i4)') 'Regions:',
c     ,                                (((kq(i,j,k),k=1,3),j=1,3),i=1,2)
c        write(0,'(1x,a8,9i4/9x,9i4)') 'Start p:',
c     ,                               (((lqp(i,j,k),k=1,3),j=1,3),i=1,2)
c        write(0,'(1x,a8,9i4/9x,9i4)') 'End   p:',
c     ,                               (((mqp(i,j,k),k=1,3),j=1,3),i=1,2)
c        write(0,'(1x,a8,9i4/9x,9i4)') 'Start r:',
c     ,                               (((lqr(i,j,k),k=1,3),j=1,3),i=1,2)
c        write(0,'(1x,a8,9i4/9x,9i4)') 'End   r:',
c     ,                               (((mqr(i,j,k),k=1,3),j=1,3),i=1,2)
cc>>>

c*** Calculate dimensions of the B2 grid zones

          do i=1,2
            do j=1,3
              do k=1,3
                nqp(i,j,k)=iabs(lqp(i,j,k)-mqp(i,j,k))
                nqr(i,j,k)=iabs(lqr(i,j,k)-mqr(i,j,k))
              end do
            end do
          end do

cc<<<
c        write(0,'(1x,a8,9i4/9x,9i4)') '    nqp:',
c     ,                               (((nqp(i,j,k),k=1,3),j=1,3),i=1,2)
c        write(0,'(1x,a8,9i4/9x,9i4)') '    nqr:',
c     ,                               (((nqr(i,j,k),k=1,3),j=1,3),i=1,2)
cc>>>

c*** Check them for consistency

          errenc=.false.
          do i=1,2      ! {
            do j=1,3      ! {
              if(nqp(i,j,1).ne.nqp(i,j,2) .or.
     .           nqp(i,j,1).ne.nqp(i,j,3)) then  ! {
                kk=j
                if(i.eq.2 .and. kk.ne.2) kk=4-kk
                write(0,*) 'Inconsistent poloidal grid dimensions. ',
     ,                                  'Region: ',znptxt(kk),znstxt(i)
                write(*,*) 'Inconsistent poloidal grid dimensions. ',
     ,                                  'Region: ',znptxt(kk),znstxt(i)
                errenc=.true.     ! }
              end if      ! }
            end do

            if(errenc) then
              write(*,*) 'Inconsistent poloidal grid dimensions.'
              write(*,*) '==> Probably, an internal error in b2agfz'
              stop
            end if

            do k=1,3      ! {
              if(nqr(i,1,k).ne.nqr(i,2,k) .or.
     .           nqr(i,1,k).ne.nqr(i,3,k)) then ! {
                write(0,*) 'Inconsistent radial grid dimensions. ',
     ,                                   'Region: ',znrtxt(k),znstxt(i)
                write(*,*) 'Inconsistent radial grid dimensions. ',
     ,                                   'Region: ',znrtxt(k),znstxt(i)
                write(*,*) '==> Check the npr specification in carre. '
                errenc=.true.     ! }
              end if      ! }
            end do      ! }
          end do
          do k=1,3      ! {
            if(nqr(1,1,k).ne.nqr(2,1,k)) then
              write(0,*) 'Inconsistent radial grid dimensions, ',
     ,                            'right and left.  Region: ',znrtxt(k)
              write(*,*) 'Inconsistent radial grid dimensions, ',
     ,                            'right and left.  Region: ',znrtxt(k)
              write(*,*) '==> Check the npr specification in carre. '
              errenc=.true.
            end if      ! }
          end do

          if(errenc) then
            write(*,*) 'Inconsistent radial grid dimensions, '
            write(*,*) '==> Check the npr specification in carre. '
            stop
          end if
c----------------------------------------------------------------------  ! }
        else if(index(equtype,'cdn-full').gt.0) then      ! {  #D
c----------------------------------------------------------------------

c<<<
c         write(0,'(1x,a8,9a4)') 'cdn-full',
c     ,            'a','b','c','d','e','f','g','h','i'
c>>>
c*** Check some grid dimensions for consistency

            errenc=.false.
            if(nprad(1).ne.nprad(3)) then
              write(0,*) 'You must have nprad(1) .eq. nprad(3)'
              write(*,*) 'You must have nprad(1) .eq. nprad(3)'
              errenc=.true.
            end if
            if(nprad(2).ne.nprad(4)) then
              write(0,*) 'You must have nprad(2) .eq. nprad(4)'
              write(*,*) 'You must have nprad(2) .eq. nprad(4)'
              errenc=.true.
            end if
            if(nprad(5).ne.nprad(4)) then
              write(0,*) 'You must have nprad(5) .eq. nprad(4)'
              write(*,*) 'You must have nprad(5) .eq. nprad(4)'
              errenc=.true.
            end if

            if(errenc) then
              write(0,*) 'nprad: ',nprad
              write(*,*) 'nprad: ',nprad
              write(*,*) 'b2agfz: inconsistent grid dimensions.'
              write(*,*) '==> check the npr(i) values in carre'
              stop
            end if

c*** Regions  #R

            ka(1)=4
            ka(2)=2
            kb(1)=0
            kb(2)=0
            kc(1)=3
            kc(2)=1
            kd(1)=5
            kd(2)=5
            ke(1)=0
            ke(2)=0
            kf(1)=3
            kf(2)=1
            kg(1)=2
            kg(2)=4
            kh(1)=0
            kh(2)=0
            ki(1)=3
            ki(2)=1

c*** Starting/ending points

            lap(1)=nppol(ka(1))
            map(1)=nptseg(5)
            lap(2)=1
            map(2)=nptseg(1)

            lbp(1)=0
            mbp(1)=0
            lbp(2)=0
            mbp(2)=0

            lcp(1)=nppol(kc(1))
            mcp(1)=lcp(1)-nptseg(6)+1
            lcp(2)=1
            mcp(2)=nptseg(1)

            ldp(1)=nptseg(3)
            mdp(1)=nppol(kd(1))
            ldp(2)=1
            mdp(2)=ldp(1)

            lep(1)=0
            mep(1)=0
            lep(2)=0
            mep(2)=0

            lfp(1)=mcp(1)
            mfp(1)=lfp(1)-nptseg(4)+1
            lfp(2)=mcp(2)
            mfp(2)=lfp(2)+nptseg(3)-1

            lgp(1)=nptseg(1)
            mgp(1)=nppol(kg(1))
            lgp(2)=nptseg(5)
            mgp(2)=1

            lhp(1)=0
            mhp(1)=0
            lhp(2)=0
            mhp(2)=0

            lip(1)=mfp(1)
            mip(1)=1
            lip(2)=mfp(2)
            mip(2)=nppol(ki(2))

            lar(1)=nprad(ka(1))
            mar(1)=1
            lar(2)=nprad(ka(2))
            mar(2)=1

            lbr(1)=0
            mbr(1)=0
            lbr(2)=0
            mbr(2)=0

            lcr(1)=1
            mcr(1)=nprad(kc(1))
            lcr(2)=1
            mcr(2)=nprad(kc(2))

            ldr(1)=nprad(kd(1))
            mdr(1)=1
            ldr(2)=nprad(kd(2))
            mdr(2)=1

            ler(1)=0
            mer(1)=0
            ler(2)=0
            mer(2)=0

            lfr(1)=1
            mfr(1)=nprad(kf(1))
            lfr(2)=1
            mfr(2)=nprad(kf(2))

            lgr(1)=nprad(kg(1))
            mgr(1)=1
            lgr(2)=nprad(kg(2))
            mgr(2)=1

            lhr(1)=0
            mhr(1)=0
            lhr(2)=0
            mhr(2)=0

            lir(1)=1
            mir(1)=nprad(ki(1))
            lir(2)=1
            mir(2)=nprad(ki(2))

c<<<
c       write(0,'(1x,a8,9i4/9x,9i4)') 'Regions:',
c     ,              (((kq(i,j,k),k=1,3),j=1,3),i=1,2)
c       write(0,'(1x,a8,9i4/9x,9i4)') 'Start p:',
c     ,             (((lqp(i,j,k),k=1,3),j=1,3),i=1,2)
c       write(0,'(1x,a8,9i4/9x,9i4)') 'End   p:',
c     ,             (((mqp(i,j,k),k=1,3),j=1,3),i=1,2)
c       write(0,'(1x,a8,9i4/9x,9i4)') 'Start r:',
c     ,             (((lqr(i,j,k),k=1,3),j=1,3),i=1,2)
c       write(0,'(1x,a8,9i4/9x,9i4)') 'End   r:',
c     ,             (((mqr(i,j,k),k=1,3),j=1,3),i=1,2)
c>>>

c*** Calculate dimensions of the B2 grid zones

          do i=1,2
            do j=1,3
              do k=1,3
                nqp(i,j,k)=iabs(lqp(i,j,k)-mqp(i,j,k))
                nqr(i,j,k)=iabs(lqr(i,j,k)-mqr(i,j,k))
              end do
            end do
          end do

c<<<
c       write(0,'(1x,a8,9i4/9x,9i4)') '    nqp:',
c     ,             (((nqp(i,j,k),k=1,3),j=1,3),i=1,2)
c       write(0,'(1x,a8,9i4/9x,9i4)') '    nqr:',
c     ,             (((nqr(i,j,k),k=1,3),j=1,3),i=1,2)
c>>>

c*** Check them for consistency

          errenc=.false.
          do i=1,2      ! {
            do j=1,3      ! {
              if(nqp(i,j,1).ne.nqp(i,j,3)) then     ! {
                kk=j
                if(i.eq.2 .and. kk.ne.2) kk=4-kk
                write(0,*) 'Inconsistent poloidal grid dimensions. ',
     ,                                  'Region: ',znptxt(kk),znstxt(i)
                write(*,*) 'Inconsistent poloidal grid dimensions. ',
     ,                                  'Region: ',znptxt(kk),znstxt(i)
                errenc=.true.     ! }
              end if      ! }
            end do

            if(errenc) then
              write(*,*) 'Inconsistent poloidal grid dimensions.'
              write(*,*) '==> Probably, an internal error in b2agfz'
              stop
            end if

            do k=1,3      ! {
              if(nqr(i,1,k).ne.nqr(i,2,k) .or.
     .           nqr(i,1,k).ne.nqr(i,3,k)) then  ! {
                write(0,*) 'Inconsistent radial grid dimensions. ',
     ,                                   'Region: ',znrtxt(k),znstxt(i)
                write(*,*) 'Inconsistent radial grid dimensions. ',
     ,                                   'Region: ',znrtxt(k),znstxt(i)
                write(*,*) '==> Check the npr specification in carre. '
                errenc=.true.     ! }
              end if      ! }
            end do      ! }
          end do
          do k=1,3      ! {
            if(nqr(1,1,k).ne.nqr(2,1,k)) then
              write(0,*) 'Inconsistent radial grid dimensions, ',
     ,                            'right and left.  Region: ',znrtxt(k)
              write(*,*) 'Inconsistent radial grid dimensions, ',
     ,                            'right and left.  Region: ',znrtxt(k)
              write(*,*) '==> Check the npr specification in carre. '
              errenc=.true.
            end if      ! }
          end do

          if(errenc) then
            write(*,*) 'Inconsistent radial grid dimensions, '
            write(*,*) '==> Check the npr specification in carre. '
            stop
          end if
c----------------------------------------------------------------------     ! }
        end if

c*** Fill in the data arrays for the B2 grid

c*** Dimensions for one half of the grid

        ny=nqr(1,1,1)+nqr(1,1,2)+nqr(1,1,3)

        errenc=.false.
        if(ny.gt.nymax) then      ! {
          write(*,*) 'Grid is too large in radial direction: ny=',
     ,                                             ny,' > nymax=',nymax
          write(*,*) '==> Increase nrmamx in CARREDIM.F'
          errenc=.true.     ! }
        end if

        if(errenc) stop

c*** Fill in the arrays

c<<<
c       write(0,*) 'b2agfz: filling the arrays. ny= ',ny
c       write(0,'(11a5)') 'ihg','ipz','irz','irg','ix','lp','lr',
c     ,              'mp','mr','iip','iir'
c>>>

c*** Two halves of the grid...

        ix=0
        nx=0
        do ihg=1,2      ! {
          nxx(ihg)=nqp(ihg,1,1)+nqp(ihg,2,1)+nqp(ihg,3,1)
          nx=nx+nxx(ihg)
          if(nx.ge.nxmax) then
            write(*,*) 'Grid is too large in poloial direction: nx=',
     ,                                           nx+1,' > nxmax=',nxmax
            write(*,*) '==> Increase npmamx in CARREDIM.F'
            stop
          end if
c*** Three poloidal zones
          do ipz=1,3      ! {
            iy=0
            ixx=ix
c*** Three radial zones
            do irz=1,3      ! {
              ix=ixx
              iyy=iy
c*** Assign the indices and counters
              irg=kq(ihg,ipz,irz)
              lr=lqr(ihg,ipz,irz)
              lp=lqp(ihg,ipz,irz)
              mr=mqr(ihg,ipz,irz)
              mp=mqp(ihg,ipz,irz)
              iir=1
              if(mr.lt.lr) iir=-1
              iip=1
              if(mp.lt.lp) iip=-1
              mp=mp-iip
              mr=mr-iir
c<<<
c              write(0,'(11i5)') ihg,ipz,irz,irg,ix,lp,lr,mp,mr,iip,iir
c>>>

c*** Calculate the grid data for the zone
              do j=lp,mp,iip      ! {
                iy=iyy
                do i=lr,mr,iir      ! {
                  crx(ix,iy,0)=r(j,i,irg)
                  crx(ix,iy,1)=r(j+iip,i,irg)
                  crx(ix,iy,2)=r(j,i+iir,irg)
                  crx(ix,iy,3)=r(j+iip,i+iir,irg)
                  cry(ix,iy,0)=z(j,i,irg)
                  cry(ix,iy,1)=z(j+iip,i,irg)
                  cry(ix,iy,2)=z(j,i+iir,irg)
                  cry(ix,iy,3)=z(j+iip,i+iir,irg)
                  fpsi(ix,iy,0)=psi(j,i,irg)
                  fpsi(ix,iy,1)=psi(j+iip,i,irg)
                  fpsi(ix,iy,2)=psi(j,i+iir,irg)
                  fpsi(ix,iy,3)=psi(j+iip,i+iir,irg)
                  psidx(ix,iy,0)=psidxm(j,i,irg)
                  psidx(ix,iy,1)=psidxm(j+iip,i,irg)
                  psidx(ix,iy,2)=psidxm(j,i+iir,irg)
                  psidx(ix,iy,3)=psidxm(j+iip,i+iir,irg)
                  psidy(ix,iy,0)=psidym(j,i,irg)
                  psidy(ix,iy,1)=psidym(j+iip,i,irg)
                  psidy(ix,iy,2)=psidym(j,i+iir,irg)
                  psidy(ix,iy,3)=psidym(j+iip,i+iir,irg)
                  ffbz(ix,iy,0)=twopi*b0r0
                  ffbz(ix,iy,1)=twopi*b0r0
                  ffbz(ix,iy,2)=twopi*b0r0
                  ffbz(ix,iy,3)=twopi*b0r0
                  iy=iy+1     ! }
                end do
                ix=ix+1     ! }
              end do      ! }
            end do      ! }
          end do
c*** Reserve room for the guard cells
          ix=ix+2     ! }
        end do

c*** periphery
        ix=-1
        do ihg=1,2      ! {
c<<<
c        write(0,*) 'b2agfz: call periph. ix,nxx=',ix,nxx(ihg)
c>>>
c*** This subroutine creates the guard cells around the grid block

          call periph(nxx(ihg),ny,
     ,                crx(ix,-1,0),cry(ix,-1,0),fpsi(ix,-1,0),
     ,                ffbz(ix,-1,0),psidx(ix,-1,0),psidy(ix,-1,0),
     ,                                                del,nxmax,nymax)
          ix=ix+nxx(1)+2      ! }
        end do
        nx=nx+2
c<<<
c        write(0,*) 'After periph. nx = ',nx
c>>>

c*** Determine the cut positions and lengths

        ncut=4
        if(ncut.gt.ncutmx) then ! {
          write(*,*) 'Too many cuts. ncut,ncutmx =',ncut,ncutmx
          write(*,*) '==> Increase ncutmx in tradui.F'
          do i=1,ncutmx
            nxcut(i)=0
            nycut(i)=0
          end do  ! }
        else  ! {
          nxcut(1)=nptseg(6)-1
          nxcut(2)=nxcut(1)+nptseg(4)-1
          nxcut(3)=nxcut(2)+nptseg(2)-1+2+nptseg(1)-1
          nxcut(4)=nxcut(3)+nptseg(3)-1
          if(index(equtype,'-down').gt.0) then
            nycut(1)=nqr(1,1,1)
            nycut(2)=nycut(1)+nqr(1,1,2)
          else
            nycut(2)=nqr(1,1,1)
            nycut(1)=nycut(2)+nqr(1,1,2)
          end if
          nycut(3)=nycut(2)
          nycut(4)=nycut(1)     ! }
        end if
        niso=1
        nxiso(1)=nxx(1)+1

c<<<
        write(0,*) 'nx, ncut, niso = ',nx, ncut, niso
        write(0,*) 'nxcut :  ',(nxcut(i),i=1,ncut)
        write(0,*) 'nycut :  ',(nycut(i),i=1,ncut)
        write(0,*) 'nxiso :  ',(nxiso(i),i=1,niso)
c>>>
c---------------------------------------------------------------------- ! }
      else if(index(equtype,'sn-down').gt.0 .or.
     .        index(equtype,'sn-up').gt.0) then ! {
c----------------------------------------------------------------------
c  construct the mesh for the bottom single null geometry

        nx=nptseg(1)+nptseg(2)+nptseg(3)-3
        ny=nprad(1)+nprad(2)-2
        npol2g=nptseg(2)-1
        npol2d=nptseg(1)-1
        npol3=nptseg(3)-1
        npol1=nppol(1)-1
        npol2=npol2g+npol2d
c***
c       print*,'npol2g=',npol2g
c       print*,'npol2d=',npol2d
c       print*,'npol3=',npol3
c***
c
c  test values of nprad compared to nx and ny
        if(npol1.ne.nx) then    ! {
          write(6,*)'error in b2agfz: npol1=',npol1,' nx=',nx
          stop      ! }
        else if(nprad(2).ne.nprad(3)) then      ! {
          write(6,*)'error in b2agfz: nprad(2)=',nprad(2),
     .      ' .ne. nprad(3)=',nprad(3)
          stop      ! }
        end if

c  edge region
c***
c       print*,'r=',r
c       print*,'z=',z
c       print*,'psi=',psi
c       print*,'psidxm=',psidxm
c***
        ireg=1
        do ipol=1,npol1     ! {
          ix=nx-ipol
          do irad=1,nprad(ireg)-1     ! {
            iy=(nprad(2)-2)+irad
            crx(ix,iy,0)=r(ipol+1,irad,ireg)
            cry(ix,iy,0)=z(ipol+1,irad,ireg)
            fpsi(ix,iy,0)=psi(ipol+1,irad,ireg)
            ffbz(ix,iy,0)=twopi*b0r0
            psidx(ix,iy,0)=psidxm(ipol+1,irad,ireg)
            psidy(ix,iy,0)=psidym(ipol+1,irad,ireg)
            crx(ix,iy,1)=r(ipol,irad,ireg)
            cry(ix,iy,1)=z(ipol,irad,ireg)
            fpsi(ix,iy,1)=psi(ipol,irad,ireg)
            ffbz(ix,iy,1)=twopi*b0r0
            psidx(ix,iy,1)=psidxm(ipol,irad,ireg)
            psidy(ix,iy,1)=psidym(ipol,irad,ireg)
            crx(ix,iy,2)=r(ipol+1,irad+1,ireg)
            cry(ix,iy,2)=z(ipol+1,irad+1,ireg)
            fpsi(ix,iy,2)=psi(ipol+1,irad+1,ireg)
            ffbz(ix,iy,2)=twopi*b0r0
            psidx(ix,iy,2)=psidxm(ipol+1,irad+1,ireg)
            psidy(ix,iy,2)=psidym(ipol+1,irad+1,ireg)
            crx(ix,iy,3)=r(ipol,irad+1,ireg)
            cry(ix,iy,3)=z(ipol,irad+1,ireg)
            fpsi(ix,iy,3)=psi(ipol,irad+1,ireg)
            ffbz(ix,iy,3)=twopi*b0r0
            psidx(ix,iy,3)=psidxm(ipol,irad+1,ireg)
            psidy(ix,iy,3)=psidym(ipol,irad+1,ireg)     ! }
          end do      ! }
        end do
c  private region: left side
        ireg=2
        do ipol=npol2,npol2d+1,-1     ! {
          ix=npol2-ipol
          do irad=1,nprad(ireg)-1     ! {
            iy=(nprad(ireg)-1)-irad
            crx(ix,iy,0)=r(ipol+1,irad+1,ireg)
            cry(ix,iy,0)=z(ipol+1,irad+1,ireg)
            fpsi(ix,iy,0)=psi(ipol+1,irad+1,ireg)
            ffbz(ix,iy,0)=twopi*b0r0
            psidx(ix,iy,0)=psidxm(ipol+1,irad+1,ireg)
            psidy(ix,iy,0)=psidym(ipol+1,irad+1,ireg)
            crx(ix,iy,1)=r(ipol,irad+1,ireg)
            cry(ix,iy,1)=z(ipol,irad+1,ireg)
            fpsi(ix,iy,1)=psi(ipol,irad+1,ireg)
            ffbz(ix,iy,1)=twopi*b0r0
            psidx(ix,iy,1)=psidxm(ipol,irad+1,ireg)
            psidy(ix,iy,1)=psidym(ipol,irad+1,ireg)
            crx(ix,iy,2)=r(ipol+1,irad,ireg)
            cry(ix,iy,2)=z(ipol+1,irad,ireg)
            fpsi(ix,iy,2)=psi(ipol+1,irad,ireg)
            ffbz(ix,iy,2)=twopi*b0r0
            psidx(ix,iy,2)=psidxm(ipol+1,irad,ireg)
            psidy(ix,iy,2)=psidym(ipol+1,irad,ireg)
            crx(ix,iy,3)=r(ipol,irad,ireg)
            cry(ix,iy,3)=z(ipol,irad,ireg)
            fpsi(ix,iy,3)=psi(ipol,irad,ireg)
            ffbz(ix,iy,3)=twopi*b0r0
            psidx(ix,iy,3)=psidxm(ipol,irad,ireg)
            psidy(ix,iy,3)=psidym(ipol,irad,ireg)     ! }
          end do      ! }
        end do
c  central region
        ireg=3
        do ipol=npol3,1,-1      ! {
          ix=npol2g+(npol3-ipol)
          do irad=1,nprad(ireg)-1     ! {
            iy=(nprad(ireg)-1)-irad
            crx(ix,iy,0)=r(ipol+1,irad+1,ireg)
            cry(ix,iy,0)=z(ipol+1,irad+1,ireg)
            fpsi(ix,iy,0)=psi(ipol+1,irad+1,ireg)
            ffbz(ix,iy,0)=twopi*b0r0
            psidx(ix,iy,0)=psidxm(ipol+1,irad+1,ireg)
            psidy(ix,iy,0)=psidym(ipol+1,irad+1,ireg)
            crx(ix,iy,1)=r(ipol,irad+1,ireg)
            cry(ix,iy,1)=z(ipol,irad+1,ireg)
            fpsi(ix,iy,1)=psi(ipol,irad+1,ireg)
            ffbz(ix,iy,1)=twopi*b0r0
            psidx(ix,iy,1)=psidxm(ipol,irad+1,ireg)
            psidy(ix,iy,1)=psidym(ipol,irad+1,ireg)
            crx(ix,iy,2)=r(ipol+1,irad,ireg)
            cry(ix,iy,2)=z(ipol+1,irad,ireg)
            fpsi(ix,iy,2)=psi(ipol+1,irad,ireg)
            ffbz(ix,iy,2)=twopi*b0r0
            psidx(ix,iy,2)=psidxm(ipol+1,irad,ireg)
            psidy(ix,iy,2)=psidym(ipol+1,irad,ireg)
            crx(ix,iy,3)=r(ipol,irad,ireg)
            cry(ix,iy,3)=z(ipol,irad,ireg)
            fpsi(ix,iy,3)=psi(ipol,irad,ireg)
            ffbz(ix,iy,3)=twopi*b0r0
            psidx(ix,iy,3)=psidxm(ipol,irad,ireg)
            psidy(ix,iy,3)=psidym(ipol,irad,ireg)     ! }
          end do      ! }
        end do
c  private region: right side
        ireg=2
        do ipol=1,npol2d      ! {
          ix=nx-ipol
          do irad=1,nprad(ireg)-1     ! {
            iy=(nprad(ireg)-1)-irad
            crx(ix,iy,0)=r(ipol+1,irad+1,ireg)
            cry(ix,iy,0)=z(ipol+1,irad+1,ireg)
            fpsi(ix,iy,0)=psi(ipol+1,irad+1,ireg)
            ffbz(ix,iy,0)=twopi*b0r0
            psidx(ix,iy,0)=psidxm(ipol+1,irad+1,ireg)
            psidy(ix,iy,0)=psidym(ipol+1,irad+1,ireg)
            crx(ix,iy,1)=r(ipol,irad+1,ireg)
            cry(ix,iy,1)=z(ipol,irad+1,ireg)
            fpsi(ix,iy,1)=psi(ipol,irad+1,ireg)
            ffbz(ix,iy,1)=twopi*b0r0
            psidx(ix,iy,1)=psidxm(ipol,irad+1,ireg)
            psidy(ix,iy,1)=psidym(ipol,irad+1,ireg)
            crx(ix,iy,2)=r(ipol+1,irad,ireg)
            cry(ix,iy,2)=z(ipol+1,irad,ireg)
            fpsi(ix,iy,2)=psi(ipol+1,irad,ireg)
            ffbz(ix,iy,2)=twopi*b0r0
            psidx(ix,iy,2)=psidxm(ipol+1,irad,ireg)
            psidy(ix,iy,2)=psidym(ipol+1,irad,ireg)
            crx(ix,iy,3)=r(ipol,irad,ireg)
            cry(ix,iy,3)=z(ipol,irad,ireg)
            fpsi(ix,iy,3)=psi(ipol,irad,ireg)
            ffbz(ix,iy,3)=twopi*b0r0
            psidx(ix,iy,3)=psidxm(ipol,irad,ireg)
            psidy(ix,iy,3)=psidym(ipol,irad,ireg)     ! }
          end do      ! }
        end do
c  periphery
        call periph(nx,ny,crx,cry,fpsi,ffbz,psidx,psidy,del,nxmax,nymax)

c*** Specify the cuts
        ncut=2
        if(ncut.gt.ncutmx) then ! {
          write(*,*) 'Too many cuts. ncut,ncutmx =',ncut,ncutmx
          write(*,*) '==> Increase ncutmx in tradui.F'
          do i=1,ncutmx
            nxcut(i)=0
            nycut(i)=0
          end do  ! }
        else  ! {
          if(index(equtype,'-down').gt.0) then
            nxcut(1)=nptseg(2)-1
          else
            nxcut(1)=nptseg(1)-1
          end if
          nxcut(2)=nxcut(1)+nptseg(3)-1
          nycut(1)=nprad(3)-1
          nycut(2)=nycut(1)  ! }
        end if
c
c----------------------------------------------------------------------  ! }
      else if(index(equtype,'limiter').gt.0) then     ! {
c----------------------------------------------------------------------
c  construct the mesh for a limiter geometry

        nx=nptseg(1)-1
        ny=nprad(1)+nprad(2)-2
        npol1=nppol(1)-1
        npol2=npol1
        npol2g=0
c***
c       print*,'npol2g=',npol2g
c       print*,'npol2d=',npol2d
c       print*,'npol2=',npol2
c***
c
c  test values of nprad compared to nx and ny
        if(npol1.ne.nx) then      ! {
          write(6,*)'error in b2agfz: npol1=',npol1,' nx=',nx
          stop      ! }
        end if

c  edge region
c***
c       print*,'r=',r
c       print*,'z=',z
c       print*,'psi=',psi
c       print*,'psidxm=',psidxm
c***
        ireg=1
        do ipol=1,npol1     ! {
          ix=nx-ipol
          do irad=1,nprad(ireg)-1     ! {
            iy=(nprad(2)-2)+irad
            crx(ix,iy,0)=r(ipol+1,irad,ireg)
            cry(ix,iy,0)=z(ipol+1,irad,ireg)
            fpsi(ix,iy,0)=psi(ipol+1,irad,ireg)
            ffbz(ix,iy,0)=twopi*b0r0
            psidx(ix,iy,0)=psidxm(ipol+1,irad,ireg)
            psidy(ix,iy,0)=psidym(ipol+1,irad,ireg)
            crx(ix,iy,1)=r(ipol,irad,ireg)
            cry(ix,iy,1)=z(ipol,irad,ireg)
            fpsi(ix,iy,1)=psi(ipol,irad,ireg)
            ffbz(ix,iy,1)=twopi*b0r0
            psidx(ix,iy,1)=psidxm(ipol,irad,ireg)
            psidy(ix,iy,1)=psidym(ipol,irad,ireg)
            crx(ix,iy,2)=r(ipol+1,irad+1,ireg)
            cry(ix,iy,2)=z(ipol+1,irad+1,ireg)
            fpsi(ix,iy,2)=psi(ipol+1,irad+1,ireg)
            ffbz(ix,iy,2)=twopi*b0r0
            psidx(ix,iy,2)=psidxm(ipol+1,irad+1,ireg)
            psidy(ix,iy,2)=psidym(ipol+1,irad+1,ireg)
            crx(ix,iy,3)=r(ipol,irad+1,ireg)
            cry(ix,iy,3)=z(ipol,irad+1,ireg)
            fpsi(ix,iy,3)=psi(ipol,irad+1,ireg)
            ffbz(ix,iy,3)=twopi*b0r0
            psidx(ix,iy,3)=psidxm(ipol,irad+1,ireg)
            psidy(ix,iy,3)=psidym(ipol,irad+1,ireg)     ! }
          end do      ! }
        end do
c  central region
        ireg=2
        do ipol=npol2,1,-1      ! {
          ix=npol2g+(npol2-ipol)
          do irad=1,nprad(ireg)-1     ! {
            iy=(nprad(ireg)-1)-irad
            crx(ix,iy,0)=r(ipol+1,irad+1,ireg)
            cry(ix,iy,0)=z(ipol+1,irad+1,ireg)
            fpsi(ix,iy,0)=psi(ipol+1,irad+1,ireg)
            ffbz(ix,iy,0)=twopi*b0r0
            psidx(ix,iy,0)=psidxm(ipol+1,irad+1,ireg)
            psidy(ix,iy,0)=psidym(ipol+1,irad+1,ireg)
            crx(ix,iy,1)=r(ipol,irad+1,ireg)
            cry(ix,iy,1)=z(ipol,irad+1,ireg)
            fpsi(ix,iy,1)=psi(ipol,irad+1,ireg)
            ffbz(ix,iy,1)=twopi*b0r0
            psidx(ix,iy,1)=psidxm(ipol,irad+1,ireg)
            psidy(ix,iy,1)=psidym(ipol,irad+1,ireg)
            crx(ix,iy,2)=r(ipol+1,irad,ireg)
            cry(ix,iy,2)=z(ipol+1,irad,ireg)
            fpsi(ix,iy,2)=psi(ipol+1,irad,ireg)
            ffbz(ix,iy,2)=twopi*b0r0
            psidx(ix,iy,2)=psidxm(ipol+1,irad,ireg)
            psidy(ix,iy,2)=psidym(ipol+1,irad,ireg)
            crx(ix,iy,3)=r(ipol,irad,ireg)
            cry(ix,iy,3)=z(ipol,irad,ireg)
            fpsi(ix,iy,3)=psi(ipol,irad,ireg)
            ffbz(ix,iy,3)=twopi*b0r0
            psidx(ix,iy,3)=psidxm(ipol,irad,ireg)
            psidy(ix,iy,3)=psidym(ipol,irad,ireg)     ! }
          end do      ! }
        end do
c  periphery
        call periph(nx,ny,crx,cry,fpsi,ffbz,psidx,psidy,del,nxmax,nymax)
c----------------------------------------------------------------------  ! }
      else      ! {
c----------------------------------------------------------------------
        write(6,*) 'fatal error in b2agfz: type not recognized.'
        stop      ! }
      end if

c======================================================================
c*** Rearrange the mesh if needed, in order to have a right-handed
c*** system of coordinates

      if(
     . (crx(1,1,1)-crx(1,1,0))*(cry(1,1,2)-cry(1,1,0)) -
     - (crx(1,1,2)-crx(1,1,0))*(cry(1,1,1)-cry(1,1,0)).lt.zero) then ! {
cc<<<
c       write(0,*) 'changing the grid orientation...'
cc>>>
        do iy=-1,ny     ! {
          do ix=-1,(nx-1)/2     ! {
            i=nx-(ix+1)

            tempo=crx(ix,iy,0)
            crx(ix,iy,0)=crx(i,iy,1)
            crx(i,iy,1)=tempo
            tempo=crx(ix,iy,2)
            crx(ix,iy,2)=crx(i,iy,3)
            crx(i,iy,3)=tempo

            tempo=cry(ix,iy,0)
            cry(ix,iy,0)=cry(i,iy,1)
            cry(i,iy,1)=tempo
            tempo=cry(ix,iy,2)
            cry(ix,iy,2)=cry(i,iy,3)
            cry(i,iy,3)=tempo

            tempo=fpsi(ix,iy,0)
            fpsi(ix,iy,0)=fpsi(i,iy,1)
            fpsi(i,iy,1)=tempo
            tempo=fpsi(ix,iy,2)
            fpsi(ix,iy,2)=fpsi(i,iy,3)
            fpsi(i,iy,3)=tempo

            tempo=psidx(ix,iy,0)
            psidx(ix,iy,0)=psidx(i,iy,1)
            psidx(i,iy,1)=tempo
            tempo=psidx(ix,iy,2)
            psidx(ix,iy,2)=psidx(i,iy,3)
            psidx(i,iy,3)=tempo

            tempo=psidy(ix,iy,0)
            psidy(ix,iy,0)=psidy(i,iy,1)
            psidy(i,iy,1)=tempo
            tempo=psidy(ix,iy,2)
            psidy(ix,iy,2)=psidy(i,iy,3)
            psidy(i,iy,3)=tempo

            if(i.ne.ix) then      ! {
              tempo=crx(ix,iy,1)
              crx(ix,iy,1)=crx(i,iy,0)
              crx(i,iy,0)=tempo
              tempo=crx(ix,iy,3)
              crx(ix,iy,3)=crx(i,iy,2)
              crx(i,iy,2)=tempo

              tempo=cry(ix,iy,1)
              cry(ix,iy,1)=cry(i,iy,0)
              cry(i,iy,0)=tempo
              tempo=cry(ix,iy,3)
              cry(ix,iy,3)=cry(i,iy,2)
              cry(i,iy,2)=tempo

              tempo=fpsi(ix,iy,1)
              fpsi(ix,iy,1)=fpsi(i,iy,0)
              fpsi(i,iy,0)=tempo
              tempo=fpsi(ix,iy,3)
              fpsi(ix,iy,3)=fpsi(i,iy,2)
              fpsi(i,iy,2)=tempo

              tempo=psidx(ix,iy,1)
              psidx(ix,iy,1)=psidx(i,iy,0)
              psidx(i,iy,0)=tempo
              tempo=psidx(ix,iy,3)
              psidx(ix,iy,3)=psidx(i,iy,2)
              psidx(i,iy,2)=tempo

              tempo=psidy(ix,iy,1)
              psidy(ix,iy,1)=psidy(i,iy,0)
              psidy(i,iy,0)=tempo
              tempo=psidy(ix,iy,3)
              psidy(ix,iy,3)=psidy(i,iy,2)
              psidy(i,iy,2)=tempo     ! }
            end if      ! }
          end do      ! }
        end do      ! }
      end if

* ..return
      return
c======================================================================
      end
