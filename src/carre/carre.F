      PROGRAM CARRE
c======================================================================
c
c  version : 1.01.99 15:49
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

*..  Ce programme permet de trouver la localisation des points X
*  pour un tokamak. Il parametrise ensuite les separatrices qui passent
*  par ces points X et il les trace ainsi que les structures reelles.
*  Une maille curviligne orthogonale alignee sur les surfaces de flux
*  est alors construite.

c*** This program builds an orthogonal curvilinear mesh aligned to the
c*** flux surfaces.  It finds the localisation of the X-points,
c*** parametrises the separatrices passing through them, and traces
c*** them together with the real structures.
c======================================================================
      IMPLICIT NONE

c  a modifier pour generaliser la geometrie:
c  modifier argsep et maille pour utiliser un critere plus general que
c  la droite ou la gauche dans la designation des separatrices.
c  Aussi, distribuer les points de facon proportionelle, et les ortho-
c  gonaliser par relaxation, avec une protection de proximite.

c  a faire:
c        1. permettre l'imposition de points fixes ou de coins


*  variables.

cank-970702: moved dimensions to the CARREDIM.F
#include <CARREDIM.F>
      REAL*8 cstlin,zero,eps_Xpt,rmax,zmax
      common /geom/ eps_Xpt
      PARAMETER (cstlin=0.00, zero=0.)
      INTEGER i, j, nx, ny, nstruc, npstru(strumx),
     .   ii(gradmx), jj(gradmx), iptx(npxmx), jptx(npxmx),
     .   npx, npxtot,ptsep(4,npxmx),
     .   nptot(4,npxmx), indplq(4,npxmx),inddef(nbdmx),
     .   nbdef, nivtot(nivmx), nbniv,ptxint,nreg,
     .   np1(nregmx), npr(nregmx), ierror,iflag,limcfg,
     .   itmp, is, ip

      REAL*8 x(nxmax), y(nymax), psi(nxmax,nymax),
     .   psidx(nxmax,nymax), psidy(nxmax,nymax),
     .   xstruc(npstmx,strumx), ystruc(npstmx,strumx),
     .   pointx(gradmx), pointy(gradmx),
     .   ptx(npxmx), pty(npxmx), xpto, ypto, fctpx(npxmx),
     .   separx(npnimx,4,npxmx), separy(npnimx,4,npxmx),
     .   nivx(npnimx,nivmx),nivy(npnimx,nivmx),distnv(5,nivmx),
     .   xn(npnimx),yn(npnimx),xmail(npmamx,nrmamx,nregmx),
     .   ymail(npmamx,nrmamx,nregmx),a00(nxmax,nymax,3),
     .   a10(nxmax,nymax,3),a01(nxmax,nymax,3),a11(nxmax,nymax,3)

!     arrays to hold copies of structures (r for "real")
!     (if virtual structures are used)
      integer :: rnstruc, rnpstru(strumx)
      REAL*8 :: rxstruc(npstmx,strumx), rystruc(npstmx,strumx)

      LOGICAL racord
      character lign80*80,nomstr(strumx)*80

      REAL*8 stp0, stpmin
      PARAMETER (stp0=0.01,stpmin=0.001)

      integer :: isetup
      logical dovirtualtargets
      parameter (dovirtualtargets=.true.)

*..Procedures
*
      INTRINSIC ABS,MIN,index
      EXTERNAL pltini,pltend,cadre,motifs,DERIVE,cntour,GRAD0,
     .   SELPTX,SPTRIS,ARGSEP,FRTIER,MAILLE,trace,entete,
     ,     trc_stk_in,trc_stk_out,
     $     virtualtargets,virtuallimiters
c======================================================================
c.. nxmax,nymax: maximum number of the data points in x and y
c.. gradmx: maximum number of points where the gradient vanishes
c.. npxmx : maximum number of the X-points
c.. nbdmx : maximum number of structures being the divertor targets
c.. strumx: maximum number of structures
c.. npstmx: maximum number of points per structure
c.. npnimx: maximum number of tracing points on a curve
c.. nivmx : maximum number of limiting level lines
c.. npmamx: maximum number of grid points in poloidal direction
c.. nrmamx: maximum number of grid points in radial direction
c.. nregmx: maximum number of regions
c.. cstlin: a linear constant added along y to artificially disconnect
c           the X-points. Set to 0 for connected double-nulls
c
c.. nx,ny : number of data points in x and y
c.. x,y   : tables of coordinates of the data points
c.. psi   : psi values at each data point
c.. psidx,psidy: values of psi derivatives in x and y
c                at each data point
c.. nstruc: number of structures
c.. npstru: number of points per structure
c.. xstruc,ystruc: coordinates of the structure points
c                  (point index, structure index)
c.. npxtot: number of the points where the gradient vanishes
c.. pointx,pointy: coordinates of the points where the gradient
c                  vanishes
c.. ii,jj : x and y indices of the cells where the gradient vanishes
c.. npx   : number of the X-points
c.. ptx,pty: X-point co-ordinates
c.. iptx,jptx: x and y indices of the cells containing the X-points
c.. xpto,ypto: coordinates of the O-point
c.. racord: determines whether the X-points are connected
c.. fctpx: the psi values at each X- or O-point
c.. separx,separy: coordinates of the points of the parametrised
c               separatrices (point index, branch index, X-point index)
c.. nptot : number of parametrisation points for each separatrix
c           (separatrix index, point index)
c.. ptsep : separatrix pointer, used as index
c           (separatrix index, point index)
c.. ptxint: index of the internal X-point in the case of disconnected
c           double-null
c.. nbdef : number of the divertor plates
c.. inddef: table of indices of the divertor plates
c.. indplq: table of the structure indices (0 means not a target)
c           (separatrix index, X-point index)
c.. nbniv : number of the limiting level lines
c.. nivx,nivy: coordinates of the points of the parametrised
c              limiting level lines (point index, curve index)
c.. nivtot: number of points for each parametrised limiting level line
c.. distnv: distance along a plate between the separatrix strike-point
c           and a limiting level line
c           (distance selector [1=real, 2=psi], curve index)
c.. nreg  : number of the grid regions depending on the configuration
c.. np1   : numbers of the grid points in poloidal direction
c           (region index)
c.. npr   : numbers of the grid points in radial direction
c           (region index)
c.. xmail,ymail: grid point coordinates
c                (poloidal, radial, region)
c.. xn,yn : working array for coordinates along a parametrised curve
c.. nrelax: maximum number of iterations in the relaxation procedure
c           of construction of the orthogonal grid
c.. relax : relaxation parameter
c.. stpmin: minimum tolerable distance between any two grid points
c           in the course of relaxation
c.. rlcept: convergence criterion in the relaxation procedure
c.. limcfg: indicates when a limiter configuration is considered (when
c           non zero). This variable is assigned the index of the
c           limiter
c     N.B.: When the limiter configuration is selected, npx is set equal
c           to 1 and the coordinates of the X-point correspond to the
c           innermost point of the limiter.
c======================================================================
*
*..1.0  Initialisation des variables par defaut et de la bibliotheque
*       graphique
*
      call defaut
      CALL pltini
      CALL cadre
      CALL motifs
*
*..2.0  Open the data files
*
      OPEN(UNIT=7, FILE='rzpsi.dat', STATUS='old')
      OPEN(UNIT=8, FILE='structure.dat', STATUS='old')
      OPEN(UNIT=9, FILE='carre.dat', STATUS='unknown')
      OPEN(UNIT=10,FILE='carre.out',STATUS='unknown')
      open(unit=11,status='scratch')
c
c  2.1  specify output format
      write(10,*)'output format: carre70'

*
*..3.0  Read the data
*

*..Read the values of x

100   format(a)
      iflag=-1
      call entete(7,'$r',iflag)
      read(7,100)lign80
      i=index(lign80,'=')
      call rdfrin(11,lign80(i+1:80),nx,ierror)
      READ(7,*) (x(i), i=1, nx)

*..Read the values of y

      call entete(7,'$z',iflag)
      read(7,100)lign80
      i=index(lign80,'=')
      call rdfrin(11,lign80(i+1:80),ny,ierror)
      READ(7,*) (y(i), i=1, ny)

      rmax = 0.0
      do i = 1, nx-1
        rmax = max(rmax, (x(i+1)-x(i))**2)
      enddo
      zmax = 0.0
      do i = 1, ny-1
        zmax = max(zmax, (y(i+1)-y(i))**2)
      enddo
      eps_Xpt = sqrt(rmax+zmax)

*..Read the values of psi

      call entete(7,'$psi',iflag)
      READ(7,*) ((psi(i,j), i=1, nx), j=1, ny)

      DO 5 j=1,ny
         DO 4 i=1,nx
            psi(i,j) = psi(i,j) + MIN(cstlin*(y(j)-y(ny/2)),zero)
    4    CONTINUE
    5 CONTINUE
c---
c  ceci sert a modifier la symetrie haut-bas de psi, de facon ad hoc.
c     write(6,*)'facteur de symetrie haut-bas'
c     read(5,*)a00(1,1,1)
c     do j=1,ny/2
c     do i=1,nx
c       xpto=psi(i,j)
c       ypto=psi(i,ny-j+1)
c       psi(i,j)=0.5*((1.+a00(1,1,1))*xpto+(1.-a00(1,1,1))*ypto)
c       psi(i,ny-j+1)=0.5*((1.+a00(1,1,1))*ypto+(1.-a00(1,1,1))*xpto)
c     enddo
c     enddo
c---
      close(unit=7)

*
*..3.1  Read the structures.
*
      nstruc=0
      call listru(8,nstruc,npstru,nomstr,xstruc,ystruc,npstmx,strumx)

*
*..4.0  Calculate the first partial derivatives in x and y and store
*       them in arrays psidx and psidy

      CALL DERIVE(nx,ny,x,y,psi,psidx,psidy)

*
*..5.0  Plot the level lines for psidx=0 and psidy=0
*
      CALL cntour(psidx,psidy,nx,ny,x(1),x(nx),y(1),y(ny))
c
c  interpolation coefficients for psi and its derivatives

      call inipsi(psi,psidx,psidy,x,y,nxmax,nymax,nx,ny,a00,a10,a01,a11)

*
*..6.0  Determine the points where the derivatives in x and y vanish
*

      CALL GRAD0(nxmax,nymax,nx,ny,x,y,gradmx,pointx,
     .          pointy,ii,jj,npxtot,a00,a10,a01,a11)

*
*..7.0  Select the X-points of interest
*

cank-970702: moved dimensions to the included file
      CALL SELPTX(npxtot,npx,pointx,pointy,ii,jj,ptx,
     .            pty,iptx,jptx,xpto,ypto,racord,limcfg)

!     when using virtual targets, needs two passes through the setup
!     steps 8 to 10

      do isetup = 1, 2
*
*..8.0  Parametrise the separatrices
*
      IF (npx.GT.0 .and. limcfg.eq.0) THEN
c
        CALL SPTRIS(nx,ny,x,y,psi,npx,ptx,pty,
     .      iptx,jptx,fctpx,separx,separy,nptot,
     .      nstruc,npstru,xstruc,ystruc,indplq,inddef,nbdef,
     .      a00,a10,a01,a11)

*
*..9.0  Arrange the separatrices
*
        CALL ARGSEP(npx,ptx,pty,fctpx,separx,separy,indplq,nptot,npnimx,
     .            ptsep,racord,ptxint,ypto,nbdef,inddef)

      ELSEIF(LIMCFG.NE.0) THEN
*
*  13.   Identify the limiter
*
        call limfnd(xpto,ypto,nivx,nivy,stp0,stpmin,distnv,nivtot,
     .      nbniv,nx,ny,x,y,psi,npx,ptx,pty,fctpx,
     .      nstruc,npstru,xstruc,ystruc,indplq,inddef,nbdef,
     .      a00,a10,a01,a11)

	do itmp=1,4
	  ptsep(itmp,1) = 0
	enddo

      ENDIF

      if(npx.gt.0) then
*
*..10.0  Find the level lines in more detail
*
c<<<
	write(0,*) '=== carre *..10.0 - before frtier'
	write(0,'(5h ptx:,1p,8e12.4/(5x,8e12.4))') (ptx(i),i=1,npx)
	write(0,'(5h pty:,1p,8e12.4/(5x,8e12.4))') (pty(i),i=1,npx)
        if(limcfg.eq.0) then
	  write(0,*) 'nptot(4,nxpoints)'
	  write(0,'(1x,16i5)') ((nptot(i,j),i=1,4),j=1,npx)
	  write(0,*) 'Strike points (presumably)'
	  write(0,'(3h x:,1p,8e12.4/(3x,8e12.4))')
     -     ((separx(nptot(i,j),i,j),i=1,4),j=1,npx)
	  write(0,'(3h y:,1p,8e12.4/(3x,8e12.4))')
     -     ((separy(nptot(i,j),i,j),i=1,4),j=1,npx)
c>>>
          call trc_stk_in('carre','*..10.0')
          CALL FRTIER(nx,ny,x,y,psi,nstruc,
     .      npstru,xstruc,ystruc,inddef,nbdef,npx,separx,
     .      separy,nptot,ptsep,racord,nivx,nivy,nivtot,
     .      nbniv,stp0,stpmin,
     .      distnv,ptxint,a00,a10,a01,a11)
          call trc_stk_out
        endif

        if ( .not. dovirtualtargets ) exit
        if ( isetup == 2 ) exit

*..10.0  Set up the virtual structure

*..      Save current structures
        rnstruc = nstruc
        rnpstru = npstru
        rxstruc = xstruc
        rystruc = ystruc
        
        nstruc = 0

*..   10.1  Set up virtual targets        

        CALL VIRTUALTARGETS(nx,ny,x,y,psi,npx,ptx,pty,
     .       fctpx,separx,separy,nptot,
     .       rnstruc,rnpstru,rxstruc,rystruc,indplq,inddef,nbdef,
     .       a00,a10,a01,a11,nstruc,npstru,xstruc,ystruc)
        
*..   10.2  Set up virtual limiters
        
        call VIRTUALLIMITERS(nivx,nivy,nivtot,nbniv,npx,ptx,pty,
     $       nstruc,npstru,xstruc,ystruc)
        
*..   10.2.1 Write out resulting structures
        
        open(UNIT=100,FILE='virtualstructure.out',STATUS='unknown')
        do is = 1, nstruc
           do ip = 1, npstru( is )
              write (100,*) xstruc(ip,is)*1000,
     $             ystruc(ip,is)*1000
           enddo
           write (100,*) ''
        enddo
        close(UNIT=100)
        
      endif                     ! npx.gt.0
      
      enddo                     ! end setup loop

*
*..12.0  Grid the regions
*
      if(npx.gt.0) then

        CALL MAILLE(nx,ny,x,y,psi,npx,xpto,ypto,racord,
     .    separx,separy,ptsep,nptot,distnv,ptxint,nstruc,npstru,
     .    xstruc,ystruc,inddef,nreg,xn,yn,xmail,ymail,
     .    np1,npr,ptx,pty,nivx,nivy,nivtot,nbniv,
     .    a00,a10,a01,a11,fctpx,limcfg)

c*
c* WARNINGS CALCULATION AND OUTPUT
c*
c        if (npx.EQ.1) CALL WARNINGS(separx,separy,nivx,nivy,
c     &  distnv(1,2),xpto,ypto)

*
*..13.0  Plot the structures, the separatrices, and the limiting level
*        lines for each region, together with the resulting grid
*

        call trace(x(1),x(nx),y(1),y(ny),separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg)

      endif ! nptx.gt.0

*
*.. Close the graphics
*
      CALL pltend

      STOP
      END
