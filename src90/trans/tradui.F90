      program tradui
!
!  version : 12.01.99 13:28
!
!======================================================================
!ank -- The comments are translated from French, sorry for errors!

!*** This program translates the grid created with carre into other
!*** grid formats
!======================================================================
#ifdef USE_ITMCARRE
      use b2mod_connectivity
      use b2ITMMapping
      use euITM_schemas  
      use euITM_routines
#endif
      use carre_types
      use carre_parameter_io
      
      implicit none

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  variables locales
      integer nxmx,nymx,ncutmx,nisomx
      parameter(nxmx=npmamx,nymx=nrmamx,ncutmx=4,nisomx=1)
      integer nin,nout,nfin,nreg,isel,nppol(nregmx),nprad(nregmx), & 
           & ifail,nx,ny, & 
           & ncut,nxcut(ncutmx),nycut(ncutmx),niso,nxiso(nisomx+1), &
           & cflag(npmamx,nrmamx,nregmx,2)
      integer b2cflag(-1:nxmx,-1:nymx,2)
      real*8 r(npmamx,nrmamx,nregmx),z(npmamx,nrmamx,nregmx), & 
     &  psi(npmamx,nrmamx,nregmx),psidxm(npmamx,nrmamx,nregmx), & 
     &  psidym(npmamx,nrmamx,nregmx),distxo
      real*8 crx(-1:nxmx,-1:nymx,0:3),cry(-1:nxmx,-1:nymx,0:3), & 
     &  bb(-1:nxmx,-1:nymx,0:3),b0r0, & 
     &  fpsi(-1:nxmx,-1:nymx,0:3),ffbz(-1:nxmx,-1:nymx,0:3), & 
     &  psidx(-1:nxmx,-1:nymx,0:3),psidy(-1:nxmx,-1:nymx,0:3)
      character nom*80

      ! connectivity and cut arrays
      integer :: nncut
      integer :: leftcut(ncutmx),rightcut(ncutmx),bottomcut(ncutmx),topcut(ncutmx)
      integer, allocatable, dimension(:,:) :: leftix,leftiy,rightix,rightiy,topix,topiy,bottomix,bottomiy
      integer, parameter :: PERIODIC_BC = 0

      type(CarreParameters) :: par

#ifdef USE_ITMCARRE      
      type(B2ITMGridMap) :: b2gd
      type(type_complexgrid) :: itmgrid
      integer :: inseltop, inselbot, nnreg(0:2)
      integer, allocatable :: region(:,:,:), resignore(:,:,:)
      integer, parameter :: istyle = -1 ! hard-wired to DG format
#endif

      ! variables for UAL I/O
#ifdef USE_ITMCARRE
      type (type_edge),pointer :: cpoedge(:) => null()
      integer :: idx, shot, run, refshot, refrun
      character(len=5)::treename
#endif


!  procedures
      external limail, ecrim1, b2agfz, b2agbb, ecrim2, ecrim3, & 
     &        ecrim4
!======================================================================
!*** nregmx: maximum number of regions
!*** npmamx: maximum number of points in poloidal direction for a data
!***         region
!*** nrmamx: maximum number of points in radial direction for a data
!***         region
!*** nxmx  : maximum number of poloidal zones in a grid of B2 type
!*** nymx  : maximum number of radial zones in a grid of B2 type
!*** psidxm: psi derivatives in x in a carre grid
!*** psidx : psi derivatives in x in a grid of type B2.5
!*** ncutmx: max. number of the regular cuts in the B2 grid
!*** nisomx: max. number of the isolating cuts in the B2 grid
!*** ncut  : actual number of the regular cuts in the B2 grid
!*** niso  : actual number of the isolating cuts in the B2 grid
!*** nxcut : location of the regular cuts in the B2 grid
!*** nycut : length of the regular cuts in the B2 grid
!*** nxiso : location of the isolating cuts in the B2 grid
!======================================================================

      nin=1
      nout=7
      nfin=8

!* 1.   Open the files

      write(6,*) 'Name of the file containing the carre grid'
      read(5,101) nom
 101  format(a)
      write(6,*) nom
      open(unit=nin,file=nom,status='old')
      open(unit=nout,status='scratch')
!     open(unit=nout,file='traduit.tmp',status='unknown')
      open(unit=nfin,file='traduit.out',status='unknown')

!* 2.   Read the parameters used to create the grid

      distxo=1.0e30
      call change(par,nin,nout,ifail)

!* 3.   Read the grid

      call limail(nin,nreg,nppol,nprad,r,z,psi,psidxm,psidym, & 
           &  cflag,npmamx,nrmamx,nregmx,par%carre_format)

!* 4.   Select the output format

      write(6,*) 'Select the output format'
      write(6,*) '1: standard mailtri format'
      write(6,*) '2: format B2.5'
      write(6,*) '3: format SONNET-DIVIMP'
      write(6,*) '4: format DG-SONNET-B2-EIRENE'
      write(6,*) '5: revised DIVIMP with grid parameters and PSI values'
#ifdef USE_ITMCARRE
      write(6,*) '6: Write ITM CPO' 
#endif
      read(5,*) isel
      write(6,*) 'The format chosen is :',isel

!* 5.   Write the translated mesh to the output file
!
! Mailtri format
!
      if(isel.eq.1) then
!
        call ecrim1(nout,nfin,r,z,par%nptseg,nreg,nppol,nprad,npmamx, & 
     &  nrmamx)
!
! B2.5 Format (Carre script default)
!
      elseif(isel.eq.2) then
!
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, & 
             & r,z,nreg,nppol,nprad,npmamx,nrmamx, & 
             & par%nptseg,psidx,psidy,&
             & psi,psidxm,psidym,cflag,b0r0, & 
             & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.false.)
        call b2agbb (nx,ny,fpsi,ffbz,bb, & 
     &    crx,cry,psidx,psidy,nxmx,nymx)
        call ecrim2(nfin,nx,ny,&
             & crx(0:nxmx,0:nymx,0:3),&
             & cry(0:nxmx,0:nymx,0:3),&
             & fpsi(0:nxmx,0:nymx,0:3),&
             & bb(0:nxmx,0:nymx,0:3),&
             & b2cflag(0:nxmx,0:nymx,1:2),nxmx,nymx,&
             & niso,nxiso,nisomx)
!
! Original SONNET/DIVIMP format
!
      elseif(isel.eq.3) then
!
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, & 
             & r,z,nreg,nppol,nprad,npmamx,nrmamx, & 
             & par%nptseg,psidx,psidy,&
             & psi,psidxm,psidym,cflag,b0r0, & 
             & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
        call b2agbb (nx,ny,fpsi,ffbz,bb, & 
     &    crx,cry,psidx,psidy,nxmx,nymx)
        call ecrim3(nfin,nx,ny,crx,cry,bb,nxmx,nymx)
!
!*** B2-Sonnet-DG format
!
      elseif(isel.eq.4) then
!
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, & 
             & r,z,nreg,nppol,nprad,npmamx,nrmamx, & 
             & par%nptseg,psidx,psidy,&
             & psi,psidxm,psidym,cflag,b0r0, & 
             & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
!<<<
!        write(0,*) 'call b2agbb'
!>>>
        call b2agbb (nx,ny,fpsi,ffbz,bb, & 
     &    crx,cry,psidx,psidy,nxmx,nymx)

        ! If in extended grid mode, set up ghost cells again
        

!<<<
!        write(0,*) 'call ecrim4: nx,ny = ',nx,ny
!>>>
        call ecrim4(nfin,nx,ny,crx,cry,bb,b0r0,nxmx,nymx, & 
     &                                ncut,nxcut,nycut,niso,nxiso)
!<<<
!        write(0,*) 'after ecrim4'
!>>>
!
! Revised DIVIMP format with additional grid information and PSI values
!
      elseif(isel.eq.5) then
!
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, & 
             & r,z,nreg,nppol,nprad,npmamx,nrmamx, & 
             & par%nptseg,psidx,psidy,&
             & psi,psidxm,psidym,cflag,b0r0, & 
             & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
        call b2agbb (nx,ny,fpsi,ffbz,bb, & 
     &   crx,cry,psidx,psidy,nxmx,nymx)
! jdemod - added fpsi to call to ecrim5
        call ecrim5(nfin,nx,ny,crx,cry,bb,b0r0,fpsi,nxmx,nymx)
!
! CPO Output
!

#ifdef USE_ITMCARRE
      elseif(isel.eq.6) then

              ! assemble the crx, cry arrays
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, & 
             & r,z,nreg,nppol,nprad,npmamx,nrmamx, & 
             & par%nptseg,psidx,psidy,&
             & psi,psidxm,psidym,cflag,b0r0, & 
             & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
              
              ! allocate connectivity arrays 
              allocate( leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny),rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
                   & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny),bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny) )

              ! assemble the connectivity arrays
              call init_connectivity (nx,ny,crx(-1:nx,-1:ny,0:3),cry(-1:nx,-1:ny,0:3), &
                   & leftix,leftiy,rightix,rightiy, &
                   & topix,topiy,bottomix,bottomiy, &
                   & leftcut,rightcut,bottomcut,topcut, &
                   & PERIODIC_BC,nncut,ncutmx,inseltop, inselbot, & 
                   & geom_match_dist, istyle )
              
              ! compute the region arrays
              allocate( region(-1:nx, -1:ny, 0:2) )
              allocate( resignore(-1:nx, -1:ny, 1:2) )
              call init_region(nx,ny,nncut,ncutmx, &
                  & leftcut,rightcut,topcut,bottomcut, &
                  & leftix,rightix,rightiy,topix,topiy,bottomiy, &
                  & region,nnreg,resignore, &
                  & crx,cry,PERIODIC_BC)

              
              ! set up the B2<->CPO mappings
              call b2ITMCreateMap( nx,ny,crx(-1:nx,-1:ny,:),cry(-1:nx,-1:ny,:),&
                   & leftix,leftiy,rightix,rightiy, &
                   & topix,topiy,bottomix,bottomiy, b2gd )

              call b2ITMFillGridDescription( b2gd, itmgrid, &
                   & nx,ny,crx(-1:nx,-1:ny,:),cry(-1:nx,-1:ny,:), &
                   & leftix,leftiy,rightix,rightiy, &
                   & topix,topiy,bottomix,bottomiy, &
                   & nnreg, topcut, region )

              allocate(cpoedge(1))
              allocate(cpoedge(1)%datainfo%dataprovider(1))
              cpoedge(1)%datainfo%dataprovider="IPP"
              allocate(cpoedge(1)%codeparam%codename(1))
              cpoedge(1)%codeparam%codename(1)="ITMCARRE"
              cpoedge(1)%time= 0.0D0

              cpoedge(1)%grid = itmgrid

              shot =12
              run = 1
              refshot = 10
              refrun =0
              treename = 'euitm'

              call euitm_create_hdf5(treename, shot, run, refshot, refrun, idx)
              !call euitm_open_hdf5(treename,shot,run,idx)
              call euitm_put(idx,"edge",cpoedge)
              call euitm_deallocate(cpoedge)
#endif      
              

      else
        write(6,*) 'Wrong value (must be 1 to 5): isel=',isel
        stop
      endif
!======================================================================
      end
