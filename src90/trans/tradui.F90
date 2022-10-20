program tradui
  !
  !  version : 12.01.99 13:28
  !
  !======================================================================
  !ank -- The comments are translated from French, sorry for errors!

  !*** This program translates the grid created with carre into other
  !*** grid formats
  !======================================================================
  use KindDefinitions
  use b2mod_connectivity
  use b2mod_grid_mapping
  use b2mod_indirect
  use b2mod_geo
  use b2mod_geo2
  use carre_types
  use tradui_constants
  use b2ag_ghostcells
  use carre_b2ag
  use carre_parameter_io
#if defined(USE_ITMCARRE) && defined(ITM_ENVIRONMENT_LOADED)
  use carre_constants
#endif

  implicit none

  !ank-970707: dimensions from the file
  !  dimensions
#include <CARREDIM.F>

  !  variables locales
  integer :: nxmx, nymx, ncutmx
  parameter(nxmx=npmamx,nymx=nrmamx,ncutmx=4)
  integer nin,nout,nfin,nreg,isel,nppol(nregmx),nprad(nregmx), &
      & ifail,nx,ny,nnx,nny, &
      & ncut,nxcut(ncutmx),nycut(ncutmx),niso,nxiso(nisomx+1), &
      & cflag(npmamx,nrmamx,nregmx,CARREOUT_NCELLFLAGS)
  integer b2cflag(-1:nxmx,-1:nymx,CARREOUT_NCELLFLAGS)
  real(rKind) :: r(npmamx,nrmamx,nregmx),z(npmamx,nrmamx,nregmx), &
      &  psi(npmamx,nrmamx,nregmx),psidxm(npmamx,nrmamx,nregmx), &
      &  psidym(npmamx,nrmamx,nregmx),distxo
  real(rKind) :: b0r0, &
      &  psidx(-1:nxmx,-1:nymx,0:3),psidy(-1:nxmx,-1:nymx,0:3),&
      &  psi_tmp(-1:nxmx,-1:nymx,0:4,0:2),&
      &  crx_tmp(-1:nxmx,-1:nymx,0:4),cry_tmp(-1:nxmx,-1:nymx,0:4)
#ifdef USE_SILO
   real(rKind) :: silo_tmp(-1:nxmx,-1:nymx)
#endif

  character nom*80

  type(CarreParameters) :: par

  type(B2GridMap) :: b2gd
  integer :: inseltop, inselbot
  integer, parameter :: istyle = -1 ! hard-wired to DG format

  logical, parameter :: ITM_OUTPUT_GHOSTCELLS = .false.


  ! variables for UAL I/O
#if defined(USE_ITMCARRE) && defined(ITM_ENVIRONMENT_LOADED)
  type(type_complexgrid) :: itmgrid
  type (type_edge),pointer :: cpoedge(:) => null()
  integer :: idx, shot, run, ix, iy
  double precision :: time
#endif

  logical, parameter :: ITM_DO_CLASSICAL_GHOSTCELLS = .false.


  !  procedures
  external limail, ecrim1, b2agfz, ecrim2, ecrim3, &
      &        ecrim4, ecrim5, ecrim2_oldformat, ecrim3_extended, &
      &        xertst
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
101 format(a)
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
  if(nreg.eq.2) then
    periodic_bc = 1
  else
    periodic_bc=0
  endif

  !* 4.   Select the output format

  write(6,*) 'Select the output format'
  write(6,*) '1: standard mailtri format'
  write(6,*) '2: format B2.5'
  write(6,*) '3: format SONNET-DIVIMP'
  write(6,*) '4: format DG-SONNET-B2-EIRENE'
  write(6,*) '5: revised DIVIMP with grid parameters and PSI values'
#if defined(USE_ITMCARRE) && defined(ITM_ENVIRONMENT_LOADED)
  write(6,*) '6: Write ITM CPO'
#endif
#ifdef USE_SILO
  write(6,*) '7: Write SILO file'
#endif
  write(6,*) '8: format B2.5 (old style)'
  write(6,*) '9: format SONNET-DIVIMP, only internal cells'
  read(5,*) isel
  write(6,*) 'The format chosen is :',isel

  !* 5.   Write the translated mesh to the output file
  call alloc_b2mod_geo(nxmx,nymx)
  call alloc_b2mod_indirect(nxmx,nymx,nncutmax)
  call xertst(nncutmax.eq.ncutmx, 'Unexpected maximum number of cuts !')
  if(isel.eq.1) then
      !
      ! Mailtri format
      !
      call ecrim1(nfin,r,z,par%nptseg,nreg,nppol,nprad,npmamx, &
          &  nrmamx)
  elseif(isel.eq.8) then
      call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
           r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
           par%nptseg,psidx,psidy,psi,psidxm,psidym,cflag,b0r0, &
           ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
      call carre_b2agbb (nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),bb(-1:nx,-1:ny,:), &
           &    crx(-1:nx,-1:ny,:),psidx(-1:nx,-1:ny,:),psidy(-1:nx,-1:ny,:))
!!$        call b2agbb (nx,ny,fpsi,ffbz,bb, &
!!$          crx,psidx,psidy,nxmx,nymx)
      call ecrim2_oldformat(nfin,nx,ny,&
           & crx(-1:nxmx,-1:nymx,0:3),&
           & cry(-1:nxmx,-1:nymx,0:3),bb(-1:nxmx,-1:nymx,0:3),nxmx,nymx)
  elseif(isel.eq.2) then
      !
      ! B2.5 Format (Carre script default)
      !
      call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
          & r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
          & par%nptseg,psidx,psidy,&
          & psi,psidxm,psidym,cflag,b0r0, &
          & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.false.)
      call carre_b2agbb (nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),bb(-1:nx,-1:ny,:), &
          &    crx(-1:nx,-1:ny,:),psidx(-1:nx,-1:ny,:),psidy(-1:nx,-1:ny,:))
      ! We do not create guard cells, but b2agfz still places
      ! the real cells starting at (0,0) (i.e. the x/y=-1 slots are empty).
      ! So here, the grid is actually stored in x=0:nx-1, y=0:ny-1, and
      ! this is what is written out to file in ecrim2
      ! Unused cells are marked as b2cflag=GRID_UNDEFINED
      call ecrim2(nfin,nx,ny,&
          & crx(0:nxmx,0:nymx,0:3),&
          & cry(0:nxmx,0:nymx,0:3),&
          & fpsi(0:nxmx,0:nymx,0:3,0),&
          & psidx(0:nxmx,0:nymx,0:3),psidy(0:nxmx,0:nymx,0:3),&
          & bb(0:nxmx,0:nymx,0:3),&
          & ffbz(0:nxmx,0:nymx,0:3),&
          & b2cflag(0:nxmx,0:nymx,:),nxmx,nymx,&
          & niso,nxiso,nisomx)
  elseif(isel.eq.3) then
      !
      ! Original SONNET/DIVIMP format
      !
      call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
          & r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
          & par%nptseg,psidx,psidy,&
          & psi,psidxm,psidym,cflag,b0r0, &
          & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
      call carre_b2agbb (nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),bb(-1:nx,-1:ny,:), &
          &    crx(-1:nx,-1:ny,:),psidx(-1:nx,-1:ny,:),psidy(-1:nx,-1:ny,:))
      call ecrim3(nfin,nx,ny,crx,cry,bb,nxmx,nymx)
      call ecrim3_extended(nfin,nx,ny,crx,cry,bb,nxmx,nymx,b2cflag)
  elseif(isel.eq.9) then
      !
      ! Original SONNET/DIVIMP format, only internal cells
      !
      call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
          & r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
          & par%nptseg,psidx,psidy,&
          & psi,psidxm,psidym,cflag,b0r0, &
          & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
      call carre_b2agbb (nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),bb(-1:nx,-1:ny,:), &
          &    crx(-1:nx,-1:ny,:),psidx(-1:nx,-1:ny,:),psidy(-1:nx,-1:ny,:))
      call ecrim3_extended(nfin,nx,ny,crx,cry,bb,nxmx,nymx,b2cflag)
  elseif(isel.eq.4) then
      !
      !*** B2-Sonnet-DG format
      !
      call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
          & r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
          & par%nptseg,psidx,psidy,&
          & psi,psidxm,psidym,cflag,b0r0, &
          & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
      call carre_b2agbb (nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),bb(-1:nx,-1:ny,:), &
          &    crx(-1:nx,-1:ny,:),psidx(-1:nx,-1:ny,:),psidy(-1:nx,-1:ny,:))
      call ecrim4(nfin,nx,ny,crx,cry,bb,b0r0,nxmx,nymx, &
          &                                ncut,nxcut,nycut,niso,nxiso)
  elseif(isel.eq.5) then
      !
      ! Revised DIVIMP format with additional grid information and PSI values
      !
      call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
          & r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
          & par%nptseg,psidx,psidy,&
          & psi,psidxm,psidym,cflag,b0r0, &
          & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)
      call carre_b2agbb (nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),bb(-1:nx,-1:ny,:), &
          &    crx(-1:nx,-1:ny,:),psidx(-1:nx,-1:ny,:),psidy(-1:nx,-1:ny,:))
      ! jdemod - added fpsi to call to ecrim5
      call ecrim5(nfin,nx,ny,crx,cry,bb,fpsi,nxmx,nymx)
  elseif(isel >= 6 .and. isel <= 7) then

      ! assemble the crx, cry arrays
      ! the same applies here as in case 2 w.r.t. grid extent: no ghost cells,
      ! storage is in 0:nx-1, 0:ny-1
      call b2agfz(nnx,nny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, &
          & r,z,nreg,nregmx,nppol,nprad,npmamx,nrmamx, &
          & par%nptseg,psidx,psidy,&
          & psi,psidxm,psidym,cflag,b0r0, &
          & ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,&
          & ITM_DO_CLASSICAL_GHOSTCELLS)
      ! This gives crx, cry, fpsi, ffbz, psidx, psidy, b2cflag

      ! nnx, nny is the size of the region actually filled with data

      if (ITM_DO_CLASSICAL_GHOSTCELLS) then
          nx = nnx
          ny = nny
      else
          ! Figure out real grid dimension nx, ny
          call computeGridSizeWithGhostCells(nnx, nny, niso, nx, ny)

          ! Compute magnetic field with the same recipe as carre...
          call carre_b2agbb(nx,ny,fpsi(-1:nx,-1:ny,:,0),ffbz(-1:nx,-1:ny,:),&
               & bb(-1:nx,-1:ny,0:3), &
               & crx(-1:nx,-1:ny,0:3), &
               & psidx(-1:nx,-1:ny,0:3), psidy(-1:nx,-1:ny,0:3) )

          ! add the ghost cells
          psi_tmp(-1:nx,-1:ny,0:3,0) = fpsi(-1:nx,-1:ny,:,0)
          psi_tmp(-1:nx,-1:ny,0:3,1) = psidx(-1:nx,-1:ny,:)
          psi_tmp(-1:nx,-1:ny,0:3,2) = psidy(-1:nx,-1:ny,:)
          crx_tmp(-1:nx,-1:ny,1:4) = crx(-1:nx,-1:ny,:)
          cry_tmp(-1:nx,-1:ny,1:4) = cry(-1:nx,-1:ny,:)

          call create_guard_cells(nnx, nny, nx, ny, niso, nxiso, &
              & crx_tmp(-1:nx,-1:ny,:), cry_tmp(-1:nx,-1:ny,:), &
              & psi_tmp(-1:nx,-1:ny,:,:), &
              & bb(-1:nx,-1:ny,0), bb(-1:nx,-1:ny,2),&
              & ffbz(-1:nx,-1:ny,:), &
              & b2cflag(-1:nx,-1:ny,:), withSourceOffset=.true.)

          fpsi(-1:nx,-1:ny,0:3,0) = psi_tmp(-1:nx,-1:ny,0:3,0)
          psidx(-1:nx,-1:ny,0:3) = psi_tmp(-1:nx,-1:ny,0:3,1)
          psidy(-1:nx,-1:ny,0:3) = psi_tmp(-1:nx,-1:ny,0:3,2)
          crx(-1:nx,-1:ny,:) = crx_tmp(-1:nx,-1:ny,1:4)
          cry(-1:nx,-1:ny,:) = cry_tmp(-1:nx,-1:ny,1:4)

!!$          !     ...and transfer it to ghost cells, with sanity checks
!!$          do iy=0,ny+1
!!$             do ix=0,nx+1
!!$                select case( b2cflag(ix,iy,CELLFLAG_TYPE) )
!!$                case(GRID_GUARD)
!!$                   bb(ix,iy,0) = bb_tmp(ix,iy,0)
!!$                   bb(ix,iy,2) = bb_tmp(ix,iy,2)
!!$                case(GRID_INTERNAL, GRID_BOUNDARY)
!!$                   if (abs(bp(ix,iy) - bb_tmp(ix,iy,0)) > 1e-7 .or. &
!!$                        & abs(bt(ix,iy) - bb_tmp(ix,iy,2)) > 1e-7) then
!!$                      write (*,*) "b2agfs: not matching carre bb at", ix, iy
!!$                   endif
!!$                end select
!!$             enddo
!!$          enddo

      end if

      ! assemble the connectivity arrays
      call init_connectivity (nx,ny, &
          & b2cflag(-1:nx,-1:ny,:), &
          & periodic_bc,nncut,inseltop,inselbot,istyle)

      ! compute the region arrays
      call init_region_extended(nx,ny,nncut,ncutmx, &
          & leftcut,rightcut,topcut,bottomcut, &
          & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny), &
          & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
          & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny), &
          & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny), &
          & region(-1:nx,-1:ny,0:2),nnreg,resignore(-1:nx,-1:ny,1:2), &
          & periodic_bc, &
          & b2cflag(-1:nx,-1:ny,:))

      ! set up the B2<->CPO mappings
      call b2CreateMap( nx,ny,crx(-1:nx,-1:ny,:),cry(-1:nx,-1:ny,:), &
          & b2cflag(-1:nx,-1:ny,:), &
          & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny), &
          & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
          & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny), &
          & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny), &
          & ITM_OUTPUT_GHOSTCELLS, b2gd, .true.)


      if (isel == 6) then
#if defined(USE_ITMCARRE) && defined(ITM_ENVIRONMENT_LOADED)

         !
         ! CPO Output
         !

         call b2ITMFillGridDescription( b2gd, itmgrid, &
              & nx,ny,crx(-1:nx,-1:ny,:),cry(-1:nx,-1:ny,:), &
              & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny), &
              & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
              & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny), &
              & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny), &
              & nnreg, topcut, region(-1:nx,-1:ny,0:2), &
              & b2cflag(-1:nx,-1:ny,:), ITM_OUTPUT_GHOSTCELLS )

         allocate(cpoedge(1))
         allocate(cpoedge(1)%datainfo%dataprovider(1))
         cpoedge(1)%datainfo%dataprovider="IPP"
         allocate(cpoedge(1)%codeparam%codename(1))
         cpoedge(1)%codeparam%codename(1)="ITMCARRE"
         cpoedge(1)%time= 0.0D0

         cpoedge(1)%grid = itmgrid

         shot = 1
         run = 1
         time = 0.0

         call open_ual(idx, shot, run, time, &
              & nmlFile = 'ual.namelist.edge.out', doCreate = .true.)

         call euitm_put(idx,"edge",cpoedge)
         call euitm_deallocate(cpoedge)

         call close_ual(idx)
#else
         write(6,*) 'ITM CPO format not available when compiled without -DUSE_ITMCARRE !'
#endif
      else if (isel == 7) then
#ifdef USE_SILO
         call b2silo_open("traduitAAA")
         call b2silo_writeGrid( "grid", b2gd, nx, ny, &
              & b2cflag(-1:nx,-1:ny,:), &
              & crx(-1:nx,-1:ny,:), cry (-1:nx,-1:ny,:))
         silo_tmp(-1:nx,-1:ny) = b2cflag(-1:nx,-1:ny,1)
         call b2silo_writeCvScalarReal( "grid", b2gd, "cflag1", nx, ny, silo_tmp(-1:nx,-1:ny) )
         silo_tmp(-1:nx,-1:ny) = b2cflag(-1:nx,-1:ny,2)
         call b2silo_writeCvScalarReal( "grid", b2gd, "cflag2", nx, ny, silo_tmp(-1:nx,-1:ny) )
         silo_tmp(-1:nx,-1:ny) = b2cflag(-1:nx,-1:ny,3)
         call b2silo_writeCvScalarReal( "grid", b2gd, "cflag3", nx, ny, silo_tmp(-1:nx,-1:ny) )
         silo_tmp(-1:nx,-1:ny) = b2cflag(-1:nx,-1:ny,4)
         call b2silo_writeCvScalarReal( "grid", b2gd, "cflag4", nx, ny, silo_tmp(-1:nx,-1:ny) )
         silo_tmp(-1:nx,-1:ny) = b2cflag(-1:nx,-1:ny,5)
         call b2silo_writeCvScalarReal( "grid", b2gd, "cflag5", nx, ny, silo_tmp(-1:nx,-1:ny) )
         call b2silo_close()
#else
         write(6,*) 'SILO format not available when compiled without -DUSE_SILO !'
#endif
      end if
  else
      write(6,*) 'Wrong value (must be 1 to 9): isel=',isel
      stop
  endif
  call dealloc_b2mod_geo
  call dealloc_b2mod_indirect
  !======================================================================
end program tradui
