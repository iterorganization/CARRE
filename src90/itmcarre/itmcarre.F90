module itmcarre

  use euITM_schemas
  use itm_assert
  use Logging
  use Helper

  use carre_main
  use carre_types
  use carre_constants
  use CarreDiagnostics
  use b2ITMMapping
  use carre_parameter_io

  implicit none

#include <CARREDIM.F>
  
contains

  Subroutine itmcarre_main(equcpo, limcpo, edgecpo)
        
    type(type_equilibrium), intent(in), pointer :: equcpo(:)
    type(type_limiter), intent(in) :: limcpo
    type(type_edge),intent(out),pointer :: edgecpo(:)

    ! internal
    integer :: ifail

    type(CarreParameters) :: par
    type(CarreEquilibrium) :: equ
    type(CarreStructures) :: struct
    type(CarreGrid) :: grid
    type(CarreDiag) :: diag


    ! 0. Initialize some defaults
    call defaut

    ! 1.1 Read code parameters (will come from type_param in the future)
    call logmsg( LOGDEBUG, "itmcarre: reading carre.dat" )

    OPEN(UNIT=9, FILE='carre.dat', STATUS='unknown')
    CALL CHANGE(par,9,0,ifail)
    if ( ifail /= 0 ) then
        stop 'itmcarre: error reading carre.dat'
    end if
    rewind(UNIT=9)

    ! 1.2 Open output file carre.out
    call carre_open_output_file()

    ! 2. Read equilibrium
    call logmsg( LOGDEBUG, "itmcarre: reading equilibrium CPO" )
    call read_equilibrium( equcpo(1), equ, par )

    ! 3. Read limiter
    call logmsg( LOGDEBUG, "itmcarre: reading limiter CPO" )
    call read_structures( limcpo, struct )

    ! 4. Call carre main program
    call logmsg( LOGDEBUG, "itmcarre: creating grid" )
    call carre_main_computation( equ, struct, par, grid, diag )

    ! 5. Put resulting grid into CPO
    call logmsg( LOGDEBUG, "itmcarre: writing edge CPO" )
    allocate( edgecpo(1) )
    call write_grid( equ, grid, par, edgecpo(1) )

  end subroutine itmcarre_main
  

  !> Transfer equilbrium grid and data from CPO into Carre data structure.
  subroutine read_equilibrium( equcpo, equ, par )
    type(type_equilibrium), intent(in) :: equcpo
    type(CarreEquilibrium), intent(out) :: equ
    type(CarreParameters), intent(inout) :: par

    call assert( equcpo % profiles_2d % grid_type(1) == '1', &
        & "read_equilibrium: Equ. CPO not in right format: no rect. grid" )
    equ%nx = size( equcpo % profiles_2d % grid % dim1 )
    equ%ny = size( equcpo % profiles_2d % grid % dim2 )

    call assert( (equ%nx <= nxmax) .and. (equ%ny <= nymax), &
        & "read_equilibrium: equilibrium grid too large" )

    equ%x(1:equ%nx) = equcpo % profiles_2d % grid % dim1
    equ%y(1:equ%ny) = equcpo % profiles_2d % grid % dim2
    equ%psi(1:equ%nx, 1:equ%ny) = equcpo % profiles_2d % psi

    ! X-points
    par%xPointNum = size(equcpo % eqgeometry % xpts % r)
    par%xPointX(1:par%xPointNum) = equcpo % eqgeometry % xpts % r(1:par%xPointNum)
    par%xPointY(1:par%xPointNum) = equcpo % eqgeometry % xpts % z(1:par%xPointNum)

    ! O-Point
    par%oPointX = equcpo % global_param % mag_axis % position % r
    par%oPointY = equcpo % global_param % mag_axis % position % z

  end subroutine read_equilibrium


  !> Transfer device structure CPO into Carre data structure.
  subroutine read_structures( limcpo, struct )
    type(type_limiter), intent(in) :: limcpo
    type(CarreStructures), intent(out) :: struct

    ! internal
    integer :: is

    struct%nstruc = size( limcpo % limiter_unit )
    call assert( struct%nstruc <= strumx, &
        & "read_structures: too large number of structures in CPO" )
    
    do is = 1, struct%nstruc
        struct%nomstr(is) = limcpo % limiter_unit(is) % name(1)(1:80)

        struct%npstru(is) = size( limcpo % limiter_unit(is) % position % r )

        call assert( struct%npstru(is) <= npstmx, &
            & "read_structures: too many points (1) in structure "//int2str(is) )

        struct%xstruc(1:struct%npstru(is), is) = limcpo % limiter_unit(is) % position % r
        struct%ystruc(1:struct%npstru(is), is) = limcpo % limiter_unit(is) % position % z
        
        if ( limcpo % limiter_unit( is ) % closed(1) == 'y' ) then
            ! Close structure: repeat first point, keep positive npstru
            struct%npstru(is) = struct%npstru(is) + 1
            call assert( struct%npstru(is) <= npstmx, &
                & "read_structures: too many points (2) in structure "//int2str(is) )

            struct%xstruc(struct%npstru(is), is) = struct%xstruc(1, is)
            struct%ystruc(struct%npstru(is), is) = struct%ystruc(1, is)
        else
            ! Open structure: invert npstru to negative
            struct%npstru(is) = - struct%npstru(is)
        end if

    end do
    
  end subroutine read_structures

  !> Transfer a grid as computed by Carre into the edge CPO.
  !> This involves assembling the grid regions and connectivity information
  !> as well as setting up the region numbering.
  subroutine write_grid(equ, grid, par, edgecpo)
    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreGrid), intent(in) :: grid

    type(type_edge), intent(out) :: edgecpo

    ! internal
    integer, parameter :: nxmx=npmamx,nymx=nrmamx,ncutmx=4,nisomx=1

    ! connectivity and cut arrays
    integer :: nncut
    integer :: leftcut(ncutmx),rightcut(ncutmx),bottomcut(ncutmx),topcut(ncutmx)
    integer, allocatable, dimension(:,:) :: leftix,leftiy,rightix,rightiy,topix,topiy,bottomix,bottomiy
    integer :: inseltop, inselbot, nnreg(0:2)
    integer, allocatable :: region(:,:,:), resignore(:,:,:)

    integer, parameter :: PERIODIC_BC = 0
    integer, parameter :: istyle = -1 ! hard-wired to DG format
    

    ! arrays for cell flags (currently not used in ITM Carre)
    integer :: b2cflag(-1:nxmax,-1:nymax,CARREOUT_NCELLFLAGS)
    integer :: cflag(npmamx,nrmamx,nreg,CARREOUT_NCELLFLAGS)

    real*8 crx(-1:nxmx,-1:nymx,0:3),cry(-1:nxmx,-1:nymx,0:3), & 
        &  bb(-1:nxmx,-1:nymx,0:3),b0r0, & 
        &  fpsi(-1:nxmx,-1:nymx,0:3),ffbz(-1:nxmx,-1:nymx,0:3), & 
        &  psidx(-1:nxmx,-1:nymx,0:3),psidy(-1:nxmx,-1:nymx,0:3)    


    integer nreg,nppol(nregmx),nprad(nregmx), & 
        &  nx,ny, & 
        &  ncut,nxcut(ncutmx),nycut(ncutmx),niso,nxiso(nisomx+1)
  
    type(B2ITMGridMap) :: gmap

    b2cflag = 0

    ! assemble the crx, cry arrays
    call b2agfz(nx,ny,crx,cry,fpsi,ffbz,b2cflag,nxmx,nymx, & 
        &    grid%xmail,grid%ymail,grid%nreg,grid%np1,par%npr,npmamx,nrmamx, & 
        &    par%nptseg,psidx,psidy,grid%psim,grid%psidxm,grid%psidym,cflag,b0r0, & 
        &    ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso,.true.)

    ! allocate connectivity arrays 
    allocate( leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny),&
        & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
        & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny),&
        & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny) )

    ! assemble the connectivity arrays
    call init_connectivity (nx,ny,crx(-1:nx,-1:ny,0:3),cry(-1:nx,-1:ny,0:3),b2cflag(-1:nx,-1:ny,:), &
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
    call b2ITMCreateMap( nx,ny,crx(-1:nx,-1:ny,:),cry(-1:nx,-1:ny,:),b2cflag(-1:nx,-1:ny,:),&
        & leftix,leftiy,rightix,rightiy, &
        & topix,topiy,bottomix,bottomiy,  gmap )

    call b2ITMFillGridDescription( gmap, edgecpo%grid, &
        & nx,ny,crx(-1:nx,-1:ny,:),cry(-1:nx,-1:ny,:), &
        & leftix,leftiy,rightix,rightiy, &
        & topix,topiy,bottomix,bottomiy, &
        & nnreg, topcut, region, b2cflag )

    allocate(edgecpo%datainfo%dataprovider(1))
    edgecpo%datainfo%dataprovider="IPP"
    allocate(edgecpo%codeparam%codename(1))
    edgecpo%codeparam%codename(1)="ITMCARRE"
    edgecpo%time= 0.0D0

  end subroutine write_grid

end module itmcarre
