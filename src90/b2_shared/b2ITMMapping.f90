module b2ITMMapping

  ! This module provides:
  !
  ! 1. A mechanism to map from the B2 data structure to the CPO data structure,
  ! consisting of a routine to set up the map (b2ITMCreateMap), a data structure
  ! to hold the map information (B2ITMGridMap) and some service routines
  ! to handle this data structure.
  !
  ! 2. A routine (b2ITMFillGridDescription) to write the B2 grid into an ITM 
  ! grid description data structure (which usually is part of a CPO). It also
  ! sets up the default subgrids for the B2 grid.
  ! 
  ! 3. Routines to transform variables stored in the B2 data structure into
  ! the form expected CPO data structure.

  use b2mod_types , B2_R8 => R8, B2_R4 => R4
  use itm_types , ITM_R8 => R8, ITM_R4 => R4 ! IGNORE
  Use euITM_schemas ! IGNORE
  use itm_grid , ITM_GRID_UNDEFINED => GRID_UNDEFINED ! IGNORE
  use itm_string ! IGNORE
  use itm_assert ! IGNORE
  use Helper
  use Logging , only: logmsg, LOGDEBUG
  use b2mod_connectivity , REMOVED_B2_R8 => R8
  use itm_grid_structured , only: gridSetupStruct1dSpace ! IGNORE
  !use b2mod_constants
  use itm_constants , pi => itm_pi ! IGNORE
  use carre_constants
  use b2mod_cellhelper

  implicit none

  ! Distance between two points at which the points are declared to be equal
  real(ITM_R8), parameter :: geom_match_dist = 1.0e-6_ITM_R8

  ! Alignment index (for example in B2 flux arrays)
  integer, parameter :: ALIGNX = 1
  integer, parameter :: ALIGNY = 0

  ! Maximum number of special vertices expected in the grid
  integer, parameter :: MAX_SPECIAL_VERTICES = 10

  ! Constants for use with the ITM grid description

  ! Space indices 
  integer, parameter :: SPACE_POLOIDALPLANE = 1
  integer, parameter :: SPACE_TOROIDALANGLE = 2

  ! Space setup: 
  !  SPACE_COUNT = SPACE_POLOIDALPLANE: do only do the poloidal plane space,
  !  SPACE_COUNT = SPACE_TOROIDALANGLE: will do the full 3d grid with two spaces.
  integer, parameter :: SPACE_COUNT = SPACE_POLOIDALPLANE

  ! Will have at maximum so many spaces
  integer, parameter :: SPACE_COUNT_MAX = 2

  ! Number of points in toroidal direction (only 1 makes sense here, this is for playing around)
  integer, parameter :: NNODES_TOROIDAL = 1

  ! Flag controlling whether the toroidal space is set up periodic or not.
  ! If periodic, the last node is connect to the first by an edge.
  ! If not periodic, an additional node at 2 pi is added
  logical, parameter :: TOROIDAL_PERIODIC = .false.


  ! Object class tuples
  integer, dimension(SPACE_COUNT_MAX), parameter :: CLASS_NODE = (/ 0, 0 /)

  integer, dimension(SPACE_COUNT_MAX), parameter :: CLASS_RZ_EDGE = (/ 1, 0 /)
  integer, dimension(SPACE_COUNT_MAX), parameter :: CLASS_PHI_EDGE = (/ 0, 1 /)

  integer, dimension(SPACE_COUNT_MAX), parameter :: CLASS_POLOIDALRADIAL_FACE = (/ 1, 1 /)
  integer, dimension(SPACE_COUNT_MAX), parameter :: CLASS_TOROIDAL_FACE = (/ 2, 0 /)

  integer, dimension(SPACE_COUNT_MAX), parameter :: CLASS_CELL = (/ 2, 1 /)

  ! Data structure holding an intermediate grid description to be 
  ! transferred into a CPO
  type B2ITMGridMap
      integer :: ncv, nfcx, nfcy, nvx
      integer :: b2nx, b2ny

      ! Mapping arrays: 
      ! 1d CPO lists -> 2d B2 data structure ( i -> (ix, iy) )
      ! b2cv( mapCvix(i), mapCviy(i) ) = cpocv(i)
      ! b2fc( mapFcix(i), mapFciy(i), mapFciFace(i) ) = cpofc(i)  
      ! for "normal" faces, mapFcIFace will be LEFT or BOTTOM

      ! b2vx( mapVxix(i), mapVxiy(i), mapVxIVx(i) ) = cpovx(i)
      ! for "normal" vertices, mapVxIVx(i) will be 1 (lower left vertex)

      integer, dimension(:), allocatable :: mapCvix, mapCviy 
      integer, dimension(:), allocatable :: mapFcix, mapFciy, mapFcIFace
      integer, dimension(:), allocatable :: mapVxix, mapVxiy, mapVxIVx

      ! 2d B2 data structure -> 1d CPO lists ( (ix, iy) -> i )
      ! cpocv( mapCvI(ix, iy) ) = b2cv(ix, iy)
      ! cpofc( mapFcI(ix, iy, iFace) ) = b2fc(ix, iy, iFace) 
      ! cpovx( mapVxI(ix, iy, iVertex) ) = b2vx(ix, iy, iVertex)

      integer, dimension(:,:), allocatable :: mapCvI
      integer, dimension(:,:,:), allocatable :: mapFcI, mapVxI

      ! Special vertices (x-points)
      ! number of special vertices
      integer :: nsv
      ! svix, sviy : positions of special vertices in B2 data structure
      ! (lower left corner of svix, sviy cell)
      ! svi: indices of special vertices in the CPO data structure
      integer, dimension(:), allocatable :: svix, sviy, svi
  end type B2ITMGridMap

  ! Subgrid name constants

  !> Number of generic subgrids
  integer, parameter :: B2_GENERIC_SUBGRID_COUNT = 6

  ! Generic subgrids (all cells, all faces)
  ! Note: special subgrids (given by region ids) don't have specific constants (see also b2mod_connectivity.f90)

  !> Subgrid of all 2d cells
  integer, parameter :: B2_SUBGRID_CELLS = 1 
  !> Subgrid of all 0d nodes
  integer, parameter :: B2_SUBGRID_NODES = 2
  !> Subgrid of all faces in order given by grid map. First x-aligned faces, then y-aligned faces
  integer, parameter :: B2_SUBGRID_FACES = 3
  !> Subgrid of all x-aligned faces in order given by grid map. 
  integer, parameter :: B2_SUBGRID_FACES_X = 4
  !> Subgrid of all y-aligned faces in order given by grid map. 
  integer, parameter :: B2_SUBGRID_FACES_Y = 5
  !> Subgrid of all X-points
  integer, parameter :: B2_SUBGRID_XPOINTS = 6


!!$  interface b2ITMTransformDataB2ToCpo
!!$      module procedure b2ITMTransformDataB2ToCPOCell, b2ITMTransformDataB2ToCPOFace
!!$  end interface

  private :: R8

contains

  ! service routines for B2ITMGridData

  subroutine allocateB2ITMGridMap( gd, nx, ny, ncv, nfcx, nfcy, nvx )
    type(B2ITMGridMap), intent(inout) :: gd
    integer, intent(in) ::  nx, ny, ncv, nfcx, nfcy, nvx

    gd%ncv = ncv
    gd%nfcx = nfcx
    gd%nfcy = nfcy
    gd%nvx = nvx

    gd%b2nx = nx
    gd%b2ny = ny

    allocate( gd%mapCvI(-1:nx+1, -1:ny+1) )
    allocate( gd%mapFcI(-1:nx+1, -1:ny+1, 0:3) )
    allocate( gd%mapVxI(-1:nx+1, -1:ny+1, 0:3) )

    allocate( gd%mapCvix(ncv), gd%mapCviy(ncv) )
    allocate( gd%mapFcix(nfcx+nfcy), gd%mapFciy(nfcx+nfcy), gd%mapFcIFace(nfcx+nfcy) )
    allocate( gd%mapVxix(nvx), gd%mapVxiy(nvx), gd%mapVxIVx(nvx) )

    gd%nsv = 0
    allocate( gd%svix(MAX_SPECIAL_VERTICES), gd%sviy(MAX_SPECIAL_VERTICES), gd%svi(MAX_SPECIAL_VERTICES) )

  end subroutine allocateB2ITMGridMap

  subroutine deallocateB2ITMGridMap( gd )
    type(B2ITMGridMap), intent(inout) :: gd

    deallocate( gd%mapCvI )
    deallocate( gd%mapFcI )
    deallocate( gd%mapVxI )

    deallocate( gd%mapCvix, gd%mapCviy )
    deallocate( gd%mapFcix, gd%mapFciy, gd%mapFcIFace )
    deallocate( gd%mapVxix, gd%mapVxiy )

    deallocate( gd%svix, gd%sviy, gd%svi )

  end subroutine deallocateB2ITMGridMap


  !> Routine that fills in a grid description which is part of a CPO
  !> using the given grid data and prepared mappings
  subroutine b2ITMFillGridDescription( gmap, itmgrid, &
      & nx,ny,crx,cry, &
      & leftix,leftiy,rightix,rightiy, &
      & topix,topiy,bottomix,bottomiy,&
      & nnreg, topcut, region, cflag, includeGhostCells, vol, gs, qc )

    type(B2ITMGridMap), intent(in) :: gmap
    type(type_complexgrid), intent(out) :: itmgrid

    ! Size of grid arrays: (-1:nx, -1:ny) 
    integer, intent(in) :: nx, ny
    !   .. output arguments
    ! vertex coordinates
    real (ITM_R8), intent(in) :: &
        & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)
    ! B2 connectivity array
    integer, intent(in) :: &
        & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny),&
        & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny),&
        & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny),&
        & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny)
    ! B2 region & cut information
    integer, intent(in) :: &
        & nnreg(0:2), topcut(:), &
        & region(-1:gmap%b2nx,-1:gmap%b2ny,0:2)
    ! Cell flags 
    integer cflag(-1:nx,-1:ny, CARREOUT_NCELLFLAGS)
    logical, intent(in) :: includeGhostCells
    ! Optional B2 measure information
    real(ITM_R8), intent(in), optional :: vol(-1:nx,-1:ny), gs(-1:nx,-1:ny,0:2), qc(-1:nx,-1:ny)

    ! internal
    integer, parameter :: NDIM = 2

    call assert( present(gs) .EQV. present(qc) )

    call fillInGridDescription()
    call fillInSubGridDescription()

  contains 

    ! Part 1: fill in grid description
    subroutine fillInGridDescription()

      ! internal
      integer :: ivx, ifc, icv, ix, iy, nix, niy, i

      allocate( itmgrid % spaces(SPACE_COUNT) )

      ! Coordinate types
      ! (dimension of space = NDIM = size( coordtype )
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % coordtype(NDIM, 1) )    
      itmgrid % spaces(SPACE_POLOIDALPLANE) % coordtype(:, 1) &
           & = (/ COORDTYPE_R, COORDTYPE_Z /)

      ! Have two types of objects: 1d edges, 2d cells
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(NDIM + 1) )

      ! Fill in node information
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(1) % geo(gmap%nvx, NDIM, 1, 1) )
      do ivx = 1, gmap % nvx
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(1) % geo(ivx, 1, 1, 1) = &
              & crx( gmap % mapVxix( ivx ), gmap % mapVxiy( ivx ), gmap % mapVxIVx( ivx ) )
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(1) % geo(ivx, 2, 1, 1) = & 
              & cry( gmap % mapVxix( ivx ), gmap % mapVxiy( ivx ), gmap % mapVxIVx( ivx ) )
      end do

      ! Fill in object definitions (i.e. what objects compose an object)

      ! 1d objects: faces
      ! ...have two boundaries
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( gmap%nfcx + gmap%nfcy, 2) )
      ! ...have two neighbours, in positive and negative coordinate direction, one on each side
      ! (for x-aligned faces: along flux surface, for y-aligned faces: orthogonal to flux surface)
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % neighbour(gmap%nfcx + gmap%nfcy, 2, 1) )      
      ! 1d object measure: face area
      if (present(gs)) allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % measure( gmap % nfcx + gmap % nfcy, 1 ) )
      ! first set all boundary & connectivity information to undefined
      itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary = GRID_UNDEFINED
      itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % neighbour = GRID_UNDEFINED
      ! x-aligned faces    
      do ifc = 1, gmap % nfcx    
          ! get position of this face in the b2 grid
          ix = gmap % mapFcix( ifc )
          iy = gmap % mapFciy( ifc )
          ! get index of start vertex 
          ! objdef dims: index of face, 1=start node, 1=one-dimensional object
          select case ( gmap % mapFcIFace( ifc ) )
          case( BOTTOM )
              ! start index: 1=start node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 1 ) = gmap % mapVxI( ix, iy, VX_LOWERLEFT )
              if (gmap % mapVxI( ix, iy, VX_LOWERLEFT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: BOTTOM face at "//int2str(ix)//","//int2str(iy)//" has no start node")
              end if
          ! end vertex: 2=end node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 2 ) = gmap % mapVxI( ix, iy, VX_LOWERRIGHT )
              if (gmap % mapVxI( ix, iy, VX_LOWERRIGHT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: BOTTOM face at "//int2str(ix)//","//int2str(iy)//" has no end node")
          end if
          case( TOP )
              ! start index: 1=start node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 1 ) = gmap % mapVxI( ix, iy, VX_UPPERLEFT )
              if (gmap % mapVxI( ix, iy, VX_UPPERLEFT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: TOP face at "//int2str(ix)//","//int2str(iy)//" has no start node")
          end if
              ! end vertex: 2=end node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 2 ) = gmap % mapVxI( ix, iy, VX_UPPERRIGHT )
              if (gmap % mapVxI( ix, iy, VX_UPPERRIGHT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: TOP face at "//int2str(ix)//","//int2str(iy)//" has no end node")
              end if
          end select

          ! measure: area
          if (present(gs)) itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % measure( ifc, 1 ) = gs(ix, iy, ALIGNX)
      end do
      ! y-aligned faces    
      do ifc = gmap % nfcx + 1, gmap % nfcx + gmap % nfcy    
          ! get position of this face in the b2 grid
          ix = gmap % mapFcix( ifc )
          iy = gmap % mapFciy( ifc )
          if (gmap%mapCvI(ix, iy) == GRID_UNDEFINED) then 
                  call logmsg(LOGWARNING, "b2ITMFillGD: writing out faces for unused cell "//int2str(ix)//","//int2str(iy))
          end if

          select case ( gmap % mapFcIFace( ifc ) )
          case( LEFT )
              ! start index: 1=start node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 1 ) = gmap % mapVxI( ix, iy, VX_LOWERLEFT )
              if (gmap % mapVxI( ix, iy, VX_LOWERLEFT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: LEFT face at "//int2str(ix)//","//int2str(iy)//" has no start node")
              end if
          ! end vertex: 2=end node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 2 ) = gmap % mapVxI( ix, iy, VX_UPPERLEFT )
              if (gmap % mapVxI( ix, iy, VX_UPPERLEFT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: LEFT face at "//int2str(ix)//","//int2str(iy)//" has no end node")
          end if
              !if (itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 1 )


          case( RIGHT )
              ! start index: 1=start node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 1 ) = gmap % mapVxI( ix, iy, VX_LOWERRIGHT )
              if (gmap % mapVxI( ix, iy, VX_LOWERRIGHT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: RIGHT face at "//int2str(ix)//","//int2str(iy)//" has no start node")
          end if
              ! end vertex: 2=end node
              itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % boundary( ifc, 2 ) = gmap % mapVxI( ix, iy, VX_UPPERRIGHT )
              if (gmap % mapVxI( ix, iy, VX_UPPERRIGHT ) == GRID_UNDEFINED) then
                  call logmsg(LOGWARNING, "b2ITMFillGD: RIGHT face at "//int2str(ix)//","//int2str(iy)//" has no end node")
              end if
          end select

          ! measure: area
          if (present(gs)) itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(2) % measure( ifc, 1 ) = gs(ix, iy, ALIGNY)*qc(ix, iy)
      end do

      ! 2d objects: cells
      ! ...have four boundaries
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % boundary( gmap%ncv, 4) )
      ! 2d object measure: cell volume
      if (present(vol)) allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % measure( gmap % ncv, 1 ) )      
      ! Also store additional geometry information: position in computational space
      ! FIXME: this should go into alternate geometry, which is not available yet for grid objects
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % geo(gmap%ncv, 2, 1, 1) )

      ! first set all boundary information to undefined
      itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % boundary = GRID_UNDEFINED

      do icv = 1, gmap % ncv
          ix = gmap % mapCvix( icv )
          iy = gmap % mapCviy( icv )

          ! Set position in computational space
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % geo(icv, 1, 1, 1) = ix
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % geo(icv, 2, 1, 1) = iy
          ! put faces composing the quadliateral in the list: left face (y-aligned)
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % boundary( icv, 1 ) = gmap % mapFcI( ix, iy, LEFT )
          ! bottom face (x-aligned
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % boundary( icv, 2 ) = gmap % mapFcI( ix, iy, BOTTOM )
          ! right face (y-aligned)
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % boundary( icv, 3 ) = gmap % mapFcI( ix, iy, RIGHT )
          ! top face (x-aligned)
          itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % boundary( icv, 4 ) = gmap % mapFcI( ix, iy, TOP )

          ! measure: volume
!!$          if (present(vol)) itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % measure( icv ) = vol( ix, iy )
      end do

      ! Fill in connectivity information
      ! ...have one neighbour per boundary
      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % neighbour( gmap%ncv, 4, 1) )
      ! first set all to undefined
      itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % neighbour = GRID_UNDEFINED

!!$      do icv = 1, gmap % ncv
!!$          ix = gmap % mapCvix( icv )
!!$          iy = gmap % mapCviy( icv )
!!$
!!$          ! left neighbour
!!$          call getNeighbour(nx, ny, leftix, leftiy, rightix, rightiy, topix, topiy, bottomix, bottomiy, &
!!$              & ix, iy, LEFT, nix, niy)
!!$          if ( isCellInDomain(nx, ny, nix, niy, includeGhostCells ) ) &
!!$              & itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % neighbour(icv,1,1) = gmap % mapCvI( nix, niy )
!!$
!!$          ! bottom neighbour
!!$          call getNeighbour(nx, ny, leftix, leftiy, rightix, rightiy, topix, topiy, bottomix, bottomiy, &
!!$              & ix, iy, BOTTOM, nix, niy)
!!$          if ( isCellInDomain(nx, ny, nix, niy, includeGhostCells ) ) &
!!$              & itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % neighbour(icv,2,1) = gmap % mapCvI( nix, niy )
!!$
!!$          ! right neighbour
!!$          call getNeighbour(nx, ny, leftix, leftiy, rightix, rightiy, topix, topiy, bottomix, bottomiy, &
!!$              & ix, iy, RIGHT, nix, niy)
!!$          if ( isCellInDomain(nx, ny, nix, niy, includeGhostCells ) ) &
!!$              & itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % neighbour(icv,3,1) = gmap % mapCvI( nix, niy )
!!$
!!$          ! top neighbour
!!$          call getNeighbour(nx, ny, leftix, leftiy, rightix, rightiy, topix, topiy, bottomix, bottomiy, &
!!$              & ix, iy, TOP, nix, niy)
!!$          if ( isCellInDomain(nx, ny, nix, niy, includeGhostCells ) ) &
!!$              & itmgrid % spaces(SPACE_POLOIDALPLANE) % objects(3) % neighbour(icv,4,1) = gmap % mapCvI( nix, niy )
!!$      end do

!!$      ! Fill in x-point indices
!!$      allocate( itmgrid % spaces(SPACE_POLOIDALPLANE) % nodes % xpoints( gmap % nsv ) )
!!$      itmgrid % spaces(SPACE_POLOIDALPLANE) % nodes % xpoints = gmap % svi(1:gmap % nsv)
!!$
!!$      ! If requested, add a second space for the toroidal angle
!!$      if (SPACE_COUNT == SPACE_TOROIDALANGLE) then
!!$          
!!$          if ( TOROIDAL_PERIODIC ) then
!!$          call gridSetupStruct1dSpace( itmgrid%spaces(SPACE_TOROIDALANGLE), &
!!$              & COORDTYPE_PHI, &
!!$                  & (/  ( ( 2*pi / NNODES_TOROIDAL ) * i, i = 0, NNODES_TOROIDAL - 1 ) /), &
!!$                  & periodic = .true. )
!!$          else
!!$              call gridSetupStruct1dSpace( itmgrid%spaces(SPACE_TOROIDALANGLE), &
!!$                  & COORDTYPE_PHI, &
!!$                  & (/  ( ( 2*pi / NNODES_TOROIDAL ) * i, i = 0, NNODES_TOROIDAL ) /) )
!!$          end if
!!$
!!$      end if

    end subroutine fillInGridDescription
!!$    ! Getter functions for getting geometry data from crx, cry arrays
!!$    ! with accounting for "fake cell strips".
!!$    subroutine getNodeCoordinates( nx, ny, crx, cry, ix, iy, iCorner, x, y )
!!$      integer, intent(in) :: nx, ny, ix, iy, iCorner
!!$      real (ITM_R8), intent(in) :: &
!!$          & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)     
!!$      real(R8), intent(out) :: x, y
!!$
!!$      ! internal
!!$      integer :: rix, riy, riCorner
!!$
!!$      rix = ix
!!$      riy = iy
!!$      riCorner= iCorner
!!$
!!$      ! correct towards left
!!$      if (ix > nx) then 
!!$          rix = ix - 1
!!$          select case (iCorner)
!!$          case(0)
!!$              riCorner = 1
!!$          case(2)
!!$              riCorner = 3
!!$          case(1,3)
!!$              stop 'fillInGridDescription.getNodeCoordinates: no data for node position (1)'
!!$          end select
!!$      end if
!!$
!!$      ! correct towards left
!!$      if (riy > ny) then 
!!$          riy = riy - 1
!!$          select case (riCorner)
!!$          case(0)
!!$              riCorner = 2
!!$          case(1)
!!$              riCorner = 3
!!$          case(2,3)
!!$              stop 'fillInGridDescription.getNodeCoordinates: no data for node position (2)'
!!$          end select
!!$      end if
!!$
!!$      x = crx(rix, riy, riCorner)
!!$      y = cry(rix, riy, riCorner)   
!!$    end subroutine getNodeCoordinates
!!$

    ! Part 2: define subgrids
    subroutine fillInSubGridDescription

      ! internal
      integer :: geoId, iRegion, subgridCount, iType, nSubgrid
      integer :: xIn, yIn, xOut, yOut, iCoreSg
      integer :: cls(SPACE_COUNT_MAX)
      integer, allocatable :: xpoints(:,:)

      geoId = geometryId(nnreg, topcut)
    
      ! Figure out total number of subgrids
      ! Do generic subgrids + subgrids
      nSubgrid = B2_GENERIC_SUBGRID_COUNT !+ regionCountTotal(geoId)
      ! Inner/outer midplane subgrids
      !nSubgrid = nSubgrid + 2

      call logmsg( LOGDEBUG, "b2ITMFillGridDescription: expecting total of "&
          &//itmInt2str(nSubgrid)//" subgrids" )
      allocate( itmgrid % subgrids( nSubgrid ) )

      ! Set up generic subgrids

      ! B2_SUBGRID_CELLS: all 2d cells, one implicit object list
      call createSubGridForClass( itmgrid, itmgrid % subgrids( B2_SUBGRID_CELLS ), &
          & CLASS_CELL(1:SPACE_COUNT), 'Cells' )

      ! B2_SUBGRID_NODES: all nodes, one implicit object list
      call createSubGridForClass( itmgrid, itmgrid % subgrids( B2_SUBGRID_NODES ), &
          & CLASS_NODE(1:SPACE_COUNT), 'Nodes' )

      ! B2_SUBGRID_FACES: all faces, one implicit object list
      call createSubGridForClass( itmgrid, itmgrid % subgrids( B2_SUBGRID_FACES ), &
          & CLASS_POLOIDALRADIAL_FACE(1:SPACE_COUNT), 'Faces' )

      ! B2_SUBGRID_FACES_X: x-aligned faces. One implicit object list, range over x faces
      ! Create subgrid with one object list
      call createSubGrid( itmgrid % subgrids( B2_SUBGRID_FACES_X ), 1, 'x-aligned faces' )
      ! Initialize implicit object list for faces (class (/1/) )
      call createImplicitObjectList( itmgrid, itmgrid % subgrids( B2_SUBGRID_FACES_X ) % list(1), &
          & CLASS_POLOIDALRADIAL_FACE(1:SPACE_COUNT) )
      itmgrid % subgrids( B2_SUBGRID_FACES_X ) % list(1) % indset(1) &
          & = createIndexListForRange( 1, gmap%nfcx )
      if ( SPACE_COUNT == SPACE_TOROIDALANGLE ) then
          itmgrid % subgrids( B2_SUBGRID_FACES_X ) % list(1) % indset(2) &
              & = createIndexListForRange( 1, 1 )
      end if

      ! B2_SUBGRID_FACES_Y: y-aligned faces. One implicit object list, range over y faces. Same procedure.
      call createSubGrid( itmgrid % subgrids( B2_SUBGRID_FACES_Y ), 1, 'y-aligned faces' )
      call createImplicitObjectList( itmgrid, itmgrid % subgrids( B2_SUBGRID_FACES_Y ) % list(1)&
          & , CLASS_POLOIDALRADIAL_FACE(1:SPACE_COUNT) )
      itmgrid % subgrids( B2_SUBGRID_FACES_Y ) % list(1) % indset(1) &
          & = createIndexListForRange( gmap%nfcx + 1, gmap%nfcx + gmap%nfcy )
      if ( SPACE_COUNT == SPACE_TOROIDALANGLE ) then
          itmgrid % subgrids( B2_SUBGRID_FACES_Y ) % list(1) % indset(2) &
              & = createIndexListForRange( 1, 1 )
      end if

!!$      ! Subgrid of all x-points (in one poloidal plane at toroidal index 1)
!!$      allocate( xpoints(gmap%nsv, SPACE_COUNT) )
!!$      xpoints = 1
!!$      xpoints(:, SPACE_POLOIDALPLANE) = gmap%svi(1:gmap%nsv)
!!$      call createSubGridForExplicitList( itmgrid, itmgrid % subgrids( B2_SUBGRID_XPOINTS ), &
!!$          & CLASS_NODE(1:SPACE_COUNT), xpoints, 'x-points' )
!!$
!!$      ! Set up specific subgrids by collection faces for regions
!!$
!!$      ! Start counting from end of generic subgrids
!!$      subgridCount = B2_GENERIC_SUBGRID_COUNT
!!$
!!$      ! Cell + face subgrids
!!$      do iType = REGIONTYPE_CELL, REGIONTYPE_XFACE
!!$          
!!$          select case(iType)
!!$          case( REGIONTYPE_CELL )
!!$              cls = CLASS_CELL
!!$          case( REGIONTYPE_YFACE, REGIONTYPE_XFACE )
!!$              cls = CLASS_POLOIDALRADIAL_FACE
!!$          end select
!!$
!!$          do iRegion = 1, regionCount(geoId, iType)              
!!$              subgridCount = subgridCount + 1
!!$
!!$              call logmsg( LOGDEBUG, "b2ITMFillGridDescription: add subgrid #"//int2str(subgridCount)//&
!!$                  & " for iType "//int2str(iType)//&
!!$                  &", iRegion "//int2str(iRegion)//": "//regionName(geoId, iType, iRegion) )
!!$
!!$              call createSubGridForExplicitList( itmgrid, itmgrid % subgrids( subgridCount ), &
!!$                  & cls(1:SPACE_COUNT), &
!!$                  & collectIndexListForRegion(gmap, region, iType, iRegion), &
!!$                  & regionName(geoId, iType, iRegion) )
!!$
!!$          end do
!!$      end do

!!$      ! Add midplane node subgrids
!!$      
!!$      ! Find the core boundary subgrid by looking for its name as defined in b2mod_connectivity
!!$      iCoreSg = gridFindSubGridByName(itmgrid, "Core boundary")
!!$      ! For double null, we need the outer half of the core boundary
!!$      if (iCoreSg == GRID_UNDEFINED) then 
!!$          iCoreSg = gridFindSubGridByName(itmgrid, "Outer core boundary")          
!!$      end if
!!$      if (iCoreSg == GRID_UNDEFINED) stop "fillInSubGridDescription: &
!!$          & didn't find core boundary subgrid for assembling outer midplane subgrid"
!!$
!!$      ! Figure out starting points for inner and outer midplane on core boundary
!!$      call findMidplaneCells(itmgrid%subgrids(iCoreSg), gmap, crx, xIn, yIn, xOut, yOut)
!!$
!!$      subgridCount = subgridCount + 1
!!$      call createSubGridForExplicitList( itmgrid, itmgrid % subgrids( subgridCount ), &
!!$          & CLASS_NODE(1:SPACE_COUNT), &
!!$          & collectRadialVertexIndexList(gmap, xIn, yIn, topix, topiy), &
!!$          & "Inner midplane" )
!!$      
!!$      subgridCount = subgridCount + 1
!!$      call createSubGridForExplicitList( itmgrid, itmgrid % subgrids( subgridCount ), &
!!$          & CLASS_NODE(1:SPACE_COUNT), &
!!$          & collectRadialVertexIndexList(gmap, xOut, yOut, topix, topiy), &
!!$          & "Outer midplane" )      
!!$
!!$      call logmsg( LOGDEBUG, "b2ITMFillGridDescription: wrote total of "&
!!$          &//int2str(subgridCount)//" subgrids (expected was "//int2str(size(itmgrid%subgrids))//')' )
!!$
!!$      call assert( subgridCount == size(itmgrid%subgrids) )
    end subroutine fillInSubGridDescription

  end subroutine b2ITMFillGridDescription


!!$  ! Figure out starting cells for inner and outer midplane on core boundary
!!$  ! by finding the points on the core boundary with minimum and maximum r positions
!!$  subroutine findMidplaneCells(coreBndSubgrid, gmap, crx, xIn, yIn, xOut, yOut)   
!!$    type(type_complexgrid_subgrid), intent(in) :: coreBndSubgrid
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    ! x/radial vertex coordinates
!!$    real (ITM_R8), intent(in) :: crx(-1:gmap%b2nx,-1:gmap%b2ny,0:3)
!!$    integer, intent(out) :: xIn, yIn, xOut, yOut
!!$
!!$
!!$    ! internal
!!$    real(R8) :: rMin, rMax
!!$    type(GridObject) :: obj
!!$    integer :: ix, iy, iObj
!!$
!!$    rMin = huge(rMin)
!!$    rMax = -huge(rMax)
!!$
!!$    xIn = huge(xIn)
!!$    xOut = huge(xOut)
!!$
!!$    ! Loop over all faces in core boundary subgrid
!!$    do iObj = 1, gridSubGridSize(coreBndSubgrid)    
!!$    
!!$        obj = subGridGetObject(coreBndSubgrid, iObj)
!!$        ! Expect a face
!!$        call assert( all(obj%cls(1:SPACE_COUNT) == CLASS_POLOIDALRADIAL_FACE(1:SPACE_COUNT)) )        
!!$        ! ...which is aligned along the x-direction
!!$        call assert( gmap % mapFcIFace(obj%ind(SPACE_POLOIDALPLANE)) == BOTTOM )
!!$        ix = gmap % mapFcix( obj%ind(SPACE_POLOIDALPLANE) )
!!$        iy = gmap % mapFciy( obj%ind(SPACE_POLOIDALPLANE) )
!!$        
!!$        ! We want the vertex associated with the cell at ix, iy, which is number 0
!!$        if ( crx(ix, iy, 0) < rMin ) then
!!$            rMin = crx(ix, iy, 0)
!!$            xIn = ix
!!$            yIn = iy
!!$        end if
!!$        if ( crx(ix, iy, 0) > rMax ) then
!!$            rMax = crx(ix, iy, 0)
!!$            xOut = ix
!!$            yOut = iy
!!$        end if
!!$    end do
!!$
!!$    call assert(xIn /= huge(xIn), "findMidplaneCells: did not find inner midplane position")
!!$    call assert(xOut /= huge(xOut), "findMidplaneCells: did not find outer midplane position")
!!$  end subroutine


!!$  !> Collect the grid indices of all vertices on the radial grid line outward 
!!$  !> starting at the vertex at position six,siy in computational space.
!!$  function collectRadialVertexIndexList(gmap, six, siy, topix, topiy) result( indexList )
!!$    integer, allocatable, dimension(:,:) :: indexList
!!$
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    integer, intent(in) :: six, siy
!!$    ! B2 connectivity array
!!$    integer, intent(in) :: &
!!$        & topix(-1:gmap%b2nx,-1:gmap%b2ny),topiy(-1:gmap%b2nx,-1:gmap%b2ny)
!!$
!!$    ! internal
!!$    integer :: ix, iy, nix, niy, nVx, iVx
!!$
!!$    ! First figure out how many points we have: start at six, siy, 
!!$    ! go towards top until running out of physical domain
!!$    nVx = 1
!!$    ix = six
!!$    iy = siy
!!$    do
!!$        ! We add one point
!!$        nVx = nVx + 1
!!$        ! Take a step upwards
!!$        nix = topix(ix, iy)
!!$        niy = topiy(ix, iy)
!!$        ! Stepped outside grid or into ghost cell?
!!$        if (.not. isCellInDomain( gmap%b2nx, gmap%b2ny, nix, niy, extended = .false.) ) then
!!$            exit
!!$        else
!!$            ix = nix
!!$            iy = niy
!!$        end if
!!$    end do
!!$    
!!$    allocate( indexList(nVx, SPACE_COUNT) )   
!!$    if (SPACE_COUNT == SPACE_TOROIDALANGLE)&
!!$        & indexList(:,SPACE_TOROIDALANGLE) = 1
!!$
!!$    ! collect indices: repeat above loop with storing indices
!!$    ix = six
!!$    iy = siy
!!$    ! Store starting point index
!!$    iVx = 1    
!!$    indexList(iVx, SPACE_POLOIDALPLANE) = gmap%mapVxI(ix, iy, VX_LOWERLEFT)
!!$    do
!!$        ! take a step
!!$        nix = topix(ix, iy)
!!$        niy = topiy(ix, iy)
!!$
!!$        ! Store index for new point
!!$        iVx = iVx + 1 
!!$        indexList(iVx, SPACE_POLOIDALPLANE) = gmap%mapVxI(nix, niy, VX_LOWERLEFT)
!!$
!!$        ! Stepped outside grid?
!!$        if (.not. isCellInDomain( gmap%b2nx, gmap%b2ny, nix, niy, extended = .false.) ) then          
!!$            exit
!!$        end if
!!$
!!$        ix = nix
!!$        iy = niy
!!$    end do
!!$
!!$    call assert( iVx == nVx )
!!$
!!$  end function collectRadialVertexIndexList
!!$
!!$
!!$  !> Build an index list of all objects of a given region type (b2mod_connectivity.REGIONTYPE_*)
!!$  !> for a given region id.
!!$  !> @param gmap the B2<->CPO grid map, as built by b2ITMCreateMap
!!$  !> @param region the B2 region array
!!$  !> @param iRegionType The region type
!!$  !> @param iRegion The region 
!!$  !> @result The list of indices for all objects that constitute this grid region. The array
!!$  !>  has two dimensions because it is given as a list of object descriptors.
!!$  function collectIndexListForRegion(gmap, region, iRegionType, iRegion) result( indexList )
!!$    integer, allocatable, dimension(:,:) :: indexList
!!$
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    integer, intent(in) :: region(-1:gmap%b2nx,-1:gmap%b2ny,0:2)
!!$    integer, intent(in) :: iRegionType, iRegion
!!$
!!$    ! internal
!!$    integer :: ix, iy, nInd, iInd, ind
!!$
!!$    ! Figure out how many indices to expect. A simple count of the form
!!$    ! nInd = count( region(:,:,iRegionType) == iRegion )
!!$    ! won't do, because we have to account for removed objects (ghost cells/faces).
!!$
!!$    ! search the relevant objects and count them
!!$    nInd = 0
!!$    do ix = -1, gmap%b2nx
!!$        do iy = -1, gmap%b2ny
!!$
!!$            if ( region(ix, iy, iRegionType) == iRegion ) then
!!$                ! Get index depending on what object type we're looking at
!!$                select case (iRegionType) 
!!$                case (REGIONTYPE_CELL)
!!$                    ind = gmap%mapCvI(ix, iy)
!!$                case (REGIONTYPE_YFACE)
!!$                    ind = gmap%mapFcyI(ix, iy, LEFT)
!!$                case (REGIONTYPE_XFACE)
!!$                    ind = gmap%mapFcxI(ix, iy, BOTTOM)
!!$                end select
!!$
!!$                ! Only count this index if not undefined
!!$                if ( ind /= GRID_UNDEFINED ) nInd = nInd + 1
!!$            end if
!!$
!!$        end do
!!$    end do
!!$
!!$    allocate( indexList(nInd, SPACE_COUNT) )
!!$    indexList = 1
!!$
!!$    ! search the relevant objects and store their index consecutively
!!$    iInd = 0
!!$    do ix = -1, gmap%b2nx
!!$        do iy = -1, gmap%b2ny
!!$
!!$            if ( region(ix, iy, iRegionType) == iRegion ) then
!!$                ! Get index depending on what object type we're looking at
!!$                select case (iRegionType) 
!!$                case (REGIONTYPE_CELL)
!!$                    ind = gmap%mapCvI(ix, iy)
!!$                case (REGIONTYPE_YFACE)
!!$                    ind = gmap%mapFcyI(ix, iy)
!!$                case (REGIONTYPE_XFACE)
!!$                    ind = gmap%mapFcxI(ix, iy)
!!$                end select
!!$
!!$                if ( ind /= GRID_UNDEFINED ) then
!!$                    iInd = iInd + 1                  
!!$                    call assert(iInd <= nInd)
!!$                    indexList( iInd, SPACE_POLOIDALPLANE ) = ind
!!$                end if
!!$            end if
!!$
!!$        end do
!!$    end do
!!$
!!$    call assert( iInd == nInd )
!!$
!!$  end function collectIndexListForRegion

!     rMin = huge(rMin)
!     rMax = -huge(rMax)

!     xIn = huge(xIn)
!     xOut = huge(xOut)

!     ! Loop over all faces in core boundary subgrid
!     do iObj = 1, gridSubGridSize(coreBndSubgrid)    
    
!         obj = subGridGetObject(coreBndSubgrid, iObj)
!         ! Expect a face
!         call assert( all(obj%cls(1:SPACE_COUNT) == CLASS_POLOIDALRADIAL_FACE(1:SPACE_COUNT)) )        
!         ! ...which is aligned along the x-direction
!         call assert( gmap % mapFcAlign(obj%ind(SPACE_POLOIDALPLANE)) == ALIGNX )
!         ix = gmap % mapFcix( obj%ind(SPACE_POLOIDALPLANE) )
!         iy = gmap % mapFciy( obj%ind(SPACE_POLOIDALPLANE) )
        
!         ! We want the vertex associated with the cell at ix, iy, which is number 0
!         if ( crx(ix, iy, 0) < rMin ) then
!             rMin = crx(ix, iy, 0)
!             xIn = ix
!             yIn = iy
!         end if
!         if ( crx(ix, iy, 0) > rMax ) then
!             rMax = crx(ix, iy, 0)
!             xOut = ix
!             yOut = iy
!         end if
!     end do

!     call assert(xIn /= huge(xIn), "findMidplaneCells: did not find inner midplane position")
!     call assert(xOut /= huge(xOut), "findMidplaneCells: did not find outer midplane position")
!   end subroutine


!!$  !> Collect the grid indices of all vertices on the radial grid line outward 
!!$  !> starting at the vertex at position six,siy in computational space.
!!$  function collectRadialVertexIndexList(gmap, six, siy, topix, topiy) result( indexList )
!!$    integer, allocatable, dimension(:,:) :: indexList
!!$
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    integer, intent(in) :: six, siy
!!$    ! B2 connectivity array
!!$    integer, intent(in) :: &
!!$        & topix(-1:gmap%b2nx,-1:gmap%b2ny),topiy(-1:gmap%b2nx,-1:gmap%b2ny)
!!$
!!$    ! internal
!!$    integer :: ix, iy, nix, niy, nVx, iVx
!!$
!!$    ! First figure out how many points we have: start at six, siy, 
!!$    ! go towards top until running out of physical domain
!!$    nVx = 1
!!$    ix = six
!!$    iy = siy
!!$    do
!!$        ! We add one point
!!$        nVx = nVx + 1
!!$        ! Take a step upwards
!!$        nix = topix(ix, iy)
!!$        niy = topiy(ix, iy)
!!$        ! Stepped outside grid or into ghost cell?
!!$        if (.not. isCellInDomain( gmap%b2nx, gmap%b2ny, nix, niy, extended = .false.) ) then
!!$            exit
!!$        else
!!$            ix = nix
!!$            iy = niy
!!$        end if
!!$    end do
!!$    
!!$    allocate( indexList(nVx, SPACE_COUNT) )   
!!$    if (SPACE_COUNT == SPACE_TOROIDALANGLE)&
!!$        & indexList(:,SPACE_TOROIDALANGLE) = 1
!!$
!!$    ! collect indices: repeat above loop with storing indices
!!$    ix = six
!!$    iy = siy
!!$    ! Store starting point index
!!$    iVx = 1    
!!$    indexList(iVx, SPACE_POLOIDALPLANE) = gmap%mapVxI(ix, iy)
!!$    do
!!$        ! take a step
!!$        nix = topix(ix, iy)
!!$        niy = topiy(ix, iy)
!!$
!!$        ! Store index for new point
!!$        iVx = iVx + 1 
!!$        indexList(iVx, SPACE_POLOIDALPLANE) = gmap%mapVxI(nix, niy)
!!$
!!$        ! Stepped outside grid?
!!$        if (.not. isCellInDomain( gmap%b2nx, gmap%b2ny, nix, niy, extended = .false.) ) then
!!$            ! Should now be in ghost cell
!!$            !call assert( isGhostCell( ix, iy ) )
!!$            exit
!!$        end if
!!$
!!$        ix = nix
!!$        iy = niy
!!$    end do
!!$
!!$    call assert( iVx == nVx )
!!$
!!$  end function collectRadialVertexIndexList


!!$  !> Build an index list of all objects of a given region type (b2mod_connectivity.REGIONTYPE_*)
!!$  !> for a given region id.
!!$  !> @param gmap the B2<->CPO grid map, as built by b2ITMCreateMap
!!$  !> @param region the B2 region array
!!$  !> @param iRegionType The region type
!!$  !> @param iRegion The region 
!!$  !> @result The list of indices for all objects that constitute this grid region. The array
!!$  !>  has two dimensions because it is given as a list of object descriptors.
!!$  function collectIndexListForRegion(gmap, region, iRegionType, iRegion) result( indexList )
!!$    integer, allocatable, dimension(:,:) :: indexList
!!$
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    integer, intent(in) :: region(-1:gmap%b2nx,-1:gmap%b2ny,0:2)
!!$    integer, intent(in) :: iRegionType, iRegion
!!$
!!$    ! internal
!!$    integer :: ix, iy, nInd, iInd, ind
!!$
!!$    ! Figure out how many indices to expect. A simple count of the form
!!$    ! nInd = count( region(:,:,iRegionType) == iRegion )
!!$    ! won't do, because we have to account for removed objects (ghost cells/faces).
!!$
!!$    ! search the relevant objects and count them
!!$    nInd = 0
!!$    do ix = -1, gmap%b2nx
!!$        do iy = -1, gmap%b2ny
!!$
!!$            if ( region(ix, iy, iRegionType) == iRegion ) then
!!$                ! Get index depending on what object type we're looking at
!!$                select case (iRegionType) 
!!$                case (REGIONTYPE_CELL)
!!$                    ind = gmap%mapCvI(ix, iy)
!!$                case (REGIONTYPE_YFACE)
!!$                    ind = gmap%mapFcyI(ix, iy)
!!$                case (REGIONTYPE_XFACE)
!!$                    ind = gmap%mapFcxI(ix, iy)
!!$                end select
!!$
!!$                ! Only count this index if not undefined
!!$                if ( ind /= GRID_UNDEFINED ) nInd = nInd + 1
!!$            end if
!!$
!!$        end do
!!$    end do
!!$
!!$    allocate( indexList(nInd, SPACE_COUNT) )
!!$    indexList = 1
!!$
!!$    ! search the relevant objects and store their index consecutively
!!$    iInd = 0
!!$    do ix = -1, gmap%b2nx
!!$        do iy = -1, gmap%b2ny
!!$
!!$            if ( region(ix, iy, iRegionType) == iRegion ) then
!!$                ! Get index depending on what object type we're looking at
!!$                select case (iRegionType) 
!!$                case (REGIONTYPE_CELL)
!!$                    ind = gmap%mapCvI(ix, iy)
!!$                case (REGIONTYPE_YFACE)
!!$                    ind = gmap%mapFcyI(ix, iy)
!!$                case (REGIONTYPE_XFACE)
!!$                    ind = gmap%mapFcxI(ix, iy)
!!$                end select
!!$
!!$                if ( ind /= GRID_UNDEFINED ) then
!!$                    iInd = iInd + 1                  
!!$                    call assert(iInd <= nInd)
!!$                    indexList( iInd, SPACE_POLOIDALPLANE ) = ind
!!$                end if
!!$            end if
!!$
!!$        end do
!!$    end do
!!$
!!$    call assert( iInd == nInd )
!!$
!!$  end function collectIndexListForRegion




  subroutine b2ITMCreateMap( nx,ny,crx,cry,cflag,&
      & leftix,leftiy,rightix,rightiy, &
      & topix,topiy,bottomix,bottomiy,includeGhostCells,gd)

    use b2mod_cellhelper

    !   ..input arguments (unchanged on exit)

    ! Size of grid arrays: (-1:nx, -1:ny) 
    integer :: nx, ny
    !   .. output arguments
    ! vertex coordinates
    real(ITM_R8), intent(in) :: &
        & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)
    integer cflag(-1:nx,-1:ny,CARREOUT_NCELLFLAGS)
    integer, intent(in) :: &
        & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny), &
        & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
        & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny), &
        & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny)
    logical, intent(in) :: includeGhostCells

    type(B2ITMGridMap), intent(inout) :: gd

    ! internal
    integer :: ix, iy, ic, ifcx, ifcy, ivx, i1, i2, nbix, nbiy, nbix2, nbiy2, iFace
    integer :: iCorner, index, iPass
    ! cvi ... control volume index
    ! fc[x,y]i ... x/y-aligned face index (fcxi: only BOTTOM and TOP used, fcyi: only LEFT and RIGHT used)
    ! vxi ... vertex index
    ! There is an additional strip of "fake" cells (even faker than ghost cells)
    ! at the top and right to be able to also write out the ghost cells
    integer, dimension(-1:nx, -1:ny) :: cvi
    integer, dimension(-1:nx, -1:ny, 0:3) :: fcxi, fcyi, vxi

    logical :: cvNeeded((nx + 2) * (ny + 2)), vxNeeded((nx + 2) * (ny + 2) * 4)
    logical :: fcXNeeded((nx + 2) * (ny + 2) * 2), fcYNeeded((nx + 2) * (ny + 2) * 2)

    integer :: fcxiReduce((nx + 2) * (ny + 2) * 2), fcyiReduce((nx + 2) * (ny + 2) * 2)
    integer :: vxiReduce((nx + 2) * (ny + 2) * 4)

    logical :: check

    ! list of identified special vertices
    ! vertex indices (ix, iy)
    integer, dimension(MAX_SPECIAL_VERTICES) :: svix, sviy, svixAlias, sviyAlias
    ! number of special vertices
    integer :: svc
    ! number of duplicate special vertices
    integer :: svcDuplicates

    ! numbers of unique objects
    integer :: ncv, nfcx, nfcy, nvx

    call logmsg( LOGDEBUG, "b2ITMCreateMap: create map for a nx="//int2str(nx)//", ny="//int2str(ny)//" b2 grid" )

    cvi = GRID_UNDEFINED
    fcxi = GRID_UNDEFINED
    fcyi = GRID_UNDEFINED
    vxi = GRID_UNDEFINED

    ! set up initial lexicographic indices
    ic = 0 ! cvs
    ifcx = 0 ! x-aligned faces
    ifcy = 0 ! y-aligned faces
    ivx = 0 ! vertices
    do ix = -1, nx
        do iy = -1, ny
            ! Cell
            ic = ic + 1
            cvi( ix, iy ) = ic

            ! Faces: associate left and bottom face with every cell where possible
            ifcy = ifcy + 1
            fcyi( ix, iy, LEFT ) = ifcy ! left face
            ifcx = ifcx + 1
            fcxi( ix, iy, BOTTOM ) = ifcx ! bottom face

            ! Vertices: associate bottom left vertex with every cell where possible
            call findExistingVertexIndex(ix, iy, VX_LOWERLEFT, index)               
            if (index == GRID_UNDEFINED) then
                ivx = ivx + 1
                vxi( ix, iy, VX_LOWERLEFT ) = ivx
            else
                vxi( ix, iy, VX_LOWERLEFT ) = index
            end if
        end do
    end do

    ! fill in index numbers for remaining faces 
    do ix = -1, nx
        do iy = -1, ny

            ! Right face: left face of left neighbour
            call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
                & topix,topiy,bottomix,bottomiy, &
                & ix, iy, RIGHT, nbix, nbiy)

            if ( isCellInDomain(nx, ny, nbix, nbiy) ) then                
                fcyi( ix, iy, RIGHT ) = fcyi( nbix, nbiy, LEFT )
            else
                ifcy = ifcy + 1
                fcyi( ix, iy, RIGHT ) = ifcy
            end if

            ! Top face: bottom face of top neighbour
            ! also top-left vertex
            call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
                & topix,topiy,bottomix,bottomiy, &
                & ix, iy, TOP, nbix, nbiy)

            if ( isCellInDomain(nx, ny, nbix, nbiy) ) then
                fcxi( ix, iy, TOP ) = fcxi( nbix, nbiy, BOTTOM )
            else
                ifcx = ifcx + 1
                fcxi( ix, iy, TOP ) = ifcx
            end if
        end do
    end do
    
    if (count(fcxi(:,:,BOTTOM) == GRID_UNDEFINED) > 0) stop "b2ITMCreateMap: there are unnumbered x-aligned faces"
    if (count(fcxi(:,:,TOP) == GRID_UNDEFINED) > 0) stop "b2ITMCreateMap: there are unnumbered x-aligned faces"
    if (count(fcyi(:,:,LEFT) == GRID_UNDEFINED) > 0) stop "b2ITMCreateMap: there are unnumbered y-aligned faces"
    if (count(fcyi(:,:,RIGHT) == GRID_UNDEFINED) > 0) stop "b2ITMCreateMap: there are unnumbered y-aligned faces"

    ! Fill in vertex numbers for remaining vertices
    ! A vertex can be shared among 4 cells (possibly more fox special vertices, but they are
    ! assumed to have a proper cell associated with them)
    do ix = -1, nx
        do iy = -1, ny

            do iCorner = VX_LOWERRIGHT, VX_UPPERRIGHT  ! 1, 3
                call findExistingVertexIndex(ix, iy, iCorner, index)               
                if (index /= GRID_UNDEFINED) then
                    vxi( ix, iy, iCorner ) = index
                else
                    ivx = ivx + 1
                    vxi( ix, iy, iCorner ) = ivx
            end if
            end do

        end do
    end do

    if (count(vxi == GRID_UNDEFINED) > 0) stop "b2ITMCreateMap: there are unnumbered vertices"

    ! Mark which cells, vertices and faces are needed in 1d lists
    cvNeeded = .false.
    fcXNeeded = .false.
    fcYNeeded = .false.
    vxNeeded = .false.
    do ix = -1, nx
        do iy = -1, ny
            if ( .not. isUnneededCell( ix, iy ) ) then

                cvNeeded(cvi(ix, iy)) = .true.

                do iCorner = 0, 3
                    vxNeeded(vxi(ix, iy, iCorner)) = .true.
                end do            

                fcYNeeded( fcyi(ix, iy, LEFT) ) = .true.
                fcYNeeded( fcyi(ix, iy, RIGHT) ) = .true.
                fcXNeeded( fcxi(ix, iy, BOTTOM) ) = .true.
                fcXNeeded( fcxi(ix, iy, TOP) ) = .true.                
            end if
        end do
    end do

    ! search x-points. 
    svc = 0
    do ix = -1, nx
        do iy = -1, ny
            ! don't do this for unneeded cells, might be not initialized
            ! This is not perfect, but special vertices are usually inside the domain,
            ! and there it should be ok.
            if ( .not. cvNeeded( cvi(ix, iy) ) ) cycle

            ! test whether vertex is special vertex
            if ( isSpecialVertex( ix, iy ) ) then
                svc = svc + 1
                svix( svc ) = ix
                sviy( svc ) = iy
            end if

        end do
    end do

    ! identify duplicate special vertices

    svixAlias = 0
    sviyAlias = 0
    svcDuplicates = 0

    do i1 = 1, svc - 1

        ix = svix( i1 )
        iy = sviy( i1 )

        ! has the point already been identified as unneeded?
        if ( .not. vxNeeded( vxi( ix, iy, VX_LOWERLEFT ) ) ) cycle

        ! compare with remaining vertices
        do i2 = i1 + 1, svc

            ! has the point already been identified as unneeded?
            if ( .not. vxNeeded( vxi( svix(i2), sviy(i2), VX_LOWERLEFT ) ) ) cycle

            if ( points_match( crx( ix, iy, 0 ), cry( ix, iy, 0 ), &
                &  crx( svix(i2), sviy(i2), 0 ), cry( svix(i2), sviy(i2), 0 ) ) ) then

                ! The special vertex with index i2 is a duplicate of the one with index i1.
                ! Mark as unneeded and set up all references to this point as aliased to the first one.
                !vxNeeded( vxi(svix(i2), sviy(i2), VX_LOWERLEFT) ) = .false.
                !where (vxi == vxi( svix(i2), sviy(i2), VX_LOWERLEFT )) vxi = vxi( ix, iy, VX_LOWERLEFT )

                svixAlias( i2 ) = ix
                sviyAlias( i2 ) = iy
                ! Bookkeeping for diagnostics
                svcDuplicates = svcDuplicates + 1
            end if
        end do
    end do

    call logmsg( LOGDEBUG, "b2ITMCreateMap: found "//int2str(svc-svcDuplicates)//" special vertices (x-points),&
        & "//int2str(svc)//" including duplicates.")

    ! number of unique cells 
    !ncv = ( nx + 2 ) * ( ny + 2 ) - count( .not. isNeeded( cvi ) )
    ncv = count( cvNeeded )
    ! number of unique faces (x and y direction)
    nfcx = count( fcXNeeded )    
    nfcy = count( fcYNeeded )
    ! number of unique vertices
    nvx = count( vxNeeded )

    ! allocate the mapping structure
    call allocateB2ITMGridMap( gd, nx, ny, ncv, nfcx, nfcy, nvx )

    ! build the mappings

    ! ...for cells
    ic = 0
    gd%mapCvI = GRID_UNDEFINED
    do ix = -1, nx
        do iy = -1, ny
            if ( cvNeeded( cvi( ix, iy ) ) ) then
                ic = ic + 1
                if ( ic > ncv ) stop 'b2ITMCreateMap: found more cells than expected'
                ! Map CPO -> B2
                gd%mapCvix(ic) = ix
                gd%mapCviy(ic) = iy
                ! Map B2 -> CPO
                gd%mapCvI( ix, iy ) = ic
            else
                ! not needed
                gd%mapCvI( ix, iy ) = GRID_UNDEFINED
            end if
        end do
    end do
    if ( ic /= ncv ) stop 'b2ITMCreateMap: found less cells than expected'    

    ! ...for x faces
    ! first set up numbering
    ic = 0
    fcxiReduce = GRID_UNDEFINED
    do i1 = 1, size(fcXNeeded)
        if (fcXNeeded(i1)) then
                ic = ic + 1
                if ( ic > nfcx ) then
                    stop 'b2ITMCreateMap: found more x-aligned faces than expected'
                end if
            fcxiReduce(i1) = ic
        end if
    end do
    ! sanity check for number of x faces
    if ( ic /= nfcx ) then
        stop 'b2ITMCreateMap: found less x-aligned faces than expected'
    end if

    ! We want a CPO face to point to a LEFT or BOTTOM cell face if possible.
    ! If no LEFT or BOTTOM face exists, alternatively point to a TOP or RIGHT face.
    ! For this we need two passes.

    gd%mapFcI = GRID_UNDEFINED

    ! first x-aligned faces
    do iPass = 1, 2
        do ix = -1, nx
            do iy = -1, ny
            
                if (iPass == 1) then 
                    iFace = BOTTOM
                else
                    iFace = TOP
                end if
                
                if ( fcXNeeded( fcxi( ix, iy, iFace ) ) ) then
                    if (isUnneededCell(ix, iy)) cycle

                    ! Get the CPO index for this face
                    ic = fcxiReduce( fcxi( ix, iy, iFace ) )
                ! Map CPO -> B2
                    if ( gd%mapFcix(ic) == GRID_UNDEFINED) then
                gd%mapFcix(ic) = ix
                gd%mapFciy(ic) = iy
                        gd%mapFcIFace(ic) = iFace
                    end if
                ! Map B2 -> CPO
                    gd%mapFcI( ix, iy, iFace ) = ic
            end if
        end do
    end do
    end do
    ! ...for y faces, continue counting the total faces in ic
    ! again, first set up numbering
    ic = nfcx
    fcyiReduce = GRID_UNDEFINED
    do i1 = 1, size(fcYNeeded)
        if (fcYNeeded(i1)) then
            ic = ic + 1
            fcyiReduce(i1) = ic
        end if
    end do
    if ( ic /= nfcx + nfcy ) then
        stop 'b2ITMCreateMap: found less y-aligned faces than expected'
    end if

    do iPass = 1, 2
    do ix = -1, nx
        do iy = -1, ny
                if (iPass == 1) then 
                    iFace = LEFT
                else
                    iFace = RIGHT
                end if
                if ( fcYNeeded( fcyi( ix, iy, iFace ) ) ) then
                    ! don't associate with unused cell
                    if (isUnneededCell(ix, iy)) cycle

                    ! Get the CPO index for this face
                    ic = fcyiReduce( fcyi( ix, iy, iFace ) )
                ! Map CPO -> B2
                    if ( gd%mapFcix(ic) == GRID_UNDEFINED) then
                gd%mapFcix(ic) = ix
                gd%mapFciy(ic) = iy
                        gd%mapFcIFace(ic) = iFace
                    end if
                ! Map B2 -> CPO
                    gd%mapFcI( ix, iy, iFace ) = ic
            end if
            end do
        end do
    end do

    ! vertices
    ! Like for faces, first set up unique numbering
    ic = 0
    vxiReduce = GRID_UNDEFINED
    do i1 = 1, size(vxNeeded)
        if (vxNeeded(i1)) then
            ic = ic + 1
            vxiReduce(i1) = ic
        end if
    end do
    if ( ic /= nvx ) then
        stop 'b2ITMCreateMap: did not found expected number of  vertices'
    end if
    gd%mapVxI = GRID_UNDEFINED
    ! First looping over the corners makes sure that all vertices for which 
    ! this is possible are associated with a lower-left vertex 
    do iCorner = VX_LOWERLEFT, VX_UPPERRIGHT ! 0, 3
    do ix = -1, nx
        do iy = -1, ny
                if ( vxNeeded( vxi( ix, iy, iCorner ) ) ) then
                    ic = vxiReduce( vxi( ix, iy, iCorner ) )
                ! Map CPO -> B2
                    if ( gd%mapVxix(ic) == GRID_UNDEFINED ) then
                gd%mapVxix(ic) = ix
                gd%mapVxiy(ic) = iy
                        gd%mapVxIVx(ic) = iCorner
                    end if
                ! Map B2 -> CPO
                    if (ic == GRID_UNDEFINED) stop "b2ITMCreateMap: found vertex pointing to ic=0"
                    gd%mapVxI( ix, iy, iCorner ) = ic
                    end if
                end do

        end do
    end do

    ! sanity check: are all vertices initialized?
    do ic = 1, ncv



        if (gd%mapVxix(ic) == GRID_UNDEFINED) then
            call logmsg(LOGWARNING, "b2ITMCreateMap: vertex "//int2str(ic)//" not properly initialized")
            end if
    end do

    ! After completing the vertex map, we can complete the special vertex (x-point) map



    ! The special vertex with index i1 is unique and needed. Copy it to the map structure
    ! identify duplicate special vertices
    gd%nsv = 0
    do ic = 1, svc
        ix = svix( ic )
        iy = sviy( ic )

        ! has the point already been identified as unneeded?
        if ( .not. vxNeeded( vxi( ix, iy, VX_LOWERLEFT ) ) ) cycle

        ! if it is needed, copy it to the mapping structure
        gd%nsv = gd%nsv + 1
        gd%svix(gd%nsv) = ix
        gd%sviy(gd%nsv) = iy
        gd%svi(ic) = gd%mapVxI( gd%svix(ic), gd%sviy(ic), VX_LOWERLEFT )
    end do

    call logmsg( LOGDEBUG, "b2ITMCreateMap: finished setting up map" )
    call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%ncv)//" unique cells")
    call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%nfcx)//" unique x-aligned faces")
    call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%nfcy)//" unique y-aligned faces")
    call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%nvx)//" unique vertices")

  contains 

    ! For a given corner vertex of a cell, check whether in any connected
    ! cell a vertex index was already assigned to this vertex
    subroutine findExistingVertexIndex(ix, iy, iCorner, index)
      integer, intent(in) :: ix, iy, iCorner
      integer, intent(out) :: index    

      ! internal
      integer :: iRot, iStep, nix, niy, nix2, niy2, iDir, nICorner

      ! already have index?
      index = vxi(ix, iy, iCorner)
      if (index /= GRID_UNDEFINED) return      

      ! Circle through the cells connected to the corner vertex with
      ! index iCorner in clockwise and counterclockwise direction
      do iRot = CLOCKWISE, COUNTERCLOCKWISE
          nix = ix
          niy = iy
          nICorner = iCorner

          do iStep = 1, 4
              ! take step
              iDir = VXCIRCLE_STEPDIR(iStep, iCorner, iRot)

              call getNeighbour(nx, ny, &
                  & leftix,leftiy,rightix,rightiy,topix,topiy,bottomix,bottomiy, &
                  & nix, niy, iDir, nix2, niy2)

              if (.not. isCellInDomain(nx, ny, nix2, niy2)) exit
              if ( isUnneededCell(nix2, niy2) ) exit

              nix = nix2
              niy = niy2
              nICorner = VXCORNER_NEXTINDEX(iDir, nICorner)

              if (points_match(crx(ix, iy, iCorner), cry(ix, iy, iCorner), crx(nix, niy, nICorner), cry(nix, niy, nICorner))) then
                  index = vxi(nix, niy, nICorner)
                  if (index /= GRID_UNDEFINED) return
      end if
          end do
      end do

      index = GRID_UNDEFINED
    end subroutine findExistingVertexIndex


!!$    logical elemental function isNeeded( index ) 
!!$      integer, intent(in) :: index
!!$
!!$      isNeeded = ( index > 0 )
!!$    end function isNeeded

    ! test whether cell (ix,iy) is a ghost cell
    function isUnneededCell( ix, iy ) 
      logical isUnneededCell
      integer, intent(in) :: ix, iy

      ! Only cells inside the "normal" B2 domain can be needed
      ! (this catches fake cells and connectivity pointing outside the domain)
      isUnneededCell = .not. isCellInDomain(nx, ny, ix, iy, extended = .true.)
      if (isUnneededCell) return

      if (includeGhostCells) then
          isUnneededCell = isUnusedCell( cflag(ix,iy,CELLFLAG_TYPE) )             
      else
          isUnneededCell = isUnusedCell( cflag(ix,iy,CELLFLAG_TYPE) ) &
              & .or. isGhostCell( cflag(ix,iy,CELLFLAG_TYPE) )
      end if

      ! Classical treatment (without cflag, no extended grid) - for reference
      !if (.not. includeGhostCells) then
      !    isUnneededCell = &
      !        & ( leftix( ix, iy ) == -2 ) &
      !        & .or. ( rightix( ix, iy ) == ( nx + 1 ) ) &
      !        & .or. ( bottomiy( ix, iy ) == -2 ) &
      !        & .or. ( topiy( ix, iy ) == ( ny + 1 ) )      
      !end if

    end function isUnneededCell


    ! Test whether the vertex associated with the cell (ix, iy) is special, i.e. a x-point
    ! This steps around the vertex (left-bottom-right-top) and checks
    ! whether the resulting position is equal to the starting position
    logical function isSpecialVertex( ix, iy ) 
      integer, intent(in) :: ix, iy

      ! internal
      integer :: x, y, xn, yn

      x = ix
      y = iy
      ! TODO: this can be simplified with the new structures in b2mod_cellhelper

      ! step left
      call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
      & topix,topiy,bottomix,bottomiy, x, y, LEFT, xn, yn)
      if ( .not. isCellInDomain( nx, ny, xn, yn, extended=includeGhostCells)) then
          isSpecialVertex = .false.
          return
      end if
      if ( isGhostCell(cflag(xn,yn,CELLFLAG_TYPE)) ) then
          isSpecialVertex = .false.
          return
      end if
      x = xn
      y = yn

      ! step bottom
      call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
          & topix,topiy,bottomix,bottomiy, x, y, BOTTOM, xn, yn)
      if ( .not. isCellInDomain( nx, ny, xn, yn, extended=includeGhostCells)) then
          isSpecialVertex = .false.
          return
      end if
      if ( isGhostCell(cflag(xn,yn,CELLFLAG_TYPE)) ) then
          isSpecialVertex = .false.
          return
      end if
      x = xn
      y = yn

      ! step right
      call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
          & topix,topiy,bottomix,bottomiy, x, y, RIGHT, xn, yn)
      if ( .not. isCellInDomain( nx, ny, xn, yn, extended=includeGhostCells)) then
          isSpecialVertex = .false.
          return
      end if
      if ( isGhostCell(cflag(xn,yn,CELLFLAG_TYPE)) ) then
          isSpecialVertex = .false.
          return
      end if
      x = xn
      y = yn

      ! step top
      call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
          & topix,topiy,bottomix,bottomiy, x, y, TOP, xn, yn)
      if ( .not. isCellInDomain( nx, ny, xn, yn, extended=includeGhostCells)) then
          isSpecialVertex = .false.
          return
      end if
      if ( isGhostCell(cflag(xn,yn,CELLFLAG_TYPE)) ) then
          isSpecialVertex = .false.
          return
      end if
      x = xn
      y = yn

      ! do we end up where we left?
      isSpecialVertex = .not. ( ( x == ix ) .and. ( y == iy ) ) 

    end function isSpecialVertex

!!$    ! Decide whether a vertex is needed based on whether a face
!!$    ! connected to this vertex is needed
!!$    logical function isVertexNeeded( ix, iy ) 
!!$      integer, intent(in) :: ix, iy
!!$
!!$      ! internal
!!$      integer :: lix, liy, bix, biy
!!$
!!$
!!$      ! Get start node of connected faces and check whether they are in the
!!$      ! domain
!!$      call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
!!$          & topix,topiy,bottomix,bottomiy, ix, iy, LEFT, lix, liy, force = .true.)
!!$      if ( .not. isNodeInDomain( nx, ny, lix, liy, includeGhostCells ) ) then
!!$          ! if neighbour not in domain, reset to current position, i.e. don't test
!!$          lix = ix
!!$          liy = iy
!!$      end if
!!$
!!$      call getNeighbour(nx, ny, leftix,leftiy,rightix,rightiy, &
!!$          & topix,topiy,bottomix,bottomiy, ix, iy, BOTTOM, bix, biy, force = .true.)
!!$      if ( .not. isNodeInDomain( nx, ny, bix, biy, includeGhostCells ) ) then
!!$          ! if neighbour not in domain, reset to current position, i.e. don't test
!!$          bix = ix
!!$          biy = iy
!!$      end if
!!$
!!$      isVertexNeeded = isNeeded( fcxi( ix, iy ) ) .or. isNeeded( fcyi( ix, iy ) ) &
!!$          & .or. isNeeded( fcxi( lix, liy ) ) .or. isNeeded( fcyi( bix, biy ) ) 
!!$
!!$    end function isVertexNeeded

  end subroutine b2ITMCreateMap

  
  ! Check whether the cell at position (ix,iy) is inside the grid.
  ! The default is to check whether it isinside  the extended grid (including the ghost cells).
  ! If the optional parameter extended is given, extended = .false. will check whether the position
  ! is inside the actual physical domain of the grid.
  function isCellInDomain( nx, ny, ix, iy, extended )
    logical :: isCellInDomain
    integer, intent(in) :: nx, ny, ix, iy
    logical, intent(in), optional :: extended
    
    ! internal
    logical :: lExtended
    
    lExtended = .true.
    if ( present( extended ) ) lExtended = extended
    
    if ( lExtended ) then
        ! in extended domain (including ghost cells)?
        isCellInDomain = ( ix >= -1 ) .and. (ix <= nx) .and. ( iy >= -1 ) .and. ( iy <= ny )
    else
        isCellInDomain = ( ix > -1 ) .and. (ix < nx) .and. ( iy > -1 ) .and. ( iy < ny )
    end if
  end function isCellInDomain
  

  ! Check whether the node associate with the cell at position (ix,iy) is included in the grid.
  function isNodeInDomain( nx, ny, ix, iy, extended )
    logical :: isNodeInDomain
    integer, intent(in) :: nx, ny, ix, iy
    logical, intent(in), optional :: extended

    ! internal
    logical :: lExtended

    lExtended = .true.
    if ( present( extended ) ) lExtended = extended

    if ( lExtended ) then
        ! in extended domain (including ghost cells)?
        isNodeInDomain = ( ix >= -1 ) .and. (ix <= nx + 1) .and. ( iy >= -1 ) .and. ( iy <= ny + 1 )
    else
        isNodeInDomain = ( ix > -1 ) .and. (ix < nx + 1) .and. ( iy > -1 ) .and. ( iy < ny + 1)
    end if
  end function isNodeInDomain


!!$  logical function isFakeCell( nx, ny, ix, iy ) 
!!$    integer, intent(in) :: nx, ny, ix, iy
!!$    
!!$    isFakeCell = (ix == nx+1) .or. (iy == ny+1)
!!$  end function isFakeCell


  ! extended neighbourhood mappings
  subroutine getNeighbour(nx, ny, &
      & leftix,leftiy,rightix,rightiy, &
      & topix,topiy,bottomix,bottomiy, &
      & ix, iy, dir, nbix, nbiy, force)
    integer, intent(in) :: nx, ny, ix, iy, dir
    integer, intent(in) :: &
        & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny),&
        & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny),&
        & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny),&
        & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny)
    integer, intent(out) :: nbix, nbiy
    logical, intent(in), optional :: force

    ! internal
    logical :: lForce

    lForce = .false.
    !if (present(force)) lForce = force

!!$    if (isFakeCell(nx, ny, ix,iy)) then
!!$        nbix = ix + dxDir(dir)
!!$        nbiy = iy + dyDir(dir)
!!$        return
!!$    end if

    select case(dir)
    case(LEFT)
        nbix = leftix(ix, iy)
        nbiy = leftiy(ix, iy)
    case(BOTTOM)
        nbix = bottomix(ix, iy)
        nbiy = bottomiy(ix, iy)
    case(RIGHT)
!!$        if (ix == nx) then
!!$            nbix = ix + 1
!!$            nbiy = iy
!!$        else
            nbix = rightix(ix, iy)
            nbiy = rightiy(ix, iy)
!!$        end if
    case(TOP)
!!$        if (iy == ny) then
!!$            nbix = ix
!!$            nbiy = iy + 1
!!$        else
            nbix = topix(ix, iy)
            nbiy = topiy(ix, iy)
!!$        end if
    end select

!!$    if (nbix == NO_CONNECTIVITY .and. lForce) then
!!$        nbix = ix + dxDir(dir)
!!$        nbiy = iy + dyDir(dir)        
!!$    end if

  end subroutine getNeighbour



!!$  ! Below here: service routines to transform data from B2 to CPO
!!$
!!$  function b2ITMTransformDataB2ToCPOCell( grid, subgridId, gmap, b2CellData ) result( cpodata )
!!$    real(ITM_R8), dimension(:), pointer :: cpodata
!!$
!!$    type(type_complexgrid), intent(in) :: grid
!!$    integer, intent(in) :: subgridId
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    real(ITM_R8), intent(in) :: b2CellData(-1:gmap%b2nx, -1:gmap%b2ny)
!!$
!!$    cpodata => b2ITMTransformDataB2ToCPOGeneral( grid, subgridId, gmap, b2CellData = b2CellData )
!!$  end function b2ITMTransformDataB2ToCPOCell
!!$
!!$  function b2ITMTransformDataB2ToCPOFace( grid, subgridId, gmap, b2FaceData ) result( cpodata )
!!$    real(ITM_R8), dimension(:), pointer :: cpodata
!!$
!!$    type(type_complexgrid), intent(in) :: grid
!!$    integer, intent(in) :: subgridId
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    real(ITM_R8), intent(in) :: b2FaceData(-1:gmap%b2nx, -1:gmap%b2ny, 0:1)
!!$
!!$    cpodata => b2ITMTransformDataB2ToCPOGeneral( grid, subgridId, gmap, b2FaceData = b2FaceData )
!!$  end function b2ITMTransformDataB2ToCPOFace
!!$
!!$  ! TODO: find a way to include this subroutine in the b2ITMTransformDataB2ToCPO interface
!!$  function b2ITMTransformDataB2ToCPOVertex( grid, subgridId, gmap, b2VertexData ) result( cpodata )
!!$    real(ITM_R8), dimension(:), pointer :: cpodata
!!$
!!$    type(type_complexgrid), intent(in) :: grid
!!$    integer, intent(in) :: subgridId
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    real(ITM_R8), intent(in) :: b2VertexData(-1:gmap%b2nx, -1:gmap%b2ny)
!!$
!!$    cpodata => b2ITMTransformDataB2ToCPOGeneral( grid, subgridId, gmap, b2VertexData = b2VertexData )
!!$  end function b2ITMTransformDataB2ToCPOVertex

!!$  !> Transform a quantity stored on faces from a 2d B2 array into a 1d CPO array for a given
!!$  !> subgrid id. Either b2CellData or b2FaceData must be given. Don't use this directly,
!!$  !> use the provided general interface b2ITMTransformDataB2ToCPO instead.
!!$  !>
!!$  !> @param grid The CPO grid description
!!$  !> @param subgridId Id of the subgrid the data is to be stored on.
!!$  !> @param gmap The grid mapping as computed by b2ITMCreateMap
!!$  !> @param b2CellData Cell data given on the 2d b2 data structure
!!$  !> @param b2FaceData Face data given on the 2d b2 data structure
!!$  function b2ITMTransformDataB2ToCPOGeneral( grid, subgridId, gmap, b2CellData, b2FaceData, b2VertexData ) result( cpodata )
!!$    real(ITM_R8), dimension(:), pointer :: cpodata
!!$
!!$    type(type_complexgrid), intent(in) :: grid
!!$    integer, intent(in) :: subgridId
!!$    type(B2ITMGridMap), intent(in) :: gmap
!!$    real(ITM_R8), intent(in), optional :: b2CellData(-1:gmap%b2nx, -1:gmap%b2ny)
!!$    real(ITM_R8), intent(in), optional :: b2FaceData(-1:gmap%b2nx, -1:gmap%b2ny, 0:1)
!!$    real(ITM_R8), intent(in), optional :: b2VertexData(-1:gmap%b2nx, -1:gmap%b2ny)
!!$
!!$    ! internal
!!$    integer :: nobjs, iobj, ifc, icv, ivx
!!$    type(GridObject) :: curObj
!!$
!!$    ! .neqv. is xor (exclusive or) 
!!$    ! (http://de.wikibooks.org/wiki/Fortran:_Fortran_95:_Logische_Ausdr%C3%BCcke)
!!$    ! TODO: FIX
!!$    !call assert( present(b2CellData) .neqv. present(b2FaceData) )
!!$
!!$    ! Allocate result vector according to subgrid size
!!$    nobjs = gridSubGridSize( grid%subgrids(subgridId) )
!!$    allocate( cpodata(nobjs) )
!!$
!!$    ! Collect all data items for the subgrid objects
!!$    do iobj = 1, nobjs
!!$        ! Get the object descriptor
!!$        curObj = subGridGetObject( grid%subgrids(subgridId), iobj )
!!$
!!$        if (present(b2CellData)) then
!!$            ! Cell data case
!!$            ! check that it's a cell
!!$            call assert( all( curObj%cls == CLASS_CELL(1:SPACE_COUNT) ) )
!!$            ! get the subobject index for the face in the 2d poloidal plane space
!!$            icv = curObj%ind(SPACE_POLOIDALPLANE)
!!$            ! copy data
!!$            cpodata(iobj) = b2CellData( gmap%mapCvix(icv), gmap%mapCviy(icv) )
!!$        else if (present(b2FaceData)) then
!!$            ! Face data case
!!$            ! check that it's a face
!!$            call assert( all( curObj%cls == CLASS_POLOIDALRADIAL_FACE(1:SPACE_COUNT) ) )
!!$            ! get the subobject index for the face in the 2d poloidal plane space
!!$            ifc = curObj%ind(SPACE_POLOIDALPLANE)
!!$            ! copy data
!!$            cpodata(iobj) = b2FaceData( gmap%mapFcix(ifc), gmap%mapFciy(ifc), gmap%mapFcAlign(ifc) )
!!$        else if (present(b2VertexData)) then
!!$            ! Vertex/Node data case
!!$            ! check that it's a vertex
!!$            call assert( all( curObj%cls == CLASS_NODE(1:SPACE_COUNT) ) )
!!$            ! get the subobject index for the face in the 2d poloidal plane space
!!$            ivx = curObj%ind(SPACE_POLOIDALPLANE)
!!$            ! copy data
!!$            cpodata(iobj) = b2VertexData( gmap%mapVxix(ivx), gmap%mapVxiy(ivx) )
!!$        end if
!!$
!!$    end do
!!$
!!$  end function b2ITMTransformDataB2ToCPOGeneral

end module b2ITMMapping
