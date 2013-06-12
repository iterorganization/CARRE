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

  use b2mod_types
  use itm_types , ITM_R8 => R8, ITM_R4 => R4 ! IGNORE
  use itm_string ! IGNORE
  use itm_assert ! IGNORE
  use Helper
  use Logging , only: logmsg, LOGDEBUG
  use b2mod_connectivity , REMOVED_B2_R8 => R8
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

!  private :: R8

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
