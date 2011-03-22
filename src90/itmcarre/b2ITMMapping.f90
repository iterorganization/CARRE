module b2ITMMapping

  use itm_types
  use euITM_schemas
  use itm_grid
  use Logging
  !use Helper

  implicit none

  ! Distance between two points at which the points are declared to be equal
  real(R8), parameter :: geom_match_dist = 1.0e-6_R8

  integer, parameter :: ALIGNX = 0
  integer, parameter :: ALIGNY = 1

  ! Data structure holding an intermediate grid description to be 
  ! transferred into a CPO
  type B2ITMGridDesc
          integer :: ncv, nfcx, nfcy, nvx          

          ! Mapping arrays: 
          ! 1d CPO lists -> 2d B2 data structure
          integer, dimension(:), allocatable :: mapCvix, mapCviy, mapFcix, mapFciy, mapFcAlign, mapVxix, mapVxiy
          ! 2d B2 data structure -> 1d CPO lists
          integer, dimension(:,:), allocatable :: mapCvI, mapFcxI, mapFcyI, mapVxI
  end type B2ITMGridDesc

contains

  ! service routines for B2ITMGridData

  subroutine allocateB2ITMGridDesc( gd, nx, ny, ncv, nfcx, nfcy, nvx )
    type(B2ITMGridDesc), intent(inout) :: gd
    integer, intent(in) ::  nx, ny, ncv, nfcx, nfcy, nvx
    
    gd%ncv = ncv
    gd%nfcx = nfcx
    gd%nfcy = nfcy
    gd%nvx = nvx

    allocate( gd%mapCvI(-1:nx, -1:ny) )
    allocate( gd%mapFcxI(-1:nx, -1:ny) )
    allocate( gd%mapFcyI(-1:nx, -1:ny) )
    allocate( gd%mapVxI(-1:nx, -1:ny) )
    
    allocate( gd%mapCvix(ncv), gd%mapCviy(ncv) )
    allocate( gd%mapFcix(nfcx+nfcy), gd%mapFciy(nfcx+nfcy), gd%mapFcAlign(nfcx+nfcy) )
    allocate( gd%mapVxix(nvx), gd%mapVxiy(nvx) )

  end subroutine allocateB2ITMGridDesc
  
  subroutine deallocateB2ITMGridDesc( gd )
    type(B2ITMGridDesc), intent(inout) :: gd

    deallocate( gd%mapCvI )
    deallocate( gd%mapFcxI )
    deallocate( gd%mapFcyI )
    deallocate( gd%mapVxI )

    deallocate( gd%mapCvix, gd%mapCviy )
    deallocate( gd%mapFcix, gd%mapFciy, gd%mapFcAlign )
    deallocate( gd%mapVxix, gd%mapVxiy )

  end subroutine deallocateB2ITMGridDesc


  !> Routine that fills in a grid description which is part of a CPO
  !> using the given grid data and prepared mappings
  subroutine b2ITMFillGridDescription( b2gd, itmgrid, &
       & nx,ny,crx,cry, &
       & leftix,leftiy,rightix,rightiy, &
       & topix,topiy,bottomix,bottomiy )
    
    type(B2ITMGridDesc), intent(in) :: b2gd
    type(type_complexgrid), intent(out) :: itmgrid

    ! Size of grid arrays: (-1:nx, -1:ny) 
    integer, intent(in) :: nx, ny
    !   .. output arguments
    ! vertex coordinates
    real (R8), intent(in) :: &
         & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)
    integer, intent(in) :: &
         & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny),&
         & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny),&
         & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny),&
         & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny)


    ! internal
    integer, parameter :: NDIM = 2

    integer :: ivx, ifc, icv, ix, iy, nix, niy

    allocate( itmgrid % spaces(2) )
    itmgrid % metric => null()

    ! Coordinate types
    ! (dimension of space = NDIM = size( coordtype )
    allocate( itmgrid % spaces(1) % coordtype(NDIM) )    
    itmgrid % spaces(1) % coordtype = (/ COORDTYPE_R, COORDTYPE_Z /)

    ! Have two types of objects: 1d edges, 2d cells
    allocate( itmgrid % spaces(1) % objects(NDIM) )
    ! nobjects: (/ b2gd%nfcx + b2gd%nfcy, b2gd%ncv /)

    ! Number of boundaries the objects have (at maximum)
!!$    itmgrid % spaces(1) % nobject_bou = (/ 2, 4 /)

    ! Fill in node information
    allocate( itmgrid % spaces(1) % nodes % geo(b2gd%nvx, NDIM, 1) )
    do ivx = 1, b2gd % nvx
            ix = b2gd % mapVxix( ivx )
            iy = b2gd % mapVxiy( ivx )
            itmgrid % spaces(1) % nodes % geo(ivx, 1, 1) =  crx( ix, iy, 0 )
            itmgrid % spaces(1) % nodes % geo(ivx, 2, 1) =  cry( ix, iy, 0 )
    end do

    ! Fill in object definitions (i.e. what objects compose an object)


    ! 1d objects: faces
    ! ...have two boundaries
    allocate( itmgrid % spaces(1) % objects(1) % boundary( b2gd%nfcx + b2gd%nfcy, 2) )
    ! ...have no neighbours. objects(1) % neighbour left unallocated.
    ! first set all boundary information to undefined
    itmgrid % spaces(1) % objects(1) % boundary = GRID_UNDEFINED
    ! x-aligned faces    
    do ifc = 1, b2gd % nfcx    
            ! get position of this face in the b2 grid
            ix = b2gd % mapFcix( ifc )
            iy = b2gd % mapFciy( ifc )
            ! get index of start vertex 
            ! objdef dims: index of face, 1=start node, 1=one-dimensional object
            itmgrid % spaces(1) % objects(1) % boundary( ifc, 1 ) = b2gd % mapVxI( ix, iy )
            ! get index of end vertex 
            ! in the b2 grid, the end node/vertex of an x-aligned face is the 
            ! vertex associated with the right neighbour cell
            nix = rightix( ix, iy )
            niy = rightiy( ix, iy )
            ! objdef dims: index of face, 2=end node, 1=one-dimensional object
            itmgrid % spaces(1) % objects(1) % boundary( ifc, 2 ) = b2gd % mapVxI( nix, niy )
    end do
    ! y-aligned faces    
    do ifc = b2gd % nfcx + 1, b2gd % nfcx + b2gd % nfcy    
            ! get position of this face in the b2 grid
            ix = b2gd % mapFcix( ifc )
            iy = b2gd % mapFciy( ifc )
            ! get index of start vertex 
            ! objdef dims: index of face, 1=start node, 1=one-dimensional object
            itmgrid % spaces(1) % objects(1) % boundary( ifc, 1 ) = b2gd % mapVxI( ix, iy )
            ! get index of end vertex 
            ! in the b2 grid, the end node/vertex of a y-aligned face is the 
            ! vertex associated with the top neighbour cell
            nix = topix( ix, iy )
            niy = topiy( ix, iy )
            ! objdef dims: index of face, 2=end node, 1=one-dimensional object
            itmgrid % spaces(1) % objects(1) % boundary( ifc, 2 ) = b2gd % mapVxI( nix, niy )
    end do

    ! 2d objects: cells
    ! ...have four boundaries
    allocate( itmgrid % spaces(1) % objects(2) % boundary( b2gd%ncv, 4) )
    ! first set all boundary information to undefined
    itmgrid % spaces(1) % objects(1) % boundary = GRID_UNDEFINED

    do icv = 1, b2gd % ncv
            ix = b2gd % mapCvix( icv )
            iy = b2gd % mapCviy( icv )

            ! put faces composing the quadliateral in the list: left face (y-aligned)
            itmgrid % spaces(1) % objects(2) % boundary( icv, 1 ) = b2gd % mapFcyI( ix, iy )
            ! bottom face (x-aligned
            itmgrid % spaces(1) % objects(2) % boundary( icv, 2 ) = b2gd % mapFcxI( ix, iy )
            ! right face (y-aligned): take left face of right neighbour
            nix = rightix( ix, iy )
            niy = rightiy( ix, iy )
            itmgrid % spaces(1) % objects(2) % boundary( icv, 3 ) = b2gd % mapFcyI( nix, niy )
            ! top face (x-aligned): take bottom face of top neighbour
            nix = topix( ix, iy )
            niy = topiy( ix, iy )
            itmgrid % spaces(1) % objects(2) % boundary( icv, 4 ) = b2gd % mapFcxI( nix, niy )
    end do

    ! Fill in connectivity information
    ! ...have one neighbour per boundary
    allocate( itmgrid % spaces(1) % objects(2) % neighbour( b2gd%ncv, 4, 1) )
    ! first set all to undefined
    itmgrid % spaces(1) % objects(2) % neighbour = GRID_UNDEFINED
    
    do icv = 1, b2gd % ncv
            ix = b2gd % mapCvix( icv )
            iy = b2gd % mapCviy( icv )

            ! left neighbour
            nix = leftix( ix, iy )
            niy = leftiy( ix, iy )
            itmgrid % spaces(1) % objects(2) % neighbour(icv,1,1) = b2gd % mapCvI( nix, niy )

            ! bottom neighbour
            nix = bottomix( ix, iy )
            niy = bottomiy( ix, iy )
            itmgrid % spaces(1) % objects(2) % neighbour(icv,2,1) = b2gd % mapCvI( nix, niy )
            
            ! right neighbour
            nix = rightix( ix, iy )
            niy = rightiy( ix, iy )
            itmgrid % spaces(1) % objects(2) % neighbour(icv,3,1) = b2gd % mapCvI( nix, niy )

            ! top neighbour
            nix = topix( ix, iy )
            niy = topiy( ix, iy )
            itmgrid % spaces(1) % objects(2) % neighbour(icv,4,1) = b2gd % mapCvI( nix, niy )
    end do

  end subroutine b2ITMFillGridDescription



  subroutine b2ITMCreateMap( nx,ny,crx,cry,&
         & leftix,leftiy,rightix,rightiy, &
         & topix,topiy,bottomix,bottomiy, gd )

      !   ..input arguments (unchanged on exit)
      
      ! Size of grid arrays: (-1:nx, -1:ny) 
      integer :: nx, ny
      !   .. output arguments
      ! vertex coordinates
      real(R8), intent(in) :: &
           & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)
      integer, intent(in) :: &
           & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny), &
           & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny), &
           & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny), &
           & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny)

      type(B2ITMGridDesc), intent(inout) :: gd

      ! internal
      integer :: ix, iy, ic, i1, i2
      ! cvi ... control volume index
      ! fc[x,y]i ... x/y-aligned face index
      ! vxi ... vertex index
      integer, dimension(-1:nx, -1:ny) :: cvi, fcxi, fcyi, vxi
      
      ! list of identified special vertices
      ! vertex indices (ix, iy)
      integer, dimension(20) :: svix, sviy, svixAlias, sviyAlias
      ! number of special vertices
      integer :: svc

      ! numbers of unique objects
      integer :: ncv, nfcx, nfcy, nvx

      call logmsg( LOGDEBUG, "b2ITMCreateMap: create map for a nx="//int2str(nx)//", ny="//int2str(ny)//" b2 grid" )
      
      ! set up initial lexicographic indices
      ic = 0
      do ix = -1, nx
              do iy = -1, ny
                      ic = ic + 1
                      cvi( ix, iy ) = ic

                      ! Faces: associate left and bottom face with every cell
                      fcyi( ix, iy ) = ic ! left face
                      fcxi( ix, iy ) = ic ! bottom face

                      ! Vertices: associate bottom left vertex with every cell
                      vxi( ix, iy ) = ic
              end do
      end do

      ! remove unneeded cells from list
      ! For the moment, this means removing the ghost cells
      ! remove unneeded faces from list
      ! This means removing all faces not in the physical domain: ghost faces
      do ix = -1, nx
              do iy = -1, ny

                      ! mark ghost cells as unneeded
                      if ( isGhostCell( ix, iy ) ) then
                              ! mark cell as unneeded by inverting the index sign
                              cvi( ix, iy ) = - cvi( ix, iy )
                      end if

                      ! mark ghost faces as unneeded
                      if ( isXFaceGhostFace( ix, iy ) ) then
                              fcxi( ix, iy ) = - fcxi( ix, iy )
                      end if
                      if ( isYFaceGhostFace( ix, iy ) ) then
                              fcyi( ix, iy ) = - fcyi( ix, iy )
                      end if
                     
              end do
      end do
      
      ! remove unneeded vertices from list. A vertex is not needed if is neither a
      ! start or end vertex of a needed faces. This has to be done after the final
      ! decisions on the faces was done.
      do ix = -1, nx
              do iy = -1, ny

                      
                      ! if either of the faces associated with this cell is needed,
                      ! the vertex associated with this cell is needed
                      !if ( isNeeded( fcxi( ix, iy ) ) .or. IsNeeded( fcyi( ix, iy ) ) ) then
                      if ( isVertexNeeded( ix, iy ) ) then 
                              ! vertex is needed
                      else
                              vxi( ix, iy ) = - vxi( ix, iy )
                      end if
              end do
      end do


      ! search x-points
      svc = 0
      do ix = -1, nx
              do iy = -1, ny
      
                      ! test whether vertex is special vertex
                      if ( isSpecialVertex( ix, iy ) ) then
                              svc = svc + 1
                              svix( svc ) = ix
                              sviy( svc ) = iy
                      end if

              end do
      end do
      call logmsg( LOGDEBUG, "b2ITMCreateMap: found "//int2str(svc)//" special vertices (x-points)" )


      ! identify duplicate special vertices
      
      svixAlias = 0
      sviyAlias = 0

      do i1 = 1, svc - 1

              ix = svix( i1 )
              iy = sviy( i1 )

              ! has the point already been identified as unneeded?
              if ( .not. isNeeded( vxi( ix, iy ) ) ) cycle

              ! compare with remaining vertices
              do i2 = i1 + 1, svc

                      ! has the point already been identified as unneeded?
                      if ( .not. isNeeded( vxi( svix(i2), sviy(i2) ) ) ) cycle

                      if ( match( crx( ix, iy, 0 ), cry( ix, iy, 0 ), &
                           &  crx( svix(i2), sviy(i2), 0 ), cry( svix(i2), sviy(i2), 0 ) ) ) then

                              vxi( svix(i2), sviy(i2) ) = - abs( vxi( svix(i2), sviy(i2) ) )

                              svixAlias( i2 ) = ix
                              sviyAlias( i2 ) = iy
                      end if                  

              end do
      end do

      ! number of unique cells 
      ncv = ( nx + 2 ) * ( ny + 2 ) - count( .not. isNeeded( cvi ) )
      ! number of unique faces (x and y direction)
      nfcx = ( nx + 2 ) * ( ny + 2 ) - count( .not. isNeeded( fcxi ) )
      nfcy = ( nx + 2 ) * ( ny + 2 ) - count( .not. isNeeded( fcyi ) )
      ! number of unique vertices
      nvx = ( nx + 2 ) * ( ny + 2 ) - count( .not. isNeeded( vxi )  )

      ! allocate the mapping structure
      call allocateB2ITMGridDesc( gd, nx, ny, ncv, nfcx, nfcy, nvx )

      ! build the mappings

      ! ...for cells
      ic = 0
      do ix = -1, nx
              do iy = -1, ny
                      if ( isNeeded( cvi( ix, iy ) ) ) then
                              ic = ic + 1
                              if ( ic > ncv ) then
                                      stop 'b2ITMCreateMap: found more cells than expected'
                              end if
                              ! Map CPO -> B2
                              gd%mapCvix(ic) = ix
                              gd%mapCviy(ic) = iy
                              ! Map B2 -> CPO
                              gd%mapCvI( ix, iy ) = ic
                      else
                              ! not needed
                              gd%mapCvI( ix, iy ) = 0
                      end if
              end do
      end do

      ! ...for x faces
      ic = 0
      do ix = -1, nx
              do iy = -1, ny
                      if ( isNeeded( fcxi( ix, iy ) ) ) then
                              ic = ic + 1
                              if ( ic > nfcx ) then
                                      stop 'b2ITMCreateMap: found more x-aligned faces than expected'
                              end if
                              ! Map CPO -> B2
                              gd%mapFcix(ic) = ix
                              gd%mapFciy(ic) = iy
                              gd%mapFcAlign(ic) = ALIGNX
                              ! Map B2 -> CPO
                              gd%mapFcxI( ix, iy ) = ic
                      else
                              ! not needed
                              gd%mapFcxI( ix, iy ) = 0
                      end if
              end do
      end do
      ! sanity check for number of x faces
      if ( ic /= nfcx ) then
              stop 'b2ITMCreateMap: found less x-aligned faces than expected'
      end if
      ! ...for y faces, continue counting the total faces in ic
      do ix = -1, nx
              do iy = -1, ny
                      if ( isNeeded( fcyi( ix, iy ) ) ) then
                              ic = ic + 1
                              if ( ic > nfcx + nfcy ) then
                                      stop 'b2ITMCreateMap: found more y-aligned faces than expected'
                              end if
                              ! Map CPO -> B2
                              gd%mapFcix(ic) = ix
                              gd%mapFciy(ic) = iy
                              gd%mapFcAlign(ic) = ALIGNY
                              ! Map B2 -> CPO
                              gd%mapFcyI( ix, iy ) = ic
                      else
                              ! not needed
                              gd%mapFcyI( ix, iy ) = 0
                      end if
              end do
      end do

      ! vertices
      ic = 0
      do ix = -1, nx
              do iy = -1, ny
                      if ( isNeeded( vxi( ix, iy ) ) ) then
                              ic = ic + 1
                              if ( ic > nvx ) then
                                      stop 'b2ITMCreateMap: found more needed vertices than expected'
                              end if
                              ! Map CPO -> B2
                              gd%mapVxix(ic) = ix
                              gd%mapVxiy(ic) = iy
                              ! Map B2 -> CPO
                              gd%mapVxI( ix, iy ) = ic
                      else
                              ! Vertex marked as not needed. Check whether
                              ! vertex was identified as special (x-point), 
                              ! and retrieve the vertex it is aliased to
                              do i1 = 1, svc
                                      if ( ( svix( i1 ) == ix ) &
                                           & .and. ( sviy( i1 ) == iy ) ) then
                                              ! Found it in the special vertex list. 
                                              ! Now set the value in the  vertex map accordingly
                                              gd%mapVxI( ix, iy ) = &
                                                   & gd%mapVxI( svixAlias( i1 ), sviyAlias( i1 ) )
                                      end if
                              end do


                      end if
              end do
      end do

      call logmsg( LOGDEBUG, "b2ITMCreateMap: finished setting up map" )
      call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%ncv)//" unique cells")
      call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%nfcx)//" unique x-aligned faces")
      call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%nfcy)//" unique y-aligned faces")
      call logmsg( LOGDEBUG, "b2ITMCreateMap: map contains  "//int2str(gd%nvx)//" unique vertices")



    contains 

      logical elemental function isNeeded( index ) 
        integer, intent(in) :: index

        isNeeded = ( index > 0 )
      end function isNeeded

      ! test whether cell (ix,iy) is a ghost cell
      function isGhostCell( ix, iy ) 
        logical isGhostCell
        integer, intent(in) :: ix, iy

        isGhostCell = &
             & ( leftix( ix, iy ) == -2 ) &
             & .or. ( rightix( ix, iy ) == ( nx + 1 ) ) &
             & .or. ( bottomiy( ix, iy ) == -2 ) &
             & .or. ( topiy( ix, iy ) == ( ny + 1 ) )

      end function isGhostCell


      ! test whether an x face (ix,iy) (the bottom face associated with cell (ix,iy))
      function isXFaceGhostFace( ix, iy ) 
        logical isXFaceGhostFace
        integer, intent(in) :: ix, iy

        ! if the given cell is not a ghost cell, the bottom face cannot be a ghost face
        if ( .not. isGhostCell( ix, iy ) ) then
                isXFaceGhostFace = .false.
                return
        end if
        
        ! ...is a ghost face if no neighbour to the bottom/south direction
        isXFaceGhostFace = .not. isInDomain( bottomix( ix, iy ), bottomiy( ix, iy ), extended = .true. )
        if ( isXFaceGhostFace ) return
        
        ! ...is a ghost face if bottom/south neighbour is a ghost cell
        if ( isGhostCell( bottomix( ix, iy ), bottomiy( ix, iy ) ) ) then
                isXFaceGhostFace = .true.
        end if

      end function isXFaceGhostFace


      ! test whether an y face (ix,iy) (the left face associated with cell (ix,iy))      
      function isYFaceGhostFace( ix, iy ) 
        logical isYFaceGhostFace
        integer, intent(in) :: ix, iy

        ! if the given cell is not a ghost cell, the left face cannot be a ghost face
        if ( .not. isGhostCell( ix, iy ) ) then
                isYFaceGhostFace = .false.
                return
        end if

        ! ...is a ghost face if no neighbour exists to the left/west direction
        isYFaceGhostFace  = .not. isInDomain( leftix( ix, iy ), leftiy( ix, iy ), extended = .true. ) 
        if ( isYFaceGhostFace ) return

        ! ...is a ghost face if left/west neighbour is a ghost cell
        if ( isGhostCell( leftix( ix, iy ), leftiy( ix, iy ) ) ) then
                isYFaceGhostFace = .true.
        end if

      end function isYFaceGhostFace



      ! Check whether a position (ix,iy) is inside the grid.
      ! The default is to check whether the extended grid (including the ghost cells).
      ! If the optional parameter extended is given, extended = .false. will check whether the position
      ! is inside the actual physical domain of the grid.
      function isInDomain( ix, iy, extended )
        logical :: isInDomain
        integer, intent(in) :: ix, iy
        logical, intent(in), optional :: extended

        ! internal
        logical :: lExtended
        
        lExtended = .true.
        if ( present( extended ) ) lExtended = extended
        
        if ( lExtended ) then
                ! in extended domain (including ghost cells)?
                isInDomain = ( ix >= -1 ) .and. (ix <= nx) .and. ( iy >= -1 ) .and. ( iy <= ny )
        else
                isInDomain = ( ix > -1 ) .and. (ix < nx) .and. ( iy > -1 ) .and. ( iy < ny )
        end if
      end function isInDomain
      

      ! Test whether the vertex associated with the cell (ix, iy) is special, i.e. a x-point
      ! This steps around the vertex (left-bottom-right-top) and checks
      ! whether the resulting position is equal to the starting position
      logical function isSpecialVertex( ix, iy ) 
        integer, intent(in) :: ix, iy

        ! internal
        integer :: x, y, xn, yn
        
        x = ix
        y = iy

        ! step left
        xn = leftix( x, y )
        yn = leftiy( x, y )
        if ( .not. isInDomain( xn, yn ) ) then
                isSpecialVertex = .false.
                return
        end if
        x = xn
        y = yn

        ! step bottom
        xn = bottomix( x, y )
        yn = bottomiy( x, y )
        if ( .not. isInDomain( xn, yn ) ) then
                isSpecialVertex = .false.
                return
        end if
        x = xn
        y = yn

        ! step right
        xn = rightix( x, y )
        yn = rightiy( x, y )
        if ( .not. isInDomain( xn, yn ) ) then
                isSpecialVertex = .false.
                return
        end if
        x = xn
        y = yn

        ! step top
        xn = topix( x, y )
        yn = topiy( x, y )
        if ( .not. isInDomain( xn, yn ) ) then
                isSpecialVertex = .false.
                return
        end if
        x = xn
        y = yn

        ! do we end up where we left?
        isSpecialVertex = .not. ( ( x == ix ) .and. ( y == iy ) ) 

      end function isSpecialVertex

      ! Decide whether a vertex is needed based on whether a face
      ! connected to this vertex is needed
      logical function isVertexNeeded( ix, iy ) 
        integer, intent(in) :: ix, iy

        ! internal
        integer :: lix, liy, bix, biy

        lix = leftix( ix, iy )
        liy = leftiy( ix, iy )
        if ( .not. isInDomain( bix, biy ) ) then
                ! if neighbour not in domain, reset to current position
                lix = ix
                liy = iy
        end if

        bix = bottomix( ix, iy )
        biy = bottomiy( ix, iy )
        if ( .not. isInDomain( bix, biy ) ) then
                ! if neighbour not in domain, reset to current position
                bix = ix
                biy = iy
        end if

        isVertexNeeded = isNeeded( fcxi( ix, iy ) ) .or. isNeeded( fcyi( ix, iy ) ) &
             & .or. isNeeded( fcxi( lix, liy ) ) .or. isNeeded( fcyi( bix, biy ) ) 
      
      end function isVertexNeeded

    end subroutine b2ITMCreateMap

    
    real(R8) function dist(x1,y1,x2,y2) 
      real(R8), intent(in) :: x1, y1, x2, y2

      dist = sqrt( (x1-x2)**2+(y1-y2)**2 )
    end function dist

    logical function match(x1,y1,x2,y2) 
      real(R8), intent(in) :: x1, y1, x2, y2

      match = dist(x1,y1,x2,y2).lt.geom_match_dist
    end function match

end module b2ITMMapping
