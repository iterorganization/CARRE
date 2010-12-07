module b2ITMMapping

  use itm_types
  use euitm_schemas

  implicit none

  ! Distance between two points at which the points are declared to be equal
  real(R8), parameter :: geom_match_dist = 1.0e-6_R8

  integer, parameter :: ALIGNX = 0
  integer, parameter :: ALIGNY = 1


contains

  subroutine b2ITMFillGridDescription( grid )

    type(type_grid_full), intent(out) :: grid

    ! internal

    integer, parameter :: NDIM = 2

    allocate( grid % spaces(2) )
    grid % metric => null()

    ! Coordinate types
    ! (dimension of space = NDIM = size( type_coord )
    allocate( grid % spaces(1) % type_coord(NDIM) )
    grid % spaces(1) % type_coord(1) = (/ COORDTYPE_R, COORDTYPE_Z /)

    ! Number of objects
    allocate( grid % spaces(1) % nobject(NDIM) )
    grid % spaces(1) % nobject = (/ nfc, ncv /)

    ! Number of boundaries the objects have (at maximum)
    allocate( grid % spaces(1) % nobject_bou(NDIM) )
    grid % spaces(1) % nobject_bou = (/ 2, 4 /)

    ! Maximum number of neighbours an object can have per side
    allocate( grid % spaces(1) % neighborside(NDIM) )
    grid % spaces(1) % neighborside = (/ 0, 1 /)

    ! Fill in node information
    allocate( grid % spaces(1) % node_value(nvx, NDIM) )

    ix = mapVxix( ivx )
    iy = mapVxiy( ivx )
    grid % spaces(1) % node_value(ivx, 1) =  crx( ix, iy, 0 )
    grid % spaces(1) % node_value(ivx, 2) =  cry( ix, iy, 0 )

    ! Fill in object definitions
    allocate( grid % spaces(1) % objdef( maxval(grid % spaces(1) % nobject), &
         & maxval(grid % spaces(1) % nobject_bou), NDIM )

    
    

  end subroutine b2ITMFillGridCPO



  subroutine b2ITMCreateMap( nx,ny,crx,cry,&
         & leftix,leftiy,rightix,rightiy, &
         & topix,topiy,bottomix,bottomiy )

      !   ..input arguments (unchanged on exit)
      
      ! Size of grid arrays: (-1:nx, -1:ny) 
      integer  nx, ny
      !   .. output arguments
      ! vertex coordinates
      real (R8) ::&
           & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)
      integer ::&
           & leftix(-1:nx,-1:ny),leftiy(-1:nx,-1:ny),&
           & rightix(-1:nx,-1:ny),rightiy(-1:nx,-1:ny),&
           & topix(-1:nx,-1:ny),topiy(-1:nx,-1:ny),&
           & bottomix(-1:nx,-1:ny),bottomiy(-1:nx,-1:ny)

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

      ! Mapping arrays: 
      ! 1d CPO lists -> 2d B2 data structure
      integer, dimension(:), allocatable :: mapCvix, mapCviy, mapFcix, mapFciy, mapFcAlign, mapVxix, mapVxiy
      ! 2d B2 data structure -> 1d CPO lists
      integer, dimension(-1:nx,-1:ny) :: mapCvI, mapFcxI, mapFcyI, mapVxI


      
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

                      ! if either of the faces associated with this cell is needed,
                      ! the vertex associated with this cell is needed
                      if ( isNeeded( fcxi( ix, iy ) ) .or. IsNeeded( fcyi( ix, iy ) ) ) then
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

      ! allocate inverse mapping
      allocate( mapCvix(ncv), mapCviy(ncv) )
      allocate( mapFcix(nfcx+nfcy), mapFciy(nfcx+nfcy), mapFcAlign(nfcx+nfcy) )
      allocate( mapVxix(nvx), mapVxiy(nvx) )

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
                              mapCvix(ic) = ix
                              mapCviy(ic) = iy
                              ! Map B2 -> CPO
                              mapCvI( ix, iy ) = ic
                      else
                              ! not needed
                              mapCvI( ix, iy ) = 0
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
                              mapFcix(ic) = ix
                              mapFciy(ic) = iy
                              mapFcAlign(ic) = ALIGNX
                              ! Map B2 -> CPO
                              mapFcxI( ix, iy ) = ic
                      else
                              ! not needed
                              mapFcxI( ix, iy ) = 0
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
                              mapFcix(ic) = ix
                              mapFciy(ic) = iy
                              mapFcAlign(ic) = ALIGNY
                              ! Map B2 -> CPO
                              mapFcyI( ix, iy ) = ic
                      else
                              ! not needed
                              mapFcyI( ix, iy ) = 0
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
                              mapVxix(ic) = ix
                              mapVxiy(ic) = iy
                              ! Map B2 -> CPO
                              mapVxI( ix, iy ) = ic
                      else
                              ! Vertex marked as not needed. Check whether
                              ! vertex was identified as special (x-point), 
                              ! and retrieve the vertex it is aliased to
                              do i1 = 1, svc
                                      if ( ( svix( i1 ) == ix ) &
                                           & .and. ( sviy( i1 ) == iy ) ) then
                                              ! Found it in the special vertex list
                                              mapVxI( ix, iy ) = &
                                                   & mapVxI( svixAlias( i1 ), sviyAlias( i1 ) )
                                      end if
                              end do


                      end if
              end do
      end do

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
