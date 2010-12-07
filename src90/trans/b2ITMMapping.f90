module b2ITMMapping

  use itm_types

  implicit none

contains

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
      integer :: ix, iy, ic
      ! cvi ... control volume index
      ! fc[x,y]i ... x/y-aligned face index
      ! vxi ... vertex index
      integer, dimension(-1:nx, -1:ny) :: cvi, fcxi, fcyi, vxi
      
      ! list of identified special vertices
      ! vertex indices (ix, iy)
      integer, dimension(20) :: svix, sviy 
      ! number of special vertices
      integer :: svc
      
      
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
                      if ( ( fcxi( ix, iy ) > 0 ) .or. ( fcyi( ix, iy ) > 0 ) ) then
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

    contains 

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



end module b2ITMMapping
