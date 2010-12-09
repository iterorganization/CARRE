module itm_grid

  use itm_types

  implicit none      

  
  integer, parameter :: GRID_UNDEFINED = 0

  ! The following are part of the ITM convention machine coordinate system
  integer, parameter :: COORDTYPE_R = 2 ! Major radius (m)
  integer, parameter :: COORDTYPE_Z = 3 ! Vertical height (m) 

contains

  subroutine sanityCheckSpace( space )
    type(type_grid_space), intent(in) :: space




    ! Coordinate types
    ! (dimension of space = NDIM = size( type_coord )
    allocate( space % type_coord(NDIM) )    
    space % type_coord = (/ type_coord /)

    ! Number of objects
    allocate( space % nobject(NDIM) )
    space % nobject = (/ size(points) - 1 /)

    ! Number of boundaries the objects have (at maximum)
    allocate( space % nobject_bou(NDIM) )
    space % nobject_bou = (/ 2 /)

    ! Maximum number of neighbours an object can have per side
    allocate( space % neighborside(NDIM) )
    space % neighborside = (/ 1 /)

    ! Fill in node information
    allocate( space % node_value(size(points), NDIM) )
    space % node_value(:, 1) = points

    ! Fill in object definitions (i.e. what objects compose an object)
    allocate( space % objdef( maxval(space % nobject), maxval(space % nobject_bou), NDIM ) )
    ! first set all to undefined
    space % objdef = GRID_UNDEFINED

    do i = 1, size(points) - 1
            ! left point of edge
            space%objdef( i, 1, 1 ) = i
            ! right point of edge
            space%objdef( i, 2, 1 ) = i + 1
    end do

    ! 1d objects: faces(edges)

    ! Fill in connectivity information
    allocate( space % neighbors( maxval( space % nobject) , &
         & maxval(space % nobject_bou), NDIM ) )
    ! first set all to undefined
    space % neighbors = GRID_UNDEFINED

    ! first edge: no left neighbour
    space%neighbors( 1, 1, 1 ) = GRID_UNDEFINED
    space%neighbors( 1, 2, 1 ) = 2

    ! internal edge: left + right neighbour
    do i = 2, size(points) - 2
            space%neighbors( i, 1, 1 ) = i - 1 
            space%neighbors( i, 2, 1 ) = i - 2
    end do

    ! last edge: no right neighbour
    space%neighbors( size(points)-1, 1, 1 ) = size(points)-2
    space%neighbors( size(points)-1, 2, 1 ) = GRID_UNDEFINED


  end subroutine sanityCheckSpace


end module itm_grid
