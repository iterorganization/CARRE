module itm_grid_access

  use itm_types
  use euITM_schemas

  implicit none


contains

  ! Return dimension of the grid described by a given grid descriptor.
  integer function gridGetNDim( grid )
    type(type_grid_full), intent(in) :: grid

    ! internal
    integer :: i

    gridGetNDim = 0
    do i = 1, size( grid % spaces ) 
            gridGetNDim = gridGetNDim + size( grid % spaces( i ) % type_coord )
    end do

  end function gridGetNDim

  ! Returns the dimensionality of the individual spaces
  subroutine gridGetSpaceDims( grid, dims ) 
    type(type_grid_full), intent(in) :: grid
    integer, dimension(:), allocatable, intent(out) :: dims

    ! internal
    integer :: i

    allocate( dims( size( grid % spaces ) ) )

    do i = 1, size( grid % spaces ) 
            dims( i ) = size( grid % spaces( i ) % type_coord )
    end do
  end subroutine gridGetSpaceDims


  ! Returns index of a node according to the implicit ordering rules for the grid descriptor
  integer function gridGetNodeIndex( grid, nodeind ) result( index )
    type(type_grid_full), intent(in) :: grid
    integer, dimension(:), intent(in) :: nodeind
    
    integer :: i, s
    
    ! TODO: size( nodeind ) == size( spaces ) 
    
    index = nodeind(1)
    s = size( grid % spaces( 1 ) % node_value, 1 )

    do i = 2, size( grid % spaces )
            index = index + s * ( nodeind( i ) - 1 )
            s = s * size( grid % spaces( i ) % node_value, 1 )
    end do
    
  end function gridGetNodeIndex


  ! Get the coordinates of a node according to it's index
  subroutine gridGetNodeCoord( grid, nodeind, coord )
    type(type_grid_full), intent(in) :: grid
    integer, dimension(:), intent(in) :: nodeind
    real(R8), dimension(:), allocatable :: coord
    
    ! internal   
    integer :: is, id, nd

    allocate( coord( gridGetNDim( grid ) ) )

    id = 0 ! coordinate counter
    do is = 1, size( grid % spaces )
            ! get dimensionality of current space
            nd = size( grid % spaces( is ) % type_coord )
            ! copy coordinates
            coord(id + 1 : id + nd ) = grid % spaces( is ) % node_value( nodeind( is ), 1:nd )
            ! increase coordinate counter
            id = id + nd
    end do

  end subroutine gridGetNodeCoord


end module itm_grid_access
