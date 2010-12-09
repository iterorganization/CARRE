module itm_grid_examples

  use itm_types
  use euITM_schemas
  use itm_grid
  
  implicit none

contains

  subroutine packageInEdgeCPO( itmgrid, edgecpo )
    type(type_grid_full), intent(in) :: itmgrid
    type(type_edge), intent(inout) :: edgecpo
    
    allocate(edgecpo%datainfo%dataprovider(1))
    edgecpo%datainfo%dataprovider="IMP3"
    allocate(edgecpo%codeparam%codename(1))
    edgecpo%codeparam%codename(1)="ITMGRIDEXAMPLES"
    edgecpo%time= 0.0D0

    edgecpo%grid = itmgrid

  end subroutine packageInEdgeCPO

  !> Example routine that assembles a 2d grid consisting of two 1d spaces
  function example_2d() result( itmgrid )
    type(type_grid_full) :: itmgrid
    
    ! internal
    integer, parameter :: NDIM = 1
    integer, parameter :: NPOINTR = 10
    integer, parameter :: NPOINTZ = 10    

    integer :: i
    real(R8) :: pointsr(NPOINTR), pointsz(NPOINTZ)

    pointsr = (/ ( 1.0_R8 * i, i=0,NPOINTR-1) /)
    pointsz = (/ ( 0.5_R8 * i, i=0,NPOINTZ-1) /)

    allocate( itmgrid % spaces(2) )
    call fill1dSpace( itmgrid % spaces(1), COORDTYPE_R, pointsr )
    call fill1dSpace( itmgrid % spaces(2), COORDTYPE_Z, pointsz )
    itmgrid % metric => null()
    
  end function example_2d

  
  subroutine fill1dSpace( space, type_coord, points )
    type(type_grid_space), intent(inout) :: space
    integer, intent(in) :: type_coord
    real(R8), intent(in), dimension(:) :: points
    
    ! internal
    integer, parameter :: NDIM = 1

    integer :: i
    
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

  end subroutine fill1dSpace



end module itm_grid_examples
