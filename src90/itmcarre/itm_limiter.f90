module itm_limiter_mockup

  ! Mockup for limiter data structure

  use euITM_utilities
  use itm_types

  implicit none

  type type_limgeometry  !    
          integer,pointer  :: npoints(:,:) => null()     ! /limgeometry/npoints - Number of points describing an element (irregular outline rzcoordinates); Matrix (nlims,max_nelements)
          integer,pointer  :: closed(:,:) ! /limgeometry/closed - Flag indicating whether a structure is closed (i.e. the last and first point are to be connected); closed(:,:) = 0 means open, closed(:,:) = 1 means closed. Matrix (nlims,max_nelements)
          type (type_rz3D) :: rzcoordinate  ! /limgeometry/rzcoordinate - Irregular outline [m]; 3D arrays (nlims,max_nelements,max_npoints)
  endtype type_limgeometry

  type type_limelement  !    
          character(len=132), dimension(:), pointer ::name => null()       ! /limelement/name - Name of this element. Should be a matrix of strings (nlims,max_nelements), but not supported by the UAL yet.
          !character(len=132), dimension(:), pointer ::id => null()       ! /limelement/id - ID of this element. Should be a matrix of strings (nlims,max_nelements), but not supported by the UAL yet.
          type (type_limgeometry) :: limgeometry  ! /limelement/limgeometry - 
  endtype type_limelement

  integer, parameter :: ITM_STRUCT_OPEN = 0
  integer, parameter :: ITM_STRUCT_CLOSED = 1


contains

  
  subroutine itmlimReadFromStructureFile(filename, limiter)
    character(*), input(in) :: filename    
    type(type_limelement), intent(inout) :: limiter

    ! internal    
    
    integer, parameter :: nstrmx = 100 ! max. number of structures
    integer, parameter :: npstmx = 200 ! max. number of points per structure

    integer :: nstruc,npstru(nstrmx)
    real*8 :: xstruc(npstmx,nstrmx),ystruc(npstmx,nstrmx)
    character nomstr(nstrmx)*80

    integer :: is

    ! open & read structure file
    OPEN(UNIT=8, FILE=name, STATUS='old')
    nstruc=0
    call listru(8,nstruc,npstru,nomstr,xstruc,ystruc,npstmx,nstrmx)
    ! ... file is closed in listru

    ! transfer to CPO structure
    allocate( limiter % name( nstruc ) )
    allocate( limiter % limgeometry % npoints( nstruc, 1 ) )
    allocate( limiter % limgeometry % closed( nstruc, 1 ) )
    allocate( limiter % limgeometry % rzcoordinate % r( nstruc, 1, maxval( npstru ) ) )
    allocate( limiter % limgeometry % rzcoordinate % z( nstruc, 1, maxval( npstru ) ) )


    limiter % limgeometry % closed = ITM_STRUCT_OPEN
    do is = 1, nstruc

            limiter % name( is ) = nomstr( is )
            limiter % limgeometry % npoints( is, 1 ) = abs( npstru( is ) )
            if ( npstru < 0 ) then             
                    limiter % limgeometry % closed( is, 1 ) = ITM_STRUCT_CLOSED
            end if
            limiter % limgeometry % rzcoordinate % r( is, 1, 1 : abs(npstru( is )) ) &
                 & = xstruc(:,is)
            limiter % limgeometry % rzcoordinate % z( is, 1, 1 : abs(npstru( is )) ) &
                 & = ystruc(:,is)

    end do
    
  end subroutine itmlimReadFromStructureFile



end module itm_limiter_mockup
