module itm_limiter_mockup2

  ! Mockup for limiter data structure

  use euITM_utilities
  use itm_types

  implicit none


  ! Second mockup, based on proposal from R. Coelho, 17.12.2010

  type type_limiter_element
     integer :: closed                ! either ITM_STRUCT_OPEN or ITM_STRUCT_CLOSED
     type (type_rz1D) :: position     
  end type type_limiter_element

  type type_limiter
     type (type_limiter_element), dimension( : ), allocatable :: element
  end type type_limiter

  integer, parameter :: ITM_STRUCT_OPEN = 0
  integer, parameter :: ITM_STRUCT_CLOSED = 1

contains

  
  subroutine itmlimReadFromStructureFile_Mockup2(filename, limiter)
    character(*), intent(in) :: filename
    type(type_limiter), intent(inout) :: limiter

    ! internal
    
    integer, parameter :: nstrmx = 100 ! max. number of structures
    integer, parameter :: npstmx = 200 ! max. number of points per structure

    integer :: nstruc,npstru(nstrmx)
    real*8 :: xstruc(npstmx,nstrmx),ystruc(npstmx,nstrmx)
    character nomstr(nstrmx)*80

    integer :: is

    ! open & read structure file using CARRE routine
    OPEN(UNIT=8, FILE=filename, STATUS='old')
    nstruc=0
    call listru(8,nstruc,npstru,nomstr,xstruc,ystruc,npstmx,nstrmx)
    ! ... file is closed in listru

    ! transfer to CPO structure
    allocate( limiter % element( nstruc ) )
    
    do is = 1, nstruc

            if ( npstru( is ) < 0 ) then
               limiter % element % closed = ITM_STRUCT_CLOSED
            else
               limiter % element % closed = ITM_STRUCT_OPEN
            end if

            allocate( limiter % element( is ) % position % r( npstru( is ) ) )
            allocate( limiter % element( is ) % position % z( npstru( is ) ) )
            limiter % element(is) % position % r = xstruc( 1:npstru(is), is )
            limiter % element(is) % position % z = ystruc( 1:npstru(is), is )

    end do
    
  end subroutine itmlimReadFromStructureFile_Mockup2



end module itm_limiter_mockup2
