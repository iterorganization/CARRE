program itmcarre_wrapper

  use euITM_schemas
  use euITM_routines
  use itmcarre
  use b2mod_ual

  use Logging

  implicit none

  type(type_equilibrium), pointer :: equcpo(:)
  type(type_limiter) :: limcpo
  type(type_edge), pointer :: edgecpo(:)

  integer :: idx

  ! open ual
  call open_ual(idx)

  ! read input cpos from UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: reading cpos" )
  call read_ual(idx, equcpo, limcpo)

  ! call itmcarre
  call logmsg( LOGDEBUG, "itmcarre_wrapper: main computation" )
  call itmcarre_main(equcpo, limcpo, edgecpo )

  ! write output cpos to UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: writing cpos" )
  call write_ual(idx, edgecpo)

  ! close ual
  call close_ual(idx)

contains

  subroutine read_ual(idx, equcpo, limcpo)
    integer, intent(in) :: idx
    type(type_equilibrium), intent(out), pointer :: equcpo(:)
    type(type_limiter), intent(out) :: limcpo
    
    call euitm_get(idx,"equilibrium",equcpo)
    call euitm_get(idx,"limiter",limcpo)
    call logmsg( LOGDEBUG, "itmcarre_wrapper: equilibrium cpo #"//int2str(size(equcpo)) )
  end subroutine read_ual

  subroutine write_ual(idx, edgecpo)
    integer, intent(in) :: idx
    type(type_edge), intent(in), pointer :: edgecpo(:)

    call euitm_put(idx,"edge",edgecpo)
  end subroutine write_ual

end program itmcarre_wrapper
