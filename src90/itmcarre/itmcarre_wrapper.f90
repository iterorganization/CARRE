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

  ! read input cpos from UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: reading cpos" )
  ! equilibrium
  call open_ual(idx, nmlFile="ual.namelist.equilibrium")
  call euitm_get(idx,"equilibrium",equcpo)
  call close_ual(idx)
  ! limiter
  call open_ual(idx, nmlFile="ual.namelist.limiter")
  call euitm_get(idx,"limiter",limcpo)
  call close_ual(idx)  

  ! call itmcarre
  call logmsg( LOGDEBUG, "itmcarre_wrapper: main computation" )
  call itmcarre_main(equcpo, limcpo, edgecpo )

  ! write output cpos to UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: writing cpos" )
  call open_ual(idx, nmlFile="ual.namelist.edge", doCreate=.true.)
  call euitm_put(idx,"edge",edgecpo)
  call close_ual(idx)

end program itmcarre_wrapper
