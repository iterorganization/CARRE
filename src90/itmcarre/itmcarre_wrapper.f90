program itmcarre_wrapper

  use euITM_schemas
  use euITM_routines
  use itmcarre
  use b2mod_ual

  use Logging
  use Helper

  implicit none

  type(type_equilibrium), pointer :: equcpo(:)
  type(type_limiter) :: limcpo
  type(type_edge), pointer :: edgecpo(:)

  integer :: idx
  real(R8) :: time

  integer, parameter :: CLOSEST_SAMPLE = 1 ! until provided by ISIP

  ! read input cpos from UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: reading cpos" )
  ! equilibrium
  call open_ual(idx, time=time, nmlFile="ual.namelist.equilibrium")
  allocate(equcpo(1))
  call euitm_get_slice(idx, "equilibrium", equcpo(1), time, CLOSEST_SAMPLE)
  call close_ual(idx)
  ! limiter
  call open_ual(idx, nmlFile="ual.namelist.limiter")
  call euitm_get(idx, "limiter", limcpo)
  call close_ual(idx)  

  ! call itmcarre
  call logmsg( LOGDEBUG, "itmcarre_wrapper: main computation" )
  call itmcarre_main(equcpo, limcpo, edgecpo)

  ! write output cpos to UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: writing cpos" )
  call open_ual(idx, time=time, nmlFile="ual.namelist.edge", doCreate=.true.)  
  edgecpo(1)%time = time
  call euitm_put_slice(idx, "edge", edgecpo(1))
  call close_ual(idx)

end program itmcarre_wrapper
