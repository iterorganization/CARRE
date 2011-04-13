program itmcarre_wrapper

  use euITM_schemas
  use euITM_routines
  use itmcarre

  use Logging

  implicit none

  type(type_equilibrium), pointer :: equcpo(:)
  type(type_limiter) :: limcpo
  type(type_edge), pointer :: edgecpo(:)

  integer :: idx

  ! read input cpos from UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: reading cpos" )
  call read_ual(idx, equcpo, limcpo)

  ! call itmcarre
  call logmsg( LOGDEBUG, "itmcarre_wrapper: main computation" )
  call itmcarre(equcpo, limcpo, edgecpo )

  ! write output cpos to UAL
  call logmsg( LOGDEBUG, "itmcarre_wrapper: writing cpos" )
  call write_ual(idx, edgecpo)

contains

  subroutine read_ual(idx, equcpo, limcpo)
    integer, intent(out) :: idx
    type(type_equilibrium), intent(out), pointer :: equcpo(:)
    type(type_limiter), intent(out) :: limcpo

    ! internal
    integer :: shot, run, refshot, refrun
    character(len=5)::treename

    namelist /b2_ual_write_namelist/ treename, shot, run, refshot, refrun

    ! default values
    shot = 2
    run = 1
    refshot = 1
    refrun = 0
    treename = 'euitm'

!!$    ! read namelist from configuration file
!!$    read (ninp(0),b2_ual_write_namelist)
!!$    write (nout(0),b2_ual_write_namelist)

    ! establish UAL access

    call euitm_open(treename,shot,run,idx)
    call euitm_get(idx,"equilibrium",equcpo) 
    write(*,*) 'This CPO has ', size(equcpo),' time slices'
    call euitm_get(idx,"limiter",limcpo) 

  end subroutine read_ual

  subroutine write_ual(idx, edgecpo)
    integer, intent(in) :: idx
    type(type_edge), intent(in), pointer :: edgecpo(:)

    call euitm_put(idx,"edge",edgecpo)
  end subroutine write_ual

  subroutine close_ual(idx)
    integer, intent(in) :: idx

    call euitm_close(idx)
  end subroutine close_ual



end program itmcarre_wrapper
