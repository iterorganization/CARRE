module itmcarre

  use euITM_schemas
  use itm_assert
  use Logging

  use carre_main
  use carre_types
  use CarreDiagnostics

  implicit none

#include <CARREDIM.F>
  
contains


  subroutine itmcarre(equcpo, limcpo, edgecpo )
        
    type(type_equilibrium), intent(in), pointer :: equcpo(:)
    type(type_limiter), intent(in) :: limcpo
    type(type_edge),intent(out),pointer :: edgecpo(:)

    ! internal
    integer :: ifail

    type(CarreParameters) :: par
    type(CarreEquilibrium) :: equ
    type(CarreStructures) :: struct
    type(CarreGrid) :: grid
    type(CarreDiag) :: diag


    ! 1. Read code parameters (will come from type_param in the future)
    call logmsg( LOGDEBUG, "itmcarre: reading carre.dat" )

    OPEN(UNIT=9, FILE='carre.dat', STATUS='unknown')
    CALL CHANGE(par,9,0,ifail)
    if ( ifail /= 0 ) then
        stop 'itmcarre: error reading carre.dat'
    end if

    ! 2. Read equilibrium  
    call logmsg( LOGDEBUG, "itmcarre: reading equilibrium CPO" )
    call read_equilibrium( equcpo(1), equ )

    ! 3. Read limiter
    call logmsg( LOGDEBUG, "itmcarre: reading limiter CPO" )
    call read_structures( limcpo, struct )

    ! 4. Call carre main program
    call logmsg( LOGDEBUG, "itmcarre: creating grid" )
    call carre_main_computation( equ, struct, par, grid, diag )


    ! 5. Put resulting grid into CPO
    call logmsg( LOGDEBUG, "itmcarre: writing edge CPO" )
    allocate( edgecpo(1) )

  end subroutine itmcarre
  


  !> Transfer equilbrium grid and data from CPO into Carre data structure.
  subroutine read_equilibrium( equcpo, equ )
    type(type_equilibrium), intent(in) :: equcpo
    type(CarreEquilibrium), intent(out) :: equ


    ! internal
    integer :: ix, iy

    call assert( equcpo % profiles_2d % grid_type(1) == '1', &
        & "read_equilibrium: Equ. CPO not in right format: no rect. grid" )
    equ%nx = size( equcpo % profiles_2d % grid % dim1 )
    equ%ny = size( equcpo % profiles_2d % grid % dim2 )

    call assert( (equ%nx <= nxmax) .and. (equ%ny <= nymax), &
        & "read_equilibrium: equilibrium grid too large" )

    equ%x(1:equ%nx) = equcpo % profiles_2d % grid % dim1
    equ%y(1:equ%ny) = equcpo % profiles_2d % grid % dim2
    equ%psi(1:equ%nx, 1:equ%ny) = equcpo % profiles_2d % psi

  end subroutine read_equilibrium


  !> Transfer device structure CPO into Carre data structure.
  subroutine read_structures( limcpo, struct )
    type(type_limiter), intent(in) :: limcpo
    type(CarreStructures), intent(out) :: struct

    ! internal
    integer :: is

    struct%nstruc = size( limcpo % limiter_unit )
    call assert( struct%nstruc <= strumx, &
        & "read_structures: too large number of structures in CPO" )
    
    do is = 1, struct%nstruc
        struct%nomstr(is) = limcpo % limiter_unit(is) % name(1)(1:80)

        struct%npstru(is) = size( limcpo % limiter_unit(is) % position % r )

        call assert( struct%npstru(is) <= npstmx, &
            & "read_structures: too many points (1) in structure "//int2str(is) )

        struct%xstruc(1:struct%npstru(is), is) = limcpo % limiter_unit(is) % position % r
        struct%ystruc(1:struct%npstru(is), is) = limcpo % limiter_unit(is) % position % z

        ! Repeat first point if closed structure
        if ( limcpo % limiter_unit( is ) % closed(1) == 'y' ) then
            struct%npstru(is) = struct%npstru(is) + 1
            call assert( struct%npstru(is) <= npstmx, &
                & "read_structures: too many points (2) in structure "//int2str(is) )

            struct%xstruc(struct%npstru(is), is) = struct%xstruc(1, is)
            struct%ystruc(struct%npstru(is), is) = struct%ystruc(1, is)
        end if

    end do
    
  end subroutine read_structures

end module itmcarre
