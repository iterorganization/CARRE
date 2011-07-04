module carre_equilibrium

  use carre_types
  use itm_assert

#ifdef USE_SILO
  use SiloIO
  use CarreSiloIO
#endif

  implicit none

#include <CARREDIM.F>

  private
  public extend_equilibrium

contains

  !> Extend a given equilibrium by throwing away data outside a given psi value and extending
  !> the remaining data.
  subroutine extend_equilibrium(equ, par, struct)
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreParameters), intent(in) :: par
    type(CarreStructures), intent(in) :: struct

    ! internal
    double precision :: dx, dy
    integer :: i, ixCutMin, ixCutMax, iyCutMin, iyCutMax
    double precision :: newPsi(NXMAX, NYMAX)

    external :: ifind
    integer :: ifind

    if (par%equExtensionMode == EQU_EXTENSION_OFF) return

    call assert( equ%nx + par%addLeft + par%addRight <= nxmax )
    call assert( equ%ny + par%addTop + par%addBottom <= nxmax )

    if (par%equExtensionMode == EQU_EXTENSION_MODE_SIMPLE) then

       call logmsg(LOGINFO, "carre_equilibrium: simple equilibrium extension enabled")

       ! cut off psi values
       where (equ%psi < par%psimin) equ%psi = huge(equ%psi)
       where (equ%psi > par%psimax) equ%psi = huge(equ%psi)

       if ( par%rMin > equ%x(1) ) then
          ixCutMin = ifind( par%rMin, equ%x, equ%nx, 1)
          equ%psi(1 : ixCutMin - 1, :) = huge(equ%psi)
       end if

       if ( par%rMax < equ%x(equ%nx) ) then
          ixCutMax = ifind( par%rMax, equ%x, equ%nx, 1)
          equ%psi(ixCutMax + 1 : equ%nx, :) = huge(equ%psi)
       end if

       if ( par%zMin > equ%y(1) ) then
          iyCutMin = ifind( par%zMin, equ%y, equ%ny, 1)
          equ%psi(:, 1 : iyCutMin - 1) = huge(equ%psi)
       end if

       if ( par%zMax < equ%y(equ%ny) ) then
          iyCutMax = ifind( par%zMax, equ%y, equ%ny, 1)
          equ%psi(:, iyCutMax + 1 : equ%ny) = huge(equ%psi)
       end if

    end if

    if (par%equExtensionMode == EQU_EXTENSION_MODE_VESSEL) then
       ! smart equilibrium cutoff: cut off points outside of vessel
       call logmsg(LOGINFO, "carre_equilibrium: vessel/smart equilibrium extension enabled")

       call equilibrium_vessel_cutoff(equ, struct)
    end if

    ! extend grid towards left, right, bottom, top
    dx = equ%x(2) - equ%x(1)
    dy = equ%y(2) - equ%y(1)

    equ%x(par%addLeft + 1 : par%addLeft + equ%nx) = equ%x(1 : equ%nx)
    equ%y(par%addBottom + 1 : par%addBottom + equ%ny) = equ%y(1 : equ%ny)
    
    equ%x(1 : par%addLeft) = (/ ( equ%x(par%addLeft + 1) - par%addLeft*dx + i*dx , i = 0, par%addLeft-1) /)
    equ%x(par%addLeft + equ%nx + 1 : par%addLeft + equ%nx + par%addRight) &
         & = (/ ( i*dx + equ%x(equ%nx + par%addLeft), i = 1, par%addRight) /)

    equ%y(1 : par%addBottom) = (/ ( equ%y(par%addBottom + 1)- par%addBottom*dy + i*dy , i = 0, par%addBottom-1) /)
    equ%y(par%addBottom + equ%ny + 1 : par%addBottom + equ%ny + par%addTop) &
         & = (/ ( i*dy + equ%y(equ%ny + par%addBottom), i = 1, par%addTop) /)

    ! move present psi data
    newPsi = huge(newPsi)
    newPsi( par%addLeft + 1 : par%addLeft + equ%nx, par%addBottom + 1 : par%addBottom + equ%ny ) &
         & = equ%psi(1 : equ%nx, 1 : equ%ny)
    equ%psi = newPsi

    equ%nx = equ%nx + par%addLeft + par%addRight
    equ%ny = equ%ny + par%addTop + par%addBottom

    ! Run extension algorithm
    call compute_distance_exact(equ, -3.0d0)

  end subroutine extend_equilibrium


  !> Compute uninitialized values of psi by computing the distance to the closest
  !> initialized value multiplied by some factor. This algorithm is fast, but imprecise.
  !> It uses a distance transform with approximate distance constants.

  subroutine compute_distance_exact(equ, factor)
    type(CarreEquilibrium), intent(inout) :: equ
    double precision, intent(in) :: factor

    ! internal
    integer :: ix, iy
    double precision :: newPsi(NXMAX, NYMAX)

    newPsi = equ%psi

    do ix = 1, equ%nx
       do iy = 1, equ%ny
          
          if ( equ%psi(ix, iy) == huge(equ%psi(ix, iy)) ) then
             newPsi(ix, iy) = minDist(ix, iy)
          end if

       end do
    end do

    equ%psi = newPsi

  contains

    !> Compute minimal psi distance to existing psi values, taking the scaling factor into account
    double precision function minDist(ixp, iyp)
      integer, intent(in) :: ixp, iyp

      ! internal
      integer :: ix, iy, ixMin, iyMin
      double precision :: dist

      minDist = huge(minDist)

      do ix = 1, equ%nx
         do iy = 1, equ%ny

            if ( equ%psi(ix, iy) == huge(equ%psi(ix, iy)) ) cycle
            
            dist = sqrt( (equ%x(ixp) - equ%x(ix)) ** 2 &
                 & + (equ%y(iyp) - equ%y(iy)) ** 2 )
            
            if ( dist < minDist ) then
               minDist = dist
               ixMin = ix
               iyMin = iy
            end if
         end do
      end do

      minDist = equ%psi(ixMin, iyMin) + minDist * factor

    end function minDist

  end subroutine compute_distance_exact


  !> Compute uninitialized values of psi by computing the distance to the closest
  !> initialized value multiplied by some factor. This algorithm is very slow, but precise.
  subroutine compute_distance_fast(equ, factor)
    type(CarreEquilibrium), intent(inout) :: equ
    double precision, intent(in) :: factor

    ! internal
    integer :: ix, iy
    double precision :: newPsi(NXMAX, NYMAX)

!!$    newPsi = equ%psi
!!$
!!$    do ix = 1, equ%nx
!!$       do iy = 1, equ%ny
!!$          
!!$          if ( equ%psi(ix, iy) == huge(equ%psi(ix, iy)) ) then
!!$             newPsi(ix, iy) = minDist(ix, iy)
!!$          end if
!!$
!!$       end do
!!$    end do
!!$
!!$    equ%psi = newPsi

  end subroutine compute_distance_fast


  subroutine equilibrium_vessel_cutoff(equ, struct)
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreStructures), intent(in) :: struct



  end subroutine equilibrium_vessel_cutoff



end module carre_equilibrium
