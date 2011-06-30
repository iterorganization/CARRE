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
  subroutine extend_equilibrium(equ, psimin, psimax, addLeft, addRight, addBottom, addTop)
    type(CarreEquilibrium), intent(inout) :: equ
    double precision, intent(in) :: psimin, psimax
    integer, intent(in) :: addLeft, addRight, addTop, addBottom

    ! internal
    double precision :: dx, dy
    integer :: i

    call assert( equ%nx + addLeft + addRight <= nxmax )
    call assert( equ%ny + addTop + addBottom <= nxmax )

    ! cut off psi values
    where (equ%psi < psimin) equ%psi = huge(equ%psi)
    where (equ%psi > psimax) equ%psi = huge(equ%psi)

    ! extend grid towards left, right, bottom, top
    dx = equ%x(2) - equ%x(1)
    dy = equ%y(2) - equ%y(1)

    equ%x(addLeft + 1 : addLeft + equ%nx) = equ%x(1 : equ%nx)
    equ%y(addBottom + 1 : addBottom + equ%ny) = equ%y(1 : equ%ny)
    
    equ%x(1 : addLeft) = (/ ( i*dx - addLeft*dx, i = 0, addLeft-1) /)
    equ%x(addLeft + equ%nx + 1 : addLeft + equ%nx + addRight) = (/ ( i*dx + equ%x(equ%nx), i = 1, addRight) /)

    equ%y(1 : addBottom) = (/ ( i*dy - addBottom*dy, i = 0, addBottom-1) /)
    equ%y(addBottom + equ%ny + 1 : addBottom + equ%ny + addTop) = (/ ( i*dy + equ%y(equ%ny), i = 1, addTop) /)

    equ%nx = equ%nx + addLeft + addRight
    equ%ny = equ%ny + addTop + addBottom

    ! Run extension algorithm
    call compute_distance_exact(equ, 1.0d0)

  end subroutine extend_equilibrium

  subroutine compute_distance_exact(equ, factor)
    type(CarreEquilibrium), intent(inout) :: equ
    double precision, intent(in) :: factor

    ! internal
    integer :: ix, iy
    double precision :: newDist(nxmax, nymax)

    do ix = 1, equ%nx
       do iy = 1, equ%ny
          
          if ( equ%psi(ix, iy) == huge(equ%psi(ix, iy)) ) then
             newDist(ix, iy) = minDist(ix, iy)
          end if

       end do
    end do

  contains

    !> Compute minimal psi distance to existing psi values, taking the scaling factor into account
    double precision function minDist(ixp, iyp)
      integer, intent(in) :: ixp, iyp

      ! internal
      integer :: ix, iy
      double precision :: dist

      minDist = huge(minDist)

      do ix = 1, equ%nx
         do iy = 1, equ%ny

            if ( equ%psi(ix, iy) == huge(equ%psi(ix, iy)) ) cycle
            
            dist = sqrt( (equ%x(ixp) - equ%x(ix)) ** 2 &
                 & + (equ%y(iyp) - equ%y(iy)) ** 2 )

            minDist = min( equ%psi(ix, iy) + dist * factor, minDist)
         end do
      end do

    end function minDist

  end subroutine compute_distance_exact



end module carre_equilibrium
