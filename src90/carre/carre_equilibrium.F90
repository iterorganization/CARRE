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
  subroutine extend_equilibrium(equ, psimin, psimax, &
       & rMin, rMax, zMin, zMax, &
       & addLeft, addRight, addBottom, addTop)
    type(CarreEquilibrium), intent(inout) :: equ
    double precision, intent(in) :: psimin, psimax, rMin, rMax, zMin, zMax
    integer, intent(in) :: addLeft, addRight, addTop, addBottom

    ! internal
    double precision :: dx, dy
    integer :: i, ixCutMin, ixCutMax, iyCutMin, iyCutMax
    double precision :: newPsi(NXMAX, NYMAX)

    external :: ifind
    integer :: ifind


    call assert( equ%nx + addLeft + addRight <= nxmax )
    call assert( equ%ny + addTop + addBottom <= nxmax )

    ! cut off psi values
    where (equ%psi < psimin) equ%psi = huge(equ%psi)
    where (equ%psi > psimax) equ%psi = huge(equ%psi)

    if ( rMin > equ%x(1) ) then
       ixCutMin = ifind( rMin, equ%x, equ%nx, 1)
       equ%psi(1 : ixCutMin - 1, :) = huge(equ%psi)
    end if

    if ( rMax < equ%x(equ%nx) ) then
       ixCutMax = ifind( rMax, equ%x, equ%nx, 1)
       equ%psi(ixCutMax + 1 : equ%nx, :) = huge(equ%psi)
    end if

    if ( zMin > equ%y(1) ) then
       iyCutMin = ifind( zMin, equ%y, equ%ny, 1)
       equ%psi(:, 1 : iyCutMin - 1) = huge(equ%psi)
    end if

    if ( zMax < equ%y(equ%ny) ) then
       iyCutMax = ifind( zMax, equ%y, equ%ny, 1)
       equ%psi(:, iyCutMax + 1 : equ%ny) = huge(equ%psi)
    end if

    ! extend grid towards left, right, bottom, top
    dx = equ%x(2) - equ%x(1)
    dy = equ%y(2) - equ%y(1)

    equ%x(addLeft + 1 : addLeft + equ%nx) = equ%x(1 : equ%nx)
    equ%y(addBottom + 1 : addBottom + equ%ny) = equ%y(1 : equ%ny)
    
    equ%x(1 : addLeft) = (/ ( equ%x(addLeft + 1) - addLeft*dx + i*dx , i = 0, addLeft-1) /)
    equ%x(addLeft + equ%nx + 1 : addLeft + equ%nx + addRight) = (/ ( i*dx + equ%x(equ%nx + addLeft), i = 1, addRight) /)

    equ%y(1 : addBottom) = (/ ( equ%y(addBottom + 1)- addBottom*dy + i*dy , i = 0, addBottom-1) /)
    equ%y(addBottom + equ%ny + 1 : addBottom + equ%ny + addTop) = (/ ( i*dy + equ%y(equ%ny + addBottom), i = 1, addTop) /)

    ! move present psi data
    newPsi = huge(newPsi)
    newPsi( addLeft + 1 : addLeft + equ%nx, addBottom + 1 : addBottom + equ%ny ) = equ%psi(1 : equ%nx, 1 : equ%ny)
    equ%psi = newPsi

    equ%nx = equ%nx + addLeft + addRight
    equ%ny = equ%ny + addTop + addBottom

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



end module carre_equilibrium
