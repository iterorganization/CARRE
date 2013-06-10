module carre_equilibrium

  use carre_types
  use itm_assert
  use carre_intersect
  use Logging
  use Helper

#ifdef USE_SILO
  use SiloIO
#endif
  use CarreSiloIO

  implicit none

#include <CARREDIM.F>

  private
  public extend_equilibrium, insideEquGrid, compute_psi_on_grid

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
    double precision :: psiPtO, psiPtX, distFactor

    external :: ifind, dist, feval2d
    integer :: ifind
    double precision :: dist, feval2d

    if (par%equExtensionMode == EQU_EXTENSION_OFF) return

    call assert( equ%nx + par%addLeft + par%addRight <= nxmax )
    call assert( equ%ny + par%addTop + par%addBottom <= nxmax )

    newPsi = equ%psi

    if (par%equExtensionMode >= EQU_EXTENSION_MODE_SIMPLE) then

       call logmsg(LOGINFO, "carre_equilibrium: equilibrium extension enabled")

       ! cut off psi values
       where (newPsi < par%psimin) newPsi = huge(newPsi)
       where (newPsi > par%psimax) newPsi = huge(newPsi)

       if ( par%rMin > equ%x(1) ) then
          ixCutMin = ifind( par%rMin, equ%x, equ%nx, 1)
          newPsi(1 : ixCutMin - 1, :) = huge(newPsi)
       end if

       if ( par%rMax < equ%x(equ%nx) ) then
          ixCutMax = ifind( par%rMax, equ%x, equ%nx, 1)
          newPsi(ixCutMax + 1 : equ%nx, :) = huge(newPsi)
       end if

       if ( par%zMin > equ%y(1) ) then
          iyCutMin = ifind( par%zMin, equ%y, equ%ny, 1)
          newPsi(:, 1 : iyCutMin - 1) = huge(newPsi)
       end if

       if ( par%zMax < equ%y(equ%ny) ) then
          iyCutMax = ifind( par%zMax, equ%y, equ%ny, 1)
          newPsi(:, iyCutMax + 1 : equ%ny) = huge(newPsi)
       end if

    end if

    if (par%equExtensionMode == EQU_EXTENSION_MODE_VESSEL) then
       ! smart equilibrium cutoff: cut off points outside of vessel
       call logmsg(LOGINFO, "carre_equilibrium: vessel/smart equilibrium extension enabled")

       ! FIXME: this does not work for limiter configurations,
       ! because the data for the "fake" x-point on the limiter structure
       ! as defined in selptx is incomplete

       call equilibrium_vessel_cutoff(equ, struct, newPsi)
    end if

    ! Extend equilibrium grid towards left, right, bottom, top
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

    ! Copy modified psi data back into into equ%psi
    equ%psi = huge(equ%psi)
    equ%psi( par%addLeft + 1 : par%addLeft + equ%nx, par%addBottom + 1 : par%addBottom + equ%ny ) &
         & = newPsi(1 : equ%nx, 1 : equ%ny)

    equ%nx = equ%nx + par%addLeft + par%addRight
    equ%ny = equ%ny + par%addTop + par%addBottom

    ! move present point indices
    equ%ii = equ%ii + par%addLeft
    equ%jj = equ%jj + par%addBottom
    equ%iptx = equ%iptx + par%addLeft
    equ%jptx = equ%jptx + par%addBottom

    call writeGridStateToSiloFile('carreEquCutoff0', equ, struct )

    ! Figure out distance factor for equilibrium extension
    ! We compute the distance between o- and x-point both in space and psi, and derive 
    ! the factor from that    
    ! TODO: limiter case has to be treated differently.
    psiPtO = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
         & equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
         & equ%xpto, equ%ypto  )
    
    psiPtX = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
         & equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
         & equ%ptx(1), equ%pty(1) )

    distFactor = ( (psiPtX - psiPtO) / dist(equ%xpto, equ%ypto, equ%ptx(1), equ%pty(1)) ) * par%equExpansionFactor

    ! Run extension algorithm
    
    if ( par%equDistanceFunction == EQU_DIST_FUN_FAST ) then
       write (*,*) "extend_equilibrium: fast"
       call compute_distance_fast(equ, distFactor)
    elseif ( par%equDistanceFunction == EQU_DIST_FUN_EXACT ) then
       write (*,*) "extend_equilibrium: exact"
       call compute_distance_exact(equ, distFactor)
    else
       stop "extend_equilibrium: unknown choice for equDistanceFun"
    endif

    ! Re-compute some data...
    ! Calculate the first partial derivatives in x and y and store
    ! them in arrays psidx and psidy
    call DERIVE(equ)
    !  interpolation coefficients for psi and its derivatives
    call inipsi(equ,nxmax,nymax)

    call writeGridStateToSiloFile('carreEquExtend0', equ, struct )

  end subroutine extend_equilibrium


  !> Compute uninitialized values of psi by computing the distance to the closest
  !> initialized value multiplied by some factor. This algorithm is very slow, but precise.
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
  !> initialized value multiplied by some factor. This algorithm is fast, but imprecise.
  !> It uses a distance transform with approximate distance constants.
  subroutine compute_distance_fast(equ, factor)
    type(CarreEquilibrium), intent(inout) :: equ
    double precision, intent(in) :: factor

    ! internal
    integer :: ix, iy, xfrom, xto, dx, yfrom, yto, dy, iPass, iNb

    integer, parameter :: N_PASS = 2, N_NB = 4    

    integer :: NB_DX(N_PASS, N_NB)
    integer :: NB_DY(N_PASS, N_NB)
    double precision :: NB_D(N_PASS, N_NB)
    double precision :: newpsi(1:equ%nx, 1:equ%ny)

    double precision :: D_DIAG, D_DX, D_DY

    D_DX = equ%x(2) - equ%x(1)
    D_DY = equ%y(2) - equ%y(1)
    D_DIAG = sqrt( D_DX**2 + D_DY**2 )

    ! Pass one: bottom-left -> top-right
    NB_DX(1, :) = (/  -1, -1,  0,  1 /)
    NB_DY(1, :) = (/   0, -1, -1, -1 /)
    NB_D(1, :) = (/ D_DX, D_DIAG, D_DY, D_DIAG /)

    ! Pass tow: top-right -> bottom-left
    NB_DX(2, :) = (/  1, -1,  0,  1 /)
    NB_DY(2, :) = (/  0, +1, +1, +1 /)
    NB_D(2, :) = (/ D_DX, D_DIAG, D_DY, D_DIAG /)

    newpsi = equ%psi(1:equ%nx, 1:equ%ny)

    do iPass = 1, N_PASS
       
       select case (iPass)
       case(1)
          ! bottom-left to top-right
          xfrom = 1
          xto = equ%nx
          dx = 1
          yfrom = 1
          yto = equ%ny
          dy = 1
       case(2)
          ! top-right-right to bottom-left
          xfrom = equ%nx 
          xto = 1
          dx = -1
          yfrom = equ%ny
          yto = 1
          dy = -1
       end select

       do iy = yfrom, yto, dy
          do ix = xfrom, xto, dx
             
             if (equ%psi(ix, iy) /= huge(equ%psi)) cycle

             do iNb = 1, N_NB
                if ( ( ix+NB_DX(iPass, iNb) < 1 ) .or. ( ix+NB_DX(iPass, iNb) > equ%nx ) ) cycle
                if ( ( iy+NB_DY(iPass, iNb) < 1 ) .or. ( iy+NB_DY(iPass, iNb) > equ%ny ) ) cycle

                if ( newpsi(ix + NB_DX(iPass, iNb), iy + NB_DY(iPass, iNb)) == huge(newpsi)) cycle

                if ( factor > 0 ) then
                   newpsi(ix, iy) = min( newpsi(ix, iy), &
                        & newpsi(ix + NB_DX(iPass, iNb), iy + NB_DY(iPass, iNb)) + NB_D(iPass, iNb) * factor )
                else
                   if (newpsi(ix, iy) == huge(newpsi)) newpsi(ix, iy) = -huge(newpsi)
                   newpsi(ix, iy) = max( newpsi(ix, iy), &
                        & newpsi(ix + NB_DX(iPass, iNb), iy + NB_DY(iPass, iNb)) + NB_D(iPass, iNb) * factor )
                end if
             end do

          end do
       end do

    end do

    equ%psi(1:equ%nx, 1:equ%ny) = newpsi

  end subroutine compute_distance_fast

  subroutine categorize_equilibrium_grid(equ, struct)
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreStructures), intent(in) :: struct
    
    ! internal
    ! How many points to grow the vessel region
    integer, parameter :: REGION_GROW_COUNT = 2
    integer :: iGrow

    ! We do inside/outside detection on the cartesian psi grid. 

    ! First compute intersection of all faces in the psi grid with the structures
    call compute_intersections()    
   
    ! Mark cells internal, starting from an internal cell. 
    ! Use the O-point for this.
    equ%pointFlag = GRID_UNDEFINED
    !call markInternalPoint( equ%iptx(1), equ%jptx(1) )
    call markInternalPoint( equ%iptx(equ%npx+1), equ%jptx(equ%npx+1) )

    ! Mark all other points as external
    where ( equ%pointFlag == GRID_UNDEFINED ) equ%pointFlag = GRID_EXTERNAL

  contains
    
    ! Compute intersections of psi grid faces with the structures.
    ! This is very similar to computeFaceStructureIntersections, but not worth to unify...
    subroutine compute_intersections()
      ! internal
      integer :: ir, iz, iFc
      double precision :: xx(2), yy(2)

      equ%faceISec = .false.

      do ir = 1, equ%nx
         do iz = 1, equ%ny
            do iFc = FACE_EQU_RADIAL, FACE_EQU_VERTICAL
               
               select case (iFc)
               case (FACE_EQU_RADIAL)
                  if (ir == equ%nx) cycle
                  xx(1) = equ%x(ir)
                  yy(1) = equ%y(iz)
                  xx(2) = equ%x(ir + 1)
                  yy(2) = equ%y(iz)
               case (FACE_EQU_VERTICAL)
                  if (iz == equ%ny) cycle
                  xx(1) = equ%x(ir)
                  yy(1) = equ%y(iz)
                  xx(2) = equ%x(ir)
                  yy(2) = equ%y(iz + 1)
               end select

               call intersect_all_structures( xx, yy, &
                    & struct, equ%faceISec(ir, iz, iFc) )

            end do
         end do
      end do

    end subroutine compute_intersections

    !> Mark a point internal, then go to all neighbour points 
    !> connect by non-intersected faces and mark them as internal
    recursive subroutine markInternalPoint( ir, iz )
      integer, intent(in) :: ir, iz

      ! internal
      integer :: dr, dz, iFc, ir2, iz2, irFc, izFc

      if (equ%pointFlag(ir, iz) /= GRID_UNDEFINED) return

      equ%pointFlag(ir, iz) = GRID_INTERNAL

      ! loop over all neighbour points
      do dr = -1, 1
         do dz = -1, 1
            ! ...but only the face neighbours
            if ( (abs(dr) + abs(dz)) /= 1) cycle

            ! Index of neighbour point
            ir2 = ir + dr
            iz2 = iz + dz

            ! Do not leave grid
            if ( (ir2 < 1) .or. (ir2 > equ%nx) ) cycle
            if ( (iz2 < 1) .or. (iz2 > equ%ny) ) cycle

            ! Figure out face alignment
            if (dr /= 0) iFc = FACE_EQU_RADIAL
            if (dz /= 0) iFc = FACE_EQU_VERTICAL

            ! Figure out face index
            irFc = ir + min(dr, 0)
            izFc = iz + min(dz, 0)

            ! If face not intersected, mark neighbour internal
            if ( .not. equ%faceISec(irFc, izFc, iFc) ) then
               call markInternalPoint(ir2, iz2)
            end if

         end do
      end do

    end subroutine markInternalPoint

  end subroutine categorize_equilibrium_grid


  !> Cut off psi data outside of plasma vessel. The idea is to keep the 
  !> original psi data in the domain where we want to compute the plasma solution,
  !> and replace it with fake data for the purpose of gridding outside of the vessel.
  !>
  !> To find the points inside the vessel we need some information from the geometry
  !> and topology analysis, which is why this routine must be called after this happened.
  subroutine equilibrium_vessel_cutoff(equ, struct, psi)
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreStructures), intent(in) :: struct
    double precision, intent(inout) :: psi(nxmax, nymax)
    
    ! internal
    ! How many points to grow the vessel region
    integer, parameter :: REGION_GROW_COUNT = 2
    integer :: pointFlag(1:nxmax, 1:nymax)
    integer :: iGrow

    call categorize_equilibrium_grid(equ, struct)

    pointFlag = equ%pointFlag

    ! Grow the region of internal points a bit to cover a bit more than the vessel
    do iGrow = 1, REGION_GROW_COUNT
       call growInternalRegion(pointFlag(1:equ%nx, 1:equ%ny))
    end do

    ! Set original equilibrium value on internal points
    where ( pointFlag /= GRID_INTERNAL ) psi = huge(psi)

  contains

    ! Grow region of internal points by one point in all directions
    subroutine growInternalRegion( flag )
      integer, intent(inout) :: flag(1:equ%nx, 1:equ%ny)
      
      ! internal
      integer :: ix, iy, ix2, iy2
      integer :: tmpFlag(1:equ%nx, 1:equ%ny)

      tmpFlag = flag

      do ix = 1, equ%nx
         do iy = 1, equ%ny
            
            if (flag(ix,iy) == GRID_INTERNAL) then
               do ix2 = max(1, ix-1), min(ix+1, equ%nx)
                  do iy2 = max(1, iy-1), min(iy+1, equ%ny)
                     tmpFlag(ix2, iy2) = GRID_INTERNAL
                  end do
               end do
            end if


         end do
      end do

      flag = tmpFlag

    end subroutine growInternalRegion

  end subroutine equilibrium_vessel_cutoff


  !> Check whether the given position (x,y) is
  !> inside the region covered by the equilibrium grid.
  logical function insideEquGrid(equ, x, y)
    type(CarreEquilibrium), intent(in) :: equ
    double precision, intent(in) :: x, y

    insideEquGrid = ( (x >= equ%x(1)) &
         & .and. (x <= equ%x(equ%nx)) &
         & .and. (y >= equ%y(1)) &
         & .and. (y <= equ%y(equ%ny)) )

  end function insideEquGrid


  !> Compute psi, psidx, psidy on carre grid
  subroutine compute_psi_on_grid( equ, grid )
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: ireg, j, i, ii, jj
    double precision :: xx, yy

    integer :: ifind
    external :: ifind

    do ireg=1, grid%nreg
        do j=1, grid%nr(ireg)
            do i= 1,grid%np1(ireg)
                xx = grid%xmail(i,j,ireg)
                yy = grid%ymail(i,j,ireg)
                ii = ifind(xx,equ%x,equ%nx,1)
                jj = ifind(yy,equ%y,equ%ny,1)
                grid%psim(i,j,ireg) = equ%a00(ii,jj,1)+equ%a10(ii,jj,1)*xx+equ%a01(ii,jj,1)*yy & 
                     & + equ%a11(ii,jj,1)*xx*yy
                grid%psidxm(i,j,ireg) = equ%a00(ii,jj,2)+equ%a10(ii,jj,2)*xx+equ%a01(ii,jj,2)*yy & 
                     & + equ%a11(ii,jj,2)*xx*yy
                grid%psidym(i,j,ireg) = equ%a00(ii,jj,3)+equ%a10(ii,jj,3)*xx+equ%a01(ii,jj,3)*yy & 
                     & + equ%a11(ii,jj,3)*xx*yy
            end do
        end do
    end do

  end subroutine compute_psi_on_grid

end module carre_equilibrium
