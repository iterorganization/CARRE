module carre_virtualstructures

  use carre_types
  use Logging
#ifdef USE_SILO
  use SiloIO
#endif
  use CarreSiloIO
  use carre_equilibrium

  implicit none

#include <CARREDIM.F>

  private
  public :: setupVirtualStructures

contains

  subroutine setupVirtualStructures(par, equ, struct)
    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(inout) :: struct

    if (par%gridExtensionMode == GRID_EXTENSION_OFF) return

    select case(par%gridExtensionMode)
    case(GRID_EXTENSION_MODE_TARGET)
       call logmsg( LOGINFO, "setupVirtualStructures: creating virtual structures (target mode)" )
    case(GRID_EXTENSION_MODE_VESSEL)
       call logmsg( LOGINFO, "setupVirtualStructures: creating virtual structures (vessel mode)" )
    end select

    struct%nstruc = 0

    CALL virtualTargets(par, equ, struct)

    !..   10.2  Set up virtual limiters if in target mode
    if (par%gridExtensionMode == GRID_EXTENSION_MODE_TARGET) then
       call virtualLimiters_targetMode(equ, struct)
    else if (par%gridExtensionMode == GRID_EXTENSION_MODE_VESSEL) then
       call virtualLimiters_vesselMode(equ, struct)
    end if

    !..   10.2.1 Diagnostics: Write out resulting structures
#ifdef USE_SILO
    call csioGetStructureSegments( struct%nstruc, struct%npstru, &
         & struct%xstruc, struct%ystruc, csioVirtualStrucNSeg, csioVirtualStrucSegments )

    call csioOpenFile('carreVirtualStr')
    call csioOpenFile()
    call csioCloseFile()                         
#endif

!!$          open(UNIT=100,FILE='virtualstructure.out',STATUS='unknown')
!!$          do is = 1, struct%nstruc
!!$             do ip = 1, abs(struct%npstru( is ))
!!$                write (100,*) struct%xstruc(ip,is)*1000, & 
!!$                     &             struct%ystruc(ip,is)*1000
!!$             enddo
!!$             write (100,*) ''
!!$          enddo
!!$          close(UNIT=100)

  end subroutine setupVirtualStructures


  !> This routine creates virtual target plates
  subroutine virtualTargets(par, equ, struct)

    !  arguments
    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(inout) :: struct

    !  variables locales
    integer :: ipx, isep, istru, i
    integer :: ip, itarget, istep, istepTot
    real*8 :: minpsitot, maxpsitot, & 
         &     minpsi(struct%rnstruc), maxpsi(struct%rnstruc), vtminpsi, vtmaxpsi, & 
         &     dir, tol
    real*8 :: tx, ty, tpsi, gx, gy, dx, dxmax, pi, alpha, p

    real*8 :: seppx(npstmx,struct%rnstruc), seppy(npstmx,struct%rnstruc)
    real*8 :: vtstartx(struct%nbdef), vtstarty(struct%nbdef)
    real*8 :: ppsi(npstmx,struct%rnstruc)
    real*8 :: tmpx(npstmx,2), tmpy(npstmx,2)
    real*8 :: vtmp1, vtmp2
    double precision :: limPsiMax, limPsiMin, limPsi, pointPsi
    double precision :: gnorm
    integer :: nvtarget, idir, vtargetipx(struct%nbdef), vtistruc(struct%nbdef)
    integer :: nptmp(2), npvtmp
    parameter(pi=3.141592654)
    logical :: istarget

    !  procedures
    real*8 feval2d, angle, dist, norm
    external feval2d, angle, dist, rotate, norm


    ! Figure out psi values for limiting curves 
    limPsiMin = huge(limPsiMin)
    limPsiMax = -huge(limPsiMax)

    do i = 1, struct%nbniv       
       ! compute psi value for first point on the curve
       limPsi = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
            & equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
            & struct%nivx(1,i), struct%nivy(1,i) )

       limPsiMin = min(limPsiMin, limPsi)
       limPsiMax = max(limPsiMax, limPsi)
    end do

    ! we create one virtual target for every separatrix segment intersecting
    ! a target

    ! First figure out psi range(s) to be covered by grid
    ! and project structure points to separatrix

    ! min/maxpsi: range of psi contours to be covered by the grid
    minpsitot = huge(minpsitot)
    maxpsitot = -minpsitot

    seppx = huge(seppx)
    seppy = huge(seppy)

    do istru = 1, struct%rnstruc

       ! if in target mode, only consider points of target structures
       if ( par%gridExtensionMode == GRID_EXTENSION_MODE_TARGET ) then

          istarget = .false.
          do i = 1, struct%nbdef
             if ( struct%inddef(i) == istru ) then
                istarget = .true.
                exit
             endif
          enddo
          if ( .not. istarget ) cycle
       endif

       minpsi(istru) = huge(minpsi)
       maxpsi(istru) = -huge(minpsi)

       write (0,*) 'virtualtargets: Structure ', istru, & 
            &        ': ', struct%rnpstru(istru), ' points'
       ! loop over all points in structure
       do ip = 1, abs(struct%rnpstru(istru))

          tx = struct%rxstruc( ip, istru )
          ty = struct%rystruc( ip, istru )

          ! compute psi at point
          ppsi(ip,istru) = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
               &           equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
               &           tx, ty )
          !write (0,*) 'Point', ip, ', psi:', ppsi(ip,istru)

          ! update min/max psi values
          minpsi(istru) = min( minpsi(istru), ppsi(ip,istru) )
          maxpsi(istru) = max( maxpsi(istru), ppsi(ip,istru) )
          minpsitot = min( minpsitot, ppsi(ip,istru) )
          maxpsitot = max( maxpsitot, ppsi(ip,istru) )

          ! find projection on separatrix for this point

          ! stepping loop towards separatrix psi value
          tpsi = ppsi(ip,istru)
          tol = 1e-2
          istep = 0
          do
             ! TODO: step towards psi value of closest separatrix,
             ! i.e. the x-point psi value closest to the current psi value
             ipx = 1
             ! check distance (in psi) to separatrix
             !$$$               write (0,*) 'rel. diff psi ',
             !$$$     $              abs( tpsi - fctpx(ipx) ) / abs( fctpx(ipx) )
             if ( abs( tpsi - equ%fctpx(ipx) ) & 
                  &              / abs( equ%fctpx(ipx) ) < tol ) then
                ! close enough: stop and store
                !$$$                  write (0,*) 'Separatrix projection for point ', ip,
                !$$$     $                 ': ', tx, ty
                seppx(ip,istru) = tx
                seppy(ip,istru) = ty
                exit
             end if

             ! not close enough: step along grad psi towards separatrix
             ! compute grad psi at current position
             gx = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                  &              equ%a00(:,:,2), equ%a10(:,:,2), equ%a01(:,:,2), equ%a11(:,:,2), & 
                  &              tx, ty )
             gy = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                  &              equ%a00(:,:,3), equ%a10(:,:,3), equ%a01(:,:,3), equ%a11(:,:,3), & 
                  &              tx, ty )
             ! take a step, including a security factor
             dx = 0.02 * (tpsi - equ%fctpx(ipx)) & 
                  &              / sqrt( gx**2 + gy**2 );
             tx = tx - dx * gx
             ty = ty - dx * gy
             ! compute psi at new position
             tpsi = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                  &              equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
                  &              tx, ty )
             ! TODO: if psi not moving in the right direction,
             ! decrease step size and try again
             istep = istep + 1
             if ( istep > 1000 ) then
                write(0,*) 'virtualtargets: did not find separatrix'
                exit
             endif
          enddo ! separatrix projection loop
       enddo ! structure point loop

       write(0,*) 'virtualtargets Structure:', istru, ', psi range:', & 
            &        minpsi(istru),  maxpsi(istru)

    enddo ! structure loop


    ! Find starting points for the virtual targets on the separatrix

    ! counter for number of targets to create
    nvtarget = 0

    ! loop over x-points
    do ipx = 1, equ%npx

       ! loop over separatrix segments
       do isep = 1, 4

          itarget = struct%indplq(isep,ipx)

          ! if separatrix segment does not intersect target, skip
          if ( itarget == 0 ) cycle

          write (0,*) 'x-point : ', ipx, ', separatrix segment: ', & 
               &           isep, ', structure no. ', itarget

          nvtarget = nvtarget + 1
          ! vtargetipx stores index of x-point associated with this target
          vtargetipx(nvtarget) = ipx
          ! vtistruc stores index of structure associated w.t. target
          vtistruc(nvtarget) = itarget
          vtstartx(nvtarget) = equ%ptx(ipx)
          vtstarty(nvtarget) = equ%pty(ipx)

          dxmax = 0

          ! find the starting point for the target
          do istru = 1, struct%rnstruc

             ! only look at targets
             if ( ( par%gridExtensionMode == GRID_EXTENSION_MODE_TARGET ) & 
                  &              .and. .not. ( istru == itarget ) ) cycle

             ! Figure out the psi range for this target.
             ! In target mode, a contour line connecting two targets
             ! has to end on both targets. The easiest is to just select the
             ! biggest range over all targets
             ! TODO: this can be improved
             select case ( par%gridExtensionMode )
             case ( GRID_EXTENSION_MODE_TARGET )
                vtminpsi = huge(vtminpsi)
                vtmaxpsi = -huge(vtminpsi)
                do itarget = 1, nvtarget
                   vtminpsi = min( vtminpsi, minpsi( vtistruc(itarget) ) )
                   vtmaxpsi = max( vtmaxpsi, maxpsi( vtistruc(itarget) ) )
                enddo

                ! TODO: fix this
!!$                ! Finally constrain the psi limits to match the 
!!$                ! limiting curves 
!!$                vtminpsi = max( vtminpsi, limPsiMin )
!!$                vtmaxpsi = min( vtmaxpsi, limPsiMax )

             case ( GRID_EXTENSION_MODE_VESSEL )
                vtminpsi = minpsitot
                vtmaxpsi = maxpsitot
             end select

             !write (0,*) 'Global psi range'
             !write (0,*) 'min psi: ', minpsitot, ', max psi: ', maxpsitot

             ! In target mode, loop over all points in only the associated
             ! structure to find starting point for virtual target
             ! on the separatrix.
             ! In vessel mode, loop over all points of all structures.

             ! loop over points in structure
             do ip = 1, abs(struct%rnpstru(istru))

                ! check whether the point is in the psi range we actually want to have
                pointPsi = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                     & equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
                     & struct%rxstruc( ip, istru ), struct%rystruc( ip, istru ) )

                ! if not, skip this point
                if ( ( pointPsi < vtminpsi ) .or. ( pointPsi > vtmaxpsi ) ) cycle

                ! Check whether the point is located on the separatrix
                ! branch associated with the target
                ! Quick fix: check whether it lies roughly in the
                ! same direction as the strike point. This should work ok.

                ! TODO: To do this properly, one has to
                ! check whether the point is on the sep. branch.
                ! However, the separatrix is at the moment
                ! only stored up to the strike point, so it would have
                ! to be retraced up to the equilibrium boundary

                tx = seppx(ip,istru)
                ty = seppy(ip,istru)

                if (tx == huge(seppx)) cycle

                alpha = angle( equ%ptx(ipx), equ%pty(ipx), & 
                     &                 equ%separx( equ%nptot( isep, ipx ), isep, ipx ), & 
                     &                 equ%separy( equ%nptot( isep, ipx ), isep, ipx ), & 
                     &                 tx, ty )

                !write (0,*) 'X/Strike point angle is ', alpha
                ! if angle to big, skip this point
                if ( alpha > (pi / 4) ) then
                   !write (0,*) 'X/Strike point angle too big, skip'
                   cycle
                endif

                ! Point on correct sep. branch. Check distance to x-point
                dx = dist( equ%ptx(ipx), equ%pty(ipx), tx, ty )
                ! if further away than previous projections, keep
                if (  dx > dxmax ) then
                   vtstartx(nvtarget) = tx
                   vtstarty(nvtarget) = ty
                   dxmax = dx
                endif
             enddo

          enddo ! structure loop
       enddo ! separatrix segment loop
    enddo ! x-point loop

    ! Diagnostic output: psi range of virtual targets vtminpsi
    call logmsg(LOGDEBUG, "virtualTargets: psi range "//real2str(vtminpsi)&
         & // " to " // real2str(vtmaxpsi))

    ! build the virtual targets
    ! TODO: refactor building the virtual targets out into subroutine
    ! build individual targets
    do itarget = 1, nvtarget

       ! compute grad psi at starting point (again)
       tx = vtstartx(itarget)
       ty = vtstarty(itarget)
       gx = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
            &        equ%a00(:,:,2), equ%a10(:,:,2), equ%a01(:,:,2), equ%a11(:,:,2), & 
            &        tx, ty )
       gy = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
            &        equ%a00(:,:,3), equ%a10(:,:,3), equ%a01(:,:,3), equ%a11(:,:,3), & 
            &        tx, ty )
       ! check in which direction the associated x-point is
       p = gx * ( equ%pty( vtargetipx( itarget ) ) - ty ) & 
            &        - gy * ( equ%ptx( vtargetipx( itarget ) ) - tx )

       ! take a step along the separatrix away from the x-point
       ! to gain some distance between virtual and real structures

       ! choose a stepsize
       dx = 1 / norm( gx, gy ) * & 
            &        norm( equ%ptx( vtargetipx( itarget ) ) - tx, & 
            &              equ%pty( vtargetipx( itarget ) ) - ty ) * 0.05

       if ( p == 0 ) stop 'Grad psi pointing towards x-point'
       if ( p > 0 ) then
          ! x-point to the left of grad psi
          ! step in clockwise right angle to grad psi
          vtstartx(itarget) = vtstartx(itarget) + dx * gy
          vtstarty(itarget) = vtstarty(itarget) - dx * gx
       else
          ! x-point to the right of grad psi
          ! step in anti-clockwise right angle to grad psi
          vtstartx(itarget) = vtstartx(itarget) - dx * gy
          vtstarty(itarget) = vtstarty(itarget) + dx * gx
       endif


       ! build virtual structure starting from the given points on the sep.
       ! step in both directions away from the separatrix
       istepTot = 0
       do idir = 1, 2 ! idir=1: along grad psi, idir=2: along minus grad psi
          istep = 1
          if ( idir == 1 ) then
             dir = 1
          else
             dir = -1
          endif

          ! build virtual structure starting from the given points on the sep.
          tx = vtstartx(itarget)
          ty = vtstarty(itarget)

          do
             ! compute grad psi
             gx = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                  & equ%a00(:,:,2), equ%a10(:,:,2), equ%a01(:,:,2), equ%a11(:,:,2), & 
                  & tx, ty )
             gy = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                  & equ%a00(:,:,3), equ%a10(:,:,3), equ%a01(:,:,3), equ%a11(:,:,3), & 
                  & tx, ty )
             gnorm = norm(gx, gy)             

             dx = 1/gnorm * (equ%x(2) - equ%x(1)) * 0.5 ;
             tx = tx + dir * dx * gx
             ty = ty + dir * dx * gy

             ! did we step outside the equilibrium grid?
             if (.not. insideEquGrid(equ, tx, ty)) exit

             tpsi = feval2d( equ%nx, equ%ny, equ%x, equ%y, & 
                  & equ%a00(:,:,1), equ%a10(:,:,1), equ%a01(:,:,1), equ%a11(:,:,1), & 
                  & tx, ty )

             nptmp(idir) = istep
             tmpx(istep,idir) = tx
             tmpy(istep,idir) = ty

             if ( tpsi > vtmaxpsi ) exit
             if ( tpsi < vtminpsi ) exit

             istep = istep + 1
             istepTot = istepTot + 1
             if ( istepTot > npstmx - 10 ) then
                write(0,*) 'virtualtargets: Unable to finish virtual target'
                exit
             endif

          enddo ! target stepping loop
       enddo ! direction loop

       ! arrange points according to orientation
       tx = vtstartx(itarget)
       ty = vtstarty(itarget)

       if ( p == 0 ) stop 'Grad psi pointing towards x-point'
       if ( p > 0 ) then
          ! x-point to the left of grad psi
          ! do the points in the direction of - grad psi first
          idir = 2
       else
          ! x-point to the right of grad psi
          ! do the points in the direction of + grad psi first
          idir = 1
       endif

       struct%nstruc = struct%nstruc + 1

       npvtmp = 0
       do istep = nptmp(idir), 1, -1
          npvtmp = npvtmp + 1
          struct%xstruc(npvtmp, struct%nstruc) = tmpx(istep,idir)
          struct%ystruc(npvtmp, struct%nstruc) = tmpy(istep,idir)
       enddo

       idir = idir + 1
       if ( idir > 2 ) idir = 1
       do istep = 1, nptmp(idir)
          npvtmp = npvtmp + 1
          struct%xstruc(npvtmp, struct%nstruc) = tmpx(istep,idir)
          struct%ystruc(npvtmp, struct%nstruc) = tmpy(istep,idir)
       enddo

       ! close target by wrapping it around
       
       ! get current extent of new structure
       dx =  maxval(struct%xstruc(1:npvtmp, struct%nstruc)) &
            &   - minval(struct%xstruc(1:npvtmp, struct%nstruc))

       vtmp1 = struct%xstruc(npvtmp, itarget) - struct%xstruc(npvtmp - 1, itarget)
       vtmp2 = struct%ystruc(npvtmp, itarget) - struct%ystruc(npvtmp - 1, itarget)
       call rotate( vtmp1, vtmp2, pi * 1.25 )
       vtmp1 = vtmp1 * 1 / norm( vtmp1, vtmp2 ) *  dx * 0.3
       vtmp2 = vtmp2 * 1 / norm( vtmp1, vtmp2 ) *  dx * 0.3

       npvtmp = npvtmp + 1
       struct%xstruc(npvtmp, itarget) = struct%xstruc(npvtmp - 1, itarget) + vtmp1
       struct%ystruc(npvtmp, itarget) = struct%ystruc(npvtmp - 1, itarget) + vtmp2

       vtmp1 = struct%xstruc(2, itarget) - struct%xstruc(1, itarget)
       vtmp2 = struct%ystruc(2, itarget) - struct%ystruc(1, itarget)
       call rotate( vtmp1, vtmp2, pi * 1.75 )
       vtmp1 = vtmp1 * 1 / norm( vtmp1, vtmp2 ) *  dx * 0.3
       vtmp2 = vtmp2 * 1 / norm( vtmp1, vtmp2 ) *  dx * 0.3

       npvtmp = npvtmp + 1
       struct%xstruc(npvtmp, itarget) = struct%xstruc(1, itarget) + vtmp1
       struct%ystruc(npvtmp, itarget) = struct%ystruc(1, itarget) + vtmp2

       ! close structure
       npvtmp = npvtmp + 1
       struct%xstruc(npvtmp, itarget) = struct%xstruc(1, itarget)
       struct%ystruc(npvtmp, itarget) = struct%ystruc(1, itarget)

       struct%npstru( struct%nstruc ) = npvtmp

    enddo ! virtual target loop

  end subroutine virtualtargets


  !> Add one virtual limiter for every limiting curve.
  !>
  !> The virtual limiters are triangles, the tip of which is placed
  !> in the middle of the limiting curve (measured
  !> in physical length along the curve) and the triangle is oriented
  !> away from the O-point
  subroutine virtualLimiters_targetMode(equ, struct)

    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(inout) :: struct

    ! internal
    real*8 :: l, x, y
    integer :: iniv

    !  procedures
    real*8 :: long
    external long

    do iniv = 1, struct%nbniv

       l = long( struct%nivx(:,iniv), struct%nivy(:,iniv), struct%nivtot(iniv) )
       call coord( struct%nivx(:,iniv), struct%nivy(:,iniv), struct%nivtot(iniv), &
            & l/2.0, x, y )

       call add_virtual_limiter(equ, struct, x, y)       
    enddo

  end subroutine virtualLimiters_targetMode


  !> Add a virtual limiter structure at point x, y
  !>
  !> The virtual limiters are triangles, the tip of which is placed
  !> at the given point  and the triangle is oriented away from the O-point
  subroutine add_virtual_limiter(equ, struct, x, y)

    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(inout) :: struct
    double precision, intent(in) :: x, y

    ! internal
    real*8 :: ox, oy, length
    double precision, parameter :: pi=3.141592654

    double precision, parameter :: TRIANGLE_SIZE = 0.1
    double precision, parameter :: TRIANGLE_ANGLE = pi / 2.5

    external :: rotate

    ox = ( x - equ%xpto ) 
    oy = ( y - equ%ypto ) 
    length = sqrt( ox ** 2 + oy ** 2 )
    ox = ox / length
    oy = oy / length
    
    struct%nstruc = struct%nstruc + 1
    struct%npstru(struct%nstruc) = 4
    struct%xstruc(1,struct%nstruc) = x
    struct%ystruc(1,struct%nstruc) = y
    
    call rotate( ox, oy, TRIANGLE_ANGLE / 2.0 )
    struct%xstruc(2,struct%nstruc) = x + ox * TRIANGLE_SIZE
    struct%ystruc(2,struct%nstruc) = y + oy * TRIANGLE_SIZE
    
    call rotate( ox, oy, - TRIANGLE_ANGLE )
    struct%xstruc(3,struct%nstruc) = x + ox * TRIANGLE_SIZE
    struct%ystruc(3,struct%nstruc) = y + oy * TRIANGLE_SIZE
    
    struct%xstruc(4,struct%nstruc) = x
    struct%ystruc(4,struct%nstruc) = y
    
  end subroutine add_virtual_limiter


end module carre_virtualStructures
