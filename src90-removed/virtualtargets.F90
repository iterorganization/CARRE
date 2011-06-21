      subroutine virtualtargets(nx,ny,x,y,psi,npx,ptx,pty, & 
     &           fctpx,separx,separy,nptot, & 
     &           rnstruc,rnpstru,rxstruc,rystruc,indplq,inddef,nbdef, & 
     &           a00,a10,a01,a11,nstruc,npstru,xstruc,ystruc)

!=======================================================================
!*** This routine creates virtual target plates
!=======================================================================

      implicit none
!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  arguments
      integer nx,ny,npx, & 
     &     nptot(4,npxmx),rnstruc,rnpstru(strumx), & 
     &     indplq(4,npxmx),nbdef,nstruc,npstru(strumx), & 
     &     inddef(nbdmx)
      real*8 x(nxmax),y(nymax),psi(nxmax,nymax),ptx(npxmx),pty(npxmx), & 
     &     fctpx(npxmx),separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), & 
     &     rxstruc(npstmx,strumx),rystruc(npstmx,strumx), & 
     &     a00(nxmax,nymax,3),a10(nxmax,nymax,3), & 
     &     a01(nxmax,nymax,3),a11(nxmax,nymax,3), & 
     &     xstruc(npstmx,strumx),ystruc(npstmx,strumx)

! nx, ny : size of 2d eqilibrium arrays
! x(nxmax), y(nymax), psi(nxmax,nymax) : equilibrium arrays
! npx : no. of special (x,o) points
!.. npx   : number of the X-points
!.. ptx,pty: X-point co-ordinates
!.. iptx,jptx: x and y indices of the cells containing the X-points
!.. fctpx: the psi values at each X- or O-point
!.. separx,separy: coordinates of the points of the parametrised
!               separatrices (point index, branch index, X-point index)
!.. nptot : number of parametrisation points for each separatrix
!           (separatrix index, point index)
!.. nstruc: number of structures
!.. npstru: number of points per structure
!.. xstruc,ystruc: coordinates of the structure points
!                  (point index, structure index)
!.. indplq: table of the structure indices (0 means not a target)
!           (separatrix index, X-point index)
!.. inddef: table of indices of the divertor plates
!.. nbdef : number of the divertor plates


!  variables locales
      integer :: ipx, isep, istru, i
      integer :: ip, itarget, istep
      real*8 :: minpsitot, maxpsitot, & 
     &     minpsi(rnstruc), maxpsi(rnstruc), vtminpsi, vtmaxpsi, & 
     &     dir, tol
      real*8 :: tx, ty, tpsi, gx, gy, dx, dxmax, pi, alpha, p

      real*8 :: seppx(npstmx,rnstruc), seppy(npstmx,rnstruc)
      real*8 :: vtstartx(nbdef), vtstarty(nbdef)
      real*8 :: ppsi(npstmx,rnstruc)
      real*8 :: tmpx(npstmx,2), tmpy(npstmx,2)
      real*8 :: rx(2), ry(2)
      real*8 :: vtmp1, vtmp2
      integer :: nvtarget, idir, vtargetipx(nbdef), vtistruc(nbdef)
      integer :: nptmp(2), npvtarget(nbdef), npvtmp
      parameter(pi=3.141592654)
      logical :: istarget

      integer, parameter :: MODE_TARGET = 1
      integer, parameter :: MODE_VESSEL = 2
      integer :: mode = MODE_VESSEL

!  procedures
      real*8 feval2d, angle, dist, norm
      external feval2d, angle, dist, rotate, norm
!=======================================================================

! we create one virtual target for every separatrix segment intersecting
! a target

      ! First figure out psi range(s) to be covered by grid
      ! and project structure points to separatrix

      ! min/maxpsi: range of psi contours to be covered by the grid
      minpsitot = huge(minpsitot)
      maxpsitot = -minpsitot

      do istru = 1, rnstruc

         ! if in target mode, only consider points of target structures
         if ( mode == MODE_TARGET ) then
            istarget = .false.
            do i = 1, nbdef
               if ( inddef(i) == istru ) then
                  istarget = .true.
                  exit
               endif
            enddo
            if ( .not. istarget ) cycle
         endif

         minpsi(istru) = huge(minpsi)
         maxpsi(istru) = -huge(minpsi)

         write (0,*) 'Structure ', istru, & 
     &        ': ', rnpstru(istru), ' points'
         ! loop over all points in structure
         do ip = 1, abs(rnpstru(istru))

            tx = rxstruc( ip, istru )
            ty = rystruc( ip, istru )

            ! compute psi at point
            ppsi(ip,istru) = feval2d( nx, ny, x, y, & 
     &           a00(:,:,1), a10(:,:,1), a01(:,:,1), a11(:,:,1), & 
     &           tx, ty )
            write (0,*) 'Point', ip, ', psi:', ppsi(ip,istru)

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
               if ( abs( tpsi - fctpx(ipx) ) & 
     &              / abs( fctpx(ipx) ) < tol ) then
               ! close enough: stop and store
!$$$                  write (0,*) 'Separatrix projection for point ', ip,
!$$$     $                 ': ', tx, ty
                  seppx(ip,istru) = tx
                  seppy(ip,istru) = ty
                  exit
               end if

               ! not close enough: step along grad psi towards separatrix
               ! compute grad psi at current position
               gx = feval2d( nx, ny, x, y, & 
     &              a00(:,:,2), a10(:,:,2), a01(:,:,2), a11(:,:,2), & 
     &              tx, ty )
               gy = feval2d( nx, ny, x, y, & 
     &              a00(:,:,3), a10(:,:,3), a01(:,:,3), a11(:,:,3), & 
     &              tx, ty )
               ! take a step, including a security factor
               dx = 0.02 * (tpsi - fctpx(ipx)) & 
     &              / sqrt( gx**2 + gy**2 );
               tx = tx - dx * gx
               ty = ty - dx * gy
               ! compute psi at new position
               tpsi = feval2d( nx, ny, x, y, & 
     &              a00(:,:,1), a10(:,:,1), a01(:,:,1), a11(:,:,1), & 
     &              tx, ty )
               ! TODO: if psi not moving in the right direction,
               ! decrease step size and try again
               istep = istep + 1
               if ( istep > 1000 ) then
                  write(0,*) 'Did not find separatrix'
                  exit
               endif
            enddo ! separatrix projection loop
         enddo ! structure point loop

         write(0,*) 'Structure:', istru, ', psi range:', & 
     &        minpsi(istru),  maxpsi(istru)

      enddo ! structure loop

      ! Find starting points for the virtual targets on the separatrix

      ! counter for number of targets to create
      nvtarget = 0

      ! loop over x-points
      do ipx = 1, npx

         ! loop over separatrix segments
         do isep = 1, 4

            itarget = indplq(isep,ipx)

            ! if separatrix segment does not intersect target, skip
            if ( itarget == 0 ) cycle

            write (0,*) 'x-point : ', ipx, ', separatrix segment: ', & 
     &           isep, ', structure no. ', itarget

            nvtarget = nvtarget + 1
            ! vtargetipx stores index of x-point associated with this target
            vtargetipx(nvtarget) = ipx
            ! vtistruc stores index of structure associated w.t. target
            vtistruc(nvtarget) = itarget
            vtstartx(nvtarget) = ptx(ipx)
            vtstarty(nvtarget) = pty(ipx)

            dxmax = 0

            ! find the starting point for the target

            do istru = 1, rnstruc

               ! In target mode, loop over all points in only the associated
               ! structure to find starting point for virtual target
               ! on the separatrix.
               ! In vessel mode, loop over all points of all structures.

               if ( ( mode == MODE_TARGET ) & 
     &              .and. .not. ( istru == itarget ) ) cycle

               ! loop over points in structure
               do ip = 1, abs(rnpstru(istru))

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

                  alpha = angle( ptx(ipx), pty(ipx), & 
     &                 separx( nptot( isep, ipx ), isep, ipx ), & 
     &                 separy( nptot( isep, ipx ), isep, ipx ), & 
     &                 tx, ty )

                  !write (0,*) 'X/Strike point angle is ', alpha
                  ! if angle to big, skip this point
                  if ( alpha > (pi / 4) ) then
                     !write (0,*) 'X/Strike point angle too big, skip'
                     cycle
                  endif

                  ! Point on correct sep. branch. Check distance to x-point
                  dx = dist( ptx(ipx), pty(ipx), tx, ty )
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

      write (0,*) 'Global psi range'
      write (0,*) 'min psi: ', minpsitot, ', max psi: ', maxpsitot

      ! TODO: refactor building the virtual targets out into subroutine

      ! Figure out the psi range for this target.
      ! In target mode, a contour line connecting two targets
      ! has to end on both targets. The easiest is to just select the
      ! biggest range over all targets
      ! TODO: this can be improved
      select case ( mode )
      case ( MODE_TARGET )
         vtminpsi = huge(vtminpsi)
         vtmaxpsi = -huge(vtminpsi)
         do itarget = 1, nvtarget
            vtminpsi = min( vtminpsi, minpsi( vtistruc(itarget) ) )
            vtmaxpsi = max( vtmaxpsi, maxpsi( vtistruc(itarget) ) )
         enddo
      case ( MODE_VESSEL )
         vtminpsi = minpsitot
         vtmaxpsi = maxpsitot
      end select

      ! build the virtual targets


      ! build individual targets
      do itarget = 1, nvtarget

         ! track the target extent to get a reference length
         rx(1) = huge(rx)
         ry(1) = huge(rx)
         rx(2) = -huge(rx)
         ry(2) = -huge(rx)

         ! compute grad psi at starting point (again)
         tx = vtstartx(itarget)
         ty = vtstarty(itarget)
         gx = feval2d( nx, ny, x, y, & 
     &        a00(:,:,2), a10(:,:,2), a01(:,:,2), a11(:,:,2), & 
     &        tx, ty )
         gy = feval2d( nx, ny, x, y, & 
     &        a00(:,:,3), a10(:,:,3), a01(:,:,3), a11(:,:,3), & 
     &        tx, ty )
         ! check in which direction the associated x-point is
         p = gx * ( pty( vtargetipx( itarget ) ) - ty ) & 
     &        - gy * ( ptx( vtargetipx( itarget ) ) - tx )

         ! take a step along the separatrix away from the x-point

         ! choose a stepsize
         dx = 1 / norm( gx, gy ) * & 
     &        norm( ptx( vtargetipx( itarget ) ) - tx, & 
     &              pty( vtargetipx( itarget ) ) - ty ) * 0.1

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
         do idir = 1, 2
            istep = 1
            if ( idir == 1 ) then
               dir = 1
            else
               dir = -1
            endif

!           build virtual structure starting from the given points on the sep.
            tx = vtstartx(itarget)
            ty = vtstarty(itarget)

            do
!              compute grad psi
               gx = feval2d( nx, ny, x, y, & 
     &              a00(:,:,2), a10(:,:,2), a01(:,:,2), a11(:,:,2), & 
     &              tx, ty )
               gy = feval2d( nx, ny, x, y, & 
     &              a00(:,:,3), a10(:,:,3), a01(:,:,3), a11(:,:,3), & 
     &              tx, ty )

               dx = 1e-3;
               tx = tx + dir * dx * gx
               ty = ty + dir * dx * gy

               tpsi = feval2d( nx, ny, x, y, & 
     &              a00(:,:,1), a10(:,:,1), a01(:,:,1), a11(:,:,1), & 
     &              tx, ty )

               nptmp(idir) = istep
               tmpx(istep,idir) = tx
               tmpy(istep,idir) = ty

               rx(1) = min( rx(1), tx )
               ry(1) = min( ry(1), ty )
               rx(2) = max( rx(2), tx )
               ry(2) = max( ry(2), ty )

               if ( tpsi > vtmaxpsi ) exit
               if ( tpsi < vtminpsi ) exit

               istep = istep + 1
               if ( istep > npstmx ) then
                  write(0,*) 'Unable to finish virtual target'
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

         nstruc = nstruc + 1

         npvtmp = 0
         do istep = nptmp(idir), 1, -1
            npvtmp = npvtmp + 1
            xstruc(npvtmp, nstruc) = tmpx(istep,idir)
            ystruc(npvtmp, nstruc) = tmpy(istep,idir)
         enddo

         ! insert starting point into middle
         npvtmp = npvtmp + 1
         xstruc(npvtmp, nstruc) = vtstartx(itarget)
         ystruc(npvtmp, nstruc) = vtstarty(itarget)

         idir = idir + 1
         if ( idir > 2 ) idir = 1
         do istep = 1, nptmp(idir)
            npvtmp = npvtmp + 1
            xstruc(npvtmp, nstruc) = tmpx(istep,idir)
            ystruc(npvtmp, nstruc) = tmpy(istep,idir)
         enddo

         ! close target by wrapping it around

         vtmp1 = xstruc(npvtmp, itarget) - xstruc(npvtmp - 1, itarget)
         vtmp2 = ystruc(npvtmp, itarget) - ystruc(npvtmp - 1, itarget)
         call rotate( vtmp1, vtmp2, pi * 1.25 )
         dx = 1 / norm( vtmp1, vtmp2 ) * ( ( rx(2) - rx(1) ) * 0.3 )
         vtmp1 = vtmp1 * dx
         vtmp2 = vtmp2 * dx

         npvtmp = npvtmp + 1
         xstruc(npvtmp, itarget) = xstruc(npvtmp - 1, itarget) + vtmp1
         ystruc(npvtmp, itarget) = ystruc(npvtmp - 1, itarget) + vtmp2

         vtmp1 = xstruc(2, itarget) - xstruc(1, itarget)
         vtmp2 = ystruc(2, itarget) - ystruc(1, itarget)
         call rotate( vtmp1, vtmp2, pi * 1.75 )
         dx = 1 / norm( vtmp1, vtmp2 ) * ( ( rx(2) - rx(1) ) * 0.3 )
         vtmp1 = vtmp1 * dx
         vtmp2 = vtmp2 * dx

         npvtmp = npvtmp + 1
         xstruc(npvtmp, itarget) = xstruc(1, itarget) + vtmp1
         ystruc(npvtmp, itarget) = ystruc(1, itarget) + vtmp2

         ! close structure
         npvtmp = npvtmp + 1
         xstruc(npvtmp, itarget) = xstruc(1, itarget)
         ystruc(npvtmp, itarget) = ystruc(1, itarget)

         npstru( nstruc ) = npvtmp

      enddo ! virtual target loop

      end
