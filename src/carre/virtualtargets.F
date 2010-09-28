      subroutine virtualtargets(nx,ny,x,y,psi,npx,ptx,pty,
     .           fctpx,separx,separy,nptot,
     .           nstruc,npstru,xstruc,ystruc,indplq,nbdef,
     .           a00,a10,a01,a11)
c
c  version : 21.07.2000 13:38
c
c=======================================================================
c*** This routine creates virtual target plates for every divertor plate
c=======================================================================

      implicit none
cank-970707: dimensions from the file
*  dimensions
#include <CARREDIM.F>

*  arguments
      integer nx,ny,npx,
     .     nptot(4,npxmx),nstruc,npstru(strumx),
     .     indplq(4,npxmx),nbdef
      real*8 x(nxmax),y(nymax),psi(nxmax,nymax),ptx(npxmx),pty(npxmx),
     .     fctpx(npxmx),separx(npnimx,4,npxmx),separy(npnimx,4,npxmx),
     .     xstruc(npstmx,strumx),ystruc(npstmx,strumx),
     .     a00(nxmax,nymax,3),a10(nxmax,nymax,3),
     .     a01(nxmax,nymax,3),a11(nxmax,nymax,3)

! nx, ny : size of 2d eqilibrium arrays
! x(nxmax), y(nymax), psi(nxmax,nymax) : equilibrium arrays
! npx : no. of special (x,o) points
c.. npx   : number of the X-points
c.. ptx,pty: X-point co-ordinates
c.. iptx,jptx: x and y indices of the cells containing the X-points
c.. fctpx: the psi values at each X- or O-point
c.. separx,separy: coordinates of the points of the parametrised
c               separatrices (point index, branch index, X-point index)
c.. nptot : number of parametrisation points for each separatrix
c           (separatrix index, point index)
c.. nstruc: number of structures
c.. npstru: number of points per structure
c.. xstruc,ystruc: coordinates of the structure points
c                  (point index, structure index)
c.. indplq: table of the structure indices (0 means not a target)
c           (separatrix index, X-point index)
c.. inddef: table of indices of the divertor plates
c.. nbdef : number of the divertor plates


*  variables locales
      integer :: ipx, isep, istru
      integer :: ip, itarget, istep
      real*8 :: minpsi, maxpsi, ppsi, dir, tol
      real*8 :: tx, ty, tpsi, gx, gy, dx, dxmax, pi, alpha, p

      real*8 :: seppx(nstruc), seppy(nstruc)
      real*8 :: tmpx(npstmx,2), tmpy(npstmx,2)
      real*8 :: vtx(npstmx,nbdef), vty(npstmx,nbdef)
      real*8 :: vtmp1, vtmp2
      integer :: nvtarget, idir, vtargetipx(nbdef)
      integer :: nptmp(2), npvtarget(nbdef), npvtmp
      parameter(pi=3.141592654)
      

*  procedures
      real*8 feval2d, angle, dist
      external feval2d, angle, dist, rotate
c=======================================================================

* we create one virtual target for every separatrix segment intersecting
* a target

      ! min/maxpsi hold range of psi contours to be covered by the grid
      minpsi = huge(minpsi)
      maxpsi = -minpsi

      ! counter for number of targets to create
      nvtarget = 0

      ! loop over x-points
      do ipx = 1, npx

         ! loop over separatrix segments     
         do isep = 1, 4           

            istru = indplq(isep,ipx)
            ! if separatrix segment does not intersect target, skip
            if ( istru == 0 ) cycle

            write (0,*) 'x-point : ', ipx, ', separatrix segment: ',
     $           isep, ', structure no. ', istru

            ! loop over all points in the associated structure to find
            ! starting point for virtual target on separatrix
            nvtarget = nvtarget + 1
            ! vtargetipx stores x-point associated with this target
            vtargetipx(nvtarget) = ipx
            seppx(nvtarget) = ptx(ipx)
            seppy(nvtarget) = pty(ipx)
            dxmax = 0

            do ip = 1, npstru(istru)
               
               tx = xstruc( ip, istru )
               ty = ystruc( ip, istru )

               ! compute psi at point
               ppsi = feval2d( nx, ny, x, y,
     $              a00(:,:,1), a10(:,:,1), a01(:,:,1), a11(:,:,1),
     $              tx, ty )

               ! update min/max psi (covering all structures)
               minpsi = min( minpsi, ppsi )
               maxpsi = max( maxpsi, ppsi )

               ! find projection on separatrix for this point               
               
               ! stepping loop towards separatrix psi value
               tpsi = ppsi
               tol = 1e-2
               istep = 0
               do
                  ! check distance (in psi) to separatrix
                  write (0,*) 'rel. diff psi ',
     $                  abs( tpsi - fctpx(ipx) ) / abs( fctpx(ipx) ) 
                  if ( abs( tpsi - fctpx(ipx) )
     $                 / abs( fctpx(ipx) ) < tol ) then
                     ! close enough: stop

                     write (0,*) 'Separatrix projection for point ', ip,
     $                    ': ', tx, ty

                     ! Check whether the point is located on the separatrix
                     ! branch associated with the target
                     ! Quick fix: check whether it lies roughly in the 
                     ! same direction as the strike point. This should work ok.

                     ! TODO: To do this properly, one has to 
                     ! check whether the point is on the sep. branch.
                     ! However, the separatrix is at the moment
                     ! only stored up to the strike point, so it would have
                     ! to be retraced up to the equilibrium boundary

                     alpha = angle( ptx(ipx), pty(ipx),
     $                    separx( nptot( isep, ipx ), isep, ipx ), 
     $                    separy( nptot( isep, ipx ), isep, ipx ),
     $                    tx, ty )

                     write (0,*) 'X/Strike point angle is ', alpha

                     ! if angle to big, skip this point
                     if ( alpha > (pi / 4) ) then
                        write (0,*) 'X/Strike point angle too big, skip'
                        exit
                     endif

                     ! Point on correct sep. branch. Check distance to x-point
                     dx = dist( ptx(ipx), pty(ipx), tx, ty )
                     ! if further away than previous projections, keep
                     if (  dx > dxmax ) then
                        seppx(nvtarget) = tx
                        seppy(nvtarget) = ty
                        dxmax = dx
                     endif

                     exit
                  end if

                  ! not close enough: step along grad psi towards separatrix
                  ! compute grad psi at current position
                  gx = feval2d( nx, ny, x, y,
     $                 a00(:,:,2), a10(:,:,2), a01(:,:,2), a11(:,:,2),
     $                 tx, ty )
                  gy = feval2d( nx, ny, x, y,
     $                 a00(:,:,3), a10(:,:,3), a01(:,:,3), a11(:,:,3),
     $                 tx, ty )
                  ! take a step, including a security factor
                  dx = 0.02 * (tpsi - fctpx(ipx))
     $                 / sqrt( gx**2 + gy**2 );
                  tx = tx - dx * gx
                  ty = ty - dx * gy                  
                  ! compute psi at new position
                  tpsi = feval2d( nx, ny, x, y,
     $                 a00(:,:,1), a10(:,:,1), a01(:,:,1), a11(:,:,1),
     $                 tx, ty )
                  ! TODO: if psi not moving in the right direction, 
                  ! decrease step size and try again
                  istep = istep + 1
                  if ( istep > 1000 ) then
                     write(0,*) 'Did not find separatrix'
                     exit
                  endif
               enddo ! separatrix projection loop

            enddo ! structure point loop

         enddo ! separatrix segment loop
      enddo ! 


      write (0,*) 'Psi range for all targets:'
      write (0,*) 'min psi: ', minpsi, ', max psi: ', maxpsi            

      ! TODO: refactor building the virtual targets out into subroutine

      ! build the virtual targets
      do itarget = 1, nvtarget

         write (100,*) 'Start'
         
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
            tx = seppx(itarget)
            ty = seppy(itarget)

            do 
!              compute grad psi
               gx = feval2d( nx, ny, x, y,
     $              a00(:,:,2), a10(:,:,2), a01(:,:,2), a11(:,:,2),
     $              tx, ty )
               gy = feval2d( nx, ny, x, y,
     $              a00(:,:,3), a10(:,:,3), a01(:,:,3), a11(:,:,3),
     $              tx, ty )
               
               dx = 1e-4;
               tx = tx + dir * dx * gx
               ty = ty + dir * dx * gy                  
                              
               tpsi = feval2d( nx, ny, x, y,
     $              a00(:,:,1), a10(:,:,1), a01(:,:,1), a11(:,:,1),
     $              tx, ty )
               
               nptmp(idir) = istep
               tmpx(istep,idir) = tx
               tmpy(istep,idir) = ty

               if ( tpsi > maxpsi ) exit
               if ( tpsi < minpsi ) exit

               istep = istep + 1
               if ( istep > npstmx ) then
                  write(0,*) 'Unable to finish virtual target'
                  exit
               endif                  

            enddo ! target stepping loop
         enddo ! direction loop

         ! arrange points according to orientation
         tx = seppx(itarget)
         ty = seppy(itarget)

         ! compute grad psi at starting point (again)
         gx = feval2d( nx, ny, x, y,
     $        a00(:,:,2), a10(:,:,2), a01(:,:,2), a11(:,:,2),
     $        tx, ty )
         gy = feval2d( nx, ny, x, y,
     $        a00(:,:,3), a10(:,:,3), a01(:,:,3), a11(:,:,3),
     $        tx, ty )
            
         ! check in which direction the associated x-point is
         p = gx * ( pty( vtargetipx( itarget ) ) - ty )
     $        - gy * ( ptx( vtargetipx( itarget ) ) - tx )
         
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

         npvtmp = 0
         do istep = nptmp(idir), 1, -1
            npvtmp = npvtmp + 1
            vtx(npvtmp, itarget) = tmpx(istep,idir)
            vty(npvtmp, itarget) = tmpy(istep,idir)
         enddo

         ! insert starting point into middle
         npvtmp = npvtmp + 1
         vtx(npvtmp, itarget) = seppx(itarget)
         vty(npvtmp, itarget) = seppy(itarget)

         idir = idir + 1
         if ( idir > 2 ) idir = 1 
         do istep = 1, nptmp(idir)
            npvtmp = npvtmp + 1
            vtx(npvtmp, itarget) = tmpx(istep,idir)
            vty(npvtmp, itarget) = tmpy(istep,idir)
         enddo
         
         ! close target by wrapping it around
        
         vtmp1 = vtx(npvtmp, itarget) - vtx(npvtmp - 1, itarget)
         vtmp2 = vty(npvtmp, itarget) - vty(npvtmp - 1, itarget)
         call rotate( vtmp1, vtmp2, pi * 1.25 )

         npvtmp = npvtmp + 1
         vtx(npvtmp, itarget) = vtx(npvtmp - 1, itarget) + vtmp1
         vty(npvtmp, itarget) = vty(npvtmp - 1, itarget) + vtmp2

         vtmp1 = vtx(2, itarget) - vtx(1, itarget)
         vtmp2 = vty(2, itarget) - vty(1, itarget)
         call rotate( vtmp1, vtmp2, pi * 1.75 )

         npvtmp = npvtmp + 1
         vtx(npvtmp, itarget) = vtx(1, itarget) + vtmp1
         vty(npvtmp, itarget) = vty(1, itarget) + vtmp2

         ! close structure
         npvtmp = npvtmp + 1
         vtx(npvtmp, itarget) = vtx(1, itarget)
         vty(npvtmp, itarget) = vty(1, itarget)

         npvtarget( itarget ) = npvtmp

      enddo ! virtual target loop

      open(UNIT=100,FILE='virtualstructure.out',STATUS='unknown')
      do itarget = 1, nvtarget
         do istep = 1, npvtarget( itarget )
            write (100,*) vtx(istep,itarget)*1000,
     $           vty(istep,itarget)*1000
         enddo
         write (100,*) ''
      enddo
      close(UNIT=100)

      end
