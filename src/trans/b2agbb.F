      subroutine b2agbb (nx,ny,fpsi,ffbz,bb,
     .  crx,cry,psidx,psidy,nxmax,nymax)
c
c  version : 24.06.98 14:25
c
c======================================================================
      implicit none
#include <CARRETYPES.F>

c======================================================================
*   ..input arguments (unchanged on exit)
      integer nx, ny,nxmax,nymax
      real(kind=R8)
     .  fpsi(-1:nxmax,-1:nymax,0:3), ffbz(-1:nxmax,-1:nymax,0:3)
      real(kind=R8)
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  psidx(-1:nxmax,-1:nymax,0:3),psidy(-1:nxmax,-1:nymax,0:3)
*   ..output arguments (unspecified on entry)
      real(kind=R8)
     .  bb(-1:nxmax,-1:nymax,0:3)

*-----------------------------------------------------------------------
*.documentation
*
*  1. purpose
*
*     B2AGBB computes the magnetic field from the flux functions.
*
*
*  3. description (see also routine b2cdca)
*
*     This routine computes the three components of the magnetic field
*     and the total magnetic field strength, all at cell centers. The
*     components (bx,by) are obtained by differencing the potential fpsi
*     from its values at cell corners. The component bz is obtained
*     directly from the field function ffbz.
*
*
*  5. parameters (see also routine b2cdcv)
*
*     nx, ny - integer, input.
*     See the calling routine for description.
*
*     vol, hx, hy, qz - real array, input.
*     See the calling routine for description.
*
*     fpsi - (-1:nx,-1:ny,0:3) real array, input.
*     For (ix,iy,i) in (-1:nx,-1:ny,0:3), fpsi(ix,iy,i) specifies the
*     potential for the magnetic field components (bx,by) at the
*     (ix,iy,i) position.
*     (In the case of straight symmetry, bx=dpsi/dy and by=-dpsi/dx.
*     In the case of toroidal symmetry, bx=(dpsi/dy)/(2*pi*R) and
*     by=-(dpsi/dx)/(2*pi*R).)
*
*     ffbz - (-1:nx,-1:ny,0:3) real array, input.
*     For (ix,iy,i) in (-1:nx,-1:ny,0:3), ffbz(ix,iy,i) specifies the
*     field function for the magnetic field component bz at the
*     (ix,iy,i) position.
*     (In the case of straight symmetry, bz=ffbz. In the case of
*     toroidal symmetry, bz=ffbz/(2*pi*R).)
*
*     bb - (-1:nx,-1:ny,0:3) real array, output.
*     For (ix,iy) in (-1:nx,-1:ny), bb(ix,iy,0:3) specifies the
*     magnetic field at the center of the (ix,iy) cell.
*     bb(ix,iy,0:2) are the (x,y,z)-components and bb(ix,iy,3) is
*     the absolute magnetic field strength.
*     (bb(,,3)=sqrt(bb(,,0)**2+bb(,,1)**2+bb(,,2)**2.)
*     It will hold that 0.lt.bb(,,3).
*
*-----------------------------------------------------------------------
*.declarations

*   ..local variables
      integer ix, iy
      real(kind=R8) t0,psdx,psdy,babs,pi,r0,b0
*   ..procedures
      intrinsic sqrt,sign
*-----------------------------------------------------------------------
*.computation

* ..compute magnetic field
      pi=4.*atan(1.)
*   ..loop over cells

      r0=0.86
      b0=2.

      do iy = -1, ny
c***
c       print*,'iy=',iy
c***
        do ix = -1, nx
*     ..compute magnetic field at cell center
         t0=0.5*pi*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)
     .     +crx(ix,iy,3))

         bb(ix,iy,0) =
     .     (fpsi(ix,iy,2)-fpsi(ix,iy,0)+fpsi(ix,iy,3)-fpsi(ix,iy,1))

c       psdx=0.25*(psidx(ix,iy,0)+psidx(ix,iy,1)+psidx(ix,iy,2)
c    .    +psidx(ix,iy,3))
c       psdy=0.25*(psidy(ix,iy,0)+psidy(ix,iy,1)+psidy(ix,iy,2)
c    .    +psidy(ix,iy,3))
c       babs=sqrt(psdx*psdx+psdy*psdy)/t0
         babs=0.25*
     .     (sqrt(psidx(ix,iy,0)**2+psidy(ix,iy,0)**2)
     .     +sqrt(psidx(ix,iy,1)**2+psidy(ix,iy,1)**2)
     .     +sqrt(psidx(ix,iy,2)**2+psidy(ix,iy,2)**2)
     .     +sqrt(psidx(ix,iy,3)**2+psidy(ix,iy,3)**2) )/t0
         bb(ix,iy,0)=sign(babs,bb(ix,iy,0))

         bb(ix,iy,1) = 0.0e0
         bb(ix,iy,2) = (ffbz(ix,iy,0)+ffbz(ix,iy,1)+ffbz(ix,iy,2)+
     .     ffbz(ix,iy,3))/(4*t0)
         bb(ix,iy,3) =
     .     sqrt(bb(ix,iy,0)**2+bb(ix,iy,1)**2+bb(ix,iy,2)**2)
        enddo
      enddo

* ..return
      return
      end
