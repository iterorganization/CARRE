
************************************************************************
      SUBROUTINE INIPSI(psi,psidx,psidy,x,y,nxmax,nymax,nx,ny,
     .  a00,a10,a01,a11)
************************************************************************
      implicit none
#include <CARRETYPES.F>
c
c  initialisation des coefficients d'interpolation pour psi et ses
c  derivees par rapport a x et a y
c
c  arguments
      integer nxmax,nymax,nx,ny
      real(kind=R8)
     .  psi(nxmax,ny),psidx(nxmax,ny),psidy(nxmax,ny),x(nx),y(ny),
     .  a00(nxmax,nymax,3),a10(nxmax,nymax,3),a01(nxmax,nymax,3),
     .  a11(nxmax,nymax,3)
c
c  calculs
      CALL COEFF(nxmax,nymax,nx,ny,x,y,psi,a00(1,1,1),a10(1,1,1),
     .  a01(1,1,1),a11(1,1,1))
      CALL COEFF(nxmax,nymax,nx,ny,x,y,psidx,a00(1,1,2),a10(1,1,2),
     .  a01(1,1,2),a11(1,1,2))
      CALL COEFF(nxmax,nymax,nx,ny,x,y,psidy,a00(1,1,3),a10(1,1,3),
     .  a01(1,1,3),a11(1,1,3))
      return
      end
