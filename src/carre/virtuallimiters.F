      subroutine virtuallimiters(nx,ny,x,y,psi,npx,ptx,pty,
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
#include <CARREDIM.F>

*  arguments

*  variables locales
      
*  procedures

c=======================================================================

* We create one virtual limiter for every limiting curve
* The virtual limiters are triangles, the tip of which is placed 
* in the middle of the limiting curve (measured
* in physical length along the curve) and the triangle is oriented
* away from the O-point



