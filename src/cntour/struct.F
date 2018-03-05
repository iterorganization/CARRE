      subroutine struct(x1,x2,y1,y2,nstruc,xstruc,ystruc,npstru)
c
c  version : 07.07.97 20:18
c
c======================================================================
      implicit none

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

c  arguments
      integer nstruc,npstru(strumx)
      real(kind=R8)
     .  x1,x2,y1,y2,xstruc(npstmx,strumx),ystruc(npstmx,strumx)
c
c  variables locales
      integer i,nin,k,npmx,np
c      parameter(npmx=100)
      parameter(npmx=npstmx)
      real x(npmx),y(npmx),xmin,xmax,ymin,ymax
      character echx*3,echy*3
c
c  procedures
      external agcurv,newpag,endpag
c======================================================================
c  calculs
      echx='LIN'
      echy='LIN'
      nin=1
c
c  copie de variables double precision a simple precision
      xmin=x1
      xmax=x2
      ymin=y1
      ymax=y2
c
c     CALL newpag(xmin, xmax, ymin, ymax, 'R (m)$', 'Z (m)$', ' ',
c    +     echx, echy)
      DO 80 k=1, nstruc
c
         if(npstru(k).lt.0) then
            np=-npstru(k)
            do 70 i=1,np
               x(i)=xstruc(i,k)
               y(i)=ystruc(i,k)
   70       continue
         else
            np=npstru(k)
            do 75 i=1,np
               x(i)=xstruc(i,k)
               y(i)=ystruc(i,k)
   75       continue
            if(x(1).ne.x(np) .or. y(1).ne.y(np)) then
               np=np+1
               x(np)=x(1)
               y(np)=y(1)
            endif
         endif
         CALL agcurv(x, 1, y, 1, np, nin)
   80 CONTINUE

c     CALL endpag
      return
      end
