      subroutine trace2(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot,
     .        nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .        nivtot,nbniv)
c======================================================================
c
c  version : 07.07.97 20:15
c
      implicit none

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

c
c  arguments
      integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv,
     .      nivtot(nbniv),ptsep(4,npx)
     
      real(kind=R8)
     .       x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx),
     .       xstruc(npstmx,strumx),ystruc(npstmx,strumx),
     .       nivx(npnimx,nbniv),nivy(npnimx,nbniv)
      
c
c  variables locales
      integer i,j,k,nin
      real x(npnimx),y(npnimx),xmin,xmax,ymin,ymax
      character echx*3,echy*3
c
c  procedures
      external agcurv,newpag,endpag
c
c  calculs
      echx='LIN'
      echy='LIN'
      nin=1

c  Copie des variables de double a simple precision.

c     xmin=0.5
c     xmax=1.2
c     ymin =-0.3
c     ymax=0.5
      xmin=x1
      xmax=x2
      ymin=y1
      ymax=y2
c***
c     xmin=0.6
c     xmax=0.8
c     ymin=0.3
c     ymax=0.46
c***
c***
c     DO 2 j=1, nstruc
c        DO 4 i=1, npstru(j)
c
c            xmin=MIN(xmin,xstruc(i,j))
c            xmax=MAX(xmax,xstruc(i,j))
c            ymin=MIN(ymin,ystruc(i,j))
c            ymax=MAX(ymax,ystruc(i,j))
c   4    CONTINUE
c   2 CONTINUE
c***
      CALL newpag(xmin, xmax, ymin, ymax, 'R (m)$', 'Z (m)$', ' ',
     +     echx, echy)

      CALL struct(x1,x2,y1,y2,nstruc,xstruc,ystruc,npstru)

      DO 10 i=1, npx
        DO 20 j=1, 4
          if(ptsep(j,i).gt.0) then
            DO 30 k=1, nptot(ptsep(j,i),i)

               x(k)=separx(k,ptsep(j,i),i)
               y(k)=separy(k,ptsep(j,i),i)

   30       CONTINUE
            if(nptot(ptsep(j,i),i).gt.0)
     .                CALL agcurv(x,1,y,1,nptot(ptsep(j,i),i),nin)
c***
c     write(26,*)'separatrix',i,j
c     do k=1,nptot(ptsep(j,i),i)
c       write(26,*)x(k),y(k)
c     enddo
          endif
c***
   20   CONTINUE
   10 CONTINUE

      DO 40 i=1, nbniv
         DO 50 k=1, nivtot(i)

               x(k)=nivx(k,i)
               y(k)=nivy(k,i)

   50    CONTINUE
c***
c     write(26,*)'frontiere',i
c     do k=1,nivtot(i)
c       write(26,*)x(k),y(k)
c     enddo
c***
            if(nivtot(i).gt.0) CALL agcurv(x,1,y,1,nivtot(i),nin)
   40 CONTINUE

      return
      end

