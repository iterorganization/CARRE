      subroutine trace(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot,
     .        nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .        nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)
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
     .        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg
     .
      real(kind=R8)
     .       x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx),
     .       xstruc(npstmx,strumx), ystruc(npstmx,strumx),
     .       nivx(npnimx,nbniv),nivy(npnimx,nbniv),
     .       xmail(npmamx,nrmamx,*),ymail(npmamx,nrmamx,*)
      logical last
c
c  variables locales
      integer i,j,k,nin
      real x(npnimx),y(npnimx),xmin,xmax,ymin,ymax
      real deltax,deltay
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
      deltax=x2-x1
      deltay=y2-y1
      xmin=(x1+x2)/2.-max(deltax,deltay)/2.
      xmax=(x1+x2)/2.+max(deltax,deltay)/2.
      ymin=(y1+y2)/2.-max(deltax,deltay)/2.
      ymax=(y1+y2)/2.+max(deltax,deltay)/2.
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

*..Trace du quadrillage des donnees.

c     do 55 i=1, nx
c        do 56 j=1, ny
c
c           x(i)=cordx(i)
c           y(j)=cordy(j)

c  56    continue
c        CALL agcurv(x,1,y,1,ny,nin)
c  55 continue

c        do 58 j=1,ny
c        do 57 i=1,nx

c           x(i)=cordx(i)
c           y(j)=cordy(i)
c  57    continue
c        CALL agcurv(x,1,y,1,ny,nin)
c  58    continue

      CALL endpag

      CALL newpag(xmin, xmax, ymin, ymax, 'R (m)$', 'Z (m)$', ' ',
     +     echx, echy)

      
      CALL struct(x1,x2,y1,y2,nstruc,xstruc,ystruc,npstru)

      DO 60 i=1, nreg
         DO 70 j=1, npr(i)
            DO 80 k=1, np1(i)

               x(k)=xmail(k,j,i)
               y(k)=ymail(k,j,i)

   80       CONTINUE
            if(np1(i).gt.0) CALL agcurv(x,1,y,1,np1(i),nin)
   70    CONTINUE
   60 CONTINUE
      do 90 i=1,nreg
      do 88 k=1,np1(i)
      do 86 j=1,npr(i)
      x(j)=xmail(k,j,i)
      y(j)=ymail(k,j,i)
86    continue
      call agcurv(x,1,y,1,npr(i),nin)
88    continue
90    continue
c***
c     x(1)=.65
c     y(1)=.39
c     x(2)=.8
c     y(2)=.39
c     call agcurv(x,1,y,1,2,2)
c***
c

      if (last) CALL endpag

      return
      end
