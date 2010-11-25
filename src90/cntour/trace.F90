      subroutine trace(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, & 
     &        nstruc,npstru,xstruc,ystruc,nivx,nivy, & 
     &        nivtot,nbniv,np1,npr,xmail,ymail,nreg)
!======================================================================
!
!  version : 07.07.97 20:15
!
      implicit none

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!
!  arguments
      integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, & 
     &        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg 

      real*8 x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), & 
     &       xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
     &       nivx(npnimx,nbniv),nivy(npnimx,nbniv), & 
     &       xmail(npmamx,nrmamx,*),ymail(npmamx,nrmamx,*)

!
!  variables locales
      integer i,j,k,nin
      real x(npnimx),y(npnimx),xmin,xmax,ymin,ymax
      real deltax,deltay
      character echx*3,echy*3
!
!  procedures
      external agcurv,newpag,endpag
!
!  calculs
      echx='LIN'
      echy='LIN'
      nin=1

!  Copie des variables de double a simple precision.

!     xmin=0.5
!     xmax=1.2
!     ymin =-0.3
!     ymax=0.5
      deltax=x2-x1
      deltay=y2-y1
      xmin=(x1+x2)/2.-max(deltax,deltay)/2.
      xmax=(x1+x2)/2.+max(deltax,deltay)/2.
      ymin=(y1+y2)/2.-max(deltax,deltay)/2.
      ymax=(y1+y2)/2.+max(deltax,deltay)/2.
!***
!     xmin=0.6
!     xmax=0.8
!     ymin=0.3
!     ymax=0.46
!***
!***
!     DO 2 j=1, nstruc
!        DO 4 i=1, npstru(j)
!
!            xmin=MIN(xmin,xstruc(i,j))
!            xmax=MAX(xmax,xstruc(i,j))
!            ymin=MIN(ymin,ystruc(i,j))
!            ymax=MAX(ymax,ystruc(i,j))
!   4    CONTINUE
!   2 CONTINUE
!***
      CALL newpag(xmin, xmax, ymin, ymax, 'R (m)$', 'Z (m)$', ' ', & 
     &     echx, echy)

      CALL struct(x1,x2,y1,y2,nstruc,xstruc,ystruc,npstru)

      DO 10 i=1, npx
        DO 20 j=1, 4
          if(ptsep(j,i).gt.0) then
            DO 30 k=1, nptot(ptsep(j,i),i)

               x(k)=separx(k,ptsep(j,i),i)
               y(k)=separy(k,ptsep(j,i),i)

   30       CONTINUE
            if(nptot(ptsep(j,i),i).gt.0) & 
     &                CALL agcurv(x,1,y,1,nptot(ptsep(j,i),i),nin)
!***
!     write(26,*)'separatrix',i,j
!     do k=1,nptot(ptsep(j,i),i)
!       write(26,*)x(k),y(k)
!     enddo
          endif
!***
   20   CONTINUE
   10 CONTINUE

      DO 40 i=1, nbniv
         DO 50 k=1, nivtot(i)

               x(k)=nivx(k,i)
               y(k)=nivy(k,i)

   50    CONTINUE
!***
!     write(26,*)'frontiere',i
!     do k=1,nivtot(i)
!       write(26,*)x(k),y(k)
!     enddo
!***
            if(nivtot(i).gt.0) CALL agcurv(x,1,y,1,nivtot(i),nin)
   40 CONTINUE

!..Trace du quadrillage des donnees.

!     do 55 i=1, nx
!        do 56 j=1, ny
!
!           x(i)=cordx(i)
!           y(j)=cordy(j)

!  56    continue
!        CALL agcurv(x,1,y,1,ny,nin)
!  55 continue

!        do 58 j=1,ny
!        do 57 i=1,nx

!           x(i)=cordx(i)
!           y(j)=cordy(i)
!  57    continue
!        CALL agcurv(x,1,y,1,ny,nin)
!  58    continue


      CALL endpag

      CALL newpag(xmin, xmax, ymin, ymax, 'R (m)$', 'Z (m)$', ' ', & 
     &     echx, echy)


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
!***
!     x(1)=.65
!     y(1)=.39
!     x(2)=.8
!     y(2)=.39
!     call agcurv(x,1,y,1,2,2)
!***
!

      CALL endpag

      return
      end
