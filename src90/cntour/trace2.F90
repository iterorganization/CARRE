      subroutine trace2(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, &
     &        nstruc,npstru,xstruc,ystruc,nivx,nivy, &
     &        nivtot,nbniv)
!======================================================================
!
!  version : 07.07.97 20:15
!
      use KindDefinitions
      implicit none

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!
!  arguments
      integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, &
     &      nivtot(nbniv),ptsep(4,npx)


      real(rKind) :: x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), &
     &       xstruc(npstmx,strumx),ystruc(npstmx,strumx), &
     &       nivx(npnimx,nbniv),nivy(npnimx,nbniv)


!
!  variables locales
      integer i,j,k,nin
      real(Single) :: x(npnimx),y(npnimx),xmin,xmax,ymin,ymax
      character echx*3,echy*3
!
!  procedures
      external agcurv,newpag,endpag,struct
!
!  calculs
      echx='LIN'
      echy='LIN'
      nin=1

!  Copie des variables de double a simple precision.

!     xmin=0.5
!     xmax=1.2
!     ymin=-0.3
!     ymax=0.5
      xmin=real(x1,Single)
      xmax=real(x2,Single)
      ymin=real(y1,Single)
      ymax=real(y2,Single)
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
!            xmin=MIN(xmin,real(xstruc(i,j),Single))
!            xmax=MAX(xmax,real(xstruc(i,j),Single))
!            ymin=MIN(ymin,real(ystruc(i,j),Single))
!            ymax=MAX(ymax,real(ystruc(i,j),Single))
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

               x(k)=real(separx(k,ptsep(j,i),i),Single)
               y(k)=real(separy(k,ptsep(j,i),i),Single)

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

               x(k)=real(nivx(k,i),Single)
               y(k)=real(nivy(k,i),Single)

   50    CONTINUE
!***
!     write(26,*)'frontiere',i
!     do k=1,nivtot(i)
!       write(26,*)x(k),y(k)
!     enddo
!***
            if(nivtot(i).gt.0) CALL agcurv(x,1,y,1,nivtot(i),nin)
   40 CONTINUE







      return
      end

