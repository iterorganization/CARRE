      subroutine trace3(xminx,xmaxx,yminx,ymaxx,separx,separy, & 
     &        ptsep,npx,nptot, & 
     &        nstruc,npstru,xstruc,ystruc,nivx,nivy, & 
     &        nivtot,nbniv,pntrat,distxo,xn1,yn1,nn1, & 
     &        repart,xptxo,yptxo,fctini,xfin,yfin,fctfin, & 
     &        a00,a01,a10,a11,psi,nx,ny,x,y)
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
     &      nivtot(nbniv),ptsep(4,npx),nreg, nn1, & 
     &      repart,nx,ny


      real*8 xminx,xmaxx,yminx,ymaxx,separx(npnimx,4,npxmx), & 
     &       separy(npnimx,4,npxmx), & 
     &       xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
     &       nivx(npnimx,nbniv),nivy(npnimx,nbniv),pntrat,distxo, & 
     &       xn1(nn1),yn1(nn1),xptxo,yptxo,fctini,xfin,yfin,fctfin, & 
     &       a00(nxmax,nymax,3),a01(nxmax,nymax,3),a10(nxmax,nymax,3), & 
     &       a11(nxmax,nymax,3),psi(nxmax,nymax),x(nxmax),y(nymax)


!
!  variables locales
      integer i,j,k,nin,nn(2),inouv,sens,npcrb(2),nt,ii,jj,dir,indstr, & 
     &  plaque
      real*8 valfct,fctnew,xt(3),yt(3),zero,xpto,ypto,fctpto
      parameter(zero=0.)
      real*8 xn(npnimx,2),yn(npnimx,2),xcrb(npnimx,2),ycrb(npnimx,2),x1, & 
     &  x2,y1,y2
      real xs(npnimx),ys(npnimx),xmin,xmax,ymin,ymax
      character echx*3,echy*3
!
!  procedures
      integer ifind
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
      xmin=xminx
      xmax=xmaxx
      ymin=yminx
      ymax=ymaxx
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

      if(pntrat.lt.distxo) then

!     ON DESSINE LA SURFACE INTERIEURE

      x1 = xn1(1)
      y1 = yn1(1)
!     On identifie le point de depart

         valfct=fctini
!..Psi value at the X-point
       x2=xfin
       y2=yfin

            sens=1






!..Parametrisation de la ligne de niveau qui passe par ce point.
          inouv=2
          xn(1,inouv)=x2
          yn(1,inouv)=y2
          nn(inouv)=1

          ii=ifind(x2,x,nx,1)
          jj=ifind(y2,y,ny,1)



!..Definition de la courbe a ne pas traverser.
          DO 14 i=1, nn1
           xcrb (i,1)=xn1(i)
           ycrb (i,1)=yn1(i)
   14     CONTINUE

          npcrb(1)=nn1
          npcrb(2)=0

!..Recherche du deuxieme point.

          dir=0

!***
!         print*,'call crbniv1 - mailcn'
!***
          CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi, & 
     &            fctfin,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx, & 
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, & 
     &            plaque,x2,y2)

!..Il faut s'assurer que la ligne de niveau part dans la bonne
!  direction.

!0195     IF (xn(2,inouv) .LT. xn(1,inouv)) THEN
          if((xn(2,inouv)-xn(1,inouv))*(xn1(2)-xn1(1))+ & 
     &          (yn(2,inouv)-yn(1,inouv))*(yn1(2)-yn1(1)) & 
     &          .lt.zero) then
            nn(inouv)=1
            dir=MOD(dir+1,4) + 1
            ii=ii - MOD(dir-2,2)
            jj=jj - MOD(dir-3,2)
          ENDIF
!..Pour les points successifs, on poursuit jusqu'a ce qu'on ferme la
!  boucle.
!***
!         print*,'call crbniv2 - mailcn'
!***

          CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi, & 
     &            fctfin,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx, & 
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, & 
     &            plaque,x2,y2)

!..Le dernier point de la courbe est egal au premier.

          xn(nn(inouv),inouv)=xn(1,inouv)
          yn(nn(inouv),inouv)=yn(1,inouv)

      endif
      do i=1,nn(inouv)
        xs(i)=xn(i,inouv)
        ys(i)=yn(i,inouv)
        enddo
      if(nn(inouv).gt.0) & 
     & CALL agcurv(xs,1,ys,1,nn(inouv),nin)








      return
      end

