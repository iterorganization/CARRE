      subroutine trace3(
     .        nstruc,npstru,xstruc,ystruc,
     .        pntrat,distxo,xn1,yn1,nn1,
     .        xfin,yfin,fctfin,
     .        psi,nx,ny,x,y)
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
      integer nstruc,npstru(strumx),nn1,nx,ny
     
      real(kind=R8)
     .       xstruc(npstmx,strumx),ystruc(npstmx,strumx),
     .       pntrat,distxo,
     .       xn1(nn1),yn1(nn1),xfin,yfin,fctfin,
     .       psi(nxmax,nymax),x(nxmax),y(nymax)
      
c
c  variables locales
      integer i,nin,nn(2),inouv,npcrb(2),ii,jj,dir,indstr,plaque
      real(kind=R8) zero
      parameter(zero=0.0_R8)
      real(kind=R8)
     .  xn(npnimx,2),yn(npnimx,2),xcrb(npnimx,2),ycrb(npnimx,2),
     .  x2,y2
      real xs(npnimx),ys(npnimx)
c
c  procedures
      integer ifind
      external agcurv,newpag,endpag
c
c  calculs
      nin=1

c  Copie des variables de double a simple precision.

      if(pntrat.lt.distxo) then

C     ON DESSINE LA SURFACE INTERIEURE

C     On identifie le point de depart
        
*..Psi value at the X-point
          x2=xfin
          y2=yfin
            
*..Parametrisation de la ligne de niveau qui passe par ce point.
          inouv=2
          xn(1,inouv)=x2
          yn(1,inouv)=y2
          nn(inouv)=1

          ii=ifind(x2,x,nx,1)
          jj=ifind(y2,y,ny,1)

*..Definition de la courbe a ne pas traverser.
          DO 14 i=1, nn1
           xcrb (i,1)=xn1(i)
           ycrb (i,1)=yn1(i)
   14     CONTINUE

          npcrb(1)=nn1
          npcrb(2)=0

*..Recherche du deuxieme point.

          dir=0
          plaque=0
c***
c         print*,'call crbniv1 - mailcn'
c***
          CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .            fctfin,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1,
     .            plaque,x2,y2)

*..Il faut s'assurer que la ligne de niveau part dans la bonne
*  direction.

c0195     IF (xn(2,inouv) .LT. xn(1,inouv)) THEN
          if((xn(2,inouv)-xn(1,inouv))*(xn1(2)-xn1(1))+
     .          (yn(2,inouv)-yn(1,inouv))*(yn1(2)-yn1(1))
     .          .lt.zero) then
            nn(inouv)=1
            dir=MOD(dir+1,4) + 1
            ii=ii - MOD(dir-2,2)
            jj=jj - MOD(dir-3,2)
          ENDIF
*..Pour les points successifs, on poursuit jusqu'a ce qu'on ferme la
*  boucle.
c***
c         print*,'call crbniv2 - mailcn'
c***

          CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .            fctfin,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1,
     .            plaque,x2,y2)

*..Le dernier point de la courbe est egal au premier.

          xn(nn(inouv),inouv)=xn(1,inouv)
          yn(nn(inouv),inouv)=yn(1,inouv)

      endif
      do i=1,nn(inouv)
        xs(i)=xn(i,inouv)
        ys(i)=yn(i,inouv)
        enddo
      if(nn(inouv).gt.0) 
     > CALL agcurv(xs,1,ys,1,nn(inouv),nin)
      
      return
      end

