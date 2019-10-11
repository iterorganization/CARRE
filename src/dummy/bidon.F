      subroutine pltini
c
c  version : 19.04.2000 22:29
c
      return
      end
      subroutine cadre
      return
      end
      subroutine motifs
      return
      end
      subroutine pltend
      return
      end
      subroutine endpag
      return
      end
c=======================================================================
      subroutine cntour(f1,f2,n1,n2,xmin,xmax,ymin,ymax)
      implicit none
#include <CARRETYPES.F>
#include <CARREDIM.F>
c  arguments
      integer n1,n2
      real(kind=R8) f1(nxmax,n2),f2(nxmax,n2),xmin,xmax,ymin,ymax

      return
      end
c=======================================================================
      subroutine trace(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot,
     .        nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .        nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)
      implicit none
#include <CARRETYPES.F>
#include <CARREDIM.F>
c  arguments
      integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv,
     .        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg

      real(kind=R8)
     .       x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx),
     .       xstruc(npstmx,strumx), ystruc(npstmx,strumx),
     .       nivx(npnimx,nbniv),nivy(npnimx,nbniv),
     .       xmail(npmamx,nrmamx,*),ymail(npmamx,nrmamx,*)
      logical last

      return
      end
c=======================================================================
      subroutine trace2(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot,
     .        nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .        nivtot,nbniv)
      implicit none
#include <CARRETYPES.F>
#include <CARREDIM.F>
c  arguments
      integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv,
     .      nivtot(nbniv),ptsep(4,npx)

      real(kind=R8)
     .       x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx),
     .       xstruc(npstmx,strumx),ystruc(npstmx,strumx),
     .       nivx(npnimx,nbniv),nivy(npnimx,nbniv)

      return
      end
c=======================================================================
      subroutine trace3(
     .        nstruc,npstru,xstruc,ystruc,
     .        pntrat,distxo,xn1,yn1,nn1,
     .        xfin,yfin,fctfin,
     .        psi,nx,ny,x,y)
      implicit none
#include <CARRETYPES.F>
#include <CARREDIM.F>
c  arguments
      integer nstruc,npstru(strumx),nn1,nx,ny

      real(kind=R8)
     .       xstruc(npstmx,strumx),ystruc(npstmx,strumx),
     .       pntrat,distxo,
     .       xn1(nn1),yn1(nn1),xfin,yfin,fctfin,
     .       psi(nxmax,nymax),x(nxmax),y(nymax)

      return
      end
