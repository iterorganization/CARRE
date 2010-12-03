! Dummy routines to satisfy NCARG dependency
      subroutine pltini
        return
      end subroutine pltini
      subroutine cadre
        return
      end subroutine cadre
      subroutine motifs
        return
      end subroutine motifs
      subroutine pltend
        return
      end subroutine pltend
      subroutine endpag
        return
      end subroutine endpag
!=======================================================================
      subroutine cntour(f1,f2,n1,n2,xmin,xmax,ymin,ymax)
        implicit none
#include <CARREDIM.F>        
        !  arguments
        integer n1,n2
        real*8 f1(nxmax,n2),f2(nxmax,n2),xmin,xmax,ymin,ymax

        return
      end subroutine cntour
!=======================================================================
      subroutine trace(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, & 
     &        nstruc,npstru,xstruc,ystruc,nivx,nivy, & 
     &        nivtot,nbniv,np1,npr,xmail,ymail,nreg)
        implicit none
#include <CARREDIM.F>        
        !  arguments
        integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, & 
             &        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg 
        real*8 x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), & 
             &       xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
             &       nivx(npnimx,nbniv),nivy(npnimx,nbniv), & 
             &       xmail(npmamx,nrmamx,*),ymail(npmamx,nrmamx,*)
        return
      end
!=======================================================================
      subroutine trace2(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, & 
           &        nstruc,npstru,xstruc,ystruc,nivx,nivy, & 
           &        nivtot,nbniv)
        implicit none
#include <CARREDIM.F>    
        !  arguments
        integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, & 
             &      nivtot(nbniv),ptsep(4,npx),nreg, nn1, & 
             &      repart,nx,ny


        real*8 x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), & 
             &       xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
             &       nivx(npnimx,nbniv),nivy(npnimx,nbniv), & 
             &       xptxo,yptxo,fctini,xfin,yfin, & 
             &       a00(nxmax,nymax,3),a01(nxmax,nymax,3),a10(nxmax,nymax,3), & 
             &       a11(nxmax,nymax,3),psi(nxmax,nymax)

        return
      end subroutine trace2
!=======================================================================
      subroutine trace3(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, & 
           &        nstruc,npstru,xstruc,ystruc,nivx,nivy, & 
           &        nivtot,nbniv)
        implicit none
#include <CARREDIM.F>    
        !  arguments
        integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, & 
             &      nivtot(nbniv),ptsep(4,npx),nreg, nn1, & 
             &      repart,nx,ny


        real*8 x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), & 
             &       xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
             &       nivx(npnimx,nbniv),nivy(npnimx,nbniv), & 
             &       xptxo,yptxo,fctini,xfin,yfin, & 
             &       a00(nxmax,nymax,3),a01(nxmax,nymax,3),a10(nxmax,nymax,3), & 
             &       a11(nxmax,nymax,3),psi(nxmax,nymax)

        return
      end subroutine trace3
!=======================================================================
      
