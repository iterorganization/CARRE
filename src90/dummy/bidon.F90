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
        use KindDefinitions
        use carre_dimensions
        implicit none
        !  arguments
        integer n1,n2
        real(rKind) :: f1(nxmax,n2),f2(nxmax,n2),xmin,xmax,ymin,ymax

        return
      end subroutine cntour
!=======================================================================
      subroutine trace(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, &
     &        nstruc,npstru,xstruc,ystruc,nivx,nivy, &
     &        nivtot,nbniv,np1,npr,xmail,ymail,nreg)
        use KindDefinitions
        use carre_dimensions
        implicit none
        !  arguments
        integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, &
             &        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg
        real(rKind) :: x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), &
             &       xstruc(npstmx,strumx),ystruc(npstmx,strumx), &
             &       nivx(npnimx,nbniv),nivy(npnimx,nbniv), &
             &       xmail(npmamx,nrmamx,*),ymail(npmamx,nrmamx,*)
        return
      end
!=======================================================================
      subroutine trace2(x1,x2,y1,y2,separx,separy,ptsep,npx,nptot, &
           &        nstruc,npstru,xstruc,ystruc,nivx,nivy, &
           &        nivtot,nbniv)
        use KindDefinitions
        use carre_dimensions
        implicit none
        !  arguments
        integer npx,nptot(4,npxmx),nstruc,npstru(strumx),nbniv, &
             &      nivtot(nbniv),ptsep(4,npx)


        real(rKind) :: x1,x2,y1,y2,separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), &
             &       xstruc(npstmx,strumx),ystruc(npstmx,strumx), &
             &       nivx(npnimx,nbniv),nivy(npnimx,nbniv)

        return
      end subroutine trace2
!=======================================================================
      subroutine trace3(x1,x2,y1,y2, &
           &        nstruc,npstru,xstruc,ystruc, &
           &        pntrat,distxo,xn1,yn1,nn1, &
           &        fctini,xfin,yfin,fctfin, &
           &        psi,nx,ny,x,y)
        use KindDefinitions
        use carre_dimensions
        implicit none
        !  arguments
        integer nstruc,npstru(strumx),nn1,nx,ny

        real(rKind) ::  x1,x2,y1,y2, &
             &       xstruc(npstmx,strumx),ystruc(npstmx,strumx), &
             &       pntrat,distxo, &
             &       xn1(nn1),yn1(nn1),fctini,xfin,yfin,fctfin, &
             &       psi(nxmax,nymax),x(nxmax),y(nymax)

        return
      end subroutine trace3
!=======================================================================

