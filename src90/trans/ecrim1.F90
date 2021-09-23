!=======================================================================
      subroutine ecrim1(nfin,rm,zm,nptseg,nreg,nppol,nprad, &
     &  npmamx,nrmamx)
      use KindDefinitions
      implicit none
!  cette sous-routine ecrit une maille curviligne orthogonale produite
!  par le programme carre, sous un format particulier.
!
!  arguments
      integer npmamx,nrmamx,nfin,nreg,nppol(*),nprad(*), &
     &  nptseg(*)
      real(rKind) :: rm(npmamx,nrmamx,*),zm(npmamx,nrmamx,*)
!
!  variables locales
      integer ntrmax
      parameter(ntrmax=3000)
      integer ncoord,nelem,ielem,iplq(4),iflx(5),ir,ipol,ireg,i,j, &
     &  e(3,ntrmax),v(3,ntrmax),icoord,itrop,iremp,cotali(ntrmax),t,r
      real(rKind) :: xmail(ntrmax/2),ymail(ntrmax/2)

!  procedures externes
      external enlpnt
!
!  iplq(i): indice de frontiere associe a la plaque i
!  iflx(i): indice de frontiere associe a la surface de flux i
!  cotali: indice du cote aligne
!
!  calcul
!
!  1. format mailtri standard
      if(nreg.eq.3 .or.nreg.eq.6) then
!  1.01   simple nul ou double nul deconnecte
        ncoord=nppol(1)*nprad(1)
        do ireg=2,nreg-1
          ncoord=ncoord+nppol(ireg)*(nprad(ireg)-1)
        enddo
      elseif(nreg.eq.5) then
!  1.01   double nul connecte
        ncoord=nppol(1)*nprad(1)+nppol(3)*nprad(3)
        do i=1,2
          ireg=((7-i)*i-2)/2
          ncoord=ncoord+nppol(ireg)*(nprad(ireg)-1)
        enddo
      else
        print*,'attention dans ecrima: nreg=',nreg
      endif

      ireg=nreg
      ncoord=ncoord+(nppol(ireg)-1)*(nprad(ireg)-1)

      if(nreg.eq.3) then
        ireg=1
        icoord=0
        do ir=1,nprad(ireg)
        do ipol=1,nppol(ireg)
          icoord=icoord+1
          xmail(icoord)=rm(ipol,ir,ireg)
          ymail(icoord)=zm(ipol,ir,ireg)
        enddo
        enddo
        do ireg=2,nreg-1
        do ir=2,nprad(ireg)
        do ipol=1,nppol(ireg)
          icoord=icoord+1
          xmail(icoord)=rm(ipol,ir,ireg)
          ymail(icoord)=zm(ipol,ir,ireg)
        enddo
        enddo
        enddo
        ireg=3
        do ir=2,nprad(ireg)
        do ipol=1,nppol(ireg)-1
          icoord=icoord+1
          xmail(icoord)=rm(ipol,ir,ireg)
          ymail(icoord)=zm(ipol,ir,ireg)
        enddo
        enddo
      else
        write(6,*)'non prevu dans ecrima: nreg=',nreg
        return
      endif

!
!  1.1  formation des elements
      nelem=0
      do ireg=1,nreg
        nelem=nelem+(nppol(ireg)-1)*(nprad(ireg)-1)
      enddo
      nelem=2*nelem
      ielem=0
      if(nreg.eq.3) then
!  1.11   simple nul

        iplq(1)=-1
        iplq(2)=-2
        iflx(1)=-3
        iflx(2)=-4
        iflx(3)=-5
!  region 1
        ireg=1

!  on commence par la premiere couche dans la region 1
        do i=1,nptseg(1)-1
          ielem=ielem+1
          v(1,ielem)=i
          v(2,ielem)=nppol(ireg)+v(1,ielem)+1
          v(3,ielem)=v(2,ielem)-1
          e(1,ielem)=ielem+1
          e(2,ielem)=2*(nppol(ireg)-1)+ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.1) e(3,ielem)=iplq(1)
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          v(2,ielem)=v(1,ielem-1)+1
          e(1,ielem)=2*(nppol(ireg)-1)*(nprad(ireg)-1)+ielem-1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          cotali(ielem)=1
        enddo

        do i=nptseg(1),nptseg(1)+nptseg(3)-2
          ielem=ielem+1
          v(1,ielem)=i
          v(2,ielem)=nppol(ireg)+v(1,ielem)+1
          v(3,ielem)=v(2,ielem)-1
          e(1,ielem)=ielem+1
          e(2,ielem)=2*(nppol(ireg)-1)+ielem+1
          e(3,ielem)=ielem-1
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          if(i.eq.nptseg(1)+nptseg(3)-2) then
            v(2,ielem)=nptseg(1)
          else
            v(2,ielem)=v(1,ielem-1)+1
          endif
          e(1,ielem)=2*(nppol(1)-1)*(nprad(1)-1) &
     &        +2*(nppol(2)-1)*(nprad(2)-1) &
     &        +2*(i-nptseg(1))+1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          cotali(ielem)=1
        enddo

        do i=nptseg(1)+nptseg(3)-1,nppol(ireg)-1
          ielem=ielem+1
          if(i.eq.nptseg(1)+nptseg(3)-1) then
            v(1,ielem)=nptseg(1)
          else
            v(1,ielem)=i
          endif
          v(2,ielem)=nppol(ireg)+i+1
          v(3,ielem)=v(2,ielem)-1
          e(1,ielem)=ielem+1
          e(2,ielem)=2*(nppol(ireg)-1)+ielem+1
          e(3,ielem)=ielem-1
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          v(2,ielem)=i+1
          e(1,ielem)=2*(nppol(1)-1)*(nprad(1)-1) &
     &      +2*(nptseg(1)-1) &
     &      +2*(i-(nptseg(1)+nptseg(3)-1))+1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.nppol(ireg)-1) e(2,ielem)=iplq(2)
          cotali(ielem)=1
        enddo
!
!  on fait maintenant les autres cellules de la region 1
        do j=2,nprad(ireg)-1
        do i=1,nppol(ireg)-1
          ielem=ielem+1
          v(1,ielem)=(j-1)*nppol(ireg)+i
          v(2,ielem)=nppol(ireg)+v(1,ielem)+1
          v(3,ielem)=v(2,ielem)-1
          e(1,ielem)=ielem+1
          if(j.eq.nprad(ireg)-1) then
            e(2,ielem)=iflx(1)
          else
            e(2,ielem)=2*(nppol(ireg)-1)+ielem+1
          endif
          e(3,ielem)=ielem-1
          if(i.eq.1) e(3,ielem)=iplq(1)
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          v(2,ielem)=v(1,ielem-1)+1
          e(1,ielem)=ielem-2*(nppol(ireg)-1)-1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.nppol(ireg)-1) e(2,ielem)=iplq(2)
          cotali(ielem)=1
        enddo
        enddo
!
!  region 2
        ireg=2
!
!  on commence par la premiere couche dans la region 2
        do i=1,nptseg(1)-1
          ielem=ielem+1
          v(1,ielem)=nppol(1)*nprad(1)+i
          v(2,ielem)=i+1
          v(3,ielem)=i
          e(1,ielem)=ielem+1
          e(2,ielem)=2*i
          e(3,ielem)=ielem-1
          if(i.eq.1) e(3,ielem)=iplq(1)
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          v(2,ielem)=v(1,ielem-1)+1
          e(1,ielem)=2*(nppol(ireg)-1)+ielem-1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          cotali(ielem)=1
        enddo

        do i=nptseg(1),nppol(ireg)-1
          ielem=ielem+1
          v(1,ielem)=nppol(1)*nprad(1)+i
          v(2,ielem)=nptseg(1)+(nptseg(3)-1)+(i-nptseg(1))+1
          if(i.eq.nptseg(1)) then
            v(3,ielem)=nptseg(1)
          else
            v(3,ielem)=v(2,ielem)-1
          endif
          e(1,ielem)=ielem+1
          e(2,ielem)=2*((nptseg(1)-1)+(nptseg(3)-1)+(i-nptseg(1)+1))
          e(3,ielem)=ielem-1
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          v(2,ielem)=v(1,ielem-1)+1
          e(1,ielem)=2*(nppol(ireg)-1)+ielem-1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.nppol(ireg)-1) e(2,ielem)=iplq(2)
          cotali(ielem)=1
        enddo
!
!  on fait maintenant les autres cellules de la region 2
        do j=2,nprad(ireg)-1
        do i=1,nppol(ireg)-1
          ielem=ielem+1
          v(1,ielem)=(j-1)*nppol(ireg)+nppol(1)*nprad(1)+i
          v(2,ielem)=v(1,ielem)-nppol(ireg)+1
          v(3,ielem)=v(2,ielem)-1
          e(1,ielem)=ielem+1
          e(2,ielem)=ielem-2*(nppol(ireg)-1)+1
          e(3,ielem)=ielem-1
          if(i.eq.1) e(3,ielem)=iplq(1)
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          v(2,ielem)=v(1,ielem-1)+1
          if(j.eq.nprad(ireg)-1) then
            e(1,ielem)=iflx(2)
          else
            e(1,ielem)=ielem+2*(nppol(ireg)-1)-1
          endif
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.nppol(ireg)-1) e(2,ielem)=iplq(2)
          cotali(ielem)=1
        enddo
        enddo
!
!  region 3
        ireg=3
!
!  on commence par la premiere couche dans la region 3
        do i=1,nptseg(3)-1
          ielem=ielem+1
          v(1,ielem)=nppol(1)*nprad(1)+nppol(2)*(nprad(2)-1)+i
          if(i.eq.nptseg(3)-1) then
            v(2,ielem)=nptseg(1)
            v(3,ielem)=nptseg(1)+nptseg(3)-2
          else
            v(2,ielem)=nptseg(1)+i
            v(3,ielem)=v(2,ielem)-1
          endif
          e(1,ielem)=ielem+1
          e(2,ielem)=2*(nptseg(1)-1)+2*i
          e(3,ielem)=ielem-1
          if(i.eq.1) e(3,ielem)=ielem+2*(nppol(ireg)-1)-1
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          if(i.eq.nptseg(3)-1) then
            v(2,ielem)=nppol(1)*nprad(1)+nppol(2)*(nprad(2)-1)+1
          else
            v(2,ielem)=v(1,ielem-1)+1
          endif
          e(1,ielem)=ielem+2*(nppol(ireg)-1)-1
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.nptseg(3)-1) e(2,ielem)=ielem-2*(nppol(ireg)-1)+1
          cotali(ielem)=1
        enddo
!
!  on fait maintenant les autres cellules de la region 2
        do j=2,nprad(ireg)-1
        do i=1,nppol(ireg)-1
          ielem=ielem+1
          v(1,ielem)=nppol(1)*nprad(1)+nppol(2)*(nprad(2)-1) &
     &      +(j-1)*(nppol(ireg)-1)+i
          if(i.eq.nppol(ireg)-1) then
            v(2,ielem)=v(1,ielem)-2*(nppol(ireg)-1)+1
            v(3,ielem)=v(1,ielem)-(nppol(ireg)-1)
          else
            v(2,ielem)=v(1,ielem)-(nppol(ireg)-1)+1
            v(3,ielem)=v(2,ielem)-1
          endif
          e(1,ielem)=ielem+1
          e(2,ielem)=ielem-2*(nppol(ireg)-1)+1
          e(3,ielem)=ielem-1
          if(i.eq.1) e(3,ielem)=ielem+2*(nppol(ireg)-1)-1
          cotali(ielem)=2
!
          ielem=ielem+1
          v(1,ielem)=v(1,ielem-1)
          v(3,ielem)=v(2,ielem-1)
          if(i.eq.nppol(ireg)-1) then
            v(2,ielem)=v(1,ielem-1)-(nppol(ireg)-2)
          else
            v(2,ielem)=v(1,ielem-1)+1
          endif
          if(j.eq.nprad(ireg)-1) then
            e(1,ielem)=iflx(3)
          else
            e(1,ielem)=ielem+2*(nppol(ireg)-1)-1
          endif
          e(2,ielem)=ielem+1
          e(3,ielem)=ielem-1
          if(i.eq.nptseg(3)-1) e(2,ielem)=ielem-2*(nppol(ireg)-1)+1
          cotali(ielem)=1
        enddo
        enddo

        nelem=ielem
!
!  on change la diagonale dans les coins droits
        t=2*(nppol(1)-1)*(nprad(1)-2)+1
        r=e(1,t)
        if(r.ne.t+1) then
          write(6,*)'mauvaise identification du 1-er coin droit'
          stop
        endif
!       call swapdi(t,r,v,e,cotali)

        t=2*(nppol(1)-1)*(nprad(1)-1)+2*(nppol(2)-1)*(nprad(2)-1)
        r=e(3,t)
        if(r.ne.t-1) then
          write(6,*)'mauvaise identification du 2-nd coin droit'
          stop
        endif
!       call swapdi(t,r,v,e,cotali)

!
!  on retire maintenant le point en trop de la liste
        itrop=nptseg(1)+nptseg(3)-1
        iremp=nptseg(1)
        call enlpnt(v,nelem,xmail,ymail,ncoord,itrop,iremp)
      else
        write(6,*)'cas non supporte dans traduit: nreg=',nreg
      endif
!
!  ecriture de la maille

      write(nfin,*)'$coord'
      write(nfin,*)'ncoord=',ncoord

      do i=1,ncoord
        write(nfin,102)i,xmail(i),ymail(i)
 102    format(i7,1p2e20.9)
      enddo
      write(nfin,*)'$fin'

      write(nfin,*)'$elements    nodes (1-3) and adjency (1-3)'
      write(nfin,*)'nelem=',nelem
      do ielem=1,nelem
        write(nfin,103)ielem,(v(j,ielem),j=1,3),(e(j,ielem),j=1,3)
 103    format(i5,2x,3i7,2x,3i8,2x,i3)
      enddo
      write(nfin,*)'$fin'

      write(nfin,*)'$indices of aligned sides'
      write(nfin,*)nelem
      do ielem=1,nelem
        write(nfin,103)ielem,cotali(ielem)
      enddo
      write(nfin,*)'$fin'

      return
      end
