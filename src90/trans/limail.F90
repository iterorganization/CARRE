!=======================================================================
      subroutine limail(nrid,nreg,nppol,nprad,r,z,psi,psidxm,psidym, &
     &  cflag,npomax,nramax,nregmx,carre_format)
      use KindDefinitions, only : rKind
      use carre_constants

      implicit none
!
!  lecture de la maille produite par le programme carre pour traitement
!  ulterieur
!
!  nrid: numero de l'unite de lecture
!  nreg: nombre de regions
!  nppol(i): nombre de points dans la direction poloidale pour la region i
!  nprad(i): ... radiale ...
!  r, z(ipol,irad,ireg): coordonnees
!  psi, psidxm, psidym: fonction psi, d(psi)/dx, et d(psi)/dy sur la
!                       maille
!  npomax: dimension declaree dans la direction poloidale
!  nramax: dimention declaree dans la direction radiale
!
!  arguments
      integer nrid,nreg,nppol(*),nprad(*),npomax,nramax,nregmx,&
           & cflag(npomax,nramax,nregmx,CARREOUT_NCELLFLAGS)
      real(rKind) :: r(npomax,nramax,*),z(npomax,nramax,*),psi(npomax,nramax,*), &
     &     psidxm(npomax,nramax,*),psidym(npomax,nramax,*)
      character*8 carre_format

!
!  variables locales
      integer ipol,irad,ireg,iflag
      character ligne*80
!
!  2.   read in the number of regions and mesh variables in each region
      iflag=-1
      call entete(nrid,'$maille',iflag)
      read(nrid,*,end=99)ligne,nreg
      do ireg=1,nreg
          read(nrid,*,end=99)ligne
          read(nrid,*,end=99)ligne,nppol(ireg),ligne,nprad(ireg)
          read(nrid,*,end=99)((r(ipol,irad,ireg),z(ipol,irad,ireg),&
               & psi(ipol,irad,ireg),psidxm(ipol,irad,ireg), &
               & psidym(ipol,irad,ireg),ipol=1,nppol(ireg)),irad=1,nprad(ireg))
          if(trim(carre_format).eq.'carre71') then
              read(nrid,*,end=99)ligne
              read(nrid,*,end=99) &
                   & ((cflag(ipol,irad,ireg,:), &
                   & ipol=1,nppol(ireg)-1),irad=1,nprad(ireg)-1)
          endif
      enddo
!
!  close input unit
      close(unit=nrid)
      return
!
99    continue
      write(6,*)'attention, erreur de lecture dans limail'
      nreg=-1
      return
      end
