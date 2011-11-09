c=======================================================================
      subroutine limail(nrid,nreg,nppol,nprad,r,z,psi,psidxm,psidym,
     .  iflag1,iflag2,npomax,nramax,carre_format)
      implicit none
c
c  lecture de la maille produite par le programme carre pour traitement
c  ulterieur
c
c  nrid: numero de l'unite de lecture
c  nreg: nombre de regions
c  nppol(i): nombre de points dans la direction poloidale pour la region i
c  nprad(i): ... radiale ...
c  r, z(ipol,irad,ireg): coordonnees 
c  psi, psidxm, psidym: fonction psi, d(psi)/dx, et d(psi)/dy sur la
c                       maille
c  npomax: dimension declaree dans la direction poloidale
c  nramax: dimention declaree dans la direction radiale
c
c  arguments
      integer nrid,nreg,nppol(*),nprad(*),npomax,nramax,
     .     iflag1(npomax,nramax,*),iflag2(npomax,nramax,*)
      real*8 r(npomax,nramax,*),z(npomax,nramax,*),psi(npomax,nramax,*),
     .     psidxm(npomax,nramax,*),psidym(npomax,nramax,*)
      character*8 carre_format
c
c  variables locales
      integer ipol,irad,ireg,iflag
      character ligne*80
c
c  2.   read in the number of regions and mesh variables in each region
      iflag=-1
      call entete(nrid,'$maille',iflag)
      read(nrid,*,end=99)ligne,nreg
      do ireg=1,nreg
        read(nrid,*,end=99)ligne
        read(nrid,*,end=99)ligne,nppol(ireg),ligne,nprad(ireg)
        read(nrid,*,end=99)((r(ipol,irad,ireg),z(ipol,irad,ireg),
     .   psi(ipol,irad,ireg),psidxm(ipol,irad,ireg),
     .   psidym(ipol,irad,ireg),ipol=1,nppol(ireg)),irad=1,nprad(ireg))
        if(trim(carre_format).eq.'carre71')) then
          read(nrid,*,end=99)ligne
          read(nrid,*,end=99)
     .     ((iflag1(ipol,irad,ireg),iflag2(ipol,irad,ireg),
     .      ipol=1,nppol(ireg)-1),irad=1,nprad(ireg)-1)
        endif
      enddo
c
c  close input unit
      close(unit=nrid)
      return
c
99    continue
      write(6,*)'attention, erreur de lecture dans limail'
      nreg=-1
      return
      end
