      SUBROUTINE SORTIE(nsep,nreg,nptseg,npr,np1,deltp1,deltpn,deltr1,
     .    deltrn,pntrat,tgarde,distxo,repart,xmail,ymail,
     .    nx,ny,x,y,a00,a10,a01,a11,ptx,pty,npx,racord,numero,
     .    fctpx)
c
c  version : 07.07.97 19:08
c
c======================================================================
      IMPLICIT NONE

*..  Cette sous-routine imprime les resultats dans un fichier de sortie:
*  maille.sor

cank-970707: dimensions from the file
*  dimensions
#include <CARREDIM.F>

*  arguments
      INTEGER nsep,nreg,repart,nptseg(nsep),npr(nreg),np1(nreg),
     .        nx,ny,npx,numero
      REAL*8 pntrat,deltp1(nsep),deltpn(nsep),deltr1(nreg),deltrn(nreg),
     .     xmail(npmamx,nrmamx,nreg),ymail(npmamx,nrmamx,nreg),
     .     tgarde(4),distxo,x(nxmax),y(nymax),
     .     a00(nxmax,nymax,3),a10(nxmax,nymax,3),
     .     a01(nxmax,nymax,3),a11(nxmax,nymax,3),
     .     ptx(npx+1),pty(npx+1),fctpx(npx+1)
      logical racord

*  variables en common
#include <COMRLX.F>
#include <COMLAN.F>

*  variables locales
      INTEGER i,j,isep,ireg,ii,jj
      REAL*8 xx,yy,psimai,psidx,psidy

*  procedures
      INTEGER ifind
      EXTERNAL ifind
c======================================================================
c.. i,j: indices
c.. isep: indice de separatrice.
c.. ireg: indice de region.
c.. ii,jj: indices d'identification en x et y du carre.
c.. numero: 1= impression des parametres pouvant servir a un essai
c           ulterieur. 2= impression des donnees de la maille.
c.. xx,yy: coordonnees du premier point d'une courbe de niveau.
c.. psimai: valeur de la fonction pour cette courbe de niveau.
c======================================================================
*..Creation du nouveau fichier.

      IF (numero .EQ. 1) THEN

*..Impression des points X et O.

        if(sellan(1:8).eq.'francais') then
          write(10,111)npx,(ptx(i),pty(i),fctpx(i),i=1,npx)
111       format(//' Nombre de points X:',i3/
     .      t5,'x',t25,'y',t45,'psi'/(t4,1pe15.8,t24,1pe15.8,
     .      t44,1pe15.8))
         elseif(sellan(1:7).eq.'english') then
          write(10,211)npx,(ptx(i),pty(i),fctpx(i),i=1,npx)
211       format(//' Number of X-points:',i3/
     .      t5,'x',t25,'y',t45,'psi'/(t4,1pe15.8,t24,1pe15.8,
     .      t44,1pe15.8))
         endif
         write(10,112)ptx(npx+1),pty(npx+1),fctpx(npx+1)
112      format(/' Point O:'/(t4,1pe15.8,t24,1pe15.8,t44,1pe15.8))
         if(npx.gt.1) then
            if(racord) then
              if(sellan(1:8).eq.'francais') then
                write(10,113)' Les points X sont raccordes.'
113             format(a//)
              elseif(sellan(1:7).eq.'english') then
                write(10,113)' X-points are connected.'
              endif
            else
              if(sellan(1:8).eq.'francais') then
                write(10,113)' Les points X ne sont pas raccordes.'
              elseif(sellan(1:7).eq.'english') then
                write(10,113)' X-points are not connected.'
              endif
            endif
         endif
c
*..Ecriture des parametres qui pouront servir au le prochain essai.
         if(sellan(1:8).eq.'francais') then
           write(10,*)'$parametres'
         elseif(sellan(1:7).eq.'english') then
           write(10,*)'$parameters'
         endif
         WRITE(10,100)repart
  100    FORMAT('repart =',I2)

         WRITE(10,1015)distxo
 1015    FORMAT('distxo =',F11.8)

         WRITE(10,101)pntrat
  101    FORMAT('pntrat =',F11.8)

         DO 10 isep=1, nsep
            WRITE(10,102)isep,nptseg(isep),isep,deltp1(isep),isep,
     .                deltpn(isep)
  102       FORMAT('nptseg(',I1,') =',I3/'deltp1(',I1,') =',F10.7/
     .          'deltpn(',I1,') =',F10.7)
   10    CONTINUE

         DO 20 ireg=1, nreg
            WRITE(10,103)ireg,npr(ireg),ireg,deltr1(ireg),ireg,
     .                deltrn(ireg)
  103       FORMAT('npr(',I1,') =',I3/'deltr1(',I1,') =',F10.7/
     .          'deltrn(',I1,') =',F10.7)
   20    CONTINUE

         DO 25 i=1, 2
            WRITE(10,104)i,tgarde(i)
  104       FORMAT('tgarde(',I1,') =',F8.5)
   25    CONTINUE

         IF (nsep .GT. 3) THEN
            DO 26 i=3, 4
               WRITE(10,104)i,tgarde(i)
   26       CONTINUE
         ENDIF
         write(10,115)'nrelax=',nrelax,'relax=',relax,'pasmin=',pasmin,
     .     'rlcept=',rlcept
 115     format(a,i5/(a,1pe11.4))

         if(sellan(1:8).eq.'francais') then
           WRITE(10,105)
  105      FORMAT('$fin')
         elseif(sellan(1:7).eq.'english') then
           write(10,205)
  205      format('$end')
         endif

*..Ecriture des points de mailles.

      ELSE IF (numero .EQ. 2) THEN
         write(10,114)
114      format(t2,'x(m)',t18,'y(m)',t34,'psi(SI)',t50,'dpsi/dx',t66,
     .     'dpsi/dy')
         write(10,*)'$maille'
         WRITE(10,106)nreg
  106    FORMAT('''nreg=''',I5)

         DO 30 ireg=1, nreg
            WRITE(10,107)ireg
  107       FORMAT('''region:''',I5)
            WRITE(10,108)np1(ireg),npr(ireg)
  108       FORMAT('''nppol=''',I5,'  ''nprad=''',I5)

            DO 32 j=1, npr(ireg)
            do 31 i=1,np1(ireg)
               xx = xmail(i,j,ireg)
               yy = ymail(i,j,ireg)
               ii = ifind(xx,x,nx,1)
               jj = ifind(yy,y,ny,1)
               psimai = a00(ii,jj,1)+a10(ii,jj,1)*xx+a01(ii,jj,1)*yy
     .                + a11(ii,jj,1)*xx*yy
               psidx = a00(ii,jj,2)+a10(ii,jj,2)*xx+a01(ii,jj,2)*yy
     .                + a11(ii,jj,2)*xx*yy
               psidy = a00(ii,jj,3)+a10(ii,jj,3)*xx+a01(ii,jj,3)*yy
     .                + a11(ii,jj,3)*xx*yy

               WRITE(10,109)xmail(i,j,ireg),ymail(i,j,ireg),psimai,
     .           psidx,psidy
c 109          FORMAT(1p5E16.9)   mis en commentaire le 13 juin 1994
  109          FORMAT(1p5E16.8)

   31       continue
   32       CONTINUE
   30    CONTINUE

         if(sellan(1:8).eq.'francais') then
           WRITE(10,110)
  110      FORMAT('$fin')
         elseif(sellan(1:7).eq.'english') then
           write(10,210)
  210      format('$end')
         endif

      ENDIF

      RETURN
      END
