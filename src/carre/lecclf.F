      SUBROUTINE LECCLF(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .              deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .              distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nm1,nm2,
     .              npx,racord,x,y,nx,ny)
c
c  version : 04.04.97 21:00
c
c======================================================================
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette sous-routine permet a l'utilisateur d'entrer tous les parame-
*  tres au clavier, en considerant chaque configuration.

*  arguments
      INTEGER nx,ny,npx,nptseg(*),npr(*),repart,nm1,nm2,limcfg
      LOGICAL racord
      REAL(kind=R8) x(nx),y(ny),xx,yy,distnv(5,*),lg(*),pntrat,
     .       deltp1(10),deltpn(10),deltr1(10),deltrn(10),fctini,difpsi,
     .       a00(nm1,nm2,3),a10(nm1,nm2,3),
     .       a01(nm1,nm2,3),a11(nm1,nm2,3),
     .       tgarde(4),xptxo,yptxo,distxo

*  variables locales
      INTEGER ii,jj
      REAL(kind=R8) fctfin,xfin,yfin

*  procedures
      INTEGER ifind
      INTRINSIC MIN
      EXTERNAL ifind
c======================================================================
c.. ii,jj: tableaux des indices en x et en y des carres ou le gradient
c          est nul.
c.. xfin,yfin: coordonnees du point le plus interieur en suivant la
c              ligne de penetration.
c.. fctfin: valeur de la fonction a ce point.
c======================================================================

*..1. Un seul point X.

      IF (npx.EQ.1 .and. limcfg.gt.0) THEN
c       Configuration limiteur

*..1.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         WRITE(6,105)lg(1)
  105    FORMAT(/'Configuration limiteur.'//
     .   'Nombre de points sur la separatrice:'/'longueur = ',F9.5)
c$   .   'Nombre de points sur la separatrice:'/'longueur = ',F9.5)
         READ(5,*)nptseg(1)

         WRITE(6,101)
         READ(5,*)deltp1(1)

         WRITE(6,102)
         READ(5,*)deltpn(1)

*..1.2  Determination de la facon de repartir les points radialement.

         WRITE(6,109)
         READ(5,*)repart

*..1.3  Determination du nombre de points dans la direction "radiale"

         WRITE(6,110)
         WRITE(6,111)distnv(1,1)
         WRITE(6,112)distnv(2,1)
         READ(5,*)npr(1)

         WRITE(6,113)
         READ(5,*)deltr1(1)

         WRITE(6,114)
         READ(5,*)deltrn(1)

         WRITE(6,122)distxo
  122    FORMAT(/'Nombre de points dans la region 2 (interieure) et'
     .     ,' penetration:'/'penetration maximale =',F11.8)
         READ(5,*)npr(2),pntrat

         pntrat = MIN(pntrat,distxo)

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + 
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

            WRITE(6,121)difpsi

         ENDIF

         WRITE(6,113)
         READ(5,*)deltr1(2)

         WRITE(6,114)
         READ(5,*)deltrn(2)

*..1.4  Determination de la longueur de garde pour chaque plaque.

         WRITE(6,123)
         READ(5,*)tgarde(1)

         WRITE(6,124)
         READ(5,*)tgarde(2)

      ELSEIF (npx.EQ.1 .and. limcfg.eq.0) THEN

*..1.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         WRITE(6,100)lg(1)
  100    FORMAT(/'Vous avez un simple nul'//
     .   'Nombre de points sur le segment qui va du point X a la',
     .   ' plaque de droite:'/'longueur = ',F8.5)
         READ(5,*)nptseg(1)

         WRITE(6,101)
  101    FORMAT(/'Distance entre les 2 premiers points:')
         READ(5,*)deltp1(1)

         WRITE(6,102)
  102    FORMAT(/'Distance entre les 2 derniers points:')
         READ(5,*)deltpn(1)

         WRITE(6,103)lg(2)
  103    FORMAT(/'Nombre de points sur le segment qui va a la plaque',
     .   ' de gauche:'/'longeur = ',F8.5)
         READ(5,*)nptseg(2)

         WRITE(6,101)
         WRITE(6,102)
         READ(5,*)deltp1(2)

         WRITE(6,102)
         READ(5,*)deltpn(2)

         WRITE(6,106)lg(3)
  106    FORMAT(/'Nombre de points sur la boucle:'/'longueur = ',F8.5)
         READ(5,*)nptseg(3)

         WRITE(6,101)
         READ(5,*)deltp1(3)

         WRITE(6,102)
         READ(5,*)deltpn(3)

*..1.2  Determination de la facon de repartir les points radialement.

         WRITE(6,109)
  109    FORMAT(/'Repartition des points sur les plaques:'/
     .     '1 pour une distribution selon le deplacement sur les',
     .     ' plaques'/
     .     '2 pour une distribution selon psi (recommande)')
         READ(5,*)repart

*..1.3  Determination du nombre de points dans la direction "radiale"

         WRITE(6,110)
  110    FORMAT(//'Nombre de points dans la direction radiale pour la ',
     .    'region 1 (SOL):')
         WRITE(6,111)distnv(1,1)
  111    FORMAT('Distance totale =',F9.6)
         WRITE(6,112)distnv(2,1)
  112    FORMAT('Difference totale de psi =',F9.5)
         READ(5,*)npr(1)

         WRITE(6,113)
  113    FORMAT(/'Distance ou difference entre les 2 premiers points:')
         READ(5,*)deltr1(1)

         WRITE(6,114)
  114    FORMAT(/'Distance ou difference entre les 2 derniers points:')
         READ(5,*)deltrn(1)

         WRITE(6,115)
  115    FORMAT(/'Nombre de points pour la region 2 (region privee):')
         WRITE(6,111)distnv(1,2)
         WRITE(6,112)distnv(2,2)
         WRITE(6,115)
         READ(5,*)npr(2)

         WRITE(6,113)
         READ(5,*)deltr1(2)

         WRITE(6,114)
         READ(5,*)deltrn(2)

         WRITE(6,120)distxo
  120    FORMAT(/'Nombre de points dans la region 3 (interieure) et'
     .     ,' penetration:'/'penetration maximale =',F11.8)
         READ(5,*)npr(3),pntrat

         pntrat = MIN(pntrat,distxo)

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + 
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

            WRITE(6,121)difpsi
  121       FORMAT(/'delta-psi =', F9.6)

         ENDIF

         WRITE(6,113)
         READ(5,*)deltr1(3)

         WRITE(6,114)
         READ(5,*)deltrn(3)

*..1.4  Determination de la longueur de garde pour chaque plaque.

         WRITE(6,123)
  123    FORMAT(/'Longueur de garde pour la plaque 1 ')
         READ(5,*)tgarde(1)

         WRITE(6,124)
  124    FORMAT(/'Longueur de garde pour la plaque 2 ')
         READ(5,*)tgarde(2)

  125    FORMAT(/'Longueur de garde pour la plaque 3 ')
  126    FORMAT(/'Longueur de garde pour la plaque 4 ')

*..2. Double nul connecte.

      ELSE IF ((npx .EQ. 2) .AND. (racord)) THEN

*..2.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         WRITE(6,130)lg(1)
  130    FORMAT(/'Vous avez un double nul connecte'//
     .   'Nombre de points sur le segment qui va du point X superieur',
     .   ' a la plaque en haut a droite:'/'longueur = ',F8.5)
         READ(5,*)nptseg(1)

         WRITE(6,101)
         READ(5,*)deltp1(1)

         WRITE(6,102)
         READ(5,*)deltpn(1)

         WRITE(6,131)lg(2)
  131    FORMAT(/'Vous avez un double nul connecte'//
     .   'Nombre de points sur le segment qui va du point X superieur',
     .   ' a la plaque en haut a gauche:'/'longueur = ',F8.5)
         READ(5,*)nptseg(2)

         WRITE(6,101)
         READ(5,*)deltp1(2)

         WRITE(6,102)
         READ(5,*)deltpn(2)

         WRITE(6,133)lg(3)
  133    FORMAT(/'Nombre de points sur le segment qui relie les points',
     .   ' X par la droite'/'longueur = ',F8.5)
         READ(5,*)nptseg(3)

         WRITE(6,101)
         READ(5,*)deltp1(3)

         WRITE(6,102)
         READ(5,*)deltpn(3)

         WRITE(6,136)lg(4)
  136    FORMAT(/'Nombre de points sur la boucle qui relie les points X'
     .   ,' par la gauche'/'longueur = ',F8.5)
         READ(5,*)nptseg(4)

         WRITE(6,101)
         READ(5,*)deltp1(4)

         WRITE(6,102)
         READ(5,*)deltpn(4)

         WRITE(6,140)lg(5)
  140    FORMAT(/'Nombre de points sur le segment qui va du',
     .   ' point X inferieur a la plaque en bas a droite:'/
     .   'longueur = ',F8.5)
         READ(5,*)nptseg(5)

         WRITE(6,101)
         READ(5,*)deltp1(5)

         WRITE(6,102)
         READ(5,*)deltpn(5)

         WRITE(6,144)lg(6)
  144    FORMAT(/'Nombre de points sur le segment qui va du',
     .   ' point X inferieur a la plaque en bas a gauche:'/
     .   'longueur = ',F8.5)
         READ(5,*)nptseg(6)

         WRITE(6,101)
         READ(5,*)deltp1(6)

         WRITE(6,102)
         READ(5,*)deltpn(6)

*..2.2  Determination de la facon de repartir les points radialement.

         WRITE(6,109)
         READ(5,*)repart

*..2.3  Determination du nombre de points dans la direction "radiale"

         WRITE(6,150)
  150    FORMAT(//'Nombre de points dans la direction radiale pour la ',
     .    'region 1 (SOL de droite):')
         WRITE(6,111)distnv(1,1)
         WRITE(6,112)distnv(2,1)
         READ(5,*)npr(1)

         WRITE(6,113)
         READ(5,*)deltr1(1)

         WRITE(6,114)
         READ(5,*)deltrn(1)

         WRITE(6,154)
  154    FORMAT(/'Pour la region 2 (region privee du haut):')
         WRITE(6,111)distnv(1,2)
         WRITE(6,112)distnv(2,2)
         READ(5,*)npr(2)

         WRITE(6,113)
         READ(5,*)deltr1(2)

         WRITE(6,114)
         READ(5,*)deltrn(2)

         WRITE(6,158)
  158    FORMAT(/'Pour la region 3 (SOL de gauche):')
         WRITE(6,111)distnv(1,3)
         WRITE(6,112)distnv(2,3)
         READ(5,*)npr(3)

         WRITE(6,113)
         READ(5,*)deltr1(3)

         WRITE(6,114)
         READ(5,*)deltrn(3)

         WRITE(6,162)
  162    FORMAT(/'Pour la region 4 (region privee du bas):')
         WRITE(6,111)distnv(1,4)
         WRITE(6,112)distnv(2,4)
         READ(5,*)npr(4)

         WRITE(6,113)
         READ(5,*)deltr1(4)

         WRITE(6,114)
         READ(5,*)deltrn(4)

         WRITE(6,164)distxo
  164    FORMAT(/'Nombre de points dans la region 5 (interieure) et'
     .     ,' penetration:'/'penetration totale =',F11.8)
         READ(5,*)npr(5),pntrat

         pntrat = MIN(pntrat,distxo)

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + 
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

            WRITE(6,121)difpsi

         ENDIF

         WRITE(6,113)
         READ(5,*)deltr1(5)

         WRITE(6,114)
         READ(5,*)deltrn(5)

*..2.4  Determination de la longueur de garde pour chaque plaque.

         WRITE(6,123)
         READ(5,*)tgarde(1)

         WRITE(6,124)
         READ(5,*)tgarde(2)

         WRITE(6,125)
         READ(5,*)tgarde(3)

         WRITE(6,126)
         READ(5,*)tgarde(4)

*..3. Double nul deconnecte.

      ELSE IF ((npx .EQ. 2) .AND. .NOT.(racord)) THEN

*..3.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         WRITE(6,170)lg(1)
  170    FORMAT(/'Vous avez un double nul deconnecte'//
     .   'Nombre de points sur le segment qui va du point X du haut'/
     .   'a la plaque droite du haut:'/'longueur = ',F8.5)
         READ(5,*)nptseg(1)

         WRITE(6,101)
         READ(5,*)deltp1(1)

         WRITE(6,102)
         READ(5,*)deltpn(1)

         WRITE(6,103)lg(2)
         READ(5,*)nptseg(2)

         WRITE(6,101)
         READ(5,*)deltp1(2)

         WRITE(6,102)
         READ(5,*)deltpn(2)

         WRITE(6,140)lg(5)
         READ(5,*)nptseg(5)

         WRITE(6,101)
         READ(5,*)deltp1(5)

         WRITE(6,102)
         READ(5,*)deltpn(5)

         WRITE(6,144)lg(6)
         READ(5,*)nptseg(6)

         WRITE(6,101)
         READ(5,*)deltp1(6)

         WRITE(6,102)
         READ(5,*)deltpn(6)

         WRITE(6,173)
  173    FORMAT(/'Nombre de points sur la partie de la boucle de la',
     .    ' separatrice'/ ' interieure qui part de la droite.')
         READ(5,*)nptseg(3)

         WRITE(6,101)
         READ(5,*)deltp1(3)

         WRITE(6,102)
         READ(5,*)deltpn(3)

         WRITE(6,174)
  174    FORMAT(/'Nombre de points sur la partie de la boucle de la',
     .    ' separatrice'/ ' interieure qui part de la gauche.')
         READ(5,*)nptseg(4)

         WRITE(6,101)
         READ(5,*)deltp1(4)

         WRITE(6,102)
         READ(5,*)deltpn(4)

*..3.2  Determination de la facon de repartir les points radialement.

         WRITE(6,109)
         READ(5,*)repart

*..3.3  Determination du nombre de points dans la direction "radiale"

         WRITE(6,180)
  180    FORMAT(//'Nombre de points dans la direction radiale pour la '/
     .   'region 1 (entre les deux separatrices):')
         WRITE(6,111)distnv(1,1)
         WRITE(6,112)distnv(2,1)
         READ(5,*)npr(1)

         WRITE(6,113)
         READ(5,*)deltr1(1)

         WRITE(6,114)
         READ(5,*)deltrn(1)

         WRITE(6,182)
  182    FORMAT(/'Pour la region 2 (region de droite):')
         WRITE(6,111)distnv(1,2)
         WRITE(6,112)distnv(2,2)
         READ(5,*)npr(2)

         WRITE(6,113)
         READ(5,*)deltr1(2)

         WRITE(6,114)
         READ(5,*)deltrn(2)

         WRITE(6,184)
  184    FORMAT(/'Pour la region 3 (region privee du haut):')
         WRITE(6,111)distnv(1,3)
         WRITE(6,112)distnv(2,3)
         READ(5,*)npr(3)

         WRITE(6,113)
         READ(5,*)deltr1(3)

         WRITE(6,114)
         READ(5,*)deltrn(3)

         WRITE(6,186)
  186    FORMAT(/'Pour la region 4 (region de gauche):')
         WRITE(6,111)distnv(1,4)
         WRITE(6,112)distnv(2,4)
         READ(5,*)npr(4)

         WRITE(6,113)
         READ(5,*)deltr1(4)

         WRITE(6,114)
         READ(5,*)deltrn(4)

         WRITE(6,188)
  188    FORMAT(/'Pour la region 5 (region privee du bas):')
         WRITE(6,111)distnv(1,5)
         WRITE(6,112)distnv(2,5)
         READ(5,*)npr(5)

         WRITE(6,113)
         READ(5,*)deltr1(5)

         WRITE(6,114)
         READ(5,*)deltrn(5)

         WRITE(6,189)distxo
  189    FORMAT(/'Nombre de points dans la region 6 (interieure) et'
     .     ,' penetration:'/'penetration totale =',F11.8)
         READ(5,*)npr(6),pntrat

         pntrat = MIN(pntrat,distxo)

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + 
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

            WRITE(6,121)difpsi

         ENDIF

         WRITE(6,113)
         READ(5,*)deltr1(6)

         WRITE(6,114)
         READ(5,*)deltrn(6)

*..3.4  Determination de la longueur de garde pour chaque plaque.

         WRITE(6,123)
         READ(5,*)tgarde(1)

         WRITE(6,124)
         READ(5,*)tgarde(2)

         WRITE(6,125)
         READ(5,*)tgarde(3)

         WRITE(6,126)
         READ(5,*)tgarde(4)

      ENDIF

      RETURN
      END
