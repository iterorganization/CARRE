      SUBROUTINE LECCLE(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .              deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .              distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nm1,nm2,
     .              npx,racord,x,y,nx,ny)
c
c  version : 05.04.97 16:31
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

      write(6,*)
      write(6,*) 'There is no valid configuration file ',
     ,                           '"carre.dat" in the current directory'
      write(6,*) 'You are therefore requested to input ',
     ,                              'the necessary parameters manually'
      write(6,*) 'Note that you will be given an opportunity ',
     ,                         'to edit the data you have to input now'
      write(6,*)
      write(6,*) 'Please read the prompts and give your answers'
      write(6,*)

*..1. Un seul point X.

      if (npx.eq.1 .and. limcfg.gt.0) then
c       Configuration limiteur

*..1.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         write(6,205)lg(1)
  205    format(/'Limiter configuration.'//
     .    'Number of nodes on the separatrix:'/'segment length = ',f8.5)
         READ(5,*)nptseg(1)

         write(6,201)
         READ(5,*)deltp1(1)

         write(6,202)
         READ(5,*)deltpn(1)

*..1.2  Determination de la facon de repartir les points radialement.

         write(6,209)
         READ(5,*)repart

*..1.3  Determination du nombre de points dans la direction "radiale"

         write(6,210)
         write(6,211)distnv(1,1)
         write(6,212)distnv(2,1)
         READ(5,*)npr(1)

         WRITE(6,213)
         READ(5,*)deltr1(1)

         WRITE(6,214)
         READ(5,*)deltrn(1)

         WRITE(6,222)distxo
  222    format(/'Number of nodes in region 2 (interior) and'
     .     ,' penetration:'/'maximum penetration =',f11.8)
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

         WRITE(6,213)
         READ(5,*)deltr1(2)

         WRITE(6,214)
         READ(5,*)deltrn(2)

*..1.4  Determination de la longueur de garde pour chaque plaque.

         write(6,223)
         READ(5,*)tgarde(1)

         WRITE(6,224)
         READ(5,*)tgarde(2)

      ELSEIF (npx.EQ.1 .and. limcfg.eq.0) THEN
c       simple nul

*..1.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         write(6,200)lg(1)
  200    format(/'Single null geometry'//
     .    'Number of nodes on the segment which connects the X-point',
     .    ' to the right plate:'/'segment length = ',f8.5)
         READ(5,*)nptseg(1)

         write(6,201)
  201    format(/'Spacing between first 2 nodes:')
         READ(5,*)deltp1(1)

         write(6,202)
  202    format(/'Spacing between last 2 nodes:')
         READ(5,*)deltpn(1)

         WRITE(6,203)lg(2)
  203    format(/'Number of nodes on the segment which connects the X',
     .     '-point to the left plate:'/'segment length = ',f8.5)
         READ(5,*)nptseg(2)

         WRITE(6,201)
         READ(5,*)deltp1(2)

         write(6,202)
         READ(5,*)deltpn(2)

         write(6,206)lg(3)
  206    format(/'Number of nodes on the loop:'/'length = ',f8.5)
         READ(5,*)nptseg(3)

         WRITE(6,201)
         READ(5,*)deltp1(3)

         WRITE(6,202)
         READ(5,*)deltpn(3)

*..1.2  Determination de la facon de repartir les points radialement.

         write(6,209)
  209    format(/'Type of "radial" distribution:'/
     .     '1 for distribution based on displacement along plates'/
     .     '2 for distribution based on psi (recommended)')
         READ(5,*)repart

*..1.3  Determination du nombre de points dans la direction "radiale"

         write(6,210)
  210    format(//'Number of nodes in the radial direction for',
     .     ' region 1 (SOL):')
         write(6,211)distnv(1,1)
  211    format('total distance =',f9.6)
         write(6,212)distnv(2,1)
  212    format('total variation in psi',f9.6)
         READ(5,*)npr(1)

         WRITE(6,213)
  213    format(/'Spacing between first 2 nodes')
         READ(5,*)deltr1(1)

         WRITE(6,214)
  214    format(/'Spacing between last 2 nodes')
         READ(5,*)deltrn(1)

         WRITE(6,211)distnv(1,2)
         WRITE(6,212)distnv(2,2)
         WRITE(6,215)
  215    format(/'Number of nodes in region 2 (private region):')
         write(6,211)distnv(1,2)
         WRITE(6,212)distnv(2,2)
         READ(5,*)npr(2)

         WRITE(6,213)
         READ(5,*)deltr1(2)

         WRITE(6,214)
         READ(5,*)deltrn(2)

         WRITE(6,220)distxo
  220    format(/'Number of nodes in region 3 (interior) and'
     .     ,' penetration:'/'maximum penetration =',f11.8)
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

         WRITE(6,213)
         READ(5,*)deltr1(3)

         WRITE(6,214)
         READ(5,*)deltrn(3)

*..1.4  Determination de la longueur de garde pour chaque plaque.

         write(6,223)
  223    format(/'Guard length for plate 1')
         READ(5,*)tgarde(1)

         WRITE(6,224)
  224    format(/'Guard length for plate 2')
         READ(5,*)tgarde(2)

  225    format(/'Guard length for plate 3')
  226    format(/'Guard length for plate 4')

*..2. Double nul connecte.

      ELSE IF ((npx .EQ. 2) .AND. (racord)) THEN

*..2.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         WRITE(6,230)lg(1)
  230    format(/'Connected double-null geometry'//
     .   'Number of nodes on segment connecting ',
     .   ' top X-point to upper right plate:'
     .   /'length = ',f8.5)
         READ(5,*)nptseg(1)

         WRITE(6,201)
         READ(5,*)deltp1(1)

         WRITE(6,202)
         READ(5,*)deltpn(1)

         WRITE(6,231)lg(2)
  231    format(/'Connected double-null geometry'//
     .   'Number of nodes on segment connecting ',
     .   ' top X-point to upper left plate:'
     .   /'length = ',f8.5)
         READ(5,*)nptseg(2)

         WRITE(6,201)
         READ(5,*)deltp1(2)

         WRITE(6,202)
         READ(5,*)deltpn(2)

         WRITE(6,233)lg(3)
  233    format(/'Number of nodes on segment which connects X-points',
     .   ' by the right'/'length of segment = ',f8.5)
         READ(5,*)nptseg(3)

         WRITE(6,201)
         READ(5,*)deltp1(3)

         WRITE(6,202)
         READ(5,*)deltpn(3)

         WRITE(6,236)lg(4)
  236    format(/'Number of nodes on segment which connects X points',
     .   ' by the left'/'length of segment = ',f8.5)
         READ(5,*)nptseg(4)

         WRITE(6,201)
         READ(5,*)deltp1(4)

         WRITE(6,202)
         READ(5,*)deltpn(4)

         WRITE(6,240)lg(5)
  240    format(/'Number of points on segment which connects bottom',
     .   ' X-point to lower right plate:'/'segment length = ',f8.5)
         READ(5,*)nptseg(5)

         WRITE(6,201)
         READ(5,*)deltp1(5)

         WRITE(6,202)
         READ(5,*)deltpn(5)

         WRITE(6,244)lg(6)
  244    format(/'Number of nodes on segment which connects bottom',
     .   ' X-point to lower left plate:'/'segment length = ',f8.5)
         READ(5,*)nptseg(6)

         WRITE(6,201)
         READ(5,*)deltp1(6)

         WRITE(6,202)
         READ(5,*)deltpn(6)

*..2.2  Determination de la facon de repartir les points radialement.

         WRITE(6,209)
         READ(5,*)repart

*..2.3  Determination du nombre de points dans la direction "radiale"

         WRITE(6,250)
  250    format(//'Number of nodes in the radial direction in region',
     .   ' 1 (right part of SOL):')
         WRITE(6,211)distnv(1,1)
         WRITE(6,212)distnv(2,1)
         READ(5,*)npr(1)

         WRITE(6,213)
         READ(5,*)deltr1(1)

         WRITE(6,214)
         READ(5,*)deltrn(1)

         WRITE(6,254)
  254    format(//'Number of nodes in the radial direction in region 2',
     .   ' (upper private region):')
         WRITE(6,211)distnv(1,2)
         WRITE(6,212)distnv(2,2)
         READ(5,*)npr(2)

         WRITE(6,213)
         READ(5,*)deltr1(2)

         WRITE(6,214)
         READ(5,*)deltrn(2)

         WRITE(6,258)
  258    format(//'Number of nodes in the radial direction in region',
     .   ' 3 (left part of SOL):')
         WRITE(6,211)distnv(1,3)
         WRITE(6,212)distnv(2,3)
         READ(5,*)npr(3)

         WRITE(6,213)
         READ(5,*)deltr1(3)

         WRITE(6,214)
         READ(5,*)deltrn(3)

         WRITE(6,262)
  262    format(//'Number of nodes in the radial direction in region 4',
     .   ' (lower private region):')
         WRITE(6,211)distnv(1,4)
         WRITE(6,212)distnv(2,4)
         READ(5,*)npr(4)

         WRITE(6,213)
         READ(5,*)deltr1(4)

         WRITE(6,214)
         READ(5,*)deltrn(4)

         WRITE(6,264)distxo
  264    format(/'Number of nodes in region 5 (interior) and'
     .     ,' penetration:'/'maximum penetration = ',f11.8)
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

         WRITE(6,213)
         READ(5,*)deltr1(5)

         WRITE(6,214)
         READ(5,*)deltrn(5)

*..2.4  Determination de la longueur de garde pour chaque plaque.

         WRITE(6,223)
         READ(5,*)tgarde(1)

         WRITE(6,224)
         READ(5,*)tgarde(2)

         WRITE(6,225)
         READ(5,*)tgarde(3)

         WRITE(6,226)
         READ(5,*)tgarde(4)

*..3. Double nul deconnecte.

      ELSE IF ((npx .EQ. 2) .AND. .NOT.(racord)) THEN

*..3.1  On determine le nombre de points sur chaque segment de separa-
*       trice.

         WRITE(6,270)lg(1)
  270    format(/'Disconnected double-null geometry'//
     .   'Number of nodes on segment connecting upper X-point to'/
     .   ' upper right plate:'/'segment length = ',f8.5)
         READ(5,*)nptseg(1)

         WRITE(6,201)
         READ(5,*)deltp1(1)

         WRITE(6,202)
         READ(5,*)deltpn(1)

         WRITE(6,203)lg(2)
         READ(5,*)nptseg(2)

         WRITE(6,201)
         READ(5,*)deltp1(2)

         WRITE(6,202)
         READ(5,*)deltpn(2)

         WRITE(6,240)lg(5)
         READ(5,*)nptseg(5)

         WRITE(6,201)
         READ(5,*)deltp1(5)

         WRITE(6,202)
         READ(5,*)deltpn(5)

         WRITE(6,244)lg(6)
         READ(5,*)nptseg(6)

         WRITE(6,201)
         READ(5,*)deltp1(6)

         WRITE(6,202)
         READ(5,*)deltpn(6)

         WRITE(6,273)
  273    format(/'Number of nodes on right side of inner separatrix.')
         READ(5,*)nptseg(3)

         WRITE(6,201)
         READ(5,*)deltp1(3)

         WRITE(6,202)
         READ(5,*)deltpn(3)

         WRITE(6,274)
  274    format(/'Number of nodes on left side of inner separatrix.')
         READ(5,*)nptseg(4)

         WRITE(6,201)
         READ(5,*)deltp1(4)

         WRITE(6,202)
         READ(5,*)deltpn(4)

*..3.2  Determination de la facon de repartir les points radialement.

         WRITE(6,209)
         READ(5,*)repart

*..3.3  Determination du nombre de points dans la direction "radiale"

         WRITE(6,280)
  280    format(//'Number of nodes in radial direction in region 1'/
     .   '(between separatrices):')
         WRITE(6,211)distnv(1,1)
         WRITE(6,212)distnv(2,1)
         READ(5,*)npr(1)

         WRITE(6,213)
         READ(5,*)deltr1(1)

         WRITE(6,214)
         READ(5,*)deltrn(1)

         WRITE(6,282)
  282    format(//'Number of nodes in radial direction in region 2'/
     .   '(right part of SOL):')
         WRITE(6,211)distnv(1,2)
         WRITE(6,212)distnv(2,2)
         READ(5,*)npr(2)

         WRITE(6,213)
         READ(5,*)deltr1(2)

         WRITE(6,214)
         READ(5,*)deltrn(2)

         WRITE(6,284)
  284    format(//'Number of nodes in radial direction in region 3'/
     .   '(upper private region):')
         WRITE(6,211)distnv(1,3)
         WRITE(6,212)distnv(2,3)
         READ(5,*)npr(3)

         WRITE(6,213)
         READ(5,*)deltr1(3)

         WRITE(6,214)
         READ(5,*)deltrn(3)

         WRITE(6,286)
  286    format(//'Number of nodes in radial direction in region 4'/
     .   '(left part of SOL):')
         WRITE(6,211)distnv(1,4)
         WRITE(6,212)distnv(2,4)
         READ(5,*)npr(4)

         WRITE(6,213)
         READ(5,*)deltr1(4)

         WRITE(6,214)
         READ(5,*)deltrn(4)

         WRITE(6,288)
  288    format(//'Number of nodes in radial direction in region 5'/
     .   '(lower private region):')
         WRITE(6,211)distnv(1,5)
         WRITE(6,212)distnv(2,5)
         READ(5,*)npr(5)

         WRITE(6,213)
         READ(5,*)deltr1(5)

         WRITE(6,214)
         READ(5,*)deltrn(5)

         WRITE(6,289)distxo
  289    format(/'Number of nodes in radial direction in region 6'
     .     ,' (interior) and penetration:'/'maximum penetration = '
     $        ,f11.8)
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

         WRITE(6,213)
         READ(5,*)deltr1(6)

         WRITE(6,214)
         READ(5,*)deltrn(6)

*..3.4  Determination de la longueur de garde pour chaque plaque.

         WRITE(6,223)
         READ(5,*)tgarde(1)

         WRITE(6,224)
         READ(5,*)tgarde(2)

         WRITE(6,225)
         READ(5,*)tgarde(3)

         WRITE(6,226)
         READ(5,*)tgarde(4)

      ENDIF

      RETURN
      END
