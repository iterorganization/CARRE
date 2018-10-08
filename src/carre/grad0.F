
************************************************************************
      SUBROUTINE GRAD0(nxmax,nymax,nx,ny,x,y,gradmx,
     .          pointx,pointy,ii,jj,npxtot,a00,a10,a01,a11)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette sous-routine verifie pour chaque carre si il y a intersection
*  entre les lignes de niveau psidx=0 et psidy=0 et si oui elle garde
*  les coordonnees en memoire.

*  arguments
      INTEGER nxmax,nymax,nx,ny,gradmx,ii(gradmx),jj(gradmx),npxtot
      REAL(kind=R8) pointx(gradmx), pointy(gradmx),x(nx),y(ny)
     .  ,a00(nxmax,nymax,3),a10(nxmax,nymax,3),a01(nxmax,nymax,3)
     .  ,a11(nxmax,nymax,3)

*  variables locales
      INTEGER i, j
      REAL(kind=R8) aa,bb,cc,x1,x2,y1,y2

*  procedures
      INTRINSIC SQRT

c=========================
c.. npxtot: nombre de points ou le gradient est nul.
c.. pointx,pointy: tableaux des coordonnees en x et y des points ou le
c                  gradient est nul.
c.. ii,jj: tableaux des indices en x et en y des carres ou le gradient
c          est nul.
c.. i,j: indices
c.. ax,bx,cx,dx(i,j,2): coefficients de l'equation:
c                           ax + bx*X + cx*Y + dx*X*Y = psidx
c.. ay,by,cy,dy(i,j,3): coefficients de l'equation:
c                           ay + by*X + cy*Y + dy*X*Y = psidy
c.. aa,bb,cc: coefficients de l'equation des racines:
c             ( -bb +- SQRT(bb**2 - 4*aa*cc)) / 2*aa
c.. y1,y2: points ou la fonction s'annule selon y.
c.. x1,x2: points correspondants ou la fonction s'annule selon x.
c=========================

*..balayage en x et en y.

      npxtot = 0
      DO 10 j=1, ny-1
         DO 20 i=1, nx-1

*..recherche des racines.

            aa = (a01(i,j,3)*a11(i,j,2) - a11(i,j,3)*a01(i,j,2))
            bb = (a00(i,j,3)*a11(i,j,2) - a10(i,j,3)*a01(i,j,2)
     .           +a01(i,j,3)*a10(i,j,2) - a11(i,j,3)*a00(i,j,2))
            cc = (a00(i,j,3)*a10(i,j,2) - a10(i,j,3)*a00(i,j,2))

            IF (4.*aa*cc .LT. bb*bb) THEN

               IF (bb.GT.0. .AND. aa.NE.0.) THEN
                  y1 = - 2.*cc/(SQRT(bb*bb - 4.*aa*cc) + bb)
                  y2 = (-bb - SQRT(bb*bb - 4.*aa*cc))/(2.*aa)
               ELSE IF (aa.NE.0.) THEN
                  y1 = 2.*cc/(SQRT(bb*bb - 4.*aa*cc) - bb)
                  y2 = (-bb + SQRT(bb*bb - 4.*aa*cc))/(2.*aa)
               ELSE IF (bb.ne.0.) THEN
                  y1 = -cc/bb
               ELSE
                  y1 = 2.*y(1)-y(2)
               ENDIF

*..Test pour savoir si le carre contient une de ces racines.

               IF (y1.GE.y(j).AND.y1.LE.y(j+1)) THEN
                  if(a10(i,j,2)+a11(i,j,2)*y1.ne.0.) then
                    x1 = (-a00(i,j,2) - a01(i,j,2)*y1)/(a10(i,j,2)
     .                   + a11(i,j,2)*y1)
                  else
                    x1 = 2.*x(1)-x(2)
                  endif
                  IF (x1.GE.x(i).AND.x1.LE.x(i+1)) THEN

*..Sauvegarde de chaque point ou le gradient s'annule.

                     npxtot = npxtot + 1
                     pointx(npxtot) = x1
                     pointy(npxtot) = y1
                     ii(npxtot) = i
                     jj(npxtot) = j

                  ENDIF
               ENDIF

               IF (aa.NE.0.) THEN
                  IF (y2.GE.y(j).AND.y2.LE.y(j+1)) THEN
                     if(a10(i,j,2)+a11(i,j,2)*y2.ne.0.) then
                        x2 = (-a00(i,j,2)-(a01(i,j,2)*y2))/(a10(i,j,2)
     .                       +(a11(i,j,2)*y2))
                     else
                        x2 = 2.*x(1)-x(2)
                     endif
                     IF (x2.GE.x(i).AND.x2.LE.x(i+1)) THEN

                        npxtot = npxtot + 1
                        pointx(npxtot) = x2
                        pointy(npxtot) = y2
                        ii(npxtot) = i
                        jj(npxtot) = j

                     ENDIF
                  ENDIF
               ENDIF
            ENDIF

   20    CONTINUE
   10 CONTINUE

      RETURN
      END
