
!***********************************************************************
      SUBROUTINE GRAD0(equ,nxmax,nymax,gradmx)
!***********************************************************************
      use KindDefinitions
      use carre_types
      IMPLICIT NONE

!..  Cette sous-routine verifie pour chaque carre si il y a intersection
!  entre les lignes de niveau psidx=0 et psidy=0 et si oui elle garde
!  les coordonnees en memoire.

!  arguments
      type(CarreEquilibrium), intent(inout) :: equ
      INTEGER nxmax,nymax,gradmx

!  variables locales
      INTEGER i, j
      REAL(rKind) :: aa,bb,cc,x1,x2,y1,y2

!  procedures
      INTRINSIC SQRT

!=========================
!.. npxtot: nombre de points ou le gradient est nul.
!.. pointx,pointy: tableaux des coordonnees en x et y des points ou le
!                  gradient est nul.
!.. ii,jj: tableaux des indices en x et en y des carres ou le gradient
!          est nul.
!.. i,j: indices
!.. ax,bx,cx,dx(i,j,2): coefficients de l'equation:
!                           ax + bx*X + cx*Y + dx*X*Y = psidx
!.. ay,by,cy,dy(i,j,3): coefficients de l'equation:
!                           ay + by*X + cy*Y + dy*X*Y = psidy
!.. aa,bb,cc: coefficients de l'equation des racines:
!             ( -bb +- SQRT(bb**2 - 4*aa*cc)) / 2*aa
!.. y1,y2: points ou la fonction s'annule selon y.
!.. x1,x2: points correspondants ou la fonction s'annule selon x.
!=========================

!..balayage en x et en y.

      equ%npxtot = 0
      DO 10 j=1, equ%ny-1
         DO 20 i=1, equ%nx-1

!..recherche des racines.

            aa = (equ%a01(i,j,3)*equ%a11(i,j,2) - equ%a11(i,j,3)*equ%a01(i,j,2))
            bb = (equ%a00(i,j,3)*equ%a11(i,j,2) - equ%a10(i,j,3)*equ%a01(i,j,2) & 
     &           +equ%a01(i,j,3)*equ%a10(i,j,2) - equ%a11(i,j,3)*equ%a00(i,j,2))
            cc = (equ%a00(i,j,3)*equ%a10(i,j,2) - equ%a10(i,j,3)*equ%a00(i,j,2))

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
                  y1 = 2.*equ%y(1)-equ%y(2)
               ENDIF

!..Test pour savoir si le carre contient une de ces racines.

               IF (y1.GE.equ%y(j).AND.y1.LE.equ%y(j+1)) THEN
                  if(equ%a10(i,j,2)+equ%a11(i,j,2)*y1.ne.0.) then
                    x1 = (-equ%a00(i,j,2) - equ%a01(i,j,2)*y1)/(equ%a10(i,j,2) & 
     &                   + equ%a11(i,j,2)*y1)
                  else
                    x1 = 2.*equ%x(1)-equ%x(2)
                  endif
                  IF (x1.GE.equ%x(i).AND.x1.LE.equ%x(i+1)) THEN

!..Sauvegarde de chaque point ou le gradient s'annule.

                     equ%npxtot = equ%npxtot + 1
                     equ%pointx(equ%npxtot) = x1
                     equ%pointy(equ%npxtot) = y1
                     equ%ii(equ%npxtot) = i
                     equ%jj(equ%npxtot) = j

                  ENDIF
               ENDIF

               if (aa.NE.0.) then
                  IF (y2.GE.equ%y(j).AND.y2.LE.equ%y(j+1)) THEN
                     if(equ%a10(i,j,2)+equ%a11(i,j,2)*y2.ne.0.) then
                        x2 = (-equ%a00(i,j,2)-(equ%a01(i,j,2)*y2))/(equ%a10(i,j,2) & 
                             &                   +(equ%a11(i,j,2)*y2))
                     else
                        x2 = 2.*equ%x(1)-equ%x(2)
                     endif
                     IF (x2.GE.equ%x(i).AND.x2.LE.equ%x(i+1)) THEN

                        equ%npxtot = equ%npxtot + 1
                        equ%pointx(equ%npxtot) = x2
                        equ%pointy(equ%npxtot) = y2
                        equ%ii(equ%npxtot) = i
                        equ%jj(equ%npxtot) = j

                     ENDIF
                  ENDIF
               end if
            ENDIF

   20    CONTINUE
   10 CONTINUE

      RETURN
      END
