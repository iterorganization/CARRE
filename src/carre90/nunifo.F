
!***********************************************************************
      SUBROUTINE NUNIFO(npoint,l,d1,dn,interv,dmin,dmax)
!***********************************************************************
      IMPLICIT NONE

!..  Cette sous-routine repartit les points de facon non uniforme sur
!  une courbe en faisant passer une parabole par le premier et le
!  dernier intervalle donnee. Les intervalles intermediaires sont
!  determinees par la parabole.

!  arguments
      INTEGER npoint
      REAL*8 l,d1,dn,interv(npoint-1),dmin,dmax

!  variables locales
      INTEGER i,n
      REAL*8 denom,aa,bb,cc

!  procedures
      INTRINSIC MIN,MAX

!=========================
!.. npoint: nombre de points de la courbe.
!.. n  : nombre d'intervalles.
!.. l  : longueur en metres de la courbe, ou difference en psi entre le
!         premier et le dernier point.
!.. d1 : grandeur du premier intervalle
!.. dn : grandeur du dernier intervalle
!.. dmin: intervalle minimum.
!.. dmax: intervalle maximum.
!.. interv: tableau contenant les intervalles de chaque point.
!.. aa,bb,cc: coefficients de l'equation: di = aa + bb*i + cc*i*i
!.. denom: denominateur.
!=========================

!..Definition du nombre d'intervalles.

      n = npoint - 1

!..Definition des coefficients.

      if(n.eq.1) then
!       aa=0.5*(d1+dn)
        aa=l
        bb=0.
        cc=0.
      elseif(n.eq.2) then
!       aa=2.*d1-dn
!       bb=dn-d1
        bb=l/(3.*(1.-d1/dn)-2.*(1.-2.*d1/dn))
        aa=-(1.-2.*d1/dn)*bb
        bb=(1.-d1/dn)*bb
        cc=0.
      else
      denom = -((n-1)**2)*(n-2)/6.
        aa = (d1*(-n)*(n+1)*(n-1)/6. - dn*(n+1)*(n-1)/3. + l*(n-1)) & 
     &    /denom
        bb = (d1*(-(n+1)*(2*n+1)/6. + n*n) + dn*((n+1)*(2*n+1)/6. - 1) & 
     &        + l*(1-n*n)/n)/denom
        cc = (n-1)*(-d1/2. - dn/2. + l/n)/denom
      endif

!..Calcul de l'intervalle

      dmin = aa + bb + cc
      dmax = aa + bb + cc

      DO 10 i=1, n
         interv(i) = aa + bb*i + cc*i*i
         dmin = MIN(interv(i),dmin)
         dmax = MAX(interv(i),dmax)
   10 CONTINUE

      RETURN
      END
