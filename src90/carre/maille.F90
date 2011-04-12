      SUBROUTINE MAILLE(nx,ny,x,y,psi,npx,xpto,ypto,racord, & 
     &     separx,separy,ptsep,nptot,distnv,ptxint,nstruc,npstru, & 
     &     xstruc,ystruc,inddef,nreg,xn,yn,xmail,ymail, & 
     &     np1,ptx,pty,nivx,nivy,nivtot,nbniv, & 
     &     a00,a10,a01,a11,fctpx,limcfg,diag,par)
  !
!  version : 23.06.98 19:53
!
!======================================================================
!ank -- The comments are translated from French, sorry for errors!

!*** This sub-routine makes the orthogonal curvilinear grid for
!*** various possible magnetic configurations (single null,
!*** connected double null, disconnected double null, or limiter)
!======================================================================
    
      use CarreSiloIO
      use CarreDiagnostics
      use carre_io
  
      IMPLICIT NONE
  
      !ank-970707: dimensions from the file
      !  dimensions
#include <CARREDIM.F>

      !  arguments
      INTEGER nx,ny,npx,ptsep(4,npx),nptot(4,npx),ptxint, & 
     &        nstruc,npstru(nstruc),nreg,inddef(4), & 
     &        np1(nregmx),nivtot(nivmx),nbniv,limcfg
      LOGICAL racord
      REAL*8 x(nxmax),y(nymax),psi(nxmax,nymax), & 
     &  xpto,ypto,separx(npnimx,4,npx), & 
     &  separy(npnimx,4,npx),xstruc(npstmx,nstruc),pty(npx+1), & 
     &  ystruc(npstmx,nstruc),xn(npnimx),yn(npnimx),ptx(npx+1), & 
     &  xmail(npmamx,nrmamx,*),ymail(npmamx,nrmamx,*),distnv(5,5), & 
     &  nivx(npnimx,nivmx),nivy(npnimx,nivmx),fctpx(npx+1), & 
     &  a00(nxmax,nymax,3),a10(nxmax,nymax,3), & 
     &  a01(nxmax,nymax,3),a11(nxmax,nymax,3)
      type(CarreDiag), intent(inout) :: diag
      type(CarreParameters), intent(inout) :: par

      !  variables en common
#include <COMLAN.F>
#include <COMRLX.F>

      !  variables locales
      INTEGER isep,ipas,ireg,ipx,sens,nn,idef, & 
     &  nsep,ii,jj,ient,isor,ifail,nbcrb,npcrb2 & 
     &  ,ptxext,i,nbcl(2),nnlast,npr1,nmail,imail
      REAL*8 dist,x2,y2,lg(10), & 
     &  xx,yy,fctini,fctfin,difpsi,dpmin(10), & 
     &  dpmax(10),drmin(10),drmax(10),xfin,yfin,gardd1, & 
     &  gardd2,xptxo,yptxo,distxo,xint,yint,xext,yext,psiint, & 
     &  psiext,xptxex,yptxex,bouclx,boucly,ll,pntrat_old
      REAL*8  sepmax(npnimx,8),sepmay(npnimx,8),spacep(npmamx,10), & 
     &  spacer(npmamx,10),pas(nrmamx),xcrb2(npnimx),ycrb2(npnimx) & 
     &  ,xbcl(npnimx,2),ybcl(npnimx,2),lbcl,xnlast(npnimx), & 
     &  ynlast(npnimx)
      CHARACTER*3 rep
      LOGICAL nuldec, correct
      integer :: nn1

      !  procedures
      INTEGER drctio,horair,ifind
      REAL*8 long,plqdst,ruban
      EXTERNAL long,COORD,drctio,horair,ifind,LECCLE,lecclf,DOUBLD, & 
     &         ruban,plqdst,trace3 & 
     &        ,trc_stk_in,trc_stk_out
      intrinsic max


!=========================
!.. npnimx: <=> npnimx
!.. nreg  : number of regions
!.. xmail,ymail: coordinates of the grid points
!                (poloidal index, radial index, region index)
!.. xn,yn : working arrays for coordinates along a parametrised curve
!.. nn    : number of points on the same curve
!.. np1   : number of points in poloidal direction
!.. ireg  : region index
!.. ipx   : X-point index
!.. ptxext: index of the outer X-point
!.. sens  : direction of movement along a target:
!           1=the same as the target points, 2=the opposite
!.. idef  : index of the target from where the routine starts
!.. a00,a10,a01,a11: coefficients.
!.. nsep  : number of the separatrices per the configuration
!.. nbcrb : number of boundary lines
!.. ii,jj : cell identification indices
!.. npmamx: maximun number of intervals
!.. ient,isor: input and output unit numbers
!.. ifail : Check whether the data should be read from the keyboard
!           because the file is incomplete
!           (0=file is acceptable, 1=unacceptable)
!.. x2,y2 : starting point for routine marche
!.. lg    : length of each separatrix
!.. dpmin,dpmax: values of the minimal and maximal interval widths
!..
!.. xnlast,ynlast: dans le cas du double nul decon., derniere courbe de
!                  niveau lorsqu'on maille la region entre les deux
!                  separatrices.
!.. nnlast: number of points on this curve
!.. nuldec: variable logique qui nous dit si oui ou non on est dans la
!           region entre les 2 separatrice du double nul deconnecte.
!======================================================================
!
!..calculs
!c<<<
!      write(0,*) '===> Entering maille.  npx, limcfg = ',npx,limcfg
!c>>>

      nuldec = .FALSE.

!..On procede au cas par cas selon la configuration des separatrices.

!----------------------------------------------------------------------

      IF (npx.EQ.1 .and. limcfg.eq.0) THEN

!----------------------------------------------------------------------
!..1.   Single null

         nbcrb = 1
         nreg = 3
         nsep = 3
         ipx = 1

!..The distance between the X-point and the O-point

         xx = separx(1,ptsep(3,ipx),ipx)
         yy = separy(1,ptsep(3,ipx),ipx)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

!..Psi value at the X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy + & 
     &            a11(ii,jj,1)*xx*yy

!..Calculate the length of each separatrix

         DO 1 isep = 1, 3

            lg(isep) = long(separx(1,ptsep(isep,ipx),ipx), & 
     &                 separy(1,ptsep(isep,ipx),ipx), & 
     &                 nptot(ptsep(isep,ipx),ipx))

    1    CONTINUE

!.. Read initial set of code parameters
         call read_code_parameters(par, distxo)

         correct = .false.
         do while (correct)

             !..Calculate the psi difference between the penetration values

             IF (par%repart .EQ. 2) THEN

                 xfin = xx + xptxo*par%pntrat
                 yfin = yy + yptxo*par%pntrat

                 ii = ifind(xfin,x,nx,1)
                 jj = ifind(yfin,y,ny,1)

                 fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + & 
                     &               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

                 difpsi = fctfin - fctini

             ENDIF

             !..Calculate the intervals, dmin and dmax

             !..Along the separatrices

             CALL NUNIFO(par%nptseg(1),lg(1),par%deltp1(1),par%deltpn(1),spacep(1,1), & 
                 &               dpmin(1),dpmax(1))

             CALL NUNIFO(par%nptseg(2),lg(2),par%deltp1(2),par%deltpn(2),spacep(1,2), & 
                 &               dpmin(2),dpmax(2))

             CALL NUNIFO(par%nptseg(3),lg(3),par%deltp1(3),par%deltpn(3),spacep(1,3), & 
                 &               dpmin(3),dpmax(3))

             !..Radial direction

             CALL NUNIFO(par%npr(1),distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                 &               spacer(1,1),drmin(1),drmax(1))

             CALL NUNIFO(par%npr(2),distnv(par%repart,2),par%deltr1(2),par%deltrn(2), & 
                 &               spacer(1,2),drmin(2),drmax(2))

             IF (par%repart .EQ. 1) THEN

                 CALL NUNIFO(par%npr(3),par%pntrat,par%deltr1(3),par%deltrn(3), & 
                     &                  spacer(1,3),drmin(3),drmax(3))

             ELSE IF (par%repart .EQ. 2) THEN

                 CALL NUNIFO(par%npr(3),difpsi,par%deltr1(3),par%deltrn(3), & 
                     &                  spacer(1,3),drmin(3),drmax(3))

             ENDIF

             !.. Check & modify code parameters
             call check_and_modify_code_parameters(par, correct)
         end do

!..1.2  Distribute the points along separatrices

         DO 6 isep=1, 3

           sepmax(1,isep) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 5 ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx), & 
     &          separy(1,ptsep(isep,ipx),ipx),nptot(ptsep(isep,ipx),ipx) & 
     &          ,dist,sepmax(ipas,isep),sepmay(ipas,isep))
    5      CONTINUE

           sepmax(par%nptseg(isep),isep)=separx(nptot(ptsep(isep,ipx),ipx), & 
     &       ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep),isep)=separy(nptot(ptsep(isep,ipx),ipx), & 
     &       ptsep(isep,ipx),ipx)
    6    CONTINUE

!..1.3  Grid region by region

!..1.3.1  Region 1

!.. Define the primary curve and the grid points

         call csioSetRegion( 1 )

         ireg=1
         np1(ireg) = 0

         DO 10 ipas=par%nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   10    CONTINUE

         DO 11 ipas=2, par%nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   11    CONTINUE

         DO 12 ipas=2, par%nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   12    CONTINUE


!..Go along the target 1

         idef = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 13 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
   13    CONTINUE

         DO 14 ipas=2,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
   14    CONTINUE

         DO 15 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
   15    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*17')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 17 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   17    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
     &              np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
     &              x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
     &              a00,a10,a01,a11,par%repart, & 
     &              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
     &              ynlast,nnlast,nuldec,.true.,diag,ireg)

!
!  1.3.2  region 2

!..Define the primary curve and the grid points

         call csioSetRegion( 2 )

         ireg=2
         np1(ireg) = 0

         DO 20 ipas=par%nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   20    CONTINUE

         DO 21 ipas=2, par%nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   21    CONTINUE

!..Go along the target 1

         idef = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 22 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
   22    CONTINUE

         DO 23 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
   23    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*25 ')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 25 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   25    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &              np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &              x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &              a00,a10,a01,a11,par%repart, & 
              &              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &              ynlast,nnlast,nuldec,.false.,diag,ireg)
!
!  1.3.3  region 3

!..Define the primary curve and the grid points

         call csioSetRegion( 3 )

         ireg=3
         np1(ireg) = 0

         DO 30 ipas=1, par%nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   30    CONTINUE

         x2 = separx(1,ptsep(3,ipx),ipx)
         y2 = separy(1,ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 31 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
   31    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         DO 32 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   32    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,par%pntrat, & 
     &       pas,np1(ireg),npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &       nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc, & 
     &       a00,a10,a01,a11,par%repart, & 
     &       xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,diag,ireg)
!----------------------------------------------------------------------

      ELSE IF ((npx.EQ.2) .AND. (racord)) THEN

!..2.   Connected double null

!----------------------------------------------------------------------
         nbcrb = 1
         nreg = 5
         nsep = 6

!..The distance between the top X-point and the O-point

         ipx = 1

         xx = separx(1,ptsep(3,ipx),ipx)
         yy = separy(1,ptsep(3,ipx),ipx)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

!..Calculate the psi value at the X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy + & 
     &            a11(ii,jj,1)*xx*yy

!..Calculate the length of each separatrix

         ipx = 1
         DO 38 isep = 1, 4
            lg(isep) = long(separx(1,ptsep(isep,ipx),ipx), & 
     &                 separy(1,ptsep(isep,ipx),ipx), & 
     &                 nptot(ptsep(isep,ipx),ipx))
   38    CONTINUE

         ipx = 2
         DO 39 isep = 1, 2
            lg(isep+4) = long(separx(1,ptsep(isep,ipx),ipx), & 
     &                   separy(1,ptsep(isep,ipx),ipx), & 
     &                   nptot(ptsep(isep,ipx),ipx))
   39    CONTINUE

!..2.1  Read all the necessary data from the file

         call read_code_parameters(par, distxo)

         correct = .false.
         do while (correct)

             !..Calculate the psi difference between the penetration values

             IF (par%repart .EQ. 2) THEN

                 xfin = xx + xptxo*par%pntrat
                 yfin = yy + yptxo*par%pntrat

                 ii = ifind(xfin,x,nx,1)
                 jj = ifind(yfin,y,ny,1)

                 fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + & 
                     &               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

                 difpsi = fctfin - fctini

             ENDIF

             !..Calculate the intervals, dmin and dmax

             !..Along the separatrices

             CALL NUNIFO(par%nptseg(1),lg(1),par%deltp1(1),par%deltpn(1),spacep(1,1), & 
                 &               dpmin(1),dpmax(1))

             CALL NUNIFO(par%nptseg(2),lg(2),par%deltp1(2),par%deltpn(2),spacep(1,2), & 
                 &               dpmin(2),dpmax(2))

             CALL NUNIFO(par%nptseg(3),lg(3),par%deltp1(3),par%deltpn(3),spacep(1,3), & 
                 &               dpmin(3),dpmax(3))

             CALL NUNIFO(par%nptseg(4),lg(4),par%deltp1(4),par%deltpn(4),spacep(1,4), & 
                 &               dpmin(4),dpmax(4))

             CALL NUNIFO(par%nptseg(5),lg(5),par%deltp1(5),par%deltpn(5),spacep(1,5), & 
                 &               dpmin(5),dpmax(5))

             CALL NUNIFO(par%nptseg(6),lg(6),par%deltp1(6),par%deltpn(6),spacep(1,6), & 
                 &               dpmin(6),dpmax(6))

             !..Radial direction

             CALL NUNIFO(npr(1),distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                 &               spacer(1,1),drmin(1),drmax(1))

             CALL NUNIFO(npr(2),distnv(par%repart,2),par%deltr1(2),par%deltrn(2), & 
                 &               spacer(1,2),drmin(2),drmax(2))

             CALL NUNIFO(npr(3),distnv(par%repart,3),par%deltr1(3),par%deltrn(3), & 
                 &               spacer(1,3),drmin(3),drmax(3))

             CALL NUNIFO(npr(4),distnv(par%repart,4),par%deltr1(4),par%deltrn(4), & 
                 &               spacer(1,4),drmin(4),drmax(4))

             IF (par%repart .EQ. 1) THEN

                 CALL NUNIFO(npr(5),par%pntrat,par%deltr1(5),par%deltrn(5), & 
                     &                  spacer(1,5),drmin(5),drmax(5))

             ELSE IF (par%repart .EQ. 2) THEN

                 CALL NUNIFO(npr(5),difpsi,par%deltr1(5),par%deltrn(5), & 
                     &                  spacer(1,5),drmin(5),drmax(5))

             ENDIF

             !.. Check & modify code parameters
             call check_and_modify_code_parameters(par, correct)
         end do

!..2.2  Distribute the points along separatrices

         ipx = 1
         DO 44 isep=1, 4

           sepmax(1,isep) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 45 ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx), & 
     &                   separy(1,ptsep(isep,ipx),ipx), & 
     &                   nptot(ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep),sepmay(ipas,isep))
   45      CONTINUE
           sepmax(par%nptseg(isep),isep)=separx(nptot(ptsep(isep,ipx),ipx), & 
     &       ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep),isep)=separy(nptot(ptsep(isep,ipx),ipx), & 
     &       ptsep(isep,ipx),ipx)
   44    CONTINUE

         ipx = 2
         DO 46 isep=1, 2

           sepmax(1,isep+4) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep+4) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 47 ipas=2, par%nptseg(isep+4)-1
              dist=dist + spacep(ipas-1,isep+4)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx), & 
     &                   separy(1,ptsep(isep,ipx),ipx), & 
     &                   nptot(ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep+4),sepmay(ipas,isep+4))

   47      CONTINUE
           sepmax(par%nptseg(isep+4),isep+4)=separx(nptot(ptsep(isep,ipx), & 
     &       ipx),ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep+4),isep+4)=separy(nptot(ptsep(isep,ipx), & 
     &       ipx),ptsep(isep,ipx),ipx)
   46    CONTINUE

!..2.3. Grid region by region

!..2.3.1 Region 1: right

!..Define the primary curve and the grid points

         ireg=1
         np1(ireg) = 0

         DO 50 ipas=par%nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   50    CONTINUE

         DO 51 ipas=2, par%nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   51    CONTINUE

         DO 52 ipas=2, par%nptseg(5)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
   52    CONTINUE


!..Go along the target 1

         idef = 1
         ipx = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 53 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
   53    CONTINUE

         DO 54 ipas=2,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
   54    CONTINUE

         ipx = 2

         DO 55 ipas=2,nptot(ptsep(1,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
   55    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(3)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*57')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 57 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   57    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
     &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
     &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
     &               a00,a10,a01,a11,par%repart, & 
     &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
     &               ynlast,nnlast,nuldec,.true.,diag,ireg)

!
!..2.3.2 Region 2: top PFR

!..Define the primary curve and the grid points

         ireg=2
         np1(ireg) = 0

         DO 60 ipas=par%nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   60    CONTINUE

         DO 61 ipas=2, par%nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   61    CONTINUE

!..Go along the target 1

         idef = 1
         ipx = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 63 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
   63    CONTINUE

         DO 64 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
   64    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*67')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 67 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   67    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.false.,diag,ireg)

!
!..2.3.3 Region 3: left

!..Define the primary curve and the grid points

         ireg=3
         np1(ireg) = 0

         DO 70 ipas=par%nptseg(2), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   70    CONTINUE

         DO 71 ipas=2, par%nptseg(4)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
   71    CONTINUE

         DO 72 ipas=2, par%nptseg(6)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
   72    CONTINUE

!..Go along the target 2

         idef = 2
         ipx = 1

         x2 = separx(nptot(ptsep(2,ipx),ipx),ptsep(2,ipx),ipx)
         y2 = separy(nptot(ptsep(2,ipx),ipx),ptsep(2,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 73 ipas=nptot(ptsep(2,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
   73    CONTINUE

         DO 74 ipas=2,nptot(ptsep(4,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(4,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(4,ipx),ipx)
   74    CONTINUE

         ipx = 2

         DO 75 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
   75    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(4)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*77')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 77 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   77    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.true.,diag,ireg)


!..2.3.4 Region 4: bottom PFR

!..Define the primary curve and the grid points

         ireg=4
         np1(ireg) = 0

         DO 80 ipas=par%nptseg(5), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
   80    CONTINUE

         DO 81 ipas=2, par%nptseg(6)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
   81    CONTINUE

!..Go along the target 3

         idef = 3
         ipx = 2

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 2

         DO 83 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
   83    CONTINUE

         DO 84 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
   84    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(4)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*87')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 87 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   87    CONTINUE


!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.false.,diag,ireg)

!
!..2.3.5  Region 5: central region

!..Define the primary curve and the grid points

         ireg=5
         np1(ireg) = 0
         ipx = 1

         DO 90 ipas=1, par%nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   90    CONTINUE

         DO 91 ipas=par%nptseg(4)-1,1,-1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
   91    CONTINUE

         x2 = separx(1,ptsep(3,ipx),ipx)
         y2 = separy(1,ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 92 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
   92    CONTINUE

         DO 93 ipas=nptot(ptsep(4,ipx),ipx)-1,1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(4,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(4,ipx),ipx)
   93    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         DO 97 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   97    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,par%pntrat, & 
     &       pas,np1(ireg),npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &       nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc, & 
     &       a00,a10,a01,a11,par%repart, & 
     &       xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,diag,ireg)
!----------------------------------------------------------------------

      ELSE IF ((npx.EQ.2) .AND. (.NOT.(racord))) THEN

!----------------------------------------------------------------------
!.3.  Disconnected double null

         nreg = 6
         nsep = 6
         ptxext = MOD(ptxint,2) + 1
         npr1 = 0

         IF (ptxint .EQ. 1) THEN
            idef = 1
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
         ENDIF

!..L'indice de niveau frontiere + 1 correspond a l'indice de region.
!..Index of the level boundary + 1 corresponds to the region index

         DO 100 i=4, 1, -1

            distnv(1,i+1) = distnv(1,i)
            distnv(2,i+1) = distnv(2,i)

  100    CONTINUE

!..Calcul de la distance entre la sep. du point X interieur et celle du
!  point X exterieur sur la plaque ou elles sont toutes deux presentes.

!..Calculate the distance between the inner and outer separatrices, if
!  they are both present, along the target

         xint = separx(nptot(ptsep(1,ptxint),ptxint),ptsep(1,ptxint), & 
     &                 ptxint)
         yint = separy(nptot(ptsep(1,ptxint),ptxint),ptsep(1,ptxint), & 
     &                 ptxint)
         xext = separx(nptot(ptsep(3,ptxext),ptxext),ptsep(3,ptxext), & 
     &                 ptxext)
         yext = separy(nptot(ptsep(3,ptxext),ptxext),ptsep(3,ptxext), & 
     &                 ptxext)

         distnv(1,1) = plqdst(xint,yint,xext,yext,xstruc(1,inddef(idef)) & 
     &            ,ystruc(1,inddef(idef)),npstru(inddef(idef)),'droite')


         ii = ifind(xint,x,nx,1)
         jj = ifind(yint,y,ny,1)

         psiint = a00(ii,jj,1) + a10(ii,jj,1)*xint + a01(ii,jj,1)*yint & 
     &          + a11(ii,jj,1)*xint*yint

         ii = ifind(xext,x,nx,1)
         jj = ifind(yext,y,ny,1)

         psiext = a00(ii,jj,1) + a10(ii,jj,1)*xext + a01(ii,jj,1)*yext & 
     &          + a11(ii,jj,1)*xext*yext

         distnv(2,1) = psiext - psiint

!..The distance between the inner X-point and the O-point

         ipx = ptxint

         xx = separx(1,ptsep(3,ipx),ipx)
         yy = separy(1,ptsep(3,ipx),ipx)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

!..Calculate the psi value at the inner X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy + & 
     &            a11(ii,jj,1)*xx*yy

!..Calculate the length of each separatrix

         ipx = 1
         DO 102 isep = 1, 2
            lg(isep) = long(separx(1,ptsep(isep,ipx),ipx), & 
     &                 separy(1,ptsep(isep,ipx),ipx), & 
     &                 nptot(ptsep(isep,ipx),ipx))
  102    CONTINUE

         ipx = 2
         DO 103 isep = 1, 2
            lg(isep+4) = long(separx(1,ptsep(isep,ipx),ipx), & 
     &                   separy(1,ptsep(isep,ipx),ipx), & 
     &                   nptot(ptsep(isep,ipx),ipx))
  103    CONTINUE

            !..3.1  Read all the necessary data from the file
            
            !.. Read initial set of code parameters
            call read_code_parameters(par, distxo)

            correct = .false.
            do while (correct)

                !..Calculate the psi difference between the penetration values

                IF (par%repart .EQ. 2) THEN

                    xfin = xx + xptxo*par%pntrat
                    yfin = yy + yptxo*par%pntrat

                    ii = ifind(xfin,x,nx,1)
                    jj = ifind(yfin,y,ny,1)

                    fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + & 
                        &               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

                    difpsi = fctfin - fctini

                ENDIF

                !..Calculate the intervals, dmin and dmax, along the separatrices

                CALL NUNIFO(par%nptseg(1),lg(1),par%deltp1(1),par%deltpn(1),spacep(1,1), & 
                    &               dpmin(1),dpmax(1))

                CALL NUNIFO(par%nptseg(2),lg(2),par%deltp1(2),par%deltpn(2),spacep(1,2), & 
                    &               dpmin(2),dpmax(2))

                CALL NUNIFO(par%nptseg(5),lg(5),par%deltp1(5),par%deltpn(5),spacep(1,5), & 
                    &               dpmin(5),dpmax(5))

                CALL NUNIFO(par%nptseg(6),lg(6),par%deltp1(6),par%deltpn(6),spacep(1,6), & 
                    &               dpmin(6),dpmax(6))


                !..Calculate the intervals, dmin and dmax, in the radial direction

                CALL NUNIFO(npr(1),distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                    &               spacer(1,1),drmin(1),drmax(1))

                CALL NUNIFO(npr(2),distnv(par%repart,2),par%deltr1(2),par%deltrn(2), & 
                    &               spacer(1,2),drmin(2),drmax(2))

                CALL NUNIFO(npr(3),distnv(par%repart,3),par%deltr1(3),par%deltrn(3), & 
                    &               spacer(1,3),drmin(3),drmax(3))

                CALL NUNIFO(npr(4),distnv(par%repart,4),par%deltr1(4),par%deltrn(4), & 
                    &               spacer(1,4),drmin(4),drmax(4))

                CALL NUNIFO(npr(5),distnv(par%repart,5),par%deltr1(5),par%deltrn(5), & 
                    &               spacer(1,5),drmin(5),drmax(5))

                IF (par%repart .EQ. 1) THEN

                    CALL NUNIFO(npr(6),par%pntrat,par%deltr1(6),par%deltrn(6), & 
                        &                  spacer(1,6),drmin(6),drmax(6))

                ELSE IF (par%repart .EQ. 2) THEN

                    CALL NUNIFO(npr(6),difpsi,par%deltr1(6),par%deltrn(6), & 
                        &                  spacer(1,6),drmin(6),drmax(6))

                ENDIF
                !rm  augmenter ce test
                if(npr(1).ne.npr1) then
                    npr1=npr(1)

                    !.3.1.1. Appel a la routine qui trouve le point sur la boucle de la
                    !        separatrice interieure ou on doit diviser cette separatrice
                    !        en deux.

                    !.3.1.1. Find the point on the separatrix loop where the separatrix
                    !        should be split into two

                    !..Initialise the primary level line

                    nn=0
                    ipx = ptxext

                    DO ipas=nptot(ptsep(3,ipx),ipx),1,-1
                        nn=nn+1
                        xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
                        yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
                    end DO

                    DO ipas=2,nptot(ptsep(4,ipx),ipx)
                        nn=nn+1
                        xn(nn)=separx(ipas,ptsep(4,ipx),ipx)
                        yn(nn)=separy(ipas,ptsep(4,ipx),ipx)
                    end DO

                    !..Co-ordinates of the outer X-point

                    xptxex = separx(1,ptsep(3,ipx),ipx)
                    yptxex = separy(1,ptsep(3,ipx),ipx)

                    !..Points of the grid to be used for the first call to doubld

                    if(npr1.eq.0) then
                        ireg=1
                        nmail=min(41,nn/5)
                        nmail=nmail+mod(nmail+1,2)
                        ll=long(separx(1,ptsep(3,ipx),ipx), & 
                            &                separy(1,ptsep(3,ipx),ipx), & 
                            &                nptot(ptsep(3,ipx),ipx))
                        xmail(1,1,ireg)=separx(nptot(ptsep(3,ipx),ipx), & 
                            &          ptsep(3,ipx),ipx)
                        ymail(1,1,ireg)=separy(nptot(ptsep(3,ipx),ipx), & 
                            &          ptsep(3,ipx),ipx)
                        do imail=nmail/2,2,-1
                            dist=ll*(nmail/2-imail+1.)/(nmail/2)
                            CALL COORD(separx(1,ptsep(3,ipx),ipx), & 
                                &            separy(1,ptsep(3,ipx),ipx),nptot(ptsep(3,ipx),ipx), & 
                                &            dist,xmail(imail,1,ireg),ymail(imail,1,ireg))
                        enddo
                        xmail(nmail/2+1,1,ireg)=xptxex
                        ymail(nmail/2+1,1,ireg)=yptxex

                        ll=long(separx(1,ptsep(4,ipx),ipx), & 
                            &                separy(1,ptsep(4,ipx),ipx), & 
                            &                nptot(ptsep(4,ipx),ipx))
                        xmail(nmail,1,ireg)=separx(nptot(ptsep(4,ipx),ipx), & 
                            &          ptsep(4,ipx),ipx)
                        ymail(nmail,1,ireg)=separy(nptot(ptsep(4,ipx),ipx), & 
                            &          ptsep(4,ipx),ipx)
                        do imail=2,nmail/2
                            dist=ll*(imail-1.)/(nmail/2)
                            CALL COORD(separx(1,ptsep(3,ipx),ipx), & 
                                &            separy(1,ptsep(3,ipx),ipx),nptot(ptsep(3,ipx),ipx), & 
                                &            dist,xmail(nmail/2+imail,1,ireg), & 
                                &            ymail(nmail/2+imail,1,ireg))
                        enddo
                    endif

                    !..Second boundary curve

                    npcrb2=0
                    ipx = ptxint

                    DO ipas=nptot(ptsep(1,ipx),ipx),1,-1
                        npcrb2=npcrb2+1
                        xcrb2(npcrb2)=separx(ipas,ptsep(1,ipx),ipx)
                        ycrb2(npcrb2)=separy(ipas,ptsep(1,ipx),ipx)
                    end DO

                    DO ipas=2,nptot(ptsep(3,ipx),ipx)
                        npcrb2=npcrb2+1
                        xcrb2(npcrb2)=separx(ipas,ptsep(3,ipx),ipx)
                        ycrb2(npcrb2)=separy(ipas,ptsep(3,ipx),ipx)
                    end DO

                    DO ipas=2,nptot(ptsep(2,ipx),ipx)
                        npcrb2=npcrb2+1
                        xcrb2(npcrb2)=separx(ipas,ptsep(2,ipx),ipx)
                        ycrb2(npcrb2)=separy(ipas,ptsep(2,ipx),ipx)
                    end DO

                    call trc_stk_in('maille','*114')
                    CALL DOUBLD(bouclx,boucly,xn,yn,nn,spacer(1,1), & 
                        &        npr(1),inddef(idef),xext,yext,xptxex,yptxex,xpto, & 
                        &        ypto,nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
                        &        ystruc,a00,a10,a01,a11,par%repart, & 
                        &        xcrb2,ycrb2,npcrb2)
                    call trc_stk_out
                endif

                ipx = ptxint
                dist=0.0

                lg(3) = ruban(separx(1,ptsep(3,ipx),ipx), & 
                    &                separy(1,ptsep(3,ipx),ipx),nptot(ptsep(3,ipx),ipx) & 
                    &                ,bouclx,boucly,dist)
                CALL NUNIFO(par%nptseg(3),lg(3),par%deltp1(3),par%deltpn(3),spacep(1,3), & 
                    &               dpmin(3),dpmax(3))

                lg(4) = long(separx(1,ptsep(3,ipx),ipx),separy(1,ptsep(3,ipx) & 
                    &              ,ipx),nptot(ptsep(3,ipx),ipx)) - lg(3)
                CALL NUNIFO(par%nptseg(4),lg(4),par%deltp1(4),par%deltpn(4),spacep(1,4), & 
                    &               dpmin(4),dpmax(4))


                !.. Check & modify code parameters
                call check_and_modify_code_parameters(par, correct)
            end do




!..3.2  Distribute the points along separatrices

!..Determine the two parts of the inner separatrix which make a loop

!..Right part

         ipx = ptxint

         nbcl(1) = 1
         xbcl(1,1) = separx(1,ptsep(3,ipx),ipx)
         ybcl(1,1) = separy(1,ptsep(3,ipx),ipx)

  120    CONTINUE

         nbcl(1) = nbcl(1) + 1
         xbcl(nbcl(1),1) = separx(nbcl(1),ptsep(3,ipx),ipx)
         ybcl(nbcl(1),1) = separy(nbcl(1),ptsep(3,ipx),ipx)

         lbcl = long(xbcl(1,1),ybcl(1,1),nbcl(1))

         IF (lbcl .LE. lg(3)) THEN
            GO TO 120
         ELSE
            xbcl(nbcl(1),1) = bouclx
            ybcl(nbcl(1),1) = boucly
         ENDIF

!..Left part

         ipx = ptxint

         nbcl(2) = 1
         xbcl(1,2) = separx(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)
         ybcl(1,2) = separy(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)

  121    CONTINUE

         nbcl(2) = nbcl(2) + 1
         xbcl(nbcl(2),2) = separx(nptot(ptsep(3,ipx),ipx) - nbcl(2) + 1, & 
     &                     ptsep(3,ipx),ipx)
         ybcl(nbcl(2),2) = separy(nptot(ptsep(3,ipx),ipx) - nbcl(2) + 1, & 
     &                     ptsep(3,ipx),ipx)

         lbcl = long(xbcl(1,2),ybcl(1,2),nbcl(2))

         IF (lbcl .LE. lg(4)) THEN
            GO TO 121
         ELSE
            xbcl(nbcl(2),2) = bouclx
            ybcl(nbcl(2),2) = boucly
         ENDIF


!..Distribute the points along each separatrix

         ipx = 1
         DO 122 isep=1, 2

           sepmax(1,isep) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 123 ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx), & 
     &                   separy(1,ptsep(isep,ipx),ipx), & 
     &                   nptot(ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep),sepmay(ipas,isep))
  123      CONTINUE
           sepmax(par%nptseg(isep),isep)=separx(nptot(ptsep(isep,ipx),ipx), & 
     &       ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep),isep)=separy(nptot(ptsep(isep,ipx),ipx), & 
     &       ptsep(isep,ipx),ipx)
  122    CONTINUE

         DO 124 isep=1, 2

           sepmax(1,isep+2) = xbcl(1,isep)
           sepmay(1,isep+2) = ybcl(1,isep)
           dist=0.

           DO 125 ipas=2, par%nptseg(isep+2)-1
              dist=dist + spacep(ipas-1,isep+2)
              CALL COORD(xbcl(1,isep),ybcl(1,isep),nbcl(isep),dist, & 
     &                   sepmax(ipas,isep+2),sepmay(ipas,isep+2))
  125      CONTINUE
           sepmax(par%nptseg(isep+2),isep+2)=xbcl(nbcl(isep),isep)
           sepmay(par%nptseg(isep+2),isep+2)=ybcl(nbcl(isep),isep)
  124    CONTINUE

         ipx = 2
         DO 126 isep=1, 2

           sepmax(1,isep+4) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep+4) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 127 ipas=2, par%nptseg(isep+4)-1
              dist=dist + spacep(ipas-1,isep+4)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx), & 
     &                   separy(1,ptsep(isep,ipx),ipx), & 
     &                   nptot(ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep+4),sepmay(ipas,isep+4))
  127      CONTINUE
           sepmax(par%nptseg(isep+4),isep+4)=separx(nptot(ptsep(isep,ipx), & 
     &       ipx),ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep+4),isep+4)=separy(nptot(ptsep(isep,ipx), & 
     &       ipx),ptsep(isep,ipx),ipx)
  126    CONTINUE

!..3.3. Grid region by region

!..3.3.1 Region 1: between the separatrices

        ireg=1
        print*, 'ireg=', ireg
         nbcrb = 2

!.. The second boundary

         npcrb2=0
         ipx = ptxext

         DO 130 ipas=nptot(ptsep(3,ipx),ipx),1,-1
            npcrb2=npcrb2+1
            xcrb2(npcrb2)=separx(ipas,ptsep(3,ipx),ipx)
            ycrb2(npcrb2)=separy(ipas,ptsep(3,ipx),ipx)
  130    CONTINUE

         DO 131 ipas=2,nptot(ptsep(4,ipx),ipx)
            npcrb2=npcrb2+1
            xcrb2(npcrb2)=separx(ipas,ptsep(4,ipx),ipx)
            ycrb2(npcrb2)=separy(ipas,ptsep(4,ipx),ipx)
  131    CONTINUE

!..Define the primary curve and the grid points

         ireg=1
         np1(ireg) = 0

         IF (ptxint .EQ. 1) THEN

            DO 132 ipas=par%nptseg(1), 1, -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
  132       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 133 ipas=par%nptseg(5), 1, -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
  133       CONTINUE

         ENDIF

         DO 134 ipas=2, par%nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
  134    CONTINUE

         DO 135 ipas=par%nptseg(4)-1, 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
  135    CONTINUE

         IF (ptxint .EQ. 1) THEN

            DO 136 ipas=2, par%nptseg(2)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
  136       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 137 ipas=2, par%nptseg(6)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
  137       CONTINUE

         ENDIF

!..Initialise the guard indices and starting target

         IF (ptxint .EQ. 1) THEN
            idef = 1
            gardd2 = par%tgarde(2)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
            gardd2 = par%tgarde(4)
         ENDIF
         gardd1 = par%tgarde(idef)

         ipx = ptxint

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = ptxint

         DO 140 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
  140    CONTINUE

         DO 141 ipas=2,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
  141    CONTINUE

         DO 142 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
  142    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*145')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 145 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  145    CONTINUE

!..Parametrise the last level line

         nnlast=0
         ipx = ptxext

         DO 146 ipas=nptot(ptsep(3,ipx),ipx),1,-1
            nnlast=nnlast+1
            xnlast(nnlast)=separx(ipas,ptsep(3,ipx),ipx)
            ynlast(nnlast)=separy(ipas,ptsep(3,ipx),ipx)
  146    CONTINUE

         DO 147 ipas=2,nptot(ptsep(4,ipx),ipx)
            nnlast=nnlast+1
            xnlast(nnlast)=separx(ipas,ptsep(4,ipx),ipx)
            ynlast(nnlast)=separy(ipas,ptsep(4,ipx),ipx)
  147    CONTINUE

         nuldec = .TRUE.

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.true.,diag,ireg)

!..Arangement of the mesh point which must coinside with outer X-point

         IF (ptxint .EQ. 1) THEN

            xmail(par%nptseg(1)+par%nptseg(3)-1,npr(1),1) = xptxex
            ymail(par%nptseg(1)+par%nptseg(3)-1,npr(1),1) = yptxex

         ELSE IF (ptxint .EQ. 2) THEN

            xmail(par%nptseg(5)+par%nptseg(3)-1,npr(1),1) = xptxex
            ymail(par%nptseg(5)+par%nptseg(3)-1,npr(1),1) = yptxex

         ENDIF

         nuldec = .FALSE.

!..3.3.2 Region 2: right

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=2
         np1(ireg) = 0

         IF (ptxint .EQ. 1) THEN

            DO 150 ipas=1,par%nptseg(1)+par%nptseg(3)-2
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  150       CONTINUE

            DO 151 ipas=1, par%nptseg(5)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
  151       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 152 ipas=1,par%nptseg(5)+par%nptseg(3)-2
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  152       CONTINUE

            DO 153 ipas=1, par%nptseg(1)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
  153       CONTINUE

         ENDIF

!..Initialise the guard indices and starting target

         IF (ptxint .EQ. 1) THEN
            idef = 1
            gardd2 = par%tgarde(3)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
            gardd2 = par%tgarde(1)
         ENDIF
         gardd1 = par%tgarde(idef)

         ipx = ptxext

         x2 = separx(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)
         y2 = separy(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = ptxext

         DO 154 ipas=nptot(ptsep(3,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
  154    CONTINUE

         DO 155 ipas=2,nptot(ptsep(1,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
  155    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*157')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 157 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  157    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.true.,diag,ireg)

!..3.3.3 Region 3: top PFR

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=3
         np1(ireg) = 0

         DO 160 ipas=par%nptseg(1),1,-1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
  160    CONTINUE

         DO 161 ipas=2, par%nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
  161    CONTINUE

!..Initialise the guard indices and starting target

         idef = 1
         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

         ipx = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)


!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 164 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
  164    CONTINUE

         DO 165 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
  165    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*167')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 167 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  167    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.false.,diag,ireg)


!..3.3.4 Region 4: left

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=4
         np1(ireg) = 0

         IF (ptxint .EQ. 1) THEN

            DO 170 ipas=par%nptseg(1)+par%nptseg(2)+par%nptseg(3)+par%nptseg(4)-3, & 
     &                             par%nptseg(1)+par%nptseg(3), -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  170       CONTINUE

            DO 171 ipas=1, par%nptseg(6)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
  171       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 172 ipas=par%nptseg(5)+par%nptseg(6)+par%nptseg(3)+par%nptseg(4)-3, & 
     &                             par%nptseg(5)+par%nptseg(3), -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  172       CONTINUE

            DO 173 ipas=1, par%nptseg(2)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
  173       CONTINUE

         ENDIF

!..Initialise the guard indices and starting target

         IF (ptxint .EQ. 1) THEN
            idef = 2
            gardd2 = par%tgarde(4)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 4
            gardd2 = par%tgarde(2)
         ENDIF
         gardd1 = par%tgarde(idef)

         ipx = ptxext

         x2 = separx(nptot(ptsep(4,ipx),ipx),ptsep(4,ipx),ipx)
         y2 = separy(nptot(ptsep(4,ipx),ipx),ptsep(4,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = ptxext

         DO 174 ipas=nptot(ptsep(4,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(4,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(4,ipx),ipx)
  174    CONTINUE

         DO 175 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
  175    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*177')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 177 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  177    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.true.,diag,ireg)

!..3.3.5 Region 5: bottom PFR

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=5
         np1(ireg) = 0

         DO 180 ipas=par%nptseg(5), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
  180    CONTINUE

         DO 181 ipas=2, par%nptseg(6)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
  181    CONTINUE

!..Initialise the guard indices and starting target

         idef = 3
         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(4)

         ipx = 2

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 2

         DO 184 ipas=nptot(ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(1,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(1,ipx),ipx)
  184    CONTINUE

         DO 185 ipas=2,nptot(ptsep(2,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(2,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(2,ipx),ipx)
  185    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*187')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 187 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  187    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &               x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &               a00,a10,a01,a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,.false.,diag,ireg)

!..3.3.6  Region 6: central region

!..Define the primary curve and the grid points

         ireg=6
         np1(ireg) = 0
         ipx = ptxint

         DO 190 ipas=1, par%nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
  190    CONTINUE

         DO 191 ipas=par%nptseg(4)-1,1,-1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
  191    CONTINUE

         x2 = separx(1,ptsep(3,ipx),ipx)
         y2 = separy(1,ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 192 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
  192    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         DO 197 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  197    CONTINUE


!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,par%pntrat, & 
     &      pas,np1(ireg),npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc, & 
     &      a00,a10,a01,a11,par%repart, & 
     &      xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,diag,ireg)
!----------------------------------------------------------------------

      ELSE IF (limcfg.eq.1) THEN

!----------------------------------------------------------------------
!..4.   Limiter configuration

         nbcrb = 1
         nreg = 2
         nsep = 1
         ipx = 1
         isep=nsep

!..The distance between the X-point and the O-point

         xx = nivx(1,1)
         yy = nivy(1,1)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

!..Calculate the psi value at the X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy + & 
     &            a11(ii,jj,1)*xx*yy


!..Calculate the length of each separatrix

         lg(isep) = long(nivx(1,1),nivy(1,1),nivtot(1))
         
         !..4.1  Read all the necessary data from the file
         
         !.. Read initial set of code parameters
         call read_code_parameters(par, distxo)

         correct = .false.
         do while (correct)

             !..Calculate the psi difference between the penetration values

             IF (par%repart .EQ. 2) THEN

                 xfin = xx + xptxo*par%pntrat
                 yfin = yy + yptxo*par%pntrat

                 ii = ifind(xfin,x,nx,1)
                 jj = ifind(yfin,y,ny,1)

                 fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin + & 
                     &               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

                 difpsi = fctfin - fctini

             ENDIF

             !..Calculate the intervals, dmin and dmax

             !..Along the separatrices

             CALL NUNIFO(par%nptseg(1),lg(1),par%deltp1(1),par%deltpn(1),spacep(1,1), & 
                 &               dpmin(1),dpmax(1))


             !..Radial direction

             CALL NUNIFO(npr(1),distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                 &               spacer(1,1),drmin(1),drmax(1))

             IF (par%repart .EQ. 1) THEN

                 CALL NUNIFO(npr(2),par%pntrat,par%deltr1(2),par%deltrn(2), & 
                     &                  spacer(1,2),drmin(2),drmax(2))

             ELSE IF (par%repart .EQ. 2) THEN

                 CALL NUNIFO(npr(2),difpsi,par%deltr1(2),par%deltrn(2), & 
                     &                  spacer(1,2),drmin(2),drmax(2))

             ENDIF

             !.. Check & modify code parameters
             call check_and_modify_code_parameters(par, correct)
         end do


!..4.2  Distribute the points along separatrices

         DO isep=1, nsep

           sepmax(1,isep) = nivx(1,1)
           sepmay(1,isep) = nivy(1,1)
           dist=0.

           DO ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(nivx(1,1),nivy(1,1),nivtot(1), & 
     &          dist,sepmax(ipas,isep),sepmay(ipas,isep))
           ENDDO

           sepmax(par%nptseg(isep),isep)=nivx(nivtot(1),1)
           sepmay(par%nptseg(isep),isep)=nivy(nivtot(1),1)
         ENDDO

!..4.3. Grid region by region

!..4.3.1  Region 1

!..Define the primary curve and the grid points

         ireg=1
         np1(ireg) = 0

         DO ipas=1, par%nptseg(1)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
         ENDDO

!..Go along the target 1

         idef = 1

         x2 = nivx(1,1)
         y2 = nivy(1,1)
!***
!        print*,'x2, y2=',x2,y2
!***

!..Initialise the primary level line

         nn=0

         DO ipas=1,nivtot(1)
            nn=nn+1
            xn(nn)=nivx(ipas,1)
            yn(nn)=nivy(ipas,1)
         ENDDO

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Determine the structure orientation

!!!!
!        sens =-drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
!    .                 npstru(inddef(idef)),x2,y2,'droite')
         sens=1
         sens=horair(xpto,ypto,x2,y2,xstruc(1,inddef(idef)), & 
     &     ystruc(1,inddef(idef)),npstru(inddef(idef)),sens)
!***
!        print*,'dans maille: sens=',sens
!***

         DO ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

         ENDDO

!..Call the routine which grids this region

!***
!        print*,'call mailrg'
!        print*,'ireg, idef, inddef=',ireg,idef,inddef(idef)
!        print*,'np1, npr=',np1(ireg),npr(ireg)
!***
!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas, & 
              &              np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny, & 
              &              x,y,psi,xpto,ypto,nstruc,npstru,xstruc,ystruc, & 
              &              a00,a10,a01,a11,par%repart, & 
              &              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &              ynlast,nnlast,nuldec,.false.,diag,ireg)

!..4.3.2  Region 2

!..Define the primary curve and the grid points

         ireg=2
         np1(ireg) = 0

         DO ipas=1, par%nptseg(1)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
         ENDDO

         x2 = nivx(1,1)
         y2 = nivy(1,1)

!..Initialise the primary level line

         nn=0

         DO ipas=1,nivtot(1)
            nn=nn+1
            xn(nn)=nivx(nn,1)
            yn(nn)=nivy(nn,1)
         ENDDO

!..Definition des pas pour le balayage vers le centre
         DO ipas=1, npr(ireg)-1
            pas(ipas) = spacer(ipas,ireg)
!***
!           print*,'ipas, pas=',ipas,pas(ipas)
!***
         ENDDO

!..Call the routine which grids this region

!***
!        print*,'call mailcn'
!***
!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,par%pntrat, & 
     &      pas,np1(ireg),npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc, & 
     &      a00,a10,a01,a11,par%repart, & 
     &      xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,diag,ireg)

!       ...
      ENDIF
!======================================================================

!..5.  Write the grid data into a file

      CALL SORTIE(nsep,nreg,np1, & 
     &  distxo,xmail,ymail,nx,ny, & 
     &  x,y,a00,a10,a01,a11,ptx,pty,npx,racord,2,fctpx,diag,par)

!c<<<
!      write(0,*) '<=== Leaving maille'
!c>>>
      RETURN
!======================================================================


CONTAINS

  subroutine read_code_parameters(par, distxo)
    type(CarreParameters), intent(inout) :: par
    real*8, intent(in) :: distxo

    ! internal
    integer :: ient,isor,ifail

    par%tgarde=0
    
!..1.1  Read all the necessary data from the file

         ient = 9
         isor = 0
         CALL CHANGE(par%nptseg,par%deltp1,par%deltpn,par%repart,&
             & par%npr,par%deltr1,par%deltrn,&
             & par%pntrat,par%tgarde,distxo,ient,isor,ifail)

!..Check whether all the data have been read from the file

         IF (ifail .EQ. 1) THEN

           if(sellan(1:8).eq.'francais') then
             CALL LECCLF(par%nptseg,par%npr,lg,&
                 & par%deltp1,par%deltpn,par%deltr1,limcfg, & 
                 & par%deltrn,par%repart,par%pntrat,par%tgarde,&
                 & distnv,xptxo,yptxo, & 
                 & distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax, & 
                 & npx,racord,x,y,nx,ny)
           elseif(sellan(1:7).eq.'english') then
             CALL LECCLE(par%nptseg,par%npr,lg,&
                 & par%deltp1,par%deltpn,par%deltr1,limcfg, & 
                 & par%deltrn,par%repart,par%pntrat,par%tgarde,&
                 & distnv,xptxo,yptxo, & 
                 & distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax, & 
                 & npx,racord,x,y,nx,ny)
           endif

         ENDIF

  end subroutine read_code_parameters


  subroutine check_and_modify_code_parameters(par, correct)
    type(CarreParameters), intent(inout) :: par
    logical, intent(out) :: correct

         CALL RAPPEL(par%nptseg,par%deltp1,par%deltpn,par%repart,&
             & par%npr,par%deltr1,par%deltrn, &
             & par%pntrat,par%tgarde,&
             & lg,difpsi,distnv,nreg,nsep,npx,&
             & dpmin,dpmax,drmin,drmax,distxo,6,correct)

!..Initialise the primary level line

         nn1=0

         DO ipas=1,nptot(ptsep(3,ipx),ipx)
            nn1=nn1+1
            xn(nn1)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn1)=separy(ipas,ptsep(3,ipx),ipx)
         end do	

! on colle la dernière ligne de niveau sur trace2 pour avoir
! la pénétration.

         call trace3(x(1),x(nx),y(1),y(ny),separx,separy, & 
     &        ptsep,npx,nptot, & 
     &        nstruc,npstru,xstruc,ystruc, & 
     &        nivx,nivy,nivtot,nbniv, & 
     &         par%pntrat,distxo,xn,yn,nn1, & 
     &         par%repart,xptxo,yptxo,fctini,xfin,yfin,fctfin, & 
     &         a00,a01,a10,a11,psi,nx,ny,x,y)

         if (correct) then
           if(sellan(1:8).eq.'francais') then
             WRITE(6,301)
           elseif(sellan(1:7).eq.'english') then
             WRITE(6,300)
           endif
           READ(5,302)rep
         endif

         if(rep(1:1).eq.'n' .or. rep(1:1).eq.'N' .or. .not.correct) then

            ient = 5
            isor = 6
            pntrat_old = par%pntrat
            CALL CHANGE(par%nptseg,par%deltp1,par%deltpn,par%repart,&
                & par%npr,par%deltr1,par%deltrn, & 
                & par%pntrat,par%tgarde,distxo,ient,isor,ifail)
            if (par%pntrat.ne.pntrat_old) then

            call endpag

            call trace2(x(1),x(nx),y(1),y(ny), & 
     &     separx,separy,ptsep,npx,nptot, & 
     &         nstruc,npstru,xstruc,ystruc,nivx,nivy, & 
     &         nivtot,nbniv)

            endif
            ! parameters have been changed and state has to be recomputed
            !GO TO 3 
         else
            CALL RAPPEL(par%nptseg,par%deltp1,par%deltpn,par%repart,&
                & npr,par%deltr1,par%deltrn, & 
     &             par%pntrat,par%tgarde,lg,difpsi,distnv,nreg,nsep,npx, & 
     &             dpmin,dpmax,drmin,drmax,distxo,10,correct)

            CALL SORTIE(nsep,nreg,np1, & 
                &  distxo,xmail,ymail,nx,ny, & 
                &  x,y,a00,a10,a01,a11,ptx,pty,npx,racord,1,fctpx,diag,par)
         ENDIF

!..Save the chosen parameters


  301        FORMAT(//T2,'Est-ce que ces valeurs sont correctes? (o/n)')
  300        format(//T2,'Do you wish to accept these values (y/n)?')
  302      FORMAT(A)


  end subroutine check_and_modify_code_parameters



      END
