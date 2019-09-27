subroutine MAILLE(equ,struct,grid,diag,par)

  ! TODO: grid%xn,... may be local

!======================================================================
!ank -- The comments are translated from French, sorry for errors!

!*** This sub-routine makes the orthogonal curvilinear grid for
!*** various possible magnetic configurations (single null,
!*** connected double null, disconnected double null, or limiter)
!======================================================================
    
      use CarreSiloIO
      use CarreDiagnostics
      use carre_types
      use carre_parameter_io
      use carre_target
      use carre_equilibrium
      use Logging
      use Helper
  
      IMPLICIT NONE
      
      !  dimensions
#include <CARREDIM.F>

      !  arguments
      type(CarreDiag), intent(inout) :: diag
      type(CarreParameters), intent(inout) :: par
      type(CarreEquilibrium), intent(inout) :: equ
      type(CarreGrid), intent(inout) :: grid
      type(CarreStructures), intent(inout) :: struct

      !  variables en common
#include <COMLAN.F>
#include <COMRLX.F>

      !  variables locales
      INTEGER isep,ipas,ireg,ipx,sens,nn,idef, & 
     &  ii,jj,ient,isor,ifail,nbcrb,npcrb2 & 
     &  ,ptxext,i,j,nbcl(2),nnlast,npr1,nmail,imail, xpind
      REAL*8 dist,x2,y2,lg(10), & 
     &  xx,yy,fctini,fctfin,difpsi,dpmin(10), & 
     &  dpmax(10),drmin(10),drmax(10),xfin,yfin,gardd1, & 
     &  gardd2,xptxo,yptxo,xint,yint,xext,yext,xextOffset,yextOffset,psiint, & 
     &  psiext,xptxex,yptxex,bouclx,boucly,ll,pntrat_old
      REAL*8  sepmax(npnimx,8),sepmay(npnimx,8),spacep(npmamx,10), & 
     &  spacer(npmamx,10),pas(nrmamx),xcrb2(npnimx),ycrb2(npnimx) & 
     &  ,xbcl(npnimx,2),ybcl(npnimx,2),lbcl,xnlast(npnimx), & 
     &  ynlast(npnimx)
      CHARACTER*3 rep
      LOGICAL nuldec, correct, extended_grid
      integer :: nn1

      !  procedures
      INTEGER horair,ifind
      REAL*8 long,ruban
      EXTERNAL long,COORD,horair,ifind,LECCLE,lecclf,DOUBLD, & 
     &         ruban,trace3 & 
     &        ,trc_stk_in,trc_stk_out
      intrinsic max


!=========================
!.. npnimx: <=> npnimx
!.. grid%nreg  : number of regions
!.. xmail,grid%ymail: coordinates of the grid points
!                (poloidal index, radial index, region index)
!.. grid%xn,grid%yn : working arrays for coordinates along a parametrised curve
!.. nn    : number of points on the same curve
!.. grid%np1   : number of points in poloidal direction
!.. ireg  : region index
!.. ipx   : X-point index
!.. ptxext: index of the outer X-point
!.. sens  : direction of movement along a target:
!           1=the same as the target points, 2=the opposite
!.. idef  : index of the target from where the routine starts
!.. equ%a00,a10,a01,a11: coefficients.
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
!      write(0,*) '===> Entering maille.  npx, limcfg = ',npx,equ%limcfg
!c>>>

      nuldec = .FALSE.
      lg = 0.0d0

      grid%radLineSepSeg = 0

      extended_grid = ( par%carreMode == CARRE_EXTENDED )

!..On procede au cas par cas selon la configuration des separatrices.

!----------------------------------------------------------------------

      IF (equ%npx.EQ.1 .and. equ%limcfg.eq.0) THEN

!----------------------------------------------------------------------
!..1.   Single null

         nbcrb = 1
         grid%nreg = 3
         equ%nsep = 3
         ipx = 1

!..The distance between the X-point and the O-point

         xx = equ%separx(1,equ%ptsep(3,ipx),ipx)
         yy = equ%separy(1,equ%ptsep(3,ipx),ipx)

         xptxo = equ%xpto - xx
         yptxo = equ%ypto - yy

         equ%distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/equ%distxo
         yptxo = yptxo/equ%distxo

!..Equ%psi value at the X-point

         ii = ifind(xx,equ%x,equ%nx,1)
         jj = ifind(yy,equ%y,equ%ny,1)

         fctini = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xx &
              & + equ%a01(ii,jj,1)*yy + equ%a11(ii,jj,1)*xx*yy

!..Calculate the length of each separatrix

         DO 1 isep = 1, 3

            lg(isep) = long(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                 equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                 equ%nptot(equ%ptsep(isep,ipx),ipx))

    1    CONTINUE

!.. Read initial set of code parameters
         call read_code_parameters(par, equ%distxo)

         correct = .false.
         do while (.not. correct)

             !..Calculate the equ%psi difference between the penetration values

             IF (par%repart .EQ. 2) THEN

                 xfin = xx + xptxo*par%pntrat
                 yfin = yy + yptxo*par%pntrat

                 ii = ifind(xfin,equ%x,equ%nx,1)
                 jj = ifind(yfin,equ%y,equ%ny,1)

                 fctfin = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xfin + & 
                      & equ%a01(ii,jj,1)*yfin + equ%a11(ii,jj,1)*xfin*yfin

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

             CALL NUNIFO(par%npr(1),struct%distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                 &               spacer(1,1),drmin(1),drmax(1))

             CALL NUNIFO(par%npr(2),struct%distnv(par%repart,2),par%deltr1(2),par%deltrn(2), & 
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

           sepmax(1,isep) = equ%separx(1,equ%ptsep(isep,ipx),ipx)
           sepmay(1,isep) = equ%separy(1,equ%ptsep(isep,ipx),ipx)
           dist=0.

           DO 5 ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &          equ%separy(1,equ%ptsep(isep,ipx),ipx),&
     &          equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &          dist,sepmax(ipas,isep),sepmay(ipas,isep))
    5      CONTINUE

           sepmax(par%nptseg(isep),isep)=equ%separx(equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &       equ%ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep),isep)=equ%separy(equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &       equ%ptsep(isep,ipx),ipx)
    6    CONTINUE

!..1.3  Grid region by region

!..1.3.1  Region 1

!.. Define the primary curve and the grid points

         call csioSetRegion( 1 )

         ireg=1
         grid%np1(ireg) = 0

         DO 10 ipas=par%nptseg(1), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
   10    CONTINUE

         DO 11 ipas=1, par%nptseg(3)-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,3)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,3)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 3
   11    CONTINUE

         DO 12 ipas=1, par%nptseg(2)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
   12    CONTINUE


!..Go along the target 1

         idef = 1

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 13 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
   13    CONTINUE

         DO 14 ipas=2,equ%nptot(equ%ptsep(3,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
   14    CONTINUE

         DO 15 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
   15    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*17')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2,&
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx),&
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx),&
              & 'droite')
         call trc_stk_out

         DO 17 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   17    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
     &              grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
     &              equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
     &              equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
     &              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
     &              ynlast,nnlast,nuldec,&
     &              xpind,xptxex,yptxex,&
     &              .true.,diag,ireg, struct, .false.)

!
!  1.3.2  region 2

!..Define the primary curve and the grid points

         call csioSetRegion( 2 )

         ireg=2
         grid%np1(ireg) = 0

         DO 20 ipas=par%nptseg(1), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
   20    CONTINUE

         DO 21 ipas=1, par%nptseg(2)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
   21    CONTINUE

!..Go along the target 1

         idef = 1

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 22 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
   22    CONTINUE

         DO 23 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
   23    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*25 ')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),&
              & x2,y2,&
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx),&
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx),&
              &  'gauche')
         call trc_stk_out

         DO 25 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   25    CONTINUE

!..Call the routine which grids this region

         print*, 'ireg=', ireg
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &              grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &              equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &              equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &              ynlast,nnlast,nuldec,&
              &              xpind,xptxex,yptxex,&
              &.false.,diag,ireg, struct, .false.)
!
!  1.3.3  region 3

!..Define the primary curve and the grid points

         call csioSetRegion( 3 )

         ireg=3
         grid%np1(ireg) = 0

         DO 30 ipas=1, par%nptseg(3)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,3)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,3)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 3
   30    CONTINUE

         x2 = equ%separx(1,equ%ptsep(3,ipx),ipx)
         y2 = equ%separy(1,equ%ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 31 ipas=1,equ%nptot(equ%ptsep(3,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
   31    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         DO 32 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   32    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,par%pntrat, & 
     &       pas,grid%np1(ireg),par%npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &       equ%nx,equ%ny,equ%x,equ%y,equ%psi,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
     &       equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
     &       xptxo,yptxo,equ%xpto,equ%ypto,struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,equ%distxo,diag,ireg)
!----------------------------------------------------------------------

      ELSE IF ((equ%npx.EQ.2) .AND. (equ%racord)) THEN

!..2.   Connected double null

!----------------------------------------------------------------------
         nbcrb = 1
         grid%nreg = 5
         equ%nsep = 6

!..The distance between the top X-point and the O-point

         ipx = 1

         xx = equ%separx(1,equ%ptsep(3,ipx),ipx)
         yy = equ%separy(1,equ%ptsep(3,ipx),ipx)

         xptxo = equ%xpto - xx
         yptxo = equ%ypto - yy

         equ%distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/equ%distxo
         yptxo = yptxo/equ%distxo

!..Calculate the equ%psi value at the X-point

         ii = ifind(xx,equ%x,equ%nx,1)
         jj = ifind(yy,equ%y,equ%ny,1)

         fctini = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xx + equ%a01(ii,jj,1)*yy + & 
     &            equ%a11(ii,jj,1)*xx*yy

!..Calculate the length of each separatrix

         ipx = 1
         DO 38 isep = 1, 4
            lg(isep) = long(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                 equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                 equ%nptot(equ%ptsep(isep,ipx),ipx))
   38    CONTINUE

         ipx = 2
         DO 39 isep = 1, 2
            lg(isep+4) = long(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%nptot(equ%ptsep(isep,ipx),ipx))
   39    CONTINUE

!..2.1  Read all the necessary data from the file

         call read_code_parameters(par, equ%distxo)

         correct = .false.
         do while (.not. correct)

             !..Calculate the equ%psi difference between the penetration values

             IF (par%repart .EQ. 2) THEN

                 xfin = xx + xptxo*par%pntrat
                 yfin = yy + yptxo*par%pntrat

                 ii = ifind(xfin,equ%x,equ%nx,1)
                 jj = ifind(yfin,equ%y,equ%ny,1)

                 fctfin = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xfin + & 
                     &               equ%a01(ii,jj,1)*yfin + equ%a11(ii,jj,1)*xfin*yfin

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

             CALL NUNIFO(par%npr(1),struct%distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                 &               spacer(1,1),drmin(1),drmax(1))

             CALL NUNIFO(par%npr(2),struct%distnv(par%repart,2),par%deltr1(2),par%deltrn(2), & 
                 &               spacer(1,2),drmin(2),drmax(2))

             CALL NUNIFO(par%npr(3),struct%distnv(par%repart,3),par%deltr1(3),par%deltrn(3), & 
                 &               spacer(1,3),drmin(3),drmax(3))

             CALL NUNIFO(par%npr(4),struct%distnv(par%repart,4),par%deltr1(4),par%deltrn(4), & 
                 &               spacer(1,4),drmin(4),drmax(4))

             IF (par%repart .EQ. 1) THEN

                 CALL NUNIFO(par%npr(5),par%pntrat,par%deltr1(5),par%deltrn(5), & 
                     &                  spacer(1,5),drmin(5),drmax(5))

             ELSE IF (par%repart .EQ. 2) THEN

                 CALL NUNIFO(par%npr(5),difpsi,par%deltr1(5),par%deltrn(5), & 
                     &                  spacer(1,5),drmin(5),drmax(5))

             ENDIF

             !.. Check & modify code parameters
             call check_and_modify_code_parameters(par, correct)
         end do

!..2.2  Distribute the points along separatrices

         ipx = 1
         DO 44 isep=1, 4

           sepmax(1,isep) = equ%separx(1,equ%ptsep(isep,ipx),ipx)
           sepmay(1,isep) = equ%separy(1,equ%ptsep(isep,ipx),ipx)
           dist=0.

           DO 45 ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%nptot(equ%ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep),sepmay(ipas,isep))
   45      CONTINUE
           sepmax(par%nptseg(isep),isep)=equ%separx(equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &       equ%ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep),isep)=equ%separy(equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &       equ%ptsep(isep,ipx),ipx)
   44    CONTINUE

         ipx = 2
         DO 46 isep=1, 2

           sepmax(1,isep+4) = equ%separx(1,equ%ptsep(isep,ipx),ipx)
           sepmay(1,isep+4) = equ%separy(1,equ%ptsep(isep,ipx),ipx)
           dist=0.

           DO 47 ipas=2, par%nptseg(isep+4)-1
              dist=dist + spacep(ipas-1,isep+4)
              CALL COORD(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%nptot(equ%ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep+4),sepmay(ipas,isep+4))

   47      CONTINUE
           sepmax(par%nptseg(isep+4),isep+4)=equ%separx(equ%nptot(equ%ptsep(isep,ipx), & 
     &       ipx),equ%ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep+4),isep+4)=equ%separy(equ%nptot(equ%ptsep(isep,ipx), & 
     &       ipx),equ%ptsep(isep,ipx),ipx)
   46    CONTINUE

!..2.3. Grid region by region

!..2.3.1 Region 1: right

!..Define the primary curve and the grid points

         ireg=1
         grid%np1(ireg) = 0

         DO 50 ipas=par%nptseg(1), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
   50    CONTINUE

         DO 51 ipas=1, par%nptseg(3)-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,3)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,3)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 3
   51    CONTINUE

         DO 52 ipas=1, par%nptseg(5)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,5)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,5)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 5
   52    CONTINUE


!..Go along the target 1

         idef = 1
         ipx = 1

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 53 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
   53    CONTINUE

         DO 54 ipas=2,equ%nptot(equ%ptsep(3,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
   54    CONTINUE

         ipx = 2

         DO 55 ipas=2,equ%nptot(equ%ptsep(1,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
   55    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(3)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*57')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & 'droite')
         call trc_stk_out

         DO 57 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   57    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
     &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
     &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
     &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
     &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
     &               ynlast,nnlast,nuldec,&
     &               xpind,xptxex,yptxex,&
     &               .true.,diag,ireg, struct, .false.)

!
!..2.3.2 Region 2: top PFR

!..Define the primary curve and the grid points

         ireg=2
         grid%np1(ireg) = 0

         DO 60 ipas=par%nptseg(1), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
   60    CONTINUE

         DO 61 ipas=1, par%nptseg(2)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
   61    CONTINUE

!..Go along the target 1

         idef = 1
         ipx = 1

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 63 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
   63    CONTINUE

         DO 64 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
   64    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*67')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & 'gauche')
         call trc_stk_out

         DO 67 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   67    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &.false.,diag,ireg, struct, .false.)

!
!..2.3.3 Region 3: left

!..Define the primary curve and the grid points

         ireg=3
         grid%np1(ireg) = 0

         DO 70 ipas=par%nptseg(2), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
   70    CONTINUE

         DO 71 ipas=1, par%nptseg(4)-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,4)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,4)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 4
   71    CONTINUE

         DO 72 ipas=1, par%nptseg(6)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,6)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,6)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 6
   72    CONTINUE

!..Go along the target 2

         idef = 2
         ipx = 1

         x2 = equ%separx(equ%nptot(equ%ptsep(2,ipx),ipx),equ%ptsep(2,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(2,ipx),ipx),equ%ptsep(2,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 73 ipas=equ%nptot(equ%ptsep(2,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
   73    CONTINUE

         DO 74 ipas=2,equ%nptot(equ%ptsep(4,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(4,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(4,ipx),ipx)
   74    CONTINUE

         ipx = 2

         DO 75 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
   75    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(4)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*77')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(2,ipx),ipx)-1,equ%ptsep(2,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(2,ipx),ipx)-1,equ%ptsep(2,ipx),ipx), &
              & 'gauche')
         call trc_stk_out

         DO 77 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   77    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &.true.,diag,ireg, struct, .false.)


!..2.3.4 Region 4: bottom PFR

!..Define the primary curve and the grid points

         ireg=4
         grid%np1(ireg) = 0

         DO 80 ipas=par%nptseg(5), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,5)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,5)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 5
   80    CONTINUE

         DO 81 ipas=1, par%nptseg(6)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,6)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,6)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 6
   81    CONTINUE

!..Go along the target 3

         idef = 3
         ipx = 2

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 2

         DO 83 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
   83    CONTINUE

         DO 84 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
   84    CONTINUE

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(4)

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*87')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & 'gauche')
         call trc_stk_out

         DO 87 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   87    CONTINUE


!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &.false.,diag,ireg, struct, .false.)

!
!..2.3.5  Region 5: central region

!..Define the primary curve and the grid points

         ireg=5
         grid%np1(ireg) = 0
         ipx = 1

         DO 90 ipas=1, par%nptseg(3)-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,3)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,3)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 3
   90    CONTINUE

         DO 91 ipas=par%nptseg(4),1,-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,4)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,4)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 4
   91    CONTINUE

         x2 = equ%separx(1,equ%ptsep(3,ipx),ipx)
         y2 = equ%separy(1,equ%ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 92 ipas=1,equ%nptot(equ%ptsep(3,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
   92    CONTINUE

         DO 93 ipas=equ%nptot(equ%ptsep(4,ipx),ipx)-1,1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(4,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(4,ipx),ipx)
   93    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         DO 97 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   97    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,par%pntrat, & 
     &       pas,grid%np1(ireg),par%npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &       equ%nx,equ%ny,equ%x,equ%y,equ%psi,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
     &       equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
     &       xptxo,yptxo,equ%xpto,equ%ypto,struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,equ%distxo,diag,ireg)
!----------------------------------------------------------------------

      ELSE IF ((equ%npx.EQ.2) .AND. (.NOT.(equ%racord))) THEN

!----------------------------------------------------------------------
!.3.  Disconnected double null

         grid%nreg = 6
         equ%nsep = 6
         ptxext = MOD(equ%ptxint,2) + 1
         npr1 = 0

         IF (equ%ptxint .EQ. 1) THEN
            idef = 1
         ELSE IF (equ%ptxint .EQ. 2) THEN
            idef = 3
         ENDIF

!..L'indice de niveau frontiere + 1 correspond a l'indice de region.
!..Index of the level boundary + 1 corresponds to the region index

         DO 100 i=4, 1, -1

            struct%distnv(1,i+1) = struct%distnv(1,i)
            struct%distnv(2,i+1) = struct%distnv(2,i)

  100    CONTINUE

!..Calcul de la distance entre la sep. du point X interieur et celle du
!  point X exterieur sur la plaque ou elles sont toutes deux presentes.

!..Calculate the distance between the inner and outer separatrices, if
!  they are both present, along the target

         xint = equ%separx(equ%nptot(equ%ptsep(1,equ%ptxint),equ%ptxint),equ%ptsep(1,equ%ptxint), & 
     &                 equ%ptxint)
         yint = equ%separy(equ%nptot(equ%ptsep(1,equ%ptxint),equ%ptxint),equ%ptsep(1,equ%ptxint), & 
     &                 equ%ptxint)
         xext = equ%separx(equ%nptot(equ%ptsep(3,ptxext),ptxext),equ%ptsep(3,ptxext), & 
     &                 ptxext)
         yext = equ%separy(equ%nptot(equ%ptsep(3,ptxext),ptxext),equ%ptsep(3,ptxext), & 
     &                 ptxext)
         xextOffset = equ%separx(equ%nptot(equ%ptsep(3,ptxext),ptxext)-1,equ%ptsep(3,ptxext), & 
     &                 ptxext)
         yextOffset = equ%separy(equ%nptot(equ%ptsep(3,ptxext),ptxext)-1,equ%ptsep(3,ptxext), & 
     &                 ptxext)

         struct%distnv(1,1) = plqdst(xint,yint,xext,yext,struct%xstruc(1,struct%inddef(idef)) & 
     &            ,struct%ystruc(1,struct%inddef(idef)),struct%npstru(struct%inddef(idef)),'droite')


         ii = ifind(xint,equ%x,equ%nx,1)
         jj = ifind(yint,equ%y,equ%ny,1)

         psiint = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xint + equ%a01(ii,jj,1)*yint & 
     &          + equ%a11(ii,jj,1)*xint*yint

         ii = ifind(xext,equ%x,equ%nx,1)
         jj = ifind(yext,equ%y,equ%ny,1)

         psiext = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xext + equ%a01(ii,jj,1)*yext & 
     &          + equ%a11(ii,jj,1)*xext*yext

         struct%distnv(2,1) = psiext - psiint

!..The distance between the inner X-point and the O-point

         ipx = equ%ptxint

         xx = equ%separx(1,equ%ptsep(3,ipx),ipx)
         yy = equ%separy(1,equ%ptsep(3,ipx),ipx)

         xptxo = equ%xpto - xx
         yptxo = equ%ypto - yy

         equ%distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/equ%distxo
         yptxo = yptxo/equ%distxo

!..Calculate the equ%psi value at the inner X-point

         ii = ifind(xx,equ%x,equ%nx,1)
         jj = ifind(yy,equ%y,equ%ny,1)

         fctini = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xx + equ%a01(ii,jj,1)*yy + & 
     &            equ%a11(ii,jj,1)*xx*yy

!..Calculate the length of each separatrix

         ipx = 1
         DO 102 isep = 1, 2
            lg(isep) = long(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                 equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                 equ%nptot(equ%ptsep(isep,ipx),ipx))
  102    CONTINUE

         ipx = 2
         DO 103 isep = 1, 2
            lg(isep+4) = long(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%nptot(equ%ptsep(isep,ipx),ipx))
  103    CONTINUE

            !..3.1  Read all the necessary data from the file
            
            !.. Read initial set of code parameters
            call read_code_parameters(par, equ%distxo)

            correct = .false.
            do while (.not. correct)

                !..Calculate the equ%psi difference between the penetration values

                IF (par%repart .EQ. 2) THEN

                    xfin = xx + xptxo*par%pntrat
                    yfin = yy + yptxo*par%pntrat

                    ii = ifind(xfin,equ%x,equ%nx,1)
                    jj = ifind(yfin,equ%y,equ%ny,1)

                    fctfin = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xfin + & 
                        &               equ%a01(ii,jj,1)*yfin + equ%a11(ii,jj,1)*xfin*yfin

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

                CALL NUNIFO(par%npr(1),struct%distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                    &               spacer(1,1),drmin(1),drmax(1))

                CALL NUNIFO(par%npr(2),struct%distnv(par%repart,2),par%deltr1(2),par%deltrn(2), & 
                    &               spacer(1,2),drmin(2),drmax(2))

                CALL NUNIFO(par%npr(3),struct%distnv(par%repart,3),par%deltr1(3),par%deltrn(3), & 
                    &               spacer(1,3),drmin(3),drmax(3))

                CALL NUNIFO(par%npr(4),struct%distnv(par%repart,4),par%deltr1(4),par%deltrn(4), & 
                    &               spacer(1,4),drmin(4),drmax(4))

                CALL NUNIFO(par%npr(5),struct%distnv(par%repart,5),par%deltr1(5),par%deltrn(5), & 
                    &               spacer(1,5),drmin(5),drmax(5))

                IF (par%repart .EQ. 1) THEN

                    CALL NUNIFO(par%npr(6),par%pntrat,par%deltr1(6),par%deltrn(6), & 
                        &                  spacer(1,6),drmin(6),drmax(6))

                ELSE IF (par%repart .EQ. 2) THEN

                    CALL NUNIFO(par%npr(6),difpsi,par%deltr1(6),par%deltrn(6), & 
                        &                  spacer(1,6),drmin(6),drmax(6))

                ENDIF
                !rm  augmenter ce test
                if(par%npr(1).ne.npr1) then
                    npr1=par%npr(1)

                    !.3.1.1. Appel a la routine qui trouve le point sur la boucle de la
                    !        separatrice interieure ou on doit diviser cette separatrice
                    !        en deux.

                    !.3.1.1. Find the point on the separatrix loop where the separatrix
                    !        should be split into two

                    !..Initialise the primary level line

                    nn=0
                    ipx = ptxext

                    DO ipas=equ%nptot(equ%ptsep(3,ipx),ipx),1,-1
                        nn=nn+1
                        grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
                        grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
                    end DO

                    DO ipas=2,equ%nptot(equ%ptsep(4,ipx),ipx)
                        nn=nn+1
                        grid%xn(nn)=equ%separx(ipas,equ%ptsep(4,ipx),ipx)
                        grid%yn(nn)=equ%separy(ipas,equ%ptsep(4,ipx),ipx)
                    end DO

                    !..Co-ordinates of the outer X-point

                    xptxex = equ%separx(1,equ%ptsep(3,ipx),ipx)
                    yptxex = equ%separy(1,equ%ptsep(3,ipx),ipx)

                    !..Points of the grid to be used for the first call to doubld

                    if(npr1.eq.0) then
                        ireg=1
                        nmail=min(41,nn/5)
                        nmail=nmail+mod(nmail+1,2)
                        ll=long(equ%separx(1,equ%ptsep(3,ipx),ipx), & 
                            &                equ%separy(1,equ%ptsep(3,ipx),ipx), & 
                            &                equ%nptot(equ%ptsep(3,ipx),ipx))
                        grid%xmail(1,1,ireg)=equ%separx(equ%nptot(equ%ptsep(3,ipx),ipx), & 
                            &          equ%ptsep(3,ipx),ipx)
                        grid%ymail(1,1,ireg)=equ%separy(equ%nptot(equ%ptsep(3,ipx),ipx), & 
                            &          equ%ptsep(3,ipx),ipx)
                        do imail=nmail/2,2,-1
                            dist=ll*(nmail/2-imail+1.)/(nmail/2)
                            CALL COORD(equ%separx(1,equ%ptsep(3,ipx),ipx), & 
                                &            equ%separy(1,equ%ptsep(3,ipx),ipx),equ%nptot(equ%ptsep(3,ipx),ipx), & 
                                &            dist,grid%xmail(imail,1,ireg),grid%ymail(imail,1,ireg))
                        enddo
                        grid%xmail(nmail/2+1,1,ireg)=xptxex
                        grid%ymail(nmail/2+1,1,ireg)=yptxex

                        ll=long(equ%separx(1,equ%ptsep(4,ipx),ipx), & 
                            &                equ%separy(1,equ%ptsep(4,ipx),ipx), & 
                            &                equ%nptot(equ%ptsep(4,ipx),ipx))
                        grid%xmail(nmail,1,ireg)=equ%separx(equ%nptot(equ%ptsep(4,ipx),ipx), & 
                            &          equ%ptsep(4,ipx),ipx)
                        grid%ymail(nmail,1,ireg)=equ%separy(equ%nptot(equ%ptsep(4,ipx),ipx), & 
                            &          equ%ptsep(4,ipx),ipx)
                        do imail=2,nmail/2
                            dist=ll*(imail-1.)/(nmail/2)
                            CALL COORD(equ%separx(1,equ%ptsep(3,ipx),ipx), & 
                                &            equ%separy(1,equ%ptsep(3,ipx),ipx),equ%nptot(equ%ptsep(3,ipx),ipx), & 
                                &            dist,grid%xmail(nmail/2+imail,1,ireg), & 
                                &            grid%ymail(nmail/2+imail,1,ireg))
                        enddo
                    endif

                    !..Second boundary curve

                    npcrb2=0
                    ipx = equ%ptxint

                    DO ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
                        npcrb2=npcrb2+1
                        xcrb2(npcrb2)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
                        ycrb2(npcrb2)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
                    end DO

                    DO ipas=2,equ%nptot(equ%ptsep(3,ipx),ipx)
                        npcrb2=npcrb2+1
                        xcrb2(npcrb2)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
                        ycrb2(npcrb2)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
                    end DO

                    DO ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
                        npcrb2=npcrb2+1
                        xcrb2(npcrb2)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
                        ycrb2(npcrb2)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
                    end DO

                    call trc_stk_in('maille','*114')
                    CALL DOUBLD(bouclx,boucly,grid%xn,grid%yn,nn,spacer(1,1), & 
                        &        par%npr(1),struct%inddef(idef),xext,yext,xextOffset,yextOffset,&
                        &        xptxex,yptxex,equ%xpto, & 
                        &        equ%ypto,equ%nx,equ%ny,equ%x,equ%y,equ%psi,struct%nstruc,struct%npstru,struct%xstruc, & 
                        &        struct%ystruc,equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
                        &        xcrb2,ycrb2,npcrb2)
                    call trc_stk_out
                endif

                ipx = equ%ptxint
                dist=0.0

                lg(3) = ruban(equ%separx(1,equ%ptsep(3,ipx),ipx), & 
                    &                equ%separy(1,equ%ptsep(3,ipx),ipx),equ%nptot(equ%ptsep(3,ipx),ipx) & 
                    &                ,bouclx,boucly,dist)
                CALL NUNIFO(par%nptseg(3),lg(3),par%deltp1(3),par%deltpn(3),spacep(1,3), & 
                    &               dpmin(3),dpmax(3))

                lg(4) = long(equ%separx(1,equ%ptsep(3,ipx),ipx),equ%separy(1,equ%ptsep(3,ipx) & 
                    &              ,ipx),equ%nptot(equ%ptsep(3,ipx),ipx)) - lg(3)
                CALL NUNIFO(par%nptseg(4),lg(4),par%deltp1(4),par%deltpn(4),spacep(1,4), & 
                    &               dpmin(4),dpmax(4))


                !.. Check & modify code parameters
                call check_and_modify_code_parameters(par, correct)
             end do




!..3.2  Distribute the points along separatrices

!..Determine the two parts of the inner separatrix which make a loop

!..Right part

         ipx = equ%ptxint

         nbcl(1) = 1
         xbcl(1,1) = equ%separx(1,equ%ptsep(3,ipx),ipx)
         ybcl(1,1) = equ%separy(1,equ%ptsep(3,ipx),ipx)

  120    CONTINUE

         nbcl(1) = nbcl(1) + 1
         xbcl(nbcl(1),1) = equ%separx(nbcl(1),equ%ptsep(3,ipx),ipx)
         ybcl(nbcl(1),1) = equ%separy(nbcl(1),equ%ptsep(3,ipx),ipx)

         lbcl = long(xbcl(1,1),ybcl(1,1),nbcl(1))

         IF (lbcl .LE. lg(3)) THEN
            GO TO 120
         ELSE
            xbcl(nbcl(1),1) = bouclx
            ybcl(nbcl(1),1) = boucly
         ENDIF

!..Left part

         ipx = equ%ptxint

         nbcl(2) = 1
         xbcl(1,2) = equ%separx(equ%nptot(equ%ptsep(3,ipx),ipx),equ%ptsep(3,ipx),ipx)
         ybcl(1,2) = equ%separy(equ%nptot(equ%ptsep(3,ipx),ipx),equ%ptsep(3,ipx),ipx)

  121    CONTINUE

         nbcl(2) = nbcl(2) + 1
         xbcl(nbcl(2),2) = equ%separx(equ%nptot(equ%ptsep(3,ipx),ipx) - nbcl(2) + 1, & 
     &                     equ%ptsep(3,ipx),ipx)
         ybcl(nbcl(2),2) = equ%separy(equ%nptot(equ%ptsep(3,ipx),ipx) - nbcl(2) + 1, & 
     &                     equ%ptsep(3,ipx),ipx)

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
             
           sepmax(1,isep) = equ%separx(1,equ%ptsep(isep,ipx),ipx)
           sepmay(1,isep) = equ%separy(1,equ%ptsep(isep,ipx),ipx)
           dist=0.

           DO 123 ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%nptot(equ%ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep),sepmay(ipas,isep))
  123      CONTINUE
           sepmax(par%nptseg(isep),isep)=equ%separx(equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &       equ%ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep),isep)=equ%separy(equ%nptot(equ%ptsep(isep,ipx),ipx), & 
     &       equ%ptsep(isep,ipx),ipx)
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

           sepmax(1,isep+4) = equ%separx(1,equ%ptsep(isep,ipx),ipx)
           sepmay(1,isep+4) = equ%separy(1,equ%ptsep(isep,ipx),ipx)
           dist=0.

           DO 127 ipas=2, par%nptseg(isep+4)-1
              dist=dist + spacep(ipas-1,isep+4)
              CALL COORD(equ%separx(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%separy(1,equ%ptsep(isep,ipx),ipx), & 
     &                   equ%nptot(equ%ptsep(isep,ipx),ipx),dist, & 
     &                   sepmax(ipas,isep+4),sepmay(ipas,isep+4))
  127      CONTINUE
           sepmax(par%nptseg(isep+4),isep+4)=equ%separx(equ%nptot(equ%ptsep(isep,ipx), & 
     &       ipx),equ%ptsep(isep,ipx),ipx)
           sepmay(par%nptseg(isep+4),isep+4)=equ%separy(equ%nptot(equ%ptsep(isep,ipx), & 
     &       ipx),equ%ptsep(isep,ipx),ipx)
  126    CONTINUE

!..3.3. Grid region by region

!..3.3.1 Region 1: between the separatrices

        ireg=1
        print*, 'ireg=', ireg
         nbcrb = 2

!.. The second boundary

         npcrb2=0
         ipx = ptxext

         DO 130 ipas=equ%nptot(equ%ptsep(3,ipx),ipx),1,-1
            npcrb2=npcrb2+1
            xcrb2(npcrb2)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            ycrb2(npcrb2)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
  130    CONTINUE

         DO 131 ipas=2,equ%nptot(equ%ptsep(4,ipx),ipx)
            npcrb2=npcrb2+1
            xcrb2(npcrb2)=equ%separx(ipas,equ%ptsep(4,ipx),ipx)
            ycrb2(npcrb2)=equ%separy(ipas,equ%ptsep(4,ipx),ipx)
  131    CONTINUE

!..Define the primary curve and the grid points

         ireg=1
         grid%np1(ireg) = 0

         IF (equ%ptxint .EQ. 1) THEN

            DO 132 ipas=par%nptseg(1), 2, -1
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
  132       CONTINUE

         ELSE IF (equ%ptxint .EQ. 2) THEN

            DO 133 ipas=par%nptseg(5), 2, -1
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,5)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,5)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 5
  133       CONTINUE

         ENDIF

         DO 134 ipas=1, par%nptseg(3)-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,3)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,3)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 3
  134    CONTINUE

         DO 135 ipas=par%nptseg(4), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,4)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,4)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 4
  135    CONTINUE

         IF (equ%ptxint .EQ. 1) THEN

            DO 136 ipas=1, par%nptseg(2)
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
  136       CONTINUE

         ELSE IF (equ%ptxint .EQ. 2) THEN

            DO 137 ipas=1, par%nptseg(6)
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,6)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,6)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 6
  137       CONTINUE

         ENDIF
         
!..Initialise the guard indices and starting target

         IF (equ%ptxint .EQ. 1) THEN
            idef = 1
            gardd2 = par%tgarde(2)
         ELSE IF (equ%ptxint .EQ. 2) THEN
            idef = 3
            gardd2 = par%tgarde(4)
         ENDIF
         gardd1 = par%tgarde(idef)

         ipx = equ%ptxint

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = equ%ptxint

         DO 140 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
  140    CONTINUE

         DO 141 ipas=2,equ%nptot(equ%ptsep(3,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
  141    CONTINUE

         DO 142 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
  142    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*145')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & 'droite')
         call trc_stk_out

         DO 145 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  145    CONTINUE

!..Parametrise the last level line

         nnlast=0
         ipx = ptxext

         DO 146 ipas=equ%nptot(equ%ptsep(3,ipx),ipx),1,-1
            nnlast=nnlast+1
            xnlast(nnlast)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            ynlast(nnlast)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
  146    CONTINUE

         DO 147 ipas=2,equ%nptot(equ%ptsep(4,ipx),ipx)
            nnlast=nnlast+1
            xnlast(nnlast)=equ%separx(ipas,equ%ptsep(4,ipx),ipx)
            ynlast(nnlast)=equ%separy(ipas,equ%ptsep(4,ipx),ipx)
  147    CONTINUE

         nuldec = .TRUE.

! figure out index of X-point on outer separatrix

         IF (equ%ptxint .EQ. 1) THEN
            xpind = par%nptseg(1)+par%nptseg(3)-1
         ELSE IF (equ%ptxint .EQ. 2) THEN
            xpind = par%nptseg(5)+par%nptseg(3)-1
         ENDIF

!..Call the routine which grids this region

         print*, 'ireg=', ireg
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast(1:nnlast), & 
              &               ynlast(1:nnlast),nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &               .true.,diag,ireg,struct, .false.)

!..Arangement of the mesh point which must coinside with outer X-point
         
         grid%xmail(xpind,par%npr(1),1) = xptxex
         grid%ymail(xpind,par%npr(1),1) = yptxex

         nuldec = .FALSE.

!!$         grid % nr = par % npr
!!$         grid % nptseg = par % nptseg
!!$         call writeGridStateToSiloFile('carreDEBUGMAIL0', equ, struct, grid)
!!$         call csioOpenFile('carreDEBUGMAIL1')
!!$         call siloWriteLineSegmentGridFromPoints( csioDbfile, 'primarylevelline', grid%xn(1:nn), grid%yn(1:nn) )
!!$         call siloWriteLineSegmentGridFromPoints( csioDbfile, 'lastlevelline', xnlast(1:nnlast), ynlast(1:nnlast) )
!!$         call siloWriteLineSegmentGridFromPoints( csioDbfile, 'firstgridline', &
!!$              &  grid%xmail(1:grid%np1(ireg),1,ireg), &
!!$              &  grid%ymail(1:grid%np1(ireg),1,ireg) )
!!$         call csioCloseFile()                                     
!!$         stop


!..3.3.2 Region 2: right

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=2
         grid%np1(ireg) = 0

         IF (equ%ptxint .EQ. 1) THEN

            DO 150 ipas=1,par%nptseg(1)+par%nptseg(3)-2
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = grid%xmail(ipas,par%npr(1),1)
               grid%ymail(grid%np1(ireg),1,ireg) = grid%ymail(ipas,par%npr(1),1)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = grid%radLineSepSeg(ipas, 1)
  150       CONTINUE

            DO 151 ipas=1, par%nptseg(5)
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,5)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,5)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 5
  151       CONTINUE

         ELSE IF (equ%ptxint .EQ. 2) THEN

            DO 152 ipas=1,par%nptseg(5)+par%nptseg(3)-2
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = grid%xmail(ipas,par%npr(1),1)
               grid%ymail(grid%np1(ireg),1,ireg) = grid%ymail(ipas,par%npr(1),1)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = grid%radLineSepSeg(ipas, 1)
  152       CONTINUE

            DO 153 ipas=1, par%nptseg(1)
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
  153       CONTINUE

         ENDIF

!..Initialise the guard indices and starting target

         IF (equ%ptxint .EQ. 1) THEN
            idef = 1
            gardd2 = par%tgarde(3)
         ELSE IF (equ%ptxint .EQ. 2) THEN
            idef = 3
            gardd2 = par%tgarde(1)
         ENDIF
         gardd1 = par%tgarde(idef)

         ipx = ptxext

         x2 = equ%separx(equ%nptot(equ%ptsep(3,ipx),ipx),equ%ptsep(3,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(3,ipx),ipx),equ%ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = ptxext

         DO 154 ipas=equ%nptot(equ%ptsep(3,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
  154    CONTINUE

         DO 155 ipas=2,equ%nptot(equ%ptsep(1,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
  155    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*157')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(3,ipx),ipx)-1,equ%ptsep(3,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(3,ipx),ipx)-1,equ%ptsep(3,ipx),ipx), &
              & 'droite')
         call trc_stk_out

         DO 157 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  157    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&     
              &.true.,diag,ireg, struct, .false.)

!..3.3.3 Region 3: top PFR

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=3
         grid%np1(ireg) = 0

         DO 160 ipas=par%nptseg(1),2,-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
  160    CONTINUE

         DO 161 ipas=1, par%nptseg(2)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
  161    CONTINUE

!..Initialise the guard indices and starting target

         idef = 1
         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

         ipx = 1

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)


!..Initialise the primary level line

         nn=0
         ipx = 1

         DO 164 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
  164    CONTINUE

         DO 165 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
  165    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*167')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & 'gauche')
         call trc_stk_out

         DO 167 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  167    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &.false.,diag,ireg, struct, .false.)


!..3.3.4 Region 4: left

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=4
         grid%np1(ireg) = 0

         IF (equ%ptxint .EQ. 1) THEN

            DO 170 ipas=par%nptseg(1)+par%nptseg(2)+par%nptseg(3)+par%nptseg(4)-3, & 
     &                             par%nptseg(1)+par%nptseg(3), -1
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = grid%xmail(ipas,par%npr(1),1)
               grid%ymail(grid%np1(ireg),1,ireg) = grid%ymail(ipas,par%npr(1),1)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = grid%radLineSepSeg(ipas, 1)
  170       CONTINUE

            DO 171 ipas=1, par%nptseg(6)
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,6)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,6) 
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 6
  171       CONTINUE

         ELSE IF (equ%ptxint .EQ. 2) THEN

            DO 172 ipas=par%nptseg(5)+par%nptseg(6)+par%nptseg(3)+par%nptseg(4)-3, & 
     &                             par%nptseg(5)+par%nptseg(3), -1
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = grid%xmail(ipas,par%npr(1),1)
               grid%ymail(grid%np1(ireg),1,ireg) = grid%ymail(ipas,par%npr(1),1)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = grid%radLineSepSeg(ipas-1, 1)
  172       CONTINUE

            DO 173 ipas=1, par%nptseg(2)
               grid%np1(ireg) = grid%np1(ireg)+1
               grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,2)
               grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,2)
               grid%radLineSepSeg(grid%np1(ireg), ireg) = 2
  173       CONTINUE

         ENDIF

!..Initialise the guard indices and starting target

         IF (equ%ptxint .EQ. 1) THEN
            idef = 2
            gardd2 = par%tgarde(4)
         ELSE IF (equ%ptxint .EQ. 2) THEN
            idef = 4
            gardd2 = par%tgarde(2)
         ENDIF
         gardd1 = par%tgarde(idef)

         ipx = ptxext

         x2 = equ%separx(equ%nptot(equ%ptsep(4,ipx),ipx),equ%ptsep(4,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(4,ipx),ipx),equ%ptsep(4,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = ptxext

         DO 174 ipas=equ%nptot(equ%ptsep(4,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(4,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(4,ipx),ipx)
  174    CONTINUE

         DO 175 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
  175    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*177')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2, &
              & equ%separx(equ%nptot(equ%ptsep(4,ipx),ipx)-1,equ%ptsep(4,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(4,ipx),ipx)-1,equ%ptsep(4,ipx),ipx), &
              & 'gauche')
         call trc_stk_out

         DO 177 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  177    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &.true.,diag,ireg, struct, .false.)

!..3.3.5 Region 5: bottom PFR

         nbcrb = 1

!..Define the primary curve and the grid points

         ireg=5
         grid%np1(ireg) = 0

         DO 180 ipas=par%nptseg(5), 2, -1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,5)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,5)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 5
  180    CONTINUE

         DO 181 ipas=1, par%nptseg(6)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,6)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,6)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 6               
  181    CONTINUE

!..Initialise the guard indices and starting target

         idef = 3
         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(4)

         ipx = 2

         x2 = equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)
         y2 = equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx),equ%ptsep(1,ipx),ipx)

!..Initialise the primary level line

         nn=0
         ipx = 2

         DO 184 ipas=equ%nptot(equ%ptsep(1,ipx),ipx),1,-1
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(1,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(1,ipx),ipx)
  184    CONTINUE

         DO 185 ipas=2,equ%nptot(equ%ptsep(2,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(2,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(2,ipx),ipx)
  185    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*187')
         sens = drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)), & 
              & struct%npstru(struct%inddef(idef)),x2,y2,&
              & equ%separx(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & equ%separy(equ%nptot(equ%ptsep(1,ipx),ipx)-1,equ%ptsep(1,ipx),ipx), &
              & 'gauche')
         call trc_stk_out

         DO 187 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  187    CONTINUE

!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &               grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &               equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &               equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &               ynlast,nnlast,nuldec,&
              &               xpind,xptxex,yptxex,&
              &.false.,diag,ireg, struct, .false.)

!..3.3.6  Region 6: central region

!..Define the primary curve and the grid points

         ireg=6
         grid%np1(ireg) = 0
         ipx = equ%ptxint

         DO 190 ipas=1, par%nptseg(3) - 1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,3)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,3)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 3
  190    CONTINUE

         DO 191 ipas=par%nptseg(4),1,-1
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,4)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,4)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 4
  191    CONTINUE

         x2 = equ%separx(1,equ%ptsep(3,ipx),ipx)
         y2 = equ%separy(1,equ%ptsep(3,ipx),ipx)

!..Initialise the primary level line

         nn=0

         DO 192 ipas=1,equ%nptot(equ%ptsep(3,ipx),ipx)
            nn=nn+1
            grid%xn(nn)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
            grid%yn(nn)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
  192    CONTINUE

!..Relate the desirable sweeping direction to the structure orientation

         DO 197 ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  197    CONTINUE


!..Call the routine which grids this region

!---
         print*, 'ireg=', ireg
!---
         CALL MAILCN(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,par%pntrat, & 
     &      pas,grid%np1(ireg),par%npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &      equ%nx,equ%ny,equ%x,equ%y,equ%psi,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
     &      equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
     &      xptxo,yptxo,equ%xpto,equ%ypto,struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,equ%distxo,diag,ireg)


         ! Check whether we forgot to set radLineSepSeg somewhere
         do ireg = 1, grid%nreg
             do ipas = 1, grid%np1(ireg)
                 if (grid%radLineSepSeg(ipas, ireg) == 0) then
                     call logmsg(LOGDEBUG, "radLineSepSeg not set: ireg=" // int2str(ireg) // &
                          & ", ipol=" // int2str(ipas))
                 end if
             end do
         end do

!----------------------------------------------------------------------

      ELSE IF (equ%limcfg.eq.1) THEN

!----------------------------------------------------------------------
!..4.   Limiter configuration

         nbcrb = 1
         grid%nreg = 2
         equ%nsep = 1
         ipx = 1
         isep=equ%nsep

!..The distance between the X-point and the O-point

         xx = struct%nivx(1,1)
         yy = struct%nivy(1,1)

         xptxo = equ%xpto - xx
         yptxo = equ%ypto - yy

         equ%distxo = SQRT((xptxo)**2 + (yptxo)**2)

!..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/equ%distxo
         yptxo = yptxo/equ%distxo

!..Calculate the equ%psi value at the X-point

         ii = ifind(xx,equ%x,equ%nx,1)
         jj = ifind(yy,equ%y,equ%ny,1)

         fctini = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xx + equ%a01(ii,jj,1)*yy + & 
     &            equ%a11(ii,jj,1)*xx*yy


!..Calculate the length of each separatrix

         lg(isep) = long(struct%nivx(1,1),struct%nivy(1,1),struct%nivtot(1))
         
         !..4.1  Read all the necessary data from the file
         
         !.. Read initial set of code parameters
         call read_code_parameters(par, equ%distxo)

         correct = .false.
         do while (.not. correct)

             !..Calculate the equ%psi difference between the penetration values

             IF (par%repart .EQ. 2) THEN

                 xfin = xx + xptxo*par%pntrat
                 yfin = yy + yptxo*par%pntrat

                 ii = ifind(xfin,equ%x,equ%nx,1)
                 jj = ifind(yfin,equ%y,equ%ny,1)

                 fctfin = equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xfin + & 
                     &               equ%a01(ii,jj,1)*yfin + equ%a11(ii,jj,1)*xfin*yfin

                 difpsi = fctfin - fctini

             ENDIF

             !..Calculate the intervals, dmin and dmax

             !..Along the separatrices

             CALL NUNIFO(par%nptseg(1),lg(1),par%deltp1(1),par%deltpn(1),spacep(1,1), & 
                 &               dpmin(1),dpmax(1))


             !..Radial direction

             CALL NUNIFO(par%npr(1),struct%distnv(par%repart,1),par%deltr1(1),par%deltrn(1), & 
                 &               spacer(1,1),drmin(1),drmax(1))

             IF (par%repart .EQ. 1) THEN

                 CALL NUNIFO(par%npr(2),par%pntrat,par%deltr1(2),par%deltrn(2), & 
                     &                  spacer(1,2),drmin(2),drmax(2))

             ELSE IF (par%repart .EQ. 2) THEN

                 CALL NUNIFO(par%npr(2),difpsi,par%deltr1(2),par%deltrn(2), & 
                     &                  spacer(1,2),drmin(2),drmax(2))

             ENDIF

             !.. Check & modify code parameters
             call check_and_modify_code_parameters(par, correct)
         end do


!..4.2  Distribute the points along separatrices

         DO isep=1, equ%nsep

           sepmax(1,isep) = struct%nivx(1,1)
           sepmay(1,isep) = struct%nivy(1,1)
           dist=0.

           DO ipas=2, par%nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(struct%nivx(1,1),struct%nivy(1,1),struct%nivtot(1), & 
     &          dist,sepmax(ipas,isep),sepmay(ipas,isep))
           ENDDO

           sepmax(par%nptseg(isep),isep)=struct%nivx(struct%nivtot(1),1)
           sepmay(par%nptseg(isep),isep)=struct%nivy(struct%nivtot(1),1)
         ENDDO

!..4.3. Grid region by region

!..4.3.1  Region 1

!..Define the primary curve and the grid points

         ireg=1
         grid%np1(ireg) = 0

         DO ipas=1, par%nptseg(1)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
         ENDDO

!..Go along the target 1

         idef = 1

         x2 = struct%nivx(1,1)
         y2 = struct%nivy(1,1)
!***
!        print*,'x2, y2=',x2,y2
!***

!..Initialise the primary level line

         nn=0

         DO ipas=1,struct%nivtot(1)
            nn=nn+1
            grid%xn(nn)=struct%nivx(ipas,1)
            grid%yn(nn)=struct%nivy(ipas,1)
         ENDDO

!..Initialise the guard indices

         gardd1 = par%tgarde(idef)
         gardd2 = par%tgarde(2)

!..Determine the structure orientation

!!!!
!        sens =-drctio(struct%xstruc(1,struct%inddef(idef)),struct%ystruc(1,struct%inddef(idef)),
!    .                 struct%npstru(struct%inddef(idef)),x2,y2,'droite')

         sens=1
         if (.not. extended_grid ) then
            sens=horair(equ%xpto,equ%ypto,x2,y2,struct%xstruc(1,struct%inddef(idef)), & 
                 &     struct%ystruc(1,struct%inddef(idef)),struct%npstru(struct%inddef(idef)),sens)            
         end if

!***
!        print*,'dans maille: sens=',sens
!***

         DO ipas=1, par%npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

         ENDDO

!..Call the routine which grids this region

!***
!        print*,'call mailrg'
!        print*,'ireg, idef, struct%inddef=',ireg,idef,struct%inddef(idef)
!        print*,'grid%np1, npr=',grid%np1(ireg),npr(ireg)
!***
!---
         print*, 'ireg=', ireg
!---
         CALL MAILRG(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,sens,pas, & 
              &              grid%np1(ireg),par%npr(ireg),struct%inddef(idef),x2,y2,equ%nx,equ%ny, & 
              &              equ%x,equ%y,equ%psi,equ%xpto,equ%ypto,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
              &              equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
              &              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast, & 
              &              ynlast,nnlast,nuldec,&
              &              xpind,xptxex,yptxex,&
              &.false.,diag,ireg, struct, extended_grid)

!..4.3.2  Region 2

!..Define the primary curve and the grid points

         ireg=2
         grid%np1(ireg) = 0

         DO ipas=1, par%nptseg(1)
            grid%np1(ireg) = grid%np1(ireg)+1
            grid%xmail(grid%np1(ireg),1,ireg) = sepmax(ipas,1)
            grid%ymail(grid%np1(ireg),1,ireg) = sepmay(ipas,1)
            grid%radLineSepSeg(grid%np1(ireg), ireg) = 1
         ENDDO

         x2 = struct%nivx(1,1)
         y2 = struct%nivy(1,1)

!..Initialise the primary level line

         nn=0

         DO ipas=1,struct%nivtot(1)
            nn=nn+1
            grid%xn(nn)=struct%nivx(nn,1)
            grid%yn(nn)=struct%nivy(nn,1)
         ENDDO

!..Definition des pas pour le balayage vers le centre
         DO ipas=1, par%npr(ireg)-1
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
         CALL MAILCN(grid%xmail(1,1,ireg),grid%ymail(1,1,ireg),grid%xn,grid%yn,nn,par%pntrat, & 
     &      pas,grid%np1(ireg),par%npr(ireg),x2,y2,xfin,yfin,fctini, & 
     &      equ%nx,equ%ny,equ%x,equ%y,equ%psi,struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
     &      equ%a00,equ%a10,equ%a01,equ%a11,par%repart, & 
     &      xptxo,yptxo,equ%xpto,equ%ypto,struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,equ%distxo,diag,ireg)

!       ...
      ENDIF
!======================================================================

!  6. Compute equ%psi and grad equ%psi at the grid points

      call compute_psi_on_grid( equ, grid )

      ! After finalization of gridding, set resolution arrays in grid data
      ! structure accordingly
      grid % nr = par % npr
      grid % nptseg = par % nptseg

!c<<<
!      write(0,*) '<=== Leaving maille'
!c>>>
      RETURN
!======================================================================

contains

  !> Read code parameters from carre.dat and allow user to change them.
  !> TODO: move this to carre_parameter_io module 
  subroutine read_code_parameters(par, distxo)
    implicit none
    type(CarreParameters), intent(inout) :: par
    real*8, intent(inout) :: distxo

    ! internal
    integer :: ient,isor,ifail
    integer :: iseg


#ifdef CARRE_NONINTERACTIVE 
    ! For non-interactive use the code parameters are initialized 
    ! at the entry into the ITMCARRE main subroutine.
    ! Do not read from file or user.
    return
#else

    par%tgarde=0
    
!..1.1  Read all the necessary data from the file

         ient = 9
         isor = 0
         CALL CHANGE(par,ient,isor,ifail)

!..Check whether all the data have been read from the file

         IF (ifail .EQ. 1) THEN

           if(sellan(1:8).eq.'francais') then
             CALL LECCLF(par%nptseg,par%npr,lg,&
                 & par%deltp1,par%deltpn,par%deltr1,equ%limcfg, & 
                 & par%deltrn,par%repart,par%pntrat,par%tgarde,&
                 & struct%distnv,xptxo,yptxo, & 
                 & distxo,xx,yy,fctini,difpsi, &
                 & equ%a00,equ%a10,equ%a01,equ%a11,nxmax,nymax, & 
                 & equ%npx,equ%racord,equ%x,equ%y,equ%nx,equ%ny)
           elseif(sellan(1:7).eq.'english') then
             CALL LECCLE(par%nptseg,par%npr,lg,&
                 & par%deltp1,par%deltpn,par%deltr1,equ%limcfg, & 
                 & par%deltrn,par%repart,par%pntrat,par%tgarde,&
                 & struct%distnv,xptxo,yptxo, & 
                 & distxo,xx,yy,fctini,difpsi, &
                 & equ%a00,equ%a10,equ%a01,equ%a11,nxmax,nymax, & 
                 & equ%npx,equ%racord,equ%x,equ%y,equ%nx,equ%ny)
           endif

         ENDIF
#endif

    ! If we do an extended grid, modify the code parameters before starting the gridding
    if (par%gridExtensionMode /= GRID_EXTENSION_OFF) then
       
        call logmsg(LOGINFO, "read_code_parameters: adjusting deltp1, deltpn for &
             &use with extended grid algorithm")

        do iseg = 1, size(lg)
            if ( lg(iseg) == 0.0d0 ) exit
            par%deltp1(iseg) = lg(iseg) / (par%nptseg(iseg) - 1)
            par%deltpn(iseg) = lg(iseg) / (par%nptseg(iseg) - 1)
        end do
    end if

  end subroutine read_code_parameters


  !> Perform sanity checks on parameters and let the user modify parameters interactively.
  !> TODO: this routine should be moved into the carre_parameter_io module.
  subroutine check_and_modify_code_parameters(par, correct)
    implicit none
    type(CarreParameters), intent(inout) :: par
    logical, intent(out) :: correct

#ifdef CARRE_NONINTERACTIVE
    ! For ITMCARRE, the code parameters cannot be modified by the user
    correct = .true.

!!$    ! But we still want to write out the carre.out file
!!$    CALL SORTIE(equ, grid, diag, par, 1)

    return
#else  

    CALL RAPPEL(par,&
         & lg,difpsi,struct%distnv,grid%nreg,equ%nsep,equ%npx,&
         & dpmin,dpmax,drmin,drmax,equ%distxo,6,correct)

    !..Initialise the primary level line

    nn1=0

    DO ipas=1,equ%nptot(equ%ptsep(3,ipx),ipx)
        nn1=nn1+1
        grid%xn(nn1)=equ%separx(ipas,equ%ptsep(3,ipx),ipx)
        grid%yn(nn1)=equ%separy(ipas,equ%ptsep(3,ipx),ipx)
    end do

    ! on colle la dernière ligne de niveau sur trace2 pour avoir
    ! la pénétration.

    call trace3(equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny),equ%separx,equ%separy, & 
         &        equ%ptsep,equ%npx,equ%nptot, & 
         &        struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
         &        struct%nivx,struct%nivy,struct%nivtot,struct%nbniv, & 
         &         par%pntrat,equ%distxo,grid%xn,grid%yn,nn1, & 
         &         par%repart,xptxo,yptxo,fctini,xfin,yfin,fctfin, & 
         &         equ%a00,equ%a01,equ%a10,equ%a11,equ%psi,equ%nx,equ%ny,equ%x,equ%y)

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
        CALL CHANGE(par,ient,isor,ifail)
        if (par%pntrat.ne.pntrat_old) then

            call endpag

            call trace2(equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny), & 
                 &     equ%separx,equ%separy,equ%ptsep,equ%npx,equ%nptot, & 
                 &         struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc,struct%nivx,struct%nivy, & 
                 &         struct%nivtot,struct%nbniv)

        endif

        ! parameters have been changed and state has to be recomputed
        correct = .false.
    else
        ! Parameters ok, write them to output file
        !..Save the chosen parameters
!!$        CALL SORTIE(equ, grid, diag, par, 1)
    ENDIF

301 FORMAT(//T2,'Est-ce que ces valeurs sont correctes? (o/n)')
300 format(//T2,'Do you wish to accept these values (y/n)?')
302 FORMAT(A)

#endif

  end subroutine check_and_modify_code_parameters

!!$  ! Modify code parameters in order to match 
!!$  subroutine setupCodeParametersForExtendedGrid(par, equ)
!!$    type(CarreParameters), intent(inout) :: par
!!$    type(CarreEquilibrium), intent(inout) :: equ
!!$
!!$    integer :: isep
!!$    double precision :: 
!!$
!!$    do isep = 1, 4
!!$        lg(isep) = long(equ%separx(1,equ%ptsep(isep,ipx),ipx), &
!!$             & equ%separy(1,equ%ptsep(isep,ipx),ipx), &
!!$             & equ%nptot(equ%ptsep(isep,ipx),ipx))
!!$    end do
!!$
!!$  end subroutine setupCodeParametersForExtendedGrid

END subroutine
