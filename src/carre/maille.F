      SUBROUTINE MAILLE(nx,ny,x,y,psi,npx,xpto,ypto,racord,
     .     separx,separy,ptsep,nptot,distnv,ptxint,nstruc,npstru,
     .     xstruc,ystruc,inddef,nreg,xn,yn,xmail,ymail,
     .     np1,npr,ptx,pty,nivx,nivy,nivtot,nbniv,
     .     a00,a10,a01,a11,fctpx,limcfg,gdpsi,racpsi,
     .     a,gdr,r,ra,rho,somort,somortp,
     .     gdsomortp,somortpur,somortpurp,gdsomortpurp,sompropo,
     .     sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .     somtot,somtotp,gdsomtotp,segt,ireg,modif,xmini,xmaxi,
     .     ymini,ymaxi,distxo,pntrat,nprsb,tgarde)
c
c  version : 23.06.98 19:53
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

c*** This subroutine makes the orthogonal curvilinear grid for
c*** various possible magnetic configurations (single null,
c*** connected double null, disconnected double null, or limiter)
c======================================================================
      IMPLICIT NONE

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>


*  arguments
      INTEGER nx,ny,npx,ptsep(4,npx),nptot(4,npx),ptxint,
     .        nstruc,npstru(nstruc),nreg,inddef(4),
     .        np1(nregmx),npr(nregmx),nivtot(nivmx),nbniv,limcfg,nn1,
     .        ireg,modif,nprsb(nregmx)

      LOGICAL racord
      REAL(kind=R8) x(nxmax),y(nymax),psi(nxmax,nymax),
     .  xpto,ypto,separx(npnimx,4,npx),
     .  separy(npnimx,4,npx),xstruc(npstmx,nstruc),pty(npx+1),
     .  ystruc(npstmx,nstruc),xn(npnimx),yn(npnimx),ptx(npx+1),
     .  xmail(npmamx,nrmamx,nregmx),ymail(npmamx,nrmamx,nregmx),
     .  distnv(5,nivmx),
     .  nivx(npnimx,nivmx),nivy(npnimx,nivmx),fctpx(npx+1),
     .  a00(nxmax,nymax,3),a10(nxmax,nymax,3),
     .  a01(nxmax,nymax,3),a11(nxmax,nymax,3),
     .  gdpsi(nrmamx,nregmx),racpsi(nrmamx,nregmx),
     .  a(nregmx),
     .  gdr(nrmamx,nregmx),
     .  r(nrmamx,nregmx),ra(nrmamx,nregmx),
     .  rho(nrmamx,nregmx),somort(nrmamx,nregmx),somortp(nrmamx,nregmx),
     .  gdsomortp(nregmx),somortpur(nrmamx,nregmx),
     .  somortpurp(nrmamx,nregmx),gdsomortpurp(nregmx),
     .  sompropo(nrmamx,nregmx),sompropop(nrmamx,nregmx),
     .  gdsompropop(nregmx),
     .  somvarr(nrmamx,nregmx),somvarrp(nrmamx,nregmx),
     .  gdsomvarrp(nregmx),
     .  somtot(nrmamx,nregmx),somtotp(nrmamx,nregmx),
     .  gdsomtotp(nregmx),segt(nrmamx,nregmx),
     .  xmini,xmaxi,ymini,ymaxi,distxo,pntrat,tgarde(4)


*  variables en common
#include <COMLAN.F>
#include <COMRLX.F>

*  variables locales
      INTEGER nptseg(10),isep,ipas,ipx,sens,nn,idef,
     .  repart,nsep,ii,jj,ient,isor,ifail,nbcrb,npcrb2
     .  ,ptxext,i,nbcl(2),nnlast,npr1,nmail,imail
      REAL(kind=R8) dist,x2,y2,lg(10),deltp1(10),deltpn(10),
     .  deltr1(10),deltrn(10),xx,yy,fctini,fctfin,difpsi,dpmin(10),
     .  dpmax(10),drmin(10),drmax(10),xfin,yfin,gardd1,
     .  gardd2,xptxo,yptxo,xint,yint,xext,yext,psiint,
     .  psiext,xptxex,yptxex,bouclx,boucly,ll,pntrat_old,psi_X(2),psi_O
      REAL(kind=R8) sepmax(npnimx,8),sepmay(npnimx,8),spacep(npmamx,10),
     .  spacer(npmamx,10),pas(nrmamx),xcrb2(npnimx),ycrb2(npnimx)
     .  ,xbcl(npnimx,2),ybcl(npnimx,2),lbcl,xnlast(npnimx),
     .  ynlast(npnimx)
      CHARACTER*3 rep
      LOGICAL nuldec, correct

*  procedures
      INTEGER drctio,horair,ifind
      REAL(kind=R8) long,plqdst,ruban
      EXTERNAL long,COORD,drctio,horair,ifind,LECCLE,lecclf,DOUBLD,
     .         ruban,plqdst,trace3
     ,        ,trc_stk_in,trc_stk_out
c=========================
c.. npnimx: <=> npnimx
c.. nreg  : number of regions
c.. xmail,ymail: coordinates of the grid points
c                (poloidal index, radial index, region index)
c.. xn,yn : working arrays for coordinates along a parametrised curve
c.. nn    : number of points on the same curve
c.. np1   : number of points in poloidal direction
c.. npr   : number of points in radial direction
c.. tgarde: guard length for each divertor target
c.. nptseg: number of points along differents segments of separatrix
c.. ireg  : region index
c.. ipx   : X-point index
c.. ptxext: index of the outer X-point
c.. sens  : direction of movement along a target:
c           1=the same as the target points, 2=the opposite
c.. idef  : index of the target from where the routine starts
c.. a00,a10,a01,a11: coefficients.
c.. repart: selector for the radial point distribution
c           1=absolute distance, 2=difference in psi
c.. nsep  : number of the separatrices per the configuration
c.. nbcrb : number of boundary lines
c.. ii,jj : cell identification indices
c.. npmamx: maximun number of intervals
c.. ient,isor: input and output unit numbers
c.. ifail : Check whether the data should be read from the keyboard
c           because the file is incomplete
c           (0=file is acceptable, 1=unacceptable)
c.. x2,y2 : starting point for routine marche
c.. pntrat: penetration value of the X-point relative to the O-point
c.. lg    : length of each separatrix
c.. deltp1,deltpn: values of the first and last intervals for each sep.
c.. deltr1,deltrn: values of the first and last intervals for each reg.
c.. dpmin,dpmax: values of the minimal and maximal interval widths
c..
c.. xnlast,ynlast: dans le cas du double nul decon., derniere courbe de
c                  niveau lorsqu'on maille la region entre les deux
c                  separatrices.
c.. nnlast: number of points on this curve
c.. nuldec: variable logique qui nous dit si oui ou non on est dans la
c           region entre les 2 separatrice du double nul deconnecte.
c======================================================================
*
*..calculs
cc<<<
c      write(0,*) '===> Entering maille.  npx, limcfg = ',npx,limcfg
cc>>>

      nuldec = .FALSE.

*..On procede au cas par cas selon la configuration des separatrices.

c----------------------------------------------------------------------

      IF (npx.EQ.1 .and. limcfg.eq.0) THEN

c----------------------------------------------------------------------
*..1.   Single null

         nbcrb = 1
         nreg = 3
         nsep = 3
         ipx = 1

*..The distance between the X-point and the O-point

         xx = separx(1,ptsep(3,ipx),ipx)
         yy = separy(1,ptsep(3,ipx),ipx)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

*..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

*..Psi value at the X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy +
     .            a11(ii,jj,1)*xx*yy

*..1.1  Read all the necessary data from the file

         ient = 9
         isor = 0

         tgarde(1)=0
         tgarde(2)=0
         tgarde(3)=0
         tgarde(4)=0
         CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .               pntrat,tgarde,distxo,psi_X,psi_O,ient,isor,ifail)

*..Calculate the length of each separatrix

         DO 1 isep = 1, 3

            lg(isep) = long(separx(1,ptsep(isep,ipx),ipx),
     .                 separy(1,ptsep(isep,ipx),ipx),
     .                 nptot(ptsep(isep,ipx),ipx))

    1    CONTINUE

*..Check whether all the data have been read from the file

         IF (ifail .EQ. 1) THEN

           if(sellan(1:8).eq.'francais') then
             CALL LECCLF(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .         deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .         distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .         npx,racord,x,y,nx,ny)
           elseif(sellan(1:7).eq.'english') then
             CALL LECCLE(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .         deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .         distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .         npx,racord,x,y,nx,ny)
           endif

         ENDIF

    3    CONTINUE

*..Calculate the psi difference between the penetration values

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin +
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

         ENDIF

*..Calculate the intervals, dmin and dmax

*..Along the separatrices

         CALL NUNIFO(nptseg(1),lg(1),deltp1(1),deltpn(1),spacep(1,1),
     .               dpmin(1),dpmax(1))

         CALL NUNIFO(nptseg(2),lg(2),deltp1(2),deltpn(2),spacep(1,2),
     .               dpmin(2),dpmax(2))

         CALL NUNIFO(nptseg(3),lg(3),deltp1(3),deltpn(3),spacep(1,3),
     .               dpmin(3),dpmax(3))

*..Radial direction

         CALL NUNIFO(npr(1),distnv(repart,1),deltr1(1),deltrn(1),
     .               spacer(1,1),drmin(1),drmax(1))

         CALL NUNIFO(npr(2),distnv(repart,2),deltr1(2),deltrn(2),
     .               spacer(1,2),drmin(2),drmax(2))

         IF (repart .EQ. 1) THEN

            CALL NUNIFO(npr(3),pntrat,deltr1(3),deltrn(3),
     .                  spacer(1,3),drmin(3),drmax(3))

         ELSE IF (repart .EQ. 2) THEN

            CALL NUNIFO(npr(3),difpsi,deltr1(3),deltrn(3),
     .                  spacer(1,3),drmin(3),drmax(3))

         ENDIF

         CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,6,correct)

*..Initialise the primary level line

         nn1=0

         DO 361 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn1=nn1+1
            xn(nn1)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn1)=separy(ipas,ptsep(3,ipx),ipx)
  361    CONTINUE

*..we append the last level line to trace2 to have the penetration depth

         if (nn1.gt.1) call trace3(
     _         nstruc,npstru,xstruc,ystruc,
     _         pntrat,distxo,xn,yn,nn1,
     .         xfin,yfin,fctfin,
     .         psi,nx,ny,x,y)

         if (correct) then
           if(sellan(1:8).eq.'francais') then
             WRITE(6,301)
  301        FORMAT(//T2,'Est-ce que ces valeurs sont correctes? (o/n)')
           elseif(sellan(1:7).eq.'english') then
             WRITE(6,300)
  300        format(//T2,'Do you wish to accept these values (y/n)?')
           endif
           READ(5,302)rep
  302      FORMAT(A)
         endif

         if(rep(1:1).eq.'n' .or. rep(1:1).eq.'N' .or. .not.correct) then

            ient = 5
            isor = 6

            pntrat_old = pntrat

            CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .                  pntrat,tgarde,distxo,psi_X,psi_O,
     .                  ient,isor,ifail)

            if (pntrat.ne.pntrat_old) then
              call endpag

              call trace2(x(1),x(nx),y(1),y(ny),
     .                    separx,separy,ptsep,npx,nptot,
     .                    nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .                    nivtot,nbniv)

            endif

            GO TO 3

         else
            CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,10,correct)

         ENDIF

*..Save the chosen parameters

         CALL SORTIE(nsep,nreg,nptseg,npr,np1,deltp1,deltpn,deltr1,
     .     deltrn,pntrat,tgarde,distxo,repart,xmail,ymail,nx,ny,
     .     x,y,a00,a10,a01,a11,ptx,pty,npx,racord,1,fctpx,
     .     gdpsi,racpsi,
     .     a,gdr,r,
     .     ra,rho,
     .     somort,somortp,
     .     gdsomortp,somortpur,somortpurp,gdsomortpurp,
     .     sompropo,sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .     somtot,somtotp,gdsomtotp,segt)


*..1.2  Distribute the points along separatrices

         DO 6 isep=1, 3

           sepmax(1,isep) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 5 ipas=2, nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx),
     .          separy(1,ptsep(isep,ipx),ipx),nptot(ptsep(isep,ipx),ipx)
     .         ,dist,sepmax(ipas,isep),sepmay(ipas,isep))
    5      CONTINUE

           sepmax(nptseg(isep),isep)=separx(nptot(ptsep(isep,ipx),ipx),
     .       ptsep(isep,ipx),ipx)
           sepmay(nptseg(isep),isep)=separy(nptot(ptsep(isep,ipx),ipx),
     .       ptsep(isep,ipx),ipx)
    6    CONTINUE

*..1.3  Grid region by region

*..1.3.1  Region 1

*.. Define the primary curve and the grid points
         ireg=1
         print*, 'ireg=', ireg
#ifdef MODIF
    8    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=9,err=8) modif
    9   continue
#else
         modif = 0
#endif

         np1(ireg) = 0

         DO 10 ipas=nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   10    CONTINUE

         DO 11 ipas=2, nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   11    CONTINUE

         DO 12 ipas=2, nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   12    CONTINUE


*..Go along the target 1

         idef = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(2)

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*17')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 17 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   17    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .              np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .              x,y,psi,nstruc,npstru,xstruc,ystruc,
     .              a00,a10,a01,a11,repart,
     .              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .              ynlast,nnlast,nuldec,1,xpto,ypto,gdpsi(1,ireg),
     .              racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .              rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .              gdsomortp(ireg),somortpur(1,ireg),
     .              somortpurp(1,ireg),gdsomortpurp(ireg),
     .              sompropo(1,ireg),sompropop(1,ireg),
     .              gdsompropop(ireg),
     .              somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .              somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .              segt(1,ireg),ireg,modif,xmini,xmaxi,
     .              ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .              nivx,nivy,
     .              nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

c
c  1.3.2  region 2

*..Define the primary curve and the grid points
         ireg=2
         print*, 'ireg=', ireg
         modif=0
#ifdef MODIF
   18    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=19,err=18) modif
   19    continue
#endif

         np1(ireg) = 0

         DO 20 ipas=nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   20    CONTINUE

         DO 21 ipas=2, nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   21    CONTINUE

*..Go along the target 1

         idef = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(2)

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*25 ')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 25 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   25    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .              np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .              x,y,psi,nstruc,npstru,xstruc,ystruc,
     .              a00,a10,a01,a11,repart,
     .              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .              ynlast,nnlast,nuldec,0,xpto,ypto,gdpsi(1,ireg),
     .              racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .              rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .              gdsomortp(ireg),somortpur(1,ireg),
     .              somortpurp(1,ireg),gdsomortpurp(ireg),
     .              sompropo(1,ireg),sompropop(1,ireg),
     .              gdsompropop(ireg),somvarr(1,ireg),somvarrp(1,ireg),
     .              gdsomvarrp(ireg),somtot(1,ireg),somtotp(1,ireg),
     .              gdsomtotp(ireg),segt(1,ireg),ireg,modif,xmini,xmaxi,
     .              ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .              nivx,nivy,
     .              nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0
c
c  1.3.3  region 3

*..Define the primary curve and the grid points

         ireg=3
         np1(ireg) = 0

         DO 30 ipas=1, nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   30    CONTINUE

         x2 = separx(1,ptsep(3,ipx),ipx)
         y2 = separy(1,ptsep(3,ipx),ipx)

*..Initialise the primary level line

         nn=0

         DO 31 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
   31    CONTINUE

*..Relate the desirable sweeping direction to the structure orientation

         DO 32 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   32    CONTINUE

*..Call the routine which grids this region

c---
         print*, 'ireg=', ireg
c---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,pntrat,
     .       pas,np1(ireg),npr(ireg),x2,y2,fctini,
     .       nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc,
     .       a00,a10,a01,a11,repart,
     .       xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,
     .       gdpsi(1,ireg),racpsi(1,ireg),
     .       a,gdr(1,ireg),r(1,ireg),ra(1,ireg),rho(1,ireg),
     .       somort(1,ireg),somortp(1,ireg),gdsomortp(ireg),
     .       somortpur(1,ireg),somortpurp(1,ireg),gdsomortpurp(ireg),
     .       sompropo(1,ireg),sompropop(1,ireg),gdsompropop(ireg),
     .       somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .       somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .       segt(1,ireg))

c----------------------------------------------------------------------
c  Printout of the remaining parameters.

        write(3,166) gdpsi(1,ireg), racpsi(1,ireg)
 166    format(T2,F8.5,T17,F8.5)

      ELSE IF ((npx.EQ.2) .AND. (racord)) THEN

*..2.   Connected double null

c----------------------------------------------------------------------
         nbcrb = 1
         nreg = 5
         nsep = 6

*..The distance between the top X-point and the O-point

         ipx = 1

         xx = separx(1,ptsep(3,ipx),ipx)
         yy = separy(1,ptsep(3,ipx),ipx)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

*..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

*..Calculate the psi value at the X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy +
     .            a11(ii,jj,1)*xx*yy

*..2.1  Read all the necessary data from the file

         ient = 9
         isor = 0

         CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .               pntrat,tgarde,distxo,psi_X,psi_O,ient,isor,ifail)

*..Calculate the length of each separatrix

         ipx = 1
         DO 38 isep = 1, 4
            lg(isep) = long(separx(1,ptsep(isep,ipx),ipx),
     .                 separy(1,ptsep(isep,ipx),ipx),
     .                 nptot(ptsep(isep,ipx),ipx))
   38    CONTINUE

         ipx = 2
         DO 39 isep = 1, 2
            lg(isep+4) = long(separx(1,ptsep(isep,ipx),ipx),
     .                   separy(1,ptsep(isep,ipx),ipx),
     .                   nptot(ptsep(isep,ipx),ipx))
   39    CONTINUE

*..Check whether all the data have been read from the file

         IF (ifail .EQ. 1) THEN

           if(sellan(1:8).eq.'francais') then
            CALL LECCLF(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .          deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .          distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .          npx,racord,x,y,nx,ny)
           elseif(sellan(1:7).eq.'english') then
            CALL LECCLE(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .          deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .          distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .          npx,racord,x,y,nx,ny)
           endif

         ENDIF

   43    CONTINUE

*..Calculate the psi difference between the penetration values

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin +
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

         ENDIF

*..Calculate the intervals, dmin and dmax

*..Along the separatrices

         CALL NUNIFO(nptseg(1),lg(1),deltp1(1),deltpn(1),spacep(1,1),
     .               dpmin(1),dpmax(1))

         CALL NUNIFO(nptseg(2),lg(2),deltp1(2),deltpn(2),spacep(1,2),
     .               dpmin(2),dpmax(2))

         CALL NUNIFO(nptseg(3),lg(3),deltp1(3),deltpn(3),spacep(1,3),
     .               dpmin(3),dpmax(3))

         CALL NUNIFO(nptseg(4),lg(4),deltp1(4),deltpn(4),spacep(1,4),
     .               dpmin(4),dpmax(4))

         CALL NUNIFO(nptseg(5),lg(5),deltp1(5),deltpn(5),spacep(1,5),
     .               dpmin(5),dpmax(5))

         CALL NUNIFO(nptseg(6),lg(6),deltp1(6),deltpn(6),spacep(1,6),
     .               dpmin(6),dpmax(6))

*..Radial direction

         CALL NUNIFO(npr(1),distnv(repart,1),deltr1(1),deltrn(1),
     .               spacer(1,1),drmin(1),drmax(1))

         CALL NUNIFO(npr(2),distnv(repart,2),deltr1(2),deltrn(2),
     .               spacer(1,2),drmin(2),drmax(2))

         CALL NUNIFO(npr(3),distnv(repart,3),deltr1(3),deltrn(3),
     .               spacer(1,3),drmin(3),drmax(3))

         CALL NUNIFO(npr(4),distnv(repart,4),deltr1(4),deltrn(4),
     .               spacer(1,4),drmin(4),drmax(4))

         IF (repart .EQ. 1) THEN

            CALL NUNIFO(npr(5),pntrat,deltr1(5),deltrn(5),
     .                  spacer(1,5),drmin(5),drmax(5))

         ELSE IF (repart .EQ. 2) THEN

            CALL NUNIFO(npr(5),difpsi,deltr1(5),deltrn(5),
     .                  spacer(1,5),drmin(5),drmax(5))

         ENDIF

         CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,6,correct)

*..Initialise the primary level line

         nn1=0

         DO 371 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn1=nn1+1
            xn(nn1)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn1)=separy(ipas,ptsep(3,ipx),ipx)
  371    CONTINUE

         if (nn1.gt.1) call trace3(
     _         nstruc,npstru,xstruc,ystruc,
     _         pntrat,distxo,xn,yn,nn1,
     .         xfin,yfin,fctfin,
     .         psi,nx,ny,x,y)

         if(correct) then
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

            pntrat_old = pntrat

            CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .                  pntrat,tgarde,distxo,psi_X,psi_O,
     .                  ient,isor,ifail)

            if (pntrat.ne.pntrat_old) then

              call endpag
              call trace2(x(1),x(nx),y(1),y(ny),
     .          separx,separy,ptsep,npx,nptot,
     .          nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .          nivtot,nbniv)

            endif

            GO TO 43
         else
            CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,10,correct)

         ENDIF

*..Save the chosen parameters

         CALL SORTIE(nsep,nreg,nptseg,npr,np1,deltp1,deltpn,deltr1,
     .     deltrn,pntrat,tgarde,distxo,repart,xmail,ymail,nx,ny,
     .     x,y,a00,a10,a01,a11,ptx,pty,npx,racord,1,fctpx,
     .     gdpsi,racpsi,a,
     .     gdr,r,ra,
     .     rho,somort,
     .     somortp,gdsomortp,somortpur,somortpurp,gdsomortpurp,
     .     sompropo,sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .     somtot,somtotp,gdsomtotp,segt)

*..2.2  Distribute the points along separatrices

         ipx = 1
         DO 44 isep=1, 4

           sepmax(1,isep) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 45 ipas=2, nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx),
     .                   separy(1,ptsep(isep,ipx),ipx),
     .                   nptot(ptsep(isep,ipx),ipx),dist,
     .                   sepmax(ipas,isep),sepmay(ipas,isep))
   45      CONTINUE
           sepmax(nptseg(isep),isep)=separx(nptot(ptsep(isep,ipx),ipx),
     .       ptsep(isep,ipx),ipx)
           sepmay(nptseg(isep),isep)=separy(nptot(ptsep(isep,ipx),ipx),
     .       ptsep(isep,ipx),ipx)
   44    CONTINUE

         ipx = 2
         DO 46 isep=1, 2

           sepmax(1,isep+4) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep+4) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 47 ipas=2, nptseg(isep+4)-1
              dist=dist + spacep(ipas-1,isep+4)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx),
     .                   separy(1,ptsep(isep,ipx),ipx),
     .                   nptot(ptsep(isep,ipx),ipx),dist,
     .                   sepmax(ipas,isep+4),sepmay(ipas,isep+4))

   47      CONTINUE
           sepmax(nptseg(isep+4),isep+4)=separx(nptot(ptsep(isep,ipx),
     .       ipx),ptsep(isep,ipx),ipx)
           sepmay(nptseg(isep+4),isep+4)=separy(nptot(ptsep(isep,ipx),
     .       ipx),ptsep(isep,ipx),ipx)
   46    CONTINUE

*..2.3. Grid region by region

*..2.3.1 Region 1: right

*..Define the primary curve and the grid points
         ireg=1
         print*, 'ireg=', ireg
#ifdef MODIF
   48    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=49,err=48) modif
   49    continue
#else
         modif = 0
#endif

         np1(ireg) = 0

         DO 50 ipas=nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   50    CONTINUE

         DO 51 ipas=2, nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   51    CONTINUE

         DO 52 ipas=2, nptseg(5)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
   52    CONTINUE


*..Go along the target 1

         idef = 1
         ipx = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(3)

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*57')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 57 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   57    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,1,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),gdsomortpurp(ireg),
     .               sompropo(1,ireg),sompropop(1,ireg),
     .               gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*
*..2.3.2 Region 2: top PFR

*..Define the primary curve and the grid points
         ireg=2
         print*, 'ireg=', ireg
#ifdef MODIF
   58    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=59,err=58) modif
   59    continue
#endif

         np1(ireg) = 0

         DO 60 ipas=nptseg(1), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
   60    CONTINUE

         DO 61 ipas=2, nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   61    CONTINUE

*..Go along the target 1

         idef = 1
         ipx = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(2)

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*67')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 67 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   67    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,0,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),
     .               ra(1,ireg),rho(1,ireg),somort(1,ireg),
     .               somortp(1,ireg),gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),gdsomortpurp(ireg),
     .               sompropo(1,ireg),sompropop(1,ireg),
     .               gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*
*..2.3.3 Region 3: left

*..Define the primary curve and the grid points
         ireg=3
         print*, 'ireg=', ireg
#ifdef MODIF
   68    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=69,err=68) modif
   69    continue
#endif

         np1(ireg) = 0

         DO 70 ipas=nptseg(2), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
   70    CONTINUE

         DO 71 ipas=2, nptseg(4)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
   71    CONTINUE

         DO 72 ipas=2, nptseg(6)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
   72    CONTINUE

*..Go along the target 2

         idef = 2
         ipx = 1

         x2 = separx(nptot(ptsep(2,ipx),ipx),ptsep(2,ipx),ipx)
         y2 = separy(nptot(ptsep(2,ipx),ipx),ptsep(2,ipx),ipx)

*..Initialise the primary level line

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

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(4)

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*77')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 77 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   77    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,1,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),
     .               gdsomortpurp(ireg),
     .               sompropo(1,ireg),sompropop(1,ireg),
     .               gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0


*..2.3.4 Region 4: bottom PFR

*..Define the primary curve and the grid points

         ireg=4
         print*, 'ireg=', ireg
#ifdef MODIF
   78    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=79,err=78) modif
   79    continue
#endif

         np1(ireg) = 0

         DO 80 ipas=nptseg(5), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
   80    CONTINUE

         DO 81 ipas=2, nptseg(6)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
   81    CONTINUE

*..Go along the target 3

         idef = 3
         ipx = 2

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(4)

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*87')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 87 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   87    CONTINUE


*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,0,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),
     .               gdsomortpurp(ireg),sompropo(1,ireg),
     .               sompropop(1,ireg),gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0
*
*..2.3.5  Region 5: central region

*..Define the primary curve and the grid points

         ireg=5
         np1(ireg) = 0
         ipx = 1

         DO 90 ipas=1, nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
   90    CONTINUE

         DO 91 ipas=nptseg(4)-1,1,-1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
   91    CONTINUE

         x2 = separx(1,ptsep(3,ipx),ipx)
         y2 = separy(1,ptsep(3,ipx),ipx)

*..Initialise the primary level line

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

*..Relate the desirable sweeping direction to the structure orientation

         DO 97 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

   97    CONTINUE

*..Call the routine which grids this region

c---
         print*, 'ireg=', ireg
c---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,pntrat,
     .       pas,np1(ireg),npr(ireg),x2,y2,fctini,
     .       nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc,
     .       a00,a10,a01,a11,repart,
     .       xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,
     .       gdpsi(1,ireg),racpsi(1,ireg),
     .       a,gdr(1,ireg),r(1,ireg),ra(1,ireg),rho(1,ireg),
     .       somort(1,ireg),somortp(1,ireg),gdsomortp(ireg),
     .       somortpur(1,ireg),somortpurp(1,ireg),gdsomortpurp(ireg),
     .       sompropo(1,ireg),sompropop(1,ireg),gdsompropop(ireg),
     .       somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .       somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .       segt(1,ireg))




c----------------------------------------------------------------------

      ELSE IF ((npx.EQ.2) .AND. (.NOT.(racord))) THEN

c----------------------------------------------------------------------
*.3.  Disconnected double null

         nreg = 6
         nsep = 6
         ptxext = MOD(ptxint,2) + 1
         npr1 = 0

         IF (ptxint .EQ. 1) THEN
            idef = 1
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
         ENDIF

*..L'indice de niveau frontiere + 1 correspond a l'indice de region.
*..Index of the level boundary + 1 corresponds to the region index

         DO 100 i=4, 1, -1

            distnv(1,i+1) = distnv(1,i)
            distnv(2,i+1) = distnv(2,i)

  100    CONTINUE

*..Calcul de la distance entre la sep. du point X interieur et celle du
*  point X exterieur sur la plaque ou elles sont toutes deux presentes.

*..Calculate the distance between the inner and outer separatrices, if
*  they are both present, along the target

         xint = separx(nptot(ptsep(1,ptxint),ptxint),ptsep(1,ptxint),
     .                 ptxint)
         yint = separy(nptot(ptsep(1,ptxint),ptxint),ptsep(1,ptxint),
     .                 ptxint)
         xext = separx(nptot(ptsep(3,ptxext),ptxext),ptsep(3,ptxext),
     .                 ptxext)
         yext = separy(nptot(ptsep(3,ptxext),ptxext),ptsep(3,ptxext),
     .                 ptxext)

         distnv(1,1) = plqdst(xint,yint,xext,yext,xstruc(1,inddef(idef))
     .            ,ystruc(1,inddef(idef)),npstru(inddef(idef)),'droite')


         ii = ifind(xint,x,nx,1)
         jj = ifind(yint,y,ny,1)

         psiint = a00(ii,jj,1) + a10(ii,jj,1)*xint + a01(ii,jj,1)*yint
     .          + a11(ii,jj,1)*xint*yint

         ii = ifind(xext,x,nx,1)
         jj = ifind(yext,y,ny,1)

         psiext = a00(ii,jj,1) + a10(ii,jj,1)*xext + a01(ii,jj,1)*yext
     .          + a11(ii,jj,1)*xext*yext

         distnv(2,1) = psiext - psiint

*..The distance between the inner X-point and the O-point

         ipx = ptxint

         xx = separx(1,ptsep(3,ipx),ipx)
         yy = separy(1,ptsep(3,ipx),ipx)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

*..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

*..Calculate the psi value at the inner X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy +
     .            a11(ii,jj,1)*xx*yy

*..3.1  Read all the necessary data from the file

         ient = 9
         isor = 0

         CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .               pntrat,tgarde,distxo,psi_X,psi_O,ient,isor,ifail)

*..Calculate the length of each separatrix

         ipx = 1
         DO 102 isep = 1, 2
            lg(isep) = long(separx(1,ptsep(isep,ipx),ipx),
     .                 separy(1,ptsep(isep,ipx),ipx),
     .                 nptot(ptsep(isep,ipx),ipx))
  102    CONTINUE

         ipx = 2
         DO 103 isep = 1, 2
            lg(isep+4) = long(separx(1,ptsep(isep,ipx),ipx),
     .                   separy(1,ptsep(isep,ipx),ipx),
     .                   nptot(ptsep(isep,ipx),ipx))
  103    CONTINUE

*..Check whether all the data have been read from the file

         IF (ifail .EQ. 1) THEN

           if(sellan(1:8).eq.'francais') then
            CALL LECCLF(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .           deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .           distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .           npx,racord,x,y,nx,ny)
           elseif(sellan(1:7).eq.'english') then
            CALL LECCLE(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .           deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .           distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .           npx,racord,x,y,nx,ny)
           endif

         ENDIF

  105    CONTINUE

*..Calculate the psi difference between the penetration values

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin +
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

         ENDIF

*..Calculate the intervals, dmin and dmax, along the separatrices

         CALL NUNIFO(nptseg(1),lg(1),deltp1(1),deltpn(1),spacep(1,1),
     .               dpmin(1),dpmax(1))

         CALL NUNIFO(nptseg(2),lg(2),deltp1(2),deltpn(2),spacep(1,2),
     .               dpmin(2),dpmax(2))

         CALL NUNIFO(nptseg(5),lg(5),deltp1(5),deltpn(5),spacep(1,5),
     .               dpmin(5),dpmax(5))

         CALL NUNIFO(nptseg(6),lg(6),deltp1(6),deltpn(6),spacep(1,6),
     .               dpmin(6),dpmax(6))


*..Calculate the intervals, dmin and dmax, in the radial direction

         CALL NUNIFO(npr(1),distnv(repart,1),deltr1(1),deltrn(1),
     .               spacer(1,1),drmin(1),drmax(1))

         CALL NUNIFO(npr(2),distnv(repart,2),deltr1(2),deltrn(2),
     .               spacer(1,2),drmin(2),drmax(2))

         CALL NUNIFO(npr(3),distnv(repart,3),deltr1(3),deltrn(3),
     .               spacer(1,3),drmin(3),drmax(3))

         CALL NUNIFO(npr(4),distnv(repart,4),deltr1(4),deltrn(4),
     .               spacer(1,4),drmin(4),drmax(4))

         CALL NUNIFO(npr(5),distnv(repart,5),deltr1(5),deltrn(5),
     .               spacer(1,5),drmin(5),drmax(5))

         IF (repart .EQ. 1) THEN

            CALL NUNIFO(npr(6),pntrat,deltr1(6),deltrn(6),
     .                  spacer(1,6),drmin(6),drmax(6))

         ELSE IF (repart .EQ. 2) THEN

            CALL NUNIFO(npr(6),difpsi,deltr1(6),deltrn(6),
     .                  spacer(1,6),drmin(6),drmax(6))

         ENDIF
crm  augmenter ce test
         if(npr(1).ne.npr1) then
            npr1=npr(1)

*.3.1.1. Appel a la routine qui trouve le point sur la boucle de la
*        separatrice interieure ou on doit diviser cette separatrice
*        en deux.

*.3.1.1. Find the point on the separatrix loop where the separatrix
*        should be split into two

*..Initialise the primary level line

            nn=0
            ipx = ptxext

            DO 110 ipas=nptot(ptsep(3,ipx),ipx),1,-1
               nn=nn+1
               xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
               yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
  110       CONTINUE

            DO 111 ipas=2,nptot(ptsep(4,ipx),ipx)
               nn=nn+1
               xn(nn)=separx(ipas,ptsep(4,ipx),ipx)
               yn(nn)=separy(ipas,ptsep(4,ipx),ipx)
  111       CONTINUE

*..Coordinates of the outer X-point

            xptxex = separx(1,ptsep(3,ipx),ipx)
            yptxex = separy(1,ptsep(3,ipx),ipx)

*..Points of the grid to be used for the first call to doubld

            if(npr1.eq.0) then
              ireg=1
              nmail=min(41,nn/5)
              nmail=nmail+mod(nmail+1,2)
              ll=long(separx(1,ptsep(3,ipx),ipx),
     .                separy(1,ptsep(3,ipx),ipx),
     .                nptot(ptsep(3,ipx),ipx))
              xmail(1,1,ireg)=separx(nptot(ptsep(3,ipx),ipx),
     .          ptsep(3,ipx),ipx)
              ymail(1,1,ireg)=separy(nptot(ptsep(3,ipx),ipx),
     .          ptsep(3,ipx),ipx)
              do imail=nmail/2,2,-1
                dist=ll*(nmail/2-imail+1.)/(nmail/2)
                CALL COORD(separx(1,ptsep(3,ipx),ipx),
     .            separy(1,ptsep(3,ipx),ipx),nptot(ptsep(3,ipx),ipx),
     .            dist,xmail(imail,1,ireg),ymail(imail,1,ireg))
              enddo
              xmail(nmail/2+1,1,ireg)=xptxex
              ymail(nmail/2+1,1,ireg)=yptxex

              ll=long(separx(1,ptsep(4,ipx),ipx),
     .                separy(1,ptsep(4,ipx),ipx),
     .                nptot(ptsep(4,ipx),ipx))
              xmail(nmail,1,ireg)=separx(nptot(ptsep(4,ipx),ipx),
     .          ptsep(4,ipx),ipx)
              ymail(nmail,1,ireg)=separy(nptot(ptsep(4,ipx),ipx),
     .          ptsep(4,ipx),ipx)
              do imail=2,nmail/2
                dist=ll*(imail-1.)/(nmail/2)
                CALL COORD(separx(1,ptsep(3,ipx),ipx),
     .            separy(1,ptsep(3,ipx),ipx),nptot(ptsep(3,ipx),ipx),
     .            dist,xmail(nmail/2+imail,1,ireg),
     .            ymail(nmail/2+imail,1,ireg))
              enddo
            endif

*..Second boundary curve

            npcrb2=0
            ipx = ptxint

            DO 112 ipas=nptot(ptsep(1,ipx),ipx),1,-1
               npcrb2=npcrb2+1
               xcrb2(npcrb2)=separx(ipas,ptsep(1,ipx),ipx)
               ycrb2(npcrb2)=separy(ipas,ptsep(1,ipx),ipx)
  112       CONTINUE

            DO 113 ipas=2,nptot(ptsep(3,ipx),ipx)
               npcrb2=npcrb2+1
               xcrb2(npcrb2)=separx(ipas,ptsep(3,ipx),ipx)
               ycrb2(npcrb2)=separy(ipas,ptsep(3,ipx),ipx)
  113       CONTINUE

            DO 114 ipas=2,nptot(ptsep(2,ipx),ipx)
               npcrb2=npcrb2+1
               xcrb2(npcrb2)=separx(ipas,ptsep(2,ipx),ipx)
               ycrb2(npcrb2)=separy(ipas,ptsep(2,ipx),ipx)
  114       CONTINUE

            call trc_stk_in('maille','*114')
            CALL DOUBLD(bouclx,boucly,xn,yn,nn,spacer(1,1),
     .        npr(1),inddef(idef),xext,yext,xptxex,yptxex,xpto,
     .        ypto,nx,ny,x,y,psi,nstruc,npstru,xstruc,
     .        ystruc,a00,a10,a01,a11,repart,
     .        xcrb2,ycrb2,npcrb2)
            call trc_stk_out
         endif

         ipx = ptxint
         dist=0.0

         lg(3) = ruban(separx(1,ptsep(3,ipx),ipx),
     .                separy(1,ptsep(3,ipx),ipx),nptot(ptsep(3,ipx),ipx)
     .                ,bouclx,boucly,dist)
         CALL NUNIFO(nptseg(3),lg(3),deltp1(3),deltpn(3),spacep(1,3),
     .               dpmin(3),dpmax(3))

         lg(4) = long(separx(1,ptsep(3,ipx),ipx),separy(1,ptsep(3,ipx)
     .              ,ipx),nptot(ptsep(3,ipx),ipx)) - lg(3)
         CALL NUNIFO(nptseg(4),lg(4),deltp1(4),deltpn(4),spacep(1,4),
     .               dpmin(4),dpmax(4))

         CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,6,correct)

*..Initialise the primary level line

         nn1=0

         DO 381 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn1=nn1+1
            xn(nn1)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn1)=separy(ipas,ptsep(3,ipx),ipx)
  381    CONTINUE

         if (nn1.gt.1) call trace3(
     _         nstruc,npstru,xstruc,ystruc,
     _         pntrat,distxo,xn,yn,nn1,
     .         xfin,yfin,fctfin,
     .         psi,nx,ny,x,y)

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

            pntrat_old = pntrat

            CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .                  pntrat,tgarde,distxo,psi_X,psi_O,
     .                  ient,isor,ifail)

            if (pntrat.ne.pntrat_old) then
              call endpag
              call trace2(x(1),x(nx),y(1),y(ny),
     .          separx,separy,ptsep,npx,nptot,
     .          nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .          nivtot,nbniv)
            endif
            GO TO 105
         else
            CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,10,correct)
         ENDIF

*..Save the chosen parameters

         CALL SORTIE(nsep,nreg,nptseg,npr,np1,deltp1,deltpn,deltr1,
     .     deltrn,pntrat,tgarde,distxo,repart,xmail,ymail,nx,ny,
     .     x,y,a00,a10,a01,a11,ptx,pty,npx,racord,1,fctpx,
     .     gdpsi,racpsi,a,
     .     gdr,r,ra,
     .     rho,somort,
     .     somortp,gdsomortp,somortpur,somortpurp,gdsomortpurp,
     .     sompropo,sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .     somtot,somtotp,gdsomtotp,segt)

*..3.2  Distribute the points along separatrices

*..Determine the two parts of the inner separatrix which make a loop

*..Right part

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

*..Left part

         ipx = ptxint

         nbcl(2) = 1
         xbcl(1,2) = separx(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)
         ybcl(1,2) = separy(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)

  121    CONTINUE

         nbcl(2) = nbcl(2) + 1
         xbcl(nbcl(2),2) = separx(nptot(ptsep(3,ipx),ipx) - nbcl(2) + 1,
     .                     ptsep(3,ipx),ipx)
         ybcl(nbcl(2),2) = separy(nptot(ptsep(3,ipx),ipx) - nbcl(2) + 1,
     .                     ptsep(3,ipx),ipx)

         lbcl = long(xbcl(1,2),ybcl(1,2),nbcl(2))

         IF (lbcl .LE. lg(4)) THEN
            GO TO 121
         ELSE
            xbcl(nbcl(2),2) = bouclx
            ybcl(nbcl(2),2) = boucly
         ENDIF

*..Distribute the points along each separatrix

         ipx = 1
         DO 122 isep=1, 2

           sepmax(1,isep) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 123 ipas=2, nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx),
     .                   separy(1,ptsep(isep,ipx),ipx),
     .                   nptot(ptsep(isep,ipx),ipx),dist,
     .                   sepmax(ipas,isep),sepmay(ipas,isep))
  123      CONTINUE
           sepmax(nptseg(isep),isep)=separx(nptot(ptsep(isep,ipx),ipx),
     .       ptsep(isep,ipx),ipx)
           sepmay(nptseg(isep),isep)=separy(nptot(ptsep(isep,ipx),ipx),
     .       ptsep(isep,ipx),ipx)
  122    CONTINUE

         DO 124 isep=1, 2

           sepmax(1,isep+2) = xbcl(1,isep)
           sepmay(1,isep+2) = ybcl(1,isep)
           dist=0.

           DO 125 ipas=2, nptseg(isep+2)-1
              dist=dist + spacep(ipas-1,isep+2)
              CALL COORD(xbcl(1,isep),ybcl(1,isep),nbcl(isep),dist,
     .                   sepmax(ipas,isep+2),sepmay(ipas,isep+2))
  125      CONTINUE
           sepmax(nptseg(isep+2),isep+2)=xbcl(nbcl(isep),isep)
           sepmay(nptseg(isep+2),isep+2)=ybcl(nbcl(isep),isep)
  124    CONTINUE

         ipx = 2
         DO 126 isep=1, 2

           sepmax(1,isep+4) = separx(1,ptsep(isep,ipx),ipx)
           sepmay(1,isep+4) = separy(1,ptsep(isep,ipx),ipx)
           dist=0.

           DO 127 ipas=2, nptseg(isep+4)-1
              dist=dist + spacep(ipas-1,isep+4)
              CALL COORD(separx(1,ptsep(isep,ipx),ipx),
     .                   separy(1,ptsep(isep,ipx),ipx),
     .                   nptot(ptsep(isep,ipx),ipx),dist,
     .                   sepmax(ipas,isep+4),sepmay(ipas,isep+4))
  127      CONTINUE
           sepmax(nptseg(isep+4),isep+4)=separx(nptot(ptsep(isep,ipx),
     .       ipx),ptsep(isep,ipx),ipx)
           sepmay(nptseg(isep+4),isep+4)=separy(nptot(ptsep(isep,ipx),
     .       ipx),ptsep(isep,ipx),ipx)
  126    CONTINUE

*..3.3. Grid region by region

*..3.3.1 Region 1: between the separatrices
        ireg=1
        print*, 'ireg=', ireg
        nbcrb = 2
#ifdef MODIF
  128   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Avec ou sans modifications?'
          write(6,*) 'Répondez 1 avec modif et 0 sans modif'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'With (1) or without (0) manual modifications?'
        endif
        read(5,*,end=129,err=128) modif
  129   continue
#else
        modif = 0
#endif

*.. The second boundary

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

*..Define the primary curve and the grid points

         np1(ireg) = 0

         IF (ptxint .EQ. 1) THEN

            DO 132 ipas=nptseg(1), 1, -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
  132       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 133 ipas=nptseg(5), 1, -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
  133       CONTINUE

         ENDIF

         DO 134 ipas=2, nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
  134    CONTINUE

         DO 135 ipas=nptseg(4)-1, 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
  135    CONTINUE

         IF (ptxint .EQ. 1) THEN

            DO 136 ipas=2, nptseg(2)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
  136       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 137 ipas=2, nptseg(6)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
  137       CONTINUE

         ENDIF

*..Initialise the guard indices and starting target

         IF (ptxint .EQ. 1) THEN
            idef = 1
            gardd2 = tgarde(2)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
            gardd2 = tgarde(4)
         ENDIF
         gardd1 = tgarde(idef)

         ipx = ptxint

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*145')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 145 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  145    CONTINUE

*..Parametrise the last level line

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

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,1,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),
     .               ra(1,ireg),rho(1,ireg),somort(1,ireg),
     .               somortp(1,ireg),gdsomortp(ireg),
     .               somortpur(1,ireg),somortpurp(1,ireg),
     .               gdsomortpurp(ireg),sompropo(1,ireg),
     .               sompropop(1,ireg),gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*..Arangement of the mesh point which must coinside with outer X-point

         IF (ptxint .EQ. 1) THEN

            xmail(nptseg(1)+nptseg(3)-1,npr(1),1) = xptxex
            ymail(nptseg(1)+nptseg(3)-1,npr(1),1) = yptxex

         ELSE IF (ptxint .EQ. 2) THEN

            xmail(nptseg(5)+nptseg(3)-1,npr(1),1) = xptxex
            ymail(nptseg(5)+nptseg(3)-1,npr(1),1) = yptxex

         ENDIF

         nuldec = .FALSE.

*..3.3.2 Region 2: right
         ireg=2
         print*, 'ireg=', ireg
#ifdef MODIF
  148    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=149,err=148) modif
  149    continue
#endif
         nbcrb = 1

*..Define the primary curve and the grid points


         np1(ireg) = 0

         IF (ptxint .EQ. 1) THEN

            DO 150 ipas=1,nptseg(1)+nptseg(3)-2
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  150       CONTINUE

            DO 151 ipas=1, nptseg(5)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
  151       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 152 ipas=1,nptseg(5)+nptseg(3)-2
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  152       CONTINUE

            DO 153 ipas=1, nptseg(1)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
  153       CONTINUE

         ENDIF

*..Initialise the guard indices and starting target

         IF (ptxint .EQ. 1) THEN
            idef = 1
            gardd2 = tgarde(3)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
            gardd2 = tgarde(1)
         ENDIF
         gardd1 = tgarde(idef)

         ipx = ptxext

         x2 = separx(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)
         y2 = separy(nptot(ptsep(3,ipx),ipx),ptsep(3,ipx),ipx)

*..Initialise the primary level line

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

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*157')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'droite')
         call trc_stk_out

         DO 157 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  157    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,1,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),
     .               gdsomortpurp(ireg),sompropo(1,ireg),
     .               sompropop(1,ireg),gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*..3.3.3 Region 3: top PFR

         nbcrb = 1

*..Define the primary curve and the grid points
         ireg=3
         print*, 'ireg=', ireg
#ifdef MODIF
  158    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=159,err=158) modif
  159    continue
#endif

         np1(ireg) = 0

         DO 160 ipas=nptseg(1),1,-1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
  160    CONTINUE

         DO 161 ipas=2, nptseg(2)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
  161    CONTINUE

*..Initialise the guard indices and starting target

         idef = 1
         gardd1 = tgarde(idef)
         gardd2 = tgarde(2)

         ipx = 1

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)


*..Initialise the primary level line

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

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*167')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 167 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  167    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,0,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),
     .               gdsomortpurp(ireg),sompropo(1,ireg),
     .               sompropop(1,ireg),gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*..3.3.4 Region 4: left
         ireg=4
         print*, 'ireg=', ireg
#ifdef MODIF
  168    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
            write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=169,err=168) modif
  169    continue
#endif
         nbcrb = 1

*..Define the primary curve and the grid points


         np1(ireg) = 0

         IF (ptxint .EQ. 1) THEN

            DO 170 ipas=nptseg(1)+nptseg(2)+nptseg(3)+nptseg(4)-3,
     .                             nptseg(1)+nptseg(3), -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  170       CONTINUE

            DO 171 ipas=1, nptseg(6)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
  171       CONTINUE

         ELSE IF (ptxint .EQ. 2) THEN

            DO 172 ipas=nptseg(5)+nptseg(6)+nptseg(3)+nptseg(4)-3,
     .                             nptseg(5)+nptseg(3), -1
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = xmail(ipas,npr(1),1)
               ymail(np1(ireg),1,ireg) = ymail(ipas,npr(1),1)
  172       CONTINUE

            DO 173 ipas=1, nptseg(2)
               np1(ireg) = np1(ireg)+1
               xmail(np1(ireg),1,ireg) = sepmax(ipas,2)
               ymail(np1(ireg),1,ireg) = sepmay(ipas,2)
  173       CONTINUE

         ENDIF

*..Initialise the guard indices and starting target

         IF (ptxint .EQ. 1) THEN
            idef = 2
            gardd2 = tgarde(4)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 4
            gardd2 = tgarde(2)
         ENDIF
         gardd1 = tgarde(idef)

         ipx = ptxext

         x2 = separx(nptot(ptsep(4,ipx),ipx),ptsep(4,ipx),ipx)
         y2 = separy(nptot(ptsep(4,ipx),ipx),ptsep(4,ipx),ipx)

*..Initialise the primary level line

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

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*177')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 177 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  177    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,1,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),
     .               gdsomortpurp(ireg),sompropo(1,ireg),
     .               sompropop(1,ireg),gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*..3.3.5 Region 5: bottom PFR
         ireg=5
         print*, 'ireg=', ireg
#ifdef MODIF
  178    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=179,err=178) modif
  179    continue
#endif
         nbcrb = 1

*..Define the primary curve and the grid points


         np1(ireg) = 0

         DO 180 ipas=nptseg(5), 1, -1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,5)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,5)
  180    CONTINUE

         DO 181 ipas=2, nptseg(6)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,6)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,6)
  181    CONTINUE

*..Initialise the guard indices and starting target

         idef = 3
         gardd1 = tgarde(idef)
         gardd2 = tgarde(4)

         ipx = 2

         x2 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y2 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

*..Initialise the primary level line

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

*..Relate the desirable sweeping direction to the structure orientation

         call trc_stk_in('maille','*187')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
     .                 npstru(inddef(idef)),x2,y2,'gauche')
         call trc_stk_out

         DO 187 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  187    CONTINUE

*..Call the routine which grids this region

c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .               np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .               x,y,psi,nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,
     .               gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .               ynlast,nnlast,nuldec,0,xpto,ypto,gdpsi(1,ireg),
     .               racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .               rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .               gdsomortp(ireg),somortpur(1,ireg),
     .               somortpurp(1,ireg),
     .               gdsomortpurp(ireg),sompropo(1,ireg),
     .               sompropop(1,ireg),gdsompropop(ireg),
     .               somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .               somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .               segt(1,ireg),ireg,modif,xmini,xmaxi,
     .               ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .               nivx,nivy,
     .               nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

         modif=0

*..3.3.6  Region 6: central region

*..Define the primary curve and the grid points

         ireg=6
         np1(ireg) = 0
         ipx = ptxint

         DO 190 ipas=1, nptseg(3)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,3)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,3)
  190    CONTINUE

         DO 191 ipas=nptseg(4)-1,1,-1
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,4)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,4)
  191    CONTINUE

         x2 = separx(1,ptsep(3,ipx),ipx)
         y2 = separy(1,ptsep(3,ipx),ipx)

*..Initialise the primary level line

         nn=0

         DO 192 ipas=1,nptot(ptsep(3,ipx),ipx)
            nn=nn+1
            xn(nn)=separx(ipas,ptsep(3,ipx),ipx)
            yn(nn)=separy(ipas,ptsep(3,ipx),ipx)
  192    CONTINUE

*..Relate the desirable sweeping direction to the structure orientation

         DO 197 ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

  197    CONTINUE


*..Call the routine which grids this region

c---
         print*, 'ireg=', ireg
c---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,pntrat,
     .      pas,np1(ireg),npr(ireg),x2,y2,fctini,
     .      nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc,
     .      a00,a10,a01,a11,repart,
     .      xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,
     .      gdpsi(1,ireg),racpsi(1,ireg),
     .      a,gdr(1,ireg),r(1,ireg),ra(1,ireg),rho(1,ireg),
     .      somort(1,ireg),somortp(1,ireg),gdsomortp(ireg),
     .      somortpur(1,ireg),somortpurp(1,ireg),gdsomortpurp(ireg),
     .      sompropo(1,ireg),sompropop(1,ireg),gdsompropop(ireg),
     .      somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .      somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .      segt(1,ireg))

c----------------------------------------------------------------------

      ELSE IF (limcfg.eq.1) THEN

c----------------------------------------------------------------------
*..4.   Limiter configuration

         nbcrb = 1
         nreg = 2
         nsep = 1
         ipx = 1
         isep=nsep

*..The distance between the X-point and the O-point

         xx = nivx(1,1)
         yy = nivy(1,1)

         xptxo = xpto - xx
         yptxo = ypto - yy

         distxo = SQRT((xptxo)**2 + (yptxo)**2)

*..Normalise the vector from the X-point to the O-point

         xptxo = xptxo/distxo
         yptxo = yptxo/distxo

*..Calculate the psi value at the X-point

         ii = ifind(xx,x,nx,1)
         jj = ifind(yy,y,ny,1)

         fctini = a00(ii,jj,1) + a10(ii,jj,1)*xx + a01(ii,jj,1)*yy +
     .            a11(ii,jj,1)*xx*yy

*..4.1  Read all the necessary data from the file

         ient = 9
         isor = 0

         CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .               pntrat,tgarde,distxo,psi_X,psi_O,ient,isor,ifail)

*..Calculate the length of each separatrix

            lg(isep) = long(nivx(1,1),nivy(1,1),nivtot(1))

*..Check whether all the data have been read from the file

         IF (ifail .EQ. 1) THEN

           if(sellan(1:8).eq.'francais') then
             CALL LECCLF(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .         deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .         distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .         npx,racord,x,y,nx,ny)
           elseif(sellan(1:7).eq.'english') then
             CALL LECCLE(nptseg,npr,lg,deltp1,deltpn,deltr1,limcfg,
     .         deltrn,repart,pntrat,tgarde,distnv,xptxo,yptxo,
     .         distxo,xx,yy,fctini,difpsi,a00,a10,a01,a11,nxmax,nymax,
     .         npx,racord,x,y,nx,ny)
           endif

         ENDIF

  203    CONTINUE

*..Calculate the psi difference between the penetration values

         IF (repart .EQ. 2) THEN

            xfin = xx + xptxo*pntrat
            yfin = yy + yptxo*pntrat

            ii = ifind(xfin,x,nx,1)
            jj = ifind(yfin,y,ny,1)

            fctfin = a00(ii,jj,1) + a10(ii,jj,1)*xfin +
     .               a01(ii,jj,1)*yfin + a11(ii,jj,1)*xfin*yfin

            difpsi = fctfin - fctini

         ENDIF

*..Calculate the intervals, dmin and dmax

*..Along the separatrices

         CALL NUNIFO(nptseg(1),lg(1),deltp1(1),deltpn(1),spacep(1,1),
     .               dpmin(1),dpmax(1))


*..Radial direction

         CALL NUNIFO(npr(1),distnv(repart,1),deltr1(1),deltrn(1),
     .               spacer(1,1),drmin(1),drmax(1))

         IF (repart .EQ. 1) THEN

            CALL NUNIFO(npr(2),pntrat,deltr1(2),deltrn(2),
     .                  spacer(1,2),drmin(2),drmax(2))

         ELSE IF (repart .EQ. 2) THEN

            CALL NUNIFO(npr(2),difpsi,deltr1(2),deltrn(2),
     .                  spacer(1,2),drmin(2),drmax(2))

         ENDIF

         CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,6,correct)

*..Initialise the primary level line

         nn1=0

         DO 391 ipas=1,nivtot(1)
            nn1=nn1+1
            xn(nn1)=nivx(ipas,1)
            yn(nn1)=nivy(ipas,1)
  391    CONTINUE

         if (nn1.gt.1) call trace3(
     _         nstruc,npstru,xstruc,ystruc,
     _         pntrat,distxo,xn,yn,nn1,
     .         xfin,yfin,fctfin,
     .         psi,nx,ny,x,y)
         if(correct) then
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

            pntrat_old = pntrat

            CALL CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .                  pntrat,tgarde,distxo,psi_X,psi_O,
     .                  ient,isor,ifail)

            if (pntrat.ne.pntrat_old) then
              call endpag

              call trace2(x(1),x(nx),y(1),y(ny),
     .          separx,separy,ptsep,npx,nptot,
     .          nstruc,npstru,xstruc,ystruc,nivx,nivy,
     .          nivtot,nbniv)

            endif
            GO TO 203
         else
            CALL RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,10,correct)

         ENDIF

*..Save the chosen parameters

         CALL SORTIE(nsep,nreg,nptseg,npr,np1,deltp1,deltpn,deltr1,
     .     deltrn,pntrat,tgarde,distxo,repart,xmail,ymail,nx,ny,
     .     x,y,a00,a10,a01,a11,ptx,pty,npx,racord,1,fctpx,
     .     gdpsi,racpsi,a,
     .     gdr,r,ra,
     .     rho,somort,
     .     somortp,gdsomortp,somortpur,somortpurp,gdsomortpurp,
     .     sompropo,sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .     somtot,somtotp,gdsomtotp,segt)



*..4.2  Distribute the points along separatrices

         DO isep=1, nsep

           sepmax(1,isep) = nivx(1,1)
           sepmay(1,isep) = nivy(1,1)
           dist=0.

           DO ipas=2, nptseg(isep)-1
              dist=dist + spacep(ipas-1,isep)
              CALL COORD(nivx(1,1),nivy(1,1),nivtot(1),
     .          dist,sepmax(ipas,isep),sepmay(ipas,isep))
           ENDDO

           sepmax(nptseg(isep),isep)=nivx(nivtot(1),1)
           sepmay(nptseg(isep),isep)=nivy(nivtot(1),1)
         ENDDO

*..4.3. Grid region by region

*..4.3.1  Region 1

*..Define the primary curve and the grid points
         ireg=1
         print*, 'ireg=', ireg
#ifdef MODIF
  378    if(sellan(1:8).eq.'francais') then
           write(6,*) 'Avec ou sans modifications?'
           write(6,*) 'Répondez 1 avec modif et 0 sans modif'
         elseif(sellan(1:7).eq.'english') then
           write(6,*) 'With (1) or without (0) manual modifications?'
         endif
         read(5,*,end=379,err=378) modif
  379    continue
#else
         modif = 0
#endif

         np1(ireg) = 0

         DO ipas=1, nptseg(1)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
         ENDDO

*..Go along the target 1

         idef = 1

         x2 = nivx(1,1)
         y2 = nivy(1,1)
c***
c        print*,'x2, y2=',x2,y2
c***

*..Initialise the primary level line

         nn=0

         DO ipas=1,nivtot(1)
            nn=nn+1
            xn(nn)=nivx(ipas,1)
            yn(nn)=nivy(ipas,1)
         ENDDO

*..Initialise the guard indices

         gardd1 = tgarde(idef)
         gardd2 = tgarde(2)

*..Determine the structure orientation

c!!!
c        sens =-drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)),
c    .                 npstru(inddef(idef)),x2,y2,'droite')
         sens=1
         sens=horair(xpto,ypto,x2,y2,xstruc(1,inddef(idef)),
     .     ystruc(1,inddef(idef)),npstru(inddef(idef)),sens)
c***
c        print*,'dans maille: sens=',sens
c***

         DO ipas=1, npr(ireg)-1

            pas(ipas) = spacer(ipas,ireg)

         ENDDO

*..Call the routine which grids this region

c***
c        print*,'call mailrg'
c        print*,'ireg, idef, inddef=',ireg,idef,inddef(idef)
c        print*,'np1, npr=',np1(ireg),npr(ireg)
c***
c---

c---
         CALL MAILRG(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,sens,pas,
     .              np1(ireg),npr(ireg),inddef(idef),x2,y2,nx,ny,
     .              x,y,psi,nstruc,npstru,xstruc,ystruc,
     .              a00,a10,a01,a11,repart,
     .              gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,xnlast,
     .              ynlast,nnlast,nuldec,0,xpto,ypto,gdpsi(1,ireg),
     .              racpsi(1,ireg),a,gdr(1,ireg),r(1,ireg),ra(1,ireg),
     .              rho(1,ireg),somort(1,ireg),somortp(1,ireg),
     .              gdsomortp(ireg),somortpur(1,ireg),
     .              somortpurp(1,ireg),
     .              gdsomortpurp(ireg),sompropo(1,ireg),
     .              sompropop(1,ireg),gdsompropop(ireg),
     .              somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .              somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .              segt(1,ireg),ireg,modif,xmini,xmaxi,
     .              ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .              nivx,nivy,
     .              nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)

*..4.3.2  Region 2

*..Define the primary curve and the grid points

         ireg=2
         np1(ireg) = 0

         DO ipas=1, nptseg(1)
            np1(ireg) = np1(ireg)+1
            xmail(np1(ireg),1,ireg) = sepmax(ipas,1)
            ymail(np1(ireg),1,ireg) = sepmay(ipas,1)
         ENDDO

         x2 = nivx(1,1)
         y2 = nivy(1,1)

*..Initialise the primary level line

         nn=0

         DO ipas=1,nivtot(1)
            nn=nn+1
            xn(nn)=nivx(nn,1)
            yn(nn)=nivy(nn,1)
         ENDDO

*..Definition des pas pour le balayage vers le centre
         DO ipas=1, npr(ireg)-1
            pas(ipas) = spacer(ipas,ireg)
c***
c           print*,'ipas, pas=',ipas,pas(ipas)
c***
         ENDDO

*..Call the routine which grids this region

c***
c        print*,'call mailcn'
c***
c---
         print*, 'ireg=', ireg
c---
         CALL MAILCN(xmail(1,1,ireg),ymail(1,1,ireg),xn,yn,nn,pntrat,
     .      pas,np1(ireg),npr(ireg),x2,y2,fctini,
     .      nx,ny,x,y,psi,nstruc,npstru,xstruc,ystruc,
     .      a00,a10,a01,a11,repart,
     .      xptxo,yptxo,xpto,ypto,nivx,nivy,nivtot,nbniv,distxo,
     .      gdpsi(1,ireg),racpsi(1,ireg),
     .      a,gdr(1,ireg),r(1,ireg),ra(1,ireg),rho(1,ireg),
     .      somort(1,ireg),somortp(1,ireg),gdsomortp(ireg),
     .      somortpur(1,ireg),somortpurp(1,ireg),gdsomortpurp(ireg),
     .      sompropo(1,ireg),sompropop(1,ireg),gdsompropop(ireg),
     .      somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg),
     .      somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg),
     .      segt(1,ireg))

c       ...
      ENDIF
c======================================================================

*..5.  Write the grid data into a file

      CALL SORTIE(nsep,nreg,nptseg,npr,np1,deltp1,deltpn,deltr1,deltrn,
     .  pntrat,tgarde,distxo,repart,xmail,ymail,nx,ny,
     .  x,y,a00,a10,a01,a11,ptx,pty,npx,racord,2,fctpx,
     .  gdpsi,
     .  racpsi,a,
     .  gdr,r,ra,
     .  rho,somort,
     .  somortp,gdsomortp,somortpur,somortpurp,gdsomortpurp,
     .  sompropo,sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .  somtot,somtotp,gdsomtotp,segt)

cc<<<
c      write(0,*) '<=== Leaving maille'
cc>>>
      RETURN
c======================================================================
      END
