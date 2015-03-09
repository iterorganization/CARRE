      PROGRAM CARRE
c======================================================================
c
c  version : 1.01.99 15:49
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

*..  Ce programme permet de trouver la localisation des points X
*  pour un tokamak. Il parametrise ensuite les separatrices qui passent
*  par ces points X et il les trace ainsi que les structures reelles.
*  Une maille curviligne orthogonale alignee sur les surfaces de flux
*  est alors construite.

c*** This program builds an orthogonal curvilinear mesh aligned to the
c*** flux surfaces.  It finds the localisation of the X-points,
c*** parametrises the separatrices passing through them, and traces
c*** them together with the real structures.
c======================================================================
      IMPLICIT NONE

c  a modifier pour generaliser la geometrie:
c  modifier argsep et maille pour utiliser un critere plus general que
c  la droite ou la gauche dans la designation des separatrices.
c  Aussi, distribuer les points de facon proportionelle, et les ortho-
c  gonaliser par relaxation, avec une protection de proximite.

c  a faire:
c        1. permettre l'imposition de points fixes ou de coins


*  variables.

cank-970702: moved dimensions to the CARREDIM.F
#include <CARREDIM.F>
#include <COMLAN.F>
#include <git_version.h>

      REAL*8 cstlin,zero,eps_Xpt,rmax,zmax
      common /geom/ eps_Xpt
      PARAMETER (cstlin=0.00, zero=0.)
      INTEGER i, j, nx, ny, nstruc, npstru(strumx),
     .   ii(gradmx), jj(gradmx), iptx(npxmx), jptx(npxmx),
     .   npx, npxtot,ptsep(4,npxmx),
     .   nptot(4,npxmx), indplq(4,npxmx),inddef(nbdmx),
     .   nbdef, nivtot(nivmx), nbniv,ptxint,nreg,
     .   np1(nregmx), npr(nregmx), ierror,iflag,limcfg,
     .   itmp,nn1,sol,ir,nprad,ipol,nppol,mini,maxi,re,rp,rz,ireg,ro,rv,
     .   ry,modif,reb,rzb,rpb,chgt,nprsb(nregmx),
     .   rg,ipx,ig,ireg1,ireg2,idep,iarr,idp2,iar2,n
    
      REAL*8 x(nxmax), y(nymax), psi(nxmax,nymax),
     .   psidx(nxmax,nymax), psidy(nxmax,nymax),
     .   xstruc(npstmx,strumx), ystruc(npstmx,strumx),
     .   pointx(gradmx), pointy(gradmx),
     .   ptx(npxmx), pty(npxmx), xpto, ypto, fctpx(npxmx),
     .   separx(npnimx,4,npxmx), separy(npnimx,4,npxmx),
     .   nivx(npnimx,nivmx),nivy(npnimx,nivmx),distnv(5,nivmx),
     .   xn(npnimx),yn(npnimx),xmail(npmamx,nrmamx,nregmx),
     .   ymail(npmamx,nrmamx,nregmx),a00(nxmax,nymax,3),
     .   a10(nxmax,nymax,3),a01(nxmax,nymax,3),a11(nxmax,nymax,3), 
     .   pntrat,distxo,gdpsi(nrmamx,nregmx),racpsi(nrmamx,nregmx),
     .   a(nregmx),
     .   gdr(nrmamx,nregmx),
     .   r(nrmamx,nregmx),ra(nrmamx,nregmx),rho(nrmamx,nregmx),
     .   somort(nrmamx,nregmx),somortp(nrmamx,nregmx),
     .   gdsomortp(nregmx),xmini,
     .   xmaxi,ymini,ymaxi,somortpur(nrmamx,nregmx),
     .   somortpurp(nrmamx,nregmx),gdsomortpurp(nregmx),
     .   sompropo(nrmamx,nregmx),sompropop(nrmamx,nregmx),
     .   gdsompropop(nregmx),
     .   somvarr(nrmamx,nregmx),somvarrp(nrmamx,nregmx),
     .   gdsomvarrp(nregmx),
     .   somtot(nrmamx,nregmx),somtotp(nrmamx,nregmx),
     .   gdsomtotp(nregmx),segt(nrmamx,nregmx),
     .   vx(npmamx),vy(npmamx),ls,tgarde(4)

      LOGICAL racord, last
      character lign80*80,nomstr(strumx)*80

      REAL*8 stp0, stpmin
      PARAMETER (stp0=0.01,stpmin=0.001)

*..Procedures
*
      INTRINSIC ABS,MIN,index
	REAL*8 long
      EXTERNAL pltini,pltend,cadre,motifs,DERIVE,cntour,GRAD0,
     .   SELPTX,SPTRIS,ARGSEP,FRTIER,MAILLE,trace,trace2,entete
     ,        ,trc_stk_in,trc_stk_out,long
c======================================================================
c.. nxmax,nymax: maximum number of the data points in x and y
c.. gradmx: maximum number of points where the gradient vanishes
c.. npxmx : maximum number of the X-points
c.. nbdmx : maximum number of structures being the divertor targets
c.. strumx: maximum number of structures
c.. npstmx: maximum number of points per structure
c.. npnimx: maximum number of tracing points on a curve
c.. nivmx : maximum number of limiting level lines
c.. npmamx: maximum number of grid points in poloidal direction
c.. nrmamx: maximum number of grid points in radial direction
c.. nregmx: maximum number of regions
c.. cstlin: a linear constant added along y to artificially disconnect
c           the X-points. Set to 0 for connected double-nulls
c
c.. nx,ny : number of data points in x and y
c.. x,y   : tables of coordinates of the data points
c.. psi   : psi values at each data point
c.. psidx,psidy: values of psi derivatives in x and y
c                at each data point
c.. nstruc: number of structures
c.. npstru: number of points per structure
c.. xstruc,ystruc: coordinates of the structure points
c                  (point index, structure index)
c.. npxtot: number of the points where the gradient vanishes
c.. pointx,pointy: coordinates of the points where the gradient
c                  vanishes
c.. ii,jj : x and y indices of the cells where the gradient vanishes
c.. npx   : number of the X-points
c.. ptx,pty: X-point co-ordinates
c.. iptx,jptx: x and y indices of the cells containing the X-points
c.. xpto,ypto: coordinates of the O-point
c.. racord: determines whether the X-points are connected
c.. fctpx: the psi values at each X- or O-point
c.. separx,separy: coordinates of the points of the parametrised
c               separatrices (point index, branch index, X-point index)
c.. nptot : number of parametrisation points for each separatrix
c           (separatrix index, point index)
c.. ptsep : separatrix pointer, used as index
c           (separatrix index, point index)
c.. ptxint: index of the internal X-point in the case of disconnected
c           double-null
c.. nbdef : number of the divertor plates
c.. inddef: table of indices of the divertor plates
c.. indplq: table of the structure indices (0 means not a target)
c           (separatrix index, X-point index)
c.. nbniv : number of the limiting level lines
c.. nivx,nivy: coordinates of the points of the parametrised
c              limiting level lines (point index, curve index)
c.. nivtot: number of points for each parametrised limiting level line
c.. distnv: distance along a plate between the separatrix strike-point
c           and a limiting level line
c           (distance selector [1=real, 2=psi], curve index)
c.. nreg  : number of the grid regions depending on the configuration
c.. np1   : numbers of the grid points in poloidal direction
c           (region index)
c.. npr   : numbers of the grid points in radial direction
c           (region index)
c.. xmail,ymail: grid point coordinates
c                (poloidal, radial, region)
c.. xn,yn : working array for coordinates along a parametrised curve
c.. nrelax: maximum number of iterations in the relaxation procedure
c           of construction of the orthogonal grid
c.. relax : relaxation parameter
c.. stpmin: minimum tolerable distance between any two grid points
c           in the course of relaxation
c.. rlcept: convergence criterion in the relaxation procedure
c.. limcfg: indicates when a limiter configuration is considered (when
c           non zero). This variable is assigned the index of the
c           limiter
c     N.B.: When the limiter configuration is selected, npx is set equal
c           to 1 and the coordinates of the X-point correspond to the
c           innermost point of the limiter.

c      GRAND ET PETIT RAYON
c      sol: variable utilisée dans le cadre du maillage des régions 
c           périphériques. Si cette valeur=1 alors la région contient 
c           des courbes de niveau passant par ypto. On peut alors déterminer 
c           les grandeurs gdpsi,racpsi,gdr,r,a,rho.
C      gdr: grand rayon (lecture des surfaces poloidales sur l'axe radial)
c        r: petit rayon
c        a: petit rayon de la séparatrice
c       ra: différence entre la petit rayon de la séparatrice et le petit
c           rayon d'une ligne de niveau.
c      rho: rapport entre le ptit rayon de la séparatrice et le petit rayon
c           d'une ligne de niveau.       
c  sensspe: sens correspondant à la fausse plaque pour le calcul des rayons.
c

C      QUALITE DE LA MAILLE GLOBALE
*  les variables servent verifier la manière dont est quantifiee la qualité de
*  la maille (mailrg)
c    somort:(somme des orthogonalités avec ort1) valeur qui prend en compte 
c           l'orthogonalité, l'espace et la taille de deux cellules consecutives
c           ainsi que la répartition des points nppol entre une ligne de niveau 
c           et celle qui la précède.
c    somortp:(somme des orthogonalités poussée) cette valeur correspond au somort
c           à la fin des itérations avec l'utilisation de ort2, le tout divisé par nppol.
* les itérations fonctionnent correctement meme avec la deuxième ligne de niveau
* donc non seulement l'instruction fonctionne car la fonction se rapproche de zéro
* mais en plus elle n'est pas à l'origine de la déformation en zig-zag
* (de toute facon si c était le cas, on le constaterait systematiquement donc c'est 
* plutot la géométrie, la géographie ainsi que la longueur des plaques et des lignes
* de niveau qui deviennent les principaux suspects)
c    gdsomortp:(grande somme des orthogonalités poussée) cette valeur correspond
c             à l'ensemble des somortp sur une meme ligne de niveau que l'on
c             divise par nppol (le but est d'avoir un indice de qualité de la région)
* gdsomortp n'a pas grande signification et peut tromper sur la qualité de la ligne 
* de niveau. Il peut être utilisé pour comparer l'influence de chaque composante 
* de la fonction ort.

C     INFLUENCE DE LA VRAIE ORTHOGONALITE SUR CLORT
c     ortpur:(orthogonalité pure) cette valeur ne tient compte que de l'orthogonalité
* ortpur peut prendre des valeurs un million de fois plus élévées que la somme des trois 
* critères donc il doit y avoir une atténuation dans le calcul de ort.
c     propo: (proportionalité) cette valeur ne tient compte que de la proportionalité
c     varr: (variance) cette valeur ne tient compte que de la variation de longueur 
c     des cellules de la maille pour trois cellules qui se suivent et sur une même ligne de niveau.
c     tot:(total) correspond à la somme de ortpur, propo et varr. on verifie juste que tot = ort
c     somortpur, sompropo, somvarr, somtot : correspondent aux sommes de ortpur, propo,
c      varr et tot pour une ligne de niveau lors du premier déplacement des noeuds.
c     somortpurp, sompropop, somvarrp, somtotp : correspondent aux sommes de ortpur,
c      propo, varr et tot pour une ligne de niveau après les itérations.
c     gdsomortpurp, gdsompropop, gdsomvarr et gdsomtotp: correspondent à la somme,
c      sur toutes les lignes de niveau de la région de somortpur, sompropo, somvarr et somtotpt.

C     CORRECTION DES DEFORMATIONS
c     segt: (segment) cette variable devait servir pour le suivi automatique du grossissement,
c     elle correspond la longueur totale de la ligne de niveau (l(nppol))  
c     modif: interrupteur qui permet de sauter l'étape de modification de déplacement
c     des noeuds (choix de l'utilisateur)
c     rr : reponse à la redefinition (si choix = 1)
c     pra : premiere reponse 
c     sra: seconde réponse
c     tra: troisieme réponse
c     la troisième lettre indique la position du segemnt: a le premier, b le deuxième,
c     c le troisième,...
c     coefa,coefb,...: coefficient de l'itération qui permet de multiplier un segment
c     de longueur ipol(1)-ipol(2) par une valeur généralement comprise entre 1 et 1.03
c     (à l'utilisateur de choisir) 
c     choix:correspond au choix de l'utilisateur pour la correction des déformations
c     =1 si tous les points sont déplacés simultanément, =2 si c'est par fragmentation,
c     =3 si les coefficients sont ceux entrés dans le programme, =4 si par fragmentation
c     assisté d'un grossissment
c     deci:décision de l'utilisateur    

c     GROSSISSEMENT AUTOMATIQUE
c     lsp : longueur du segment parfait. correspond à la taille et la partie du 
c      fragment de la ligne de niveau qui doit être représenté dans le zoom, elle
c      sert de réference dans l'instruction de calcul.
c     ls : longueur du segment. correspond à la taille du segment de la ligne de
c     niveau pour chaque indice nppol calculé. 
C     idep: point de départ
c     iarr: point d'arrivée
c     tpr: valeur temporaire. conserve la valeur du point d'arrivée du segment
c     précédent pour en faire le point de départ du segment suivant.  
c     deltay: difference entre ymaxi et ymini
c     deltax: difference entre xmaxi et xmini
c     deldelta(x ou y): difference entre les deux delta
c     disdelta(x ou y): dispersion de delta, correspond à l'agrandissement des
c      des bords sur le plus petit axe
c     rpt(rapport): nombre de points dans le segment de séparatrice (=npimx)
c     rptd(rapport par deux): première moitié des points du segment de separatrice
c     rptt(rapport par trois): premier tiers des points du segment de separatrice
c     rptq: premier quart des points du segment de separatrice

c======================================================================
*
*..1.0  Initialisation des variables par defaut et de la bibliotheque
*       graphique
*
      call defaut
      CALL pltini
      CALL cadre
      CALL motifs
*
*..2.0  Open the data files
*
      OPEN(UNIT=7, FILE='rzpsi.dat', STATUS='old')
      OPEN(UNIT=8, FILE='structure.dat', STATUS='old')
      OPEN(UNIT=9, FILE='carre.dat', STATUS='unknown')
      OPEN(UNIT=10,FILE='carre.out',STATUS='unknown')
      open(unit=11,status='scratch')
c
c  2.1  specify output format
      write(10,*)'output format: carre70; version : ', gitversion

*
*..3.0  Read the data
101   CONTINUE
  
*..Read the values of x
      
100   format(a)
     
      iflag=-1
      rewind(7)
      rewind(8)
      rewind(9)
      rewind(10)
      call entete(7,'$r',iflag)
      read(7,100)lign80
      i=index(lign80,'=')
      call rdfrin(11,lign80(i+1:80),nx,ierror)
      READ(7,*) (x(i), i=1, nx)

*..Read the values of y

      call entete(7,'$z',iflag)
      read(7,100)lign80
      i=index(lign80,'=')
      call rdfrin(11,lign80(i+1:80),ny,ierror)
      READ(7,*) (y(i), i=1, ny)

      rmax = 0.0
      do i = 1, nx-1
        rmax = max(rmax, (x(i+1)-x(i))**2)
      enddo
      zmax = 0.0
      do i = 1, ny-1
        zmax = max(zmax, (y(i+1)-y(i))**2)
      enddo
      eps_Xpt = sqrt(rmax+zmax)

*..Read the values of psi

      call entete(7,'$psi',iflag)
      READ(7,*) ((psi(i,j), i=1, nx), j=1, ny)

      DO 5 j=1,ny
         DO 4 i=1,nx
            psi(i,j) = psi(i,j) + MIN(cstlin*(y(j)-y(ny/2)),zero)
    4    CONTINUE
    5 CONTINUE
c---
c  ceci sert a modifier la symetrie haut-bas de psi, de facon ad hoc.
c     write(6,*)'facteur de symetrie haut-bas'
c     read(5,*)a00(1,1,1)
c     do j=1,ny/2
c     do i=1,nx
c       xpto=psi(i,j)
c       ypto=psi(i,ny-j+1)
c       psi(i,j)=0.5*((1.+a00(1,1,1))*xpto+(1.-a00(1,1,1))*ypto)
c       psi(i,ny-j+1)=0.5*((1.+a00(1,1,1))*ypto+(1.-a00(1,1,1))*xpto)
c     enddo
c     enddo
c---
      close(unit=7)

*
*..3.1  Read the structures.

      nstruc=0
      call listru(8,nstruc,npstru,nomstr,xstruc,ystruc,npstmx,strumx)

*
*..4.0  Calculate the first partial derivatives in x and y and store
*       them in arrays psidx and psidy

      CALL DERIVE(nx,ny,x,y,psi,psidx,psidy)

*
*..5.0  Plot the level lines for psidx=0 and psidy=0
*
      CALL cntour(psidx,psidy,nx,ny,x(1),x(nx),y(1),y(ny))
c
c  interpolation coefficients for psi and its derivatives

      call inipsi(psi,psidx,psidy,x,y,nxmax,nymax,nx,ny,a00,a10,a01,a11)

*
*..6.0  Determine the points where the derivatives in x and y vanish
*

      CALL GRAD0(nxmax,nymax,nx,ny,x,y,gradmx,pointx,
     .          pointy,ii,jj,npxtot,a00,a10,a01,a11)

*
*..7.0  Select the X-points of interest
*

cank-970702: moved dimensions to the included file
      CALL SELPTX(npxtot,npx,pointx,pointy,ii,jj,ptx,
     .            pty,iptx,jptx,xpto,ypto,racord,limcfg)


*  
         
*..8.0  Parametrise the separatrices
*
      IF (npx.GT.0 .and. limcfg.eq.0) THEN
c
        CALL SPTRIS(nx,ny,x,y,psi,npx,ptx,pty,
     .      iptx,jptx,fctpx,separx,separy,nptot,
     .      nstruc,npstru,xstruc,ystruc,indplq,inddef,nbdef,
     .      a00,a10,a01,a11)
        
	  
        
*
*..9.0  Arrange the separatrices
*

        CALL ARGSEP(npx,ptx,pty,fctpx,separx,separy,indplq,nptot,npnimx,
     .            ptsep,racord,ptxint,ypto,nbdef,inddef)

      ELSEIF(LIMCFG.NE.0) THEN
*
*  13.   Identify the limiter
*
        call limfnd(xpto,ypto,nivx,nivy,stp0,stpmin,distnv,nivtot,
     .      nbniv,nx,ny,x,y,psi,npx,ptx,pty,fctpx,
     .      nstruc,npstru,xstruc,ystruc,indplq,inddef,nbdef,
     .      a00,a10,a01,a11)

	do itmp=1,4
	  ptsep(itmp,1) = 0
	enddo

      ENDIF

      if(npx.gt.0) then
*
*..10.0  Find the level lines in more detail
*
c<<<
	write(0,*) '=== carre *..10.0 - before frtier'
	write(0,'(5h ptx:,1p,8e12.4/(5x,8e12.4))') ptx(1:npx)
	write(0,'(5h pty:,1p,8e12.4/(5x,8e12.4))') pty(1:npx)
        if(limcfg.eq.0) then
	  write(0,*) 'nptot(4,nxpoints)'
	  write(0,'(1x,16i5)') ((nptot(i,j),i=1,4),j=1,npx)
	  write(0,*) 'Strike points (presumably)'
	  write(0,'(3h x:,1p,8e12.4/(3x,8e12.4))')
     -	   ((separx(nptot(i,j),i,j),i=1,4),j=1,npx)
	  write(0,'(3h y:,1p,8e12.4/(3x,8e12.4))')
     -	   ((separy(nptot(i,j),i,j),i=1,4),j=1,npx)
c>>>
          call trc_stk_in('carre','*..10.0')
          CALL FRTIER(nx,ny,x,y,psi,nstruc,
     .      npstru,xstruc,ystruc,inddef,nbdef,npx,separx,
     .      separy,nptot,ptsep,racord,nivx,nivy,nivtot,
     .      nbniv,stp0,stpmin,
     .      distnv,ptxint,a00,a10,a01,a11)
          call trc_stk_out
        endif
      
	call trace2(x(1),x(nx),y(1),y(ny),separx,separy,
     _	       ptsep,npx,nptot,
     _	       nstruc,npstru,xstruc,ystruc,
     _	       nivx,nivy,nivtot,nbniv,pntrat,
     _         distxo,xn,yn,nn1)

*
*..11.0  Grid the regions
*
        modif=0
        CALL MAILLE(nx,ny,x,y,psi,npx,xpto,ypto,racord,
     .    separx,separy,ptsep,nptot,distnv,ptxint,nstruc,npstru,
     .    xstruc,ystruc,inddef,nreg,xn,yn,xmail,ymail,
     .    np1,npr,ptx,pty,nivx,nivy,nivtot,nbniv,
     .    a00,a10,a01,a11,fctpx,limcfg,gdpsi,racpsi,
     .    a,gdr,r,ra,rho,sol,somort,ir,nprad,ipol,somortp,
     .    nppol,gdsomortp,somortpur,somortpurp,gdsomortpurp,
     .    sompropo,
     .    sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .    somtot,somtotp,gdsomtotp,segt,ireg,modif,xmini,xmaxi,
     .    ymini,ymaxi,re,distxo,pntrat,nprsb,tgarde)

c*
c* WARNINGS CALCULATION AND OUTPUT
c*
c        if (npx.EQ.1) CALL WARNINGS(separx,separy,nivx,nivy,
c     &  distnv(1,2),xpto,ypto)

*
*..12.0  Plot the structures, the separatrices, and the limiting level
*        lines for each region, together with the resulting grid
*

        if (modif.eq.0) call endpag
        call trace(x(1),x(nx),y(1),y(ny),separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,npr,xmail,ymail,nreg,.false.)

#ifdef EXTRA_GRAPHS
*  13.0 on propose à l'utilisateur dans le cas simple-nul s'il souhaite
*       observer quelques agrandissements le long de la séparatrice.
* les valeurs ci-dessous correspondent aux positions des segments de séparatrice dans 
* un cas de réacteur et ont ete selectionné parmi plusieurs dizaines de clichés effectués 
* manuellement. Pour adapter les grossissements suivants à tous les cas de figure, il 
* faut utiliser les coordoonées de la séparatrice en xmini, xmaxi, ymini et ymaxi.
* Une des possibilités consisterait à fragmenter la séparatrice à prendre la longueur de 
* chaque segment en fonction du nombre de points total de la séparatrice. Toutefois, 
* comme avec les lignes de niveau, les grossissements peuvent être de mauvaise qualité.
* Il est probable que le delta x et le delta y soient trop differents, ce qui ecraserait
* le dessin. Pour éviter ce genre de problème, l'un des quatre bords du cadre devra dependre 
* des trois autres. 1) faire la difference en x et la difference en y (xmaxi-xmini et 
* ymaxi-ymini). 2) faire la difference des deux (delta x - delta y)
* 2 bis) penser à faire une étude sur la limite de qualité du grossissement, c'est
* à dire voir à partir de quand la difference entre les deux delta écrase le dessin.
* 3) si la difference entre les deux delta est trop grande alors le delta le plus
* grand entre delta x et delta y devient la référence et la difference entre les
* deux delta est distribuée equitablement sur les bords de l'autre delta.

  555   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Souhaitez-vous effectuer un suivi automatique?'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Do you wish to have an automated follow-up?'
        endif
   88   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Type 1 for yes and 2 for no'
        endif
	read(5,*,end=31, err=88) re

        if (re.eq.1) then
           last = re.eq.1
           call endpag

	   xmini= 2.44
	   xmaxi= 2.60
	   ymini= -1.48
	   ymaxi= -1.32
           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Région 2 près de la plaque 1'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'Region 2 near plate 1'
           endif
           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)
       
           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Observation du point X'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'X-point region'
           endif
           xmini= 2.36
	   xmaxi= 2.56
	   ymini= -1.60
	   ymaxi= -1.40

           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 près du point X par la gauche'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'HFS separatrix segment above X-point'
           endif
           xmini= 2.35
	   xmaxi= 2.55
	   ymini= -1.40
	   ymaxi= -1.20

           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 à gauche'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'HFS separatrix'
           endif
           xmini= 1.95
	   xmaxi= 2.15
	   ymini= -0.20
	   ymaxi= 0.00

           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 proche du sommet'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'HFS separatrix near top of machine'
           endif
           xmini= 2.00
	   xmaxi= 2.20
	   ymini= 0.94
	   ymaxi= 1.14
           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 au sommet'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'Separatrix at top of machine'
           endif
           xmini= 2.60
	   xmaxi= 2.80
	   ymini= 1.60
	   ymaxi= 1.80
           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 proche du sommet à droite'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'LFS separatrix near top of machine'
           endif
           xmini= 3.30
	   xmaxi= 3.50
	   ymini= 1.20
	   ymaxi= 1.40

           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 à droite'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'LFS separatrix near outer midplane'
           endif
           xmini= 3.70
	   xmaxi= 3.90
	   ymini= 0.00
	   ymaxi= 0.20
           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Frontière 1/3 proche du point X par la droite'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'LFS separatrix above X-point'
           endif
           xmini= 2.70
	   xmaxi= 2.90
	   ymini= -1.30
	   ymaxi= -1.10

           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Région 2 près du point X'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'Region 2 near X-point'
           endif
           xmini= 2.50
	   xmaxi= 2.70
	   ymini= -1.60
	   ymaxi= -1.40

           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,last)

           if(sellan(1:8).eq.'francais') then
             write(6,*) 'Région 2 près de la plaque 2'
           elseif(sellan(1:7).eq.'english') then
             write(6,*) 'Region 2 near plate 2'
           endif
           xmini= 2.60
	   xmaxi= 2.80
	   ymini= -1.70
	   ymaxi= -1.50
        
           call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,npr,xmail,ymail,nreg,.not.last)
       
        endif
        
  987	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Voir la région 2?'
          write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Do you wish to see region 2?'
          write(6,*) 'Type 1 for yes and 2 for no'
        endif
	read(5,*,end=39, err=987) ro

   39   if (ro.eq.1) then
          call endpag
          xmini= 2.40
	  xmaxi= 2.70
	  ymini= -1.70
	  ymaxi= -1.40
          call trace(xmini,xmaxi,ymini,ymaxi,
     .           separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,npr,xmail,ymail,nreg,.false.)
        endif

 986	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Voir la séparatrice à la frontière 1/2 ?'
          write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Do you wish to see the separatrix to region 2?'
          write(6,*) 'Type 1 for yes and 2 for no'
        endif
        read(5,*,end=36, err=986) ry

   36   if (ry.eq.1) then
          call endpag
          xmini= 2.55
	  xmaxi= 2.60
	  ymini= -1.52
          ymaxi= -1.47
          call trace(xmini,xmaxi,ymini,ymaxi,
     .           separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,.true.)

          xmini= 2.58
	  xmaxi= 2.63
	  ymini= -1.58
          ymaxi= -1.53
          call trace(xmini,xmaxi,ymini,ymaxi,
     .		 separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,.true.)

          xmini= 2.6
	  xmaxi= 2.65
	  ymini= -1.6
          ymaxi= -1.55
          call trace(xmini,xmaxi,ymini,ymaxi,
     .		 separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,.true.)

          xmini= 2.63
	  xmaxi= 2.68
	  ymini= -1.63
	  ymaxi= -1.58
          call trace(xmini,xmaxi,ymini,ymaxi,
     .		   separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,
     .           nivx,nivy,nivtot,nbniv,np1,npr,xmail,ymail,nreg,.true.)

          xmini= 2.65
	  xmaxi= 2.7
	  ymini= -1.65
	  ymaxi= -1.6
          call trace(xmini,xmaxi,ymini,ymaxi,
     .           separx,separy,ptsep,npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,npr,xmail,ymail,nreg,.false.)

        endif

        if(sellan(1:8).eq.'francais') then
          write(6,*) 'Souhaitez-vous revoir la séparatrice?'
        else if(sellan(1:7).eq.'english') then
          write(6,*) 'Do you wish to see the separatrix again?'
        endif
   76   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Type 1 for yes and 2 for no'
        endif
	read(5,*,end=31, err=76) rv
        if (rv.eq.1) go to 36

  666   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Souhaitez-vous effectuer un agrandissement?'
        else if(sellan(1:7).eq.'english') then
          write(6,*) 'Do you wish to make a zoom?'
        endif
   77   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Type 1 for yes and 2 for no'
        endif
	read(5,*,end=31, err=77) re

   31   if (re.eq.2) then
          GO TO 119
	endif
	
    9	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Définissez les bords.'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Define the edges.'
        endif
   10	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Limite inférieure en R'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Lower bound in R'
        endif
	read(5,*,end=11, err=10) xmini
   11	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Limite supérieure en R'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Upper bound in R'
        endif
	read(5,*,end=12,err=11) xmaxi
   12	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Limite inferieure en Z'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Lower bound in Z'
        endif
	read (5,*,end=13,err=12) ymini
   13	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Limite superieure en Z'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Upper bound in Z'
        endif
	read(5,*,end=14,err=13) ymaxi
 
   14   call endpag
        call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,npr,xmail,ymail,nreg,.false.)
         
        rp = 1
        do while (rp.eq.1)
  913	  if(sellan(1:8).eq.'francais') then
            write(6,*) 'Voulez-vous faire un autre agrandissement?'
	    write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
          else if(sellan(1:7).eq.'english') then
            write(6,*) 'Do you wish to make another zoom?'
            write(6,*) 'Type 1 for yes and 2 for no'
          endif
          read(5,*,end=69, err=913) rp
   69     continue 
          if (rp.eq.1) then
            if(sellan(1:8).eq.'francais') then
              write(6,*) 'Définissez les bords.'
            elseif(sellan(1:7).eq.'english') then
              write(6,*) 'Define the edges.'
            endif
   70	    if(sellan(1:8).eq.'francais') then
              write(6,*) 'Limite inférieure en R'
            elseif(sellan(1:7).eq.'english') then
              write(6,*) 'Lower bound in R'
            endif
            read(5,*,end=71, err=70) xmini
   71	    if(sellan(1:8).eq.'francais') then
              write(6,*) 'Limite supérieure en R'
            elseif(sellan(1:7).eq.'english') then
              write(6,*) 'Upper bound in R'
            endif
	    read(5,*,end=72,err=71) xmaxi
   72	    if(sellan(1:8).eq.'francais') then
              write(6,*) 'Limite inférieure en Z'
            elseif(sellan(1:7).eq.'english') then
              write(6,*) 'Lower bound in Z'
            endif
	    read (5,*,end=73,err=72) ymini
   73	    if(sellan(1:8).eq.'francais') then
              write(6,*) 'Limite supérieure en Z'
            elseif(sellan(1:7).eq.'english') then
              write(6,*) 'Upper bound in Z'
            endif
	    read(5,*,end=74,err=73) ymaxi
 
   74       call endpag
            call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,npr,xmail,ymail,nreg,.false.)

	  endif
	enddo
  119   continue

	if(sellan(1:8).eq.'francais') then
          write(6,*) 'Souhaitez-vous voir les gardes?'
        else if(sellan(1:7).eq.'english') then
          write(6,*) 'Do you wish to see the guard regions?'
        endif
   78   if(sellan(1:8).eq.'francais') then
          write(6,*) 'Répondez 1 si c''est oui ou 2 si c''est non'
        elseif(sellan(1:7).eq.'english') then
          write(6,*) 'Type 1 for yes and 2 for no'
        endif
	read(5,*,end=1119, err=78) rg

	if (rg.eq.1) then

	  do ipx = 1, npx

	    do ig = 1, 2

              if (npx.eq.1) then
	        ireg1 = 1
	        ireg2 = 2
	      else if (racord) then
	        ireg1 = (ig-1)*2+1
	        ireg2 = 2*ipx
              else ! non raccorde
	        ireg2 = 2*ipx+1
	        if (ipx.eq.ptxint) then
	          ireg1 = 1
	        else
	          ireg1 = 2*ig
                endif
              end if
	          
              if (tgarde((ipx-1)*2+ig).gt.0.) then
	          
	        if (ig.eq.1) then
                  idep = 1
	          iarr = 1
	          idp2 = 1
	          iar2 = 1
	        else if (ig.eq.2) then
	          idep = np1(ireg1)
	          iarr = np1(ireg1)
	          idp2 = np1(ireg2)
	          iar2 = np1(ireg2)
	        endif

	        vx(1)=xmail(idep,1,ireg1)
	        vy(1)=ymail(idep,1,ireg1)
	        ls = 0
	        n = 0
	        do while (ls.lt.tgarde((ipx-1)*2+1)
     .           .and. n.lt.np1(ireg1)) 
	          if (ig.eq.1) then
	            iarr=iarr+1
	            iar2=iar2+1
	            vx(iarr-idep+1)=xmail(iarr,1,ireg1)
                    vy(iarr-idep+1)=ymail(iarr,1,ireg1)
	          else if (ig.eq.2) then
	            idep=idep-1
	            idp2=idp2-1
	            vx(iarr-idep+1)=xmail(idep,1,ireg1)
                    vy(iarr-idep+1)=ymail(idep,1,ireg1)
	          endif
	          n=iarr-idep+1
	          ls=long(vx,vy,n)
                enddo
	          
	        xmini= min(minval(xmail(idep:iarr,1:npr(ireg1),ireg1)),
     .                     minval(xmail(idp2:iar2,1:npr(ireg2),ireg2)))
	        xmaxi= max(maxval(xmail(idep:iarr,1:npr(ireg1),ireg1)),
     .                     maxval(xmail(idp2:iar2,1:npr(ireg2),ireg2)))
	        ymini= min(minval(ymail(idep:iarr,1:npr(ireg1),ireg1)),
     .                     minval(ymail(idp2:iar2,1:npr(ireg2),ireg2)))
	        ymaxi= max(maxval(ymail(idep:iarr,1:npr(ireg1),ireg1)),
     .                     maxval(ymail(idp2:iar2,1:npr(ireg2),ireg2)))

                call endpag
                call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .             npx,nptot,
     .             nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .             np1,npr,xmail,ymail,nreg,.false.)
	      end if
	    end do

          end do
        end if

 1119   continue
#endif
        call endpag

      ENDIF
    
*
*.. Close the graphics
*
  111  CALL pltend

      STOP
      END
