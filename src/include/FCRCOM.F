c  version : 05.02.99 13:35
c
      integer nnstr,nmstr,nsgmx,nrgnx,ntrgx,ngpr,ngpz,nxptm
      parameter (nsgmx=6, nrgnx=6, ntrgx=4, nxptm=2)
      parameter (ngpr=1025,ngpz=1025, nnstr=60, nmstr=1000)
      integer(kind=I4) repart,nrelax,nxpt,
     ,  nsgm,nptseg(nsgmx),nrgn,npr(nrgnx),ntrg,
     ,  nstr,lstr(nnstr),nclstr,
     ,  nr,nz,
     ,  lm_cnfg
      logical ldgv2
      real(kind=R4) lm_pntrt,lm_grcln
      real(kind=R8)
     ,  deltr1(nrgnx),deltrn(nrgnx),deltp1(nsgmx),deltpn(nsgmx),
     ,  xstr(nmstr),ystr(nmstr),xptcntr(3,nxptm),xlpcntr(3),
     ,  rbtor,relax,rlcept,pasmin,xpttol,pntrat,tgarde(ntrgx)
      real(kind=R8) pfm(ngpr,ngpz),rgr(ngpr),zgr(ngpz)
      common/fcrcom/pfm,rgr,zgr,
     ,  relax,pasmin,rlcept,pntrat,
     ,  deltp1,deltr1,deltpn,deltrn,tgarde,
     ,  xstr,ystr,rbtor,
     ,  xptcntr,xlpcntr,xpttol,
     ,  lm_pntrt,lm_grcln,
     i  repart,nrelax,nxpt,
     ,  nsgm,nptseg,nrgn,npr,ntrg,
     ,  nstr,lstr,nclstr,nr,nz,
     ,  lm_cnfg,
     l  ldgv2  
c
c*** nnstr : maximum number of structures to be imported from dg
c*** nmstr : maximum number of nodes in all the structures
c*** nsgmx : maximum number of "segments of separatrices" (Carre)
c*** nrgnx : maximum number of "regions" (Carre)
c*** ntrgx : maximum number of "targets" (Carre)
c*** ngpr  : maximum number of rows in pfm matrix
c*** ngpz  : maximum number of columns in pfm matrix
c*** nxptm : maximum number of pre-defined x-points
c*** nsgm  : actual number of "segments of separatrices" (Carre)
c*** nrgn  : actual number of "regions" (Carre)
c*** nptseg: number of "grid points" on each "segment of separatrices"
c*** deltp : first and last steps in point distribution on "segment"
c*** npr   : number of "flux surfaces" in each "region"
c*** deltr : first and last steps in surface distribution in "region"
c*** ntrg  : number of targets
c*** tgarde: guard length in front of targets
c*** nstr  : number of the structures imported from dg
c*** lstr  : array containing their lengths
c*** nclstr: number of closed structures (they come first)
c*** xstr,
c*** ystr  : coordinates of the structure nodes
c*** rbtor : product of the major radius by the toroidal field
c*** pfm   : poloidal magnetic flux table
c*** rgr   : R values for pfm
c*** zgr   : Z values for pfm
c*** nr    : actual number of rows in pfm matrix
c*** nz    : actual number of columns in pfm matrix
c*** repart,
c*** nrelax,
c*** relax,
c*** pasmin,
c*** rlcept,
c*** pntrat : scalar parameters for Carre
c*** nxpt   : actual number of pre-defined x-points
c*** xptcntr: coordinates of the x-points
c*** xlpcntr: coordinates of the o-point
c*** xpttol : tolerance parameter for distinguishing the x-points
c*** ldgv2  : true if the input from DG v.2.0 or later
