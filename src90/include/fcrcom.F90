      module fcrcom
      use KindDefinitions

      integer nnstr,nmstr,nsgmx,nrgnx,ntrgx,ngpr,ngpz,nxptm
      parameter (nsgmx=6, nrgnx=6, ntrgx=4, nxptm=2)
      parameter (ngpr=1025,ngpz=1025, nnstr=60, nmstr=1000)
      integer(Short) :: repart,nrelax,nxpt, &
     &  nsgm,nptseg(nsgmx),nrgn,npr(nrgnx),ntrg, &
     &  nstr,nstrv,lstr(nnstr),lstrv(nnstr),nclstr, &
     &  nr,nz, &
     &  lm_cnfg, &
     &  carre_mode, grid_ext_mode, equ_ext_mode, &
     &  vess_elm(nmstr), nvess, npp, fclbl(nmstr)
      logical ldgv2
      logical lclstr(nnstr), lclstrv(nnstr)
      real(Single) :: lm_pntrt,lm_grcln
      real(rKind) :: relax,pasmin,rlcept,pntrat, &
     &  deltp1(nsgmx),deltr1(nrgnx), &
     &  deltpn(nsgmx),deltrn(nrgnx),tgarde(ntrgx), &
     &  xstr(nmstr),ystr(nmstr),rbtor, &
     &  xptcntr(3,nxptm),xlpcntr(3),xpttol, &
     &  p1(3,nmstr), p2(3,nmstr), xstrv(nmstr), ystrv(nmstr)
      real(rKind) :: pfm(ngpr,ngpz),rgr(ngpr),zgr(ngpz)
      real(rKind) :: target_res
!
!*** nnstr : maximum number of structures to be imported from dg
!*** nmstr : maximum number of nodes in all the structures
!*** nsgmx : maximum number of "segments of separatrices" (Carre)
!*** nrgnx : maximum number of "regions" (Carre)
!*** ntrgx : maximum number of "targets" (Carre)
!*** ngpr  : maximum number of rows in pfm matrix
!*** ngpz  : maximum number of columns in pfm matrix
!*** nxptm : maximum number of pre-defined X-points
!*** nsgm  : actual number of "segments of separatrices" (Carre)
!*** nrgn  : actual number of "regions" (Carre)
!*** nptseg: number of "grid points" on each "segment of separatrices"
!*** deltp : first and last steps in point distribution on "segment"
!*** npr   : number of "flux surfaces" in each "region"
!*** deltr : first and last steps in surface distribution in "region"
!*** ntrg  : number of targets
!*** tgarde: guard length in front of targets
!*** nstr  : number of the structures imported from dg
!*** nstrv : number of additional vessel structures to be created/written for carre2
!*** lstr  : array containing their lengths
!*** nclstr: number of closed structures (they come first)
!*** xstr,
!*** ystr  : co-ordinates of the structure nodes
!*** rbtor : product of the major radius by the toroidal field
!*** pfm   : poloidal magnetic flux table
!*** rgr   : R values for pfm
!*** zgr   : Z values for pfm
!*** nr    : actual number of rows in pfm matrix
!*** nz    : actual number of columns in pfm matrix
!*** repart,
!*** nrelax,
!*** relax,
!*** pasmin,
!*** rlcept,
!*** pntrat : scalar parameters for Carre
!*** nxpt   : actual number of pre-defined X-points
!*** xptcntr: co-ordinates of the X-points
!*** xlpcntr: co-ordinates of the O-point
!*** xpttol : tolerance parameter for distinguishing the X-points
!*** ldgv2  : true if the input from DG v.2.0 or later

      end module fcrcom
