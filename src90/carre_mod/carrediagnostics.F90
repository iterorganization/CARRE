module CarreDiagnostics

  use KindDefinitions
  use carre_dimensions

  implicit none

  private 
  public CarreDiag, cdClearRegion, cdClear

  !> Derived type holding carre diagnostic output
  type CarreDiag

!!$           INTEGER
!!$          ir,nprad,ipol,nppol,mini,maxi,re,rp,rz,ireg,ro,rv, & 
!!$               &   ry,modif,reb,rzb,rpb,chgt,nprsb(nregmx), & 
!!$               &   rg,ipx,ig,ireg1,ireg2,idep,iarr,idp2,iar2,n
!!$
!!$          REAL*8 
!!$          pntrat,distxo,, & 
!!$               & ,xmini, & 
!!$               &   xmaxi,ymini,ymaxi
!!$          &   , & 
!!$               &   vx(npmamx),vy(npmamx),ls,tgarde(4)
          

          ! GRAND ET PETIT RAYON
          !      sol: variable utilisée dans le cadre du maillage des régions
          !           périphériques. Si cette valeur=1 alors la région contient
          !           des courbes de niveau passant par ypto. On peut alors déterminer
          !           les grandeurs gdpsi,racpsi,gdr,r,a,rho.
          integer :: sol
          !      undocumented:
          real(rKind) :: racpsi(nrmamx,nregmx), segt(nrmamx,nregmx), gdpsi(nrmamx,nregmx)
          !      gdr: grand rayon (lecture des surfaces poloidales sur l'axe radial)
          real(rKind) :: gdr(nrmamx,nregmx)
          !        r: petit rayon
          real(rKind) :: r(nrmamx,nregmx)
          !        a: petit rayon de la séparatrice
          real(rKind) :: a(nregmx)
          !       ra: différence entre la petit rayon de la séparatrice et le petit
          !           rayon d'une ligne de niveau.
          real(rKind) :: ra(nrmamx,nregmx)
          !      rho: rapport entre le ptit rayon de la séparatrice et le petit rayon
          !           d'une ligne de niveau.
          real(rKind) :: rho(nrmamx,nregmx)
          !  ???sensspe: sens correspondant à la fausse plaque pour le calcul des rayons.
                              
          !      QUALITE DE LA MAILLE GLOBALE
          !  les variables servent verifier la manière dont est quantifiee la qualité de
          !  la maille (mailrg)
          !    somort:(somme des orthogonalités avec ort1) valeur qui prend en compte
          !           l'orthogonalité, l'espace et la taille de deux cellules consecutives
          !           ainsi que la répartition des points nppol entre une ligne de niveau
          !           et celle qui la précède.
          !    somortp:(somme des orthogonalités poussée) cette valeur correspond au somort
          !           à la fin des itérations avec l'utilisation de ort2, le tout divisé par nppol.
          real(rKind) :: somort(nrmamx,nregmx),somortp(nrmamx,nregmx)
               
               
          ! les itérations fonctionnent correctement meme avec la deuxième ligne de niveau
          ! donc non seulement l'instruction fonctionne car la fonction se rapproche de zéro
          ! mais en plus elle n'est pas à l'origine de la déformation en zig-zag
          ! (de toute facon si c était le cas, on le constaterait systematiquement donc c'est
          ! plutot la géométrie, la géographie ainsi que la longueur des plaques et des lignes
          ! de niveau qui deviennent les principaux suspects)
          !    gdsomortp:(grande somme des orthogonalités poussée) cette valeur correspond
          !             à l'ensemble des somortp sur une meme ligne de niveau que l'on
          !             divise par nppol (le but est d'avoir un indice de qualité de la région)
          ! gdsomortp n'a pas grande signification et peut tromper sur la qualité de la ligne
          ! de niveau. Il peut être utilisé pour comparer l'influence de chaque composante
          ! de la fonction ort.
          real(rKind) :: gdsomortp(nregmx)

          ! INFLUENCE DE LA VRAIE ORTHOGONALITE SUR CLORT
          ! ortpur:(orthogonalité pure) cette valeur ne tient compte que de l'orthogonalité
          ! ortpur peut prendre des valeurs un million de fois plus élévées que la somme des trois
          ! critères donc il doit y avoir une atténuation dans le calcul de ort.

          ! propo: (proportionalité) cette valeur ne tient compte que de la proportionalité
          ! varr: (variance) cette valeur ne tient compte que de la variation de longueur
          ! des cellules de la maille pour trois cellules qui se suivent et sur une même ligne de niveau.
          ! tot:(total) correspond à la somme de ortpur, propo et varr. on verifie juste que tot = ort
          ! somortpur, sompropo, somvarr, somtot : correspondent aux sommes de ortpur, propo,
          ! varr et tot pour une ligne de niveau lors du premier déplacement des noeuds.
          real(rKind) :: somortpur(nrmamx,nregmx), sompropo(nrmamx,nregmx), &
               & somvarr(nrmamx,nregmx)!, somtot(nrmamx,nregmx)

          ! somortpurp, sompropop, somvarrp, somtotp : correspondent aux sommes de ortpur,
          ! propo, varr et tot pour une ligne de niveau après les itérations.
          real(rKind) :: somortpurp(nrmamx,nregmx), sompropop(nrmamx,nregmx), &
               & somvarrp(nrmamx,nregmx)!, somtotp(nrmamx,nregmx)
       
          ! gdsomortpurp, gdsompropop, gdsomvarr et gdsomtotp: correspondent à la somme,
          ! sur toutes les lignes de niveau de la région de somortpur, sompropo, somvarr et somtotpt.
          real(rKind) :: gdsomortpurp(nregmx), gdsompropop(nregmx), &
               & gdsomvarrp(nregmx) !, gdsomtotp(nregmx)


  end type CarreDiag


contains

  subroutine cdClearRegion( diag, ireg )
    type(CarreDiag), intent(inout) :: diag
    integer, intent(in) :: ireg
    
    call cdClear( diag, ireg, ireg )

  end subroutine cdClearRegion


  subroutine cdClear( diag, irf, irt )
    
    type(CarreDiag), intent(inout) :: diag
    integer, intent(in) :: irf, irt

    diag%somort(:,irf:irt)=0.0
    diag%somortp(:,irf:irt)=0.0
    diag%somortpurp(:,irf:irt)=0.0
    diag%sompropo(:,irf:irt)=0.0
    diag%sompropop(:,irf:irt)=0.0
    diag%somortpur(:,irf:irt)=0.0
    diag%somvarr(:,irf:irt)=0.0
    diag%somvarrp(:,irf:irt)=0.0
    diag%segt(:,irf:irt)=0.0

    diag%gdsomortp(irf:irt)=0.0
    diag%gdsompropop(irf:irt)=0.0
    diag%gdsomortpurp(irf:irt)=0.0
    diag%gdsomvarrp(irf:irt)=0.0

  end subroutine cdClear
    
end module CarreDiagnostics














!!$! carre.F90
!!$integer
!!$sol,ir,nprad,ipol,nppol,mini,maxi,re,rp,rz,ireg,ro,rv, & 
!!$     &   ry,modif,reb,rzb,rpb,chgt,nprsb(nregmx), & 
!!$     &   rg,ipx,ig,ireg1,ireg2,idep,iarr,idp2,iar2,n
!!$
!!$REAL*8 
!!$pntrat,distxo,gdpsi(nrmamx,nregmx),racpsi(nrmamx,nregmx), & 
!!$     &   a(nregmx), & 
!!$     &   gdr(nrmamx,nregmx), & 
!!$     &   r(nrmamx,nregmx),ra(nrmamx,nregmx)
!!$&   somort(nrmamx,nregmx),somortp(nrmamx,nregmx), & 
!!$     &   gdsomortp(nregmx),xmini, & 
!!$     &   xmaxi,ymini,ymaxi,somortpur(nrmamx,nregmx), & 
!!$     &   somortpurp(nrmamx,nregmx),gdsomortpurp(nregmx), & 
!!$     &   sompropo(nrmamx,nregmx),sompropop(nrmamx,nregmx), & 
!!$     &   gdsompropop(nregmx), & 
!!$     &   somvarr(nrmamx,nregmx),somvarrp(nrmamx,nregmx), & 
!!$     &   gdsomvarrp(nregmx), & 
!!$     &   somtot(nrmamx,nregmx),somtotp(nrmamx,nregmx), & 
!!$     &   gdsomtotp(nregmx),segt(nrmamx,nregmx), & 
!!$     &   vx(npmamx),vy(npmamx),ls,tgarde(4)
!!$
!!$! MAILLE
!!$! call
!!$gdpsi,racpsi, & 
!!$     &    a,gdr,r,ra,rho,sol,somort,ir,nprad,ipol,somortp, & 
!!$     &    nppol,gdsomortp,somortpur,somortpurp,gdsomortpurp, & 
!!$     &    sompropo, & 
!!$     &    sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp, & 
!!$     &    somtot,somtotp,gdsomtotp,segt,ireg,modif,xmini,xmaxi, & 
!!$     &    ymini,ymaxi,re,distxo,pntrat,nprsb,tgarde
!!$
!!$integer
!!$nn1, & 
!!$     &        sol,ir,nprad,ipol,nppol,ireg,modif,re,nprsb(nregmx)
!!$
!!$real
!!$&  gdpsi(nrmamx,nregmx),racpsi(nrmamx,nregmx),xrad(nrmamx), & 
!!$     &  yrad(nrmamx), & 
!!$     &  fctrad(nrmamx), & 
!!$     &  a(nregmx), & 
!!$     &  gdr(nrmamx,nregmx), & 
!!$     &  r(nrmamx,nregmx),ra(nrmamx,nregmx), & 
!!$     &  rho(nrmamx,nregmx),somort(nrmamx,nregmx),somortp(nrmamx,nregmx), & 
!!$     &  gdsomortp(nregmx),somortpur(nrmamx,nregmx), & 
!!$     &  somortpurp(nrmamx,nregmx),gdsomortpurp(nregmx), & 
!!$     &  sompropo(nrmamx,nregmx),sompropop(nrmamx,nregmx), & 
!!$     &  gdsompropop(nregmx), & 
!!$     &  somvarr(nrmamx,nregmx),somvarrp(nrmamx,nregmx), & 
!!$     &  gdsomvarrp(nregmx), & 
!!$     &  somtot(nrmamx,nregmx),somtotp(nrmamx,nregmx), & 
!!$     &  gdsomtotp(nregmx),segt(nrmamx,nregmx), & 
!!$     &  xmini,xmaxi,ymini,ymaxi,distxo,pntrat,tgarde(4)
!!$
!!$
!!$! mailrg
!!$&  gdpsi(nrmamx,nregmx),racpsi(nrmamx,nregmx),xrad(nrmamx), & 
!!$     &  yrad(nrmamx), & 
!!$     &  fctrad(nrmamx), & 
!!$     &  a(nregmx), & 
!!$     &  gdr(nrmamx,nregmx), & 
!!$     &  r(nrmamx,nregmx),ra(nrmamx,nregmx), & 
!!$     &  rho(nrmamx,nregmx),somort(nrmamx,nregmx),somortp(nrmamx,nregmx), & 
!!$     &  gdsomortp(nregmx),somortpur(nrmamx,nregmx), & 
!!$     &  somortpurp(nrmamx,nregmx),gdsomortpurp(nregmx), & 
!!$     &  sompropo(nrmamx,nregmx),sompropop(nrmamx,nregmx), & 
!!$     &  gdsompropop(nregmx), & 
!!$     &  somvarr(nrmamx,nregmx),somvarrp(nrmamx,nregmx), & 
!!$     &  gdsomvarrp(nregmx), & 
!!$     &  somtot(nrmamx,nregmx),somtotp(nrmamx,nregmx), & 
!!$     &  gdsomtotp(nregmx),segt(nrmamx,nregmx), & 
!!$     &  xmini,xmaxi,ymini,ymaxi,distxo,pntrat,tgarde(4)
!!$
!!$! mailcn
!!$fctfin, & 
!!$     &       gdpsi(1,ireg),racpsi(1,ireg),npr,ireg,xrad,yrad,fctrad, & 
!!$     &       a,gdr(1,ireg),r(1,ireg),ra(1,ireg),rho(1,ireg), & 
!!$     &       somort(1,ireg),somortp(1,ireg),gdsomortp(ireg), & 
!!$     &       somortpur(1,ireg),somortpurp(1,ireg),gdsomortpurp(ireg), & 
!!$     &       sompropo(1,ireg),sompropop(1,ireg),gdsompropop(ireg), & 
!!$     &       somvarr(1,ireg),somvarrp(1,ireg),gdsomvarrp(ireg), & 
!!$     &       somtot(1,ireg),somtotp(1,ireg),gdsomtotp(ireg), & 
!!$     &       segt(1,ireg))
