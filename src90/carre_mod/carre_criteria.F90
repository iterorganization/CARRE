module carre_criteria

  use KindDefinitions

  implicit none

contains

  !  calcul des fonctions qui s'annulent quand deux series de points sont
  !  disposees de facon "orthogonale".
  !
  !  x1, y1: coordonnees des points de maille de reference (sur la courbe
  !          precedente
  !  x2, y2: coordonnees des points de maille a ajuster
  !  ort: tableau de la fonction dont il faut trouver le zero
  !  pasmin: plus petit espacement entre deux points de maille
  !  g1, g2: gardes gauche et droite
  !  l0: tableau des positions initiales
  !  l2: tableau des positions actuelles
  subroutine clort(x1,y1,x2,y2,ort,nppol,pasmin,g1,g2,l0,l2, &
       & ortpur,propo,varr)

    !  arguments
    integer, intent(in) :: nppol
    double precision, intent(in) :: x1(nppol),y1(nppol),x2(nppol),y2(nppol)
    double precision, intent(in) :: g1,g2,l0(nppol),l2(nppol),pasmin
    double precision, intent(out) :: ort(nppol)
    double precision, intent(out), optional :: ortpur(nppol),propo(nppol), varr(nppol)

    !  variables locales
    integer i
    real(rKind) :: cs1,cs2,cs3,cs4,l1m,l1p,l2m,l2p,l12,zero,fac1,fac2,fac, &
         &  l12t,un
    parameter(zero=0.,un=1.)

    ! local working arrays holding criteria
    double precision :: lOrtpur(nppol),lPropo(nppol), lVarr(nppol)

    !  calculs
    i=1
    ort=zero
    l1p=sqrt((x1(i)-x1(i+1))**2+(y1(i)-y1(i+1))**2)
    l2p=sqrt((x2(i)-x2(i+1))**2+(y2(i)-y2(i+1))**2)

    !  on trouve empiriquement que la convergence est meilleure avec une
    !  valeur de l12 constante.
    l12t=sqrt((x2(i)-x1(i))**2+(y2(i)-y1(i))**2)
    if(g1.gt.zero .and. g2.gt.zero) then
       do i=2,nppol-1
          fac1=(g1/(g1+l0(i)))**2
          fac2=(g2/(g2+l0(nppol)-l0(i)))**2
          fac=fac1*(un-fac2)+fac2
          l12=fac*l0(nppol)+(1.-fac)*l12t
          !
          l1m=l1p
          l2m=l2p
          l1p=sqrt((x1(i)-x1(i+1))**2+(y1(i)-y1(i+1))**2)
          l2p=sqrt((x2(i)-x2(i+1))**2+(y2(i)-y2(i+1))**2)
          cs1=(x1(i)-x2(i))*(x2(i-1)-x2(i)) &
               &       +(y1(i)-y2(i))*(y2(i-1)-y2(i))
          cs2=(x1(i)-x2(i))*(x2(i+1)-x2(i)) &
               &       +(y1(i)-y2(i))*(y2(i+1)-y2(i))
          cs3=(x2(i)-x1(i))*(x1(i-1)-x1(i)) &
               &       +(y2(i)-y1(i))*(y1(i-1)-y1(i))
          cs4=(x2(i)-x1(i))*(x1(i+1)-x1(i)) &
               &       +(y2(i)-y1(i))*(y1(i+1)-y1(i))
          if (l2m.gt.zero) cs1=cs1/(l2m*l12)
          if (l2p.gt.zero) cs2=cs2/(l2p*l12)
          if (l1m.gt.zero) cs3=cs3/(l1m*l12)
          if (l1p.gt.zero) cs4=cs4/(l1p*l12)

          ! Orthogonality
          lOrtpur(i) = cs2+cs3-cs1-cs4
          ! Maintain point distribution of separatrix
          lPropo(i) = - ((g1/l2(i))**2+(g2/(l2(nppol)-l2(i)))**2) &
               & *(l2(i)-l0(i)/l0(nppol)*l2(nppol))/(pasmin+g1+g2)
          ! Point distance
          lVarr(i)= (pasmin/(l2(i)-l2(i-1)))**2 &
               & - (pasmin/(l2(i+1)-l2(i)))**2
          ! Full criterium function
          ort(i) = lOrtpur(i) + lPropo(i) + lVarr(i)

          ! Old code for reference:
!!$          ort(i)= &
!!$               &       cs2+cs3-cs1-cs4 &
!!$               &      -((g1/l2(i))**2+(g2/(l2(nppol)-l2(i)))**2) &
!!$               &      *(l2(i)-l0(i)/l0(nppol)*l2(nppol))/(pasmin+g1+g2) &
!!$               &      +(pasmin/(l2(i)-l2(i-1)))**2-(pasmin/(l2(i+1)-l2(i)))**2

       enddo
    else
       do i=2,nppol-1
          l12=l12t
          !
          l1m=l1p
          l2m=l2p
          l1p=sqrt((x1(i)-x1(i+1))**2+(y1(i)-y1(i+1))**2)
          l2p=sqrt((x2(i)-x2(i+1))**2+(y2(i)-y2(i+1))**2)
          cs1=(x1(i)-x2(i))*(x2(i-1)-x2(i)) &
               &       +(y1(i)-y2(i))*(y2(i-1)-y2(i))
          cs2=(x1(i)-x2(i))*(x2(i+1)-x2(i)) &
               &       +(y1(i)-y2(i))*(y2(i+1)-y2(i))
          cs3=(x2(i)-x1(i))*(x1(i-1)-x1(i)) &
               &       +(y2(i)-y1(i))*(y1(i-1)-y1(i))
          cs4=(x2(i)-x1(i))*(x1(i+1)-x1(i)) &
               &       +(y2(i)-y1(i))*(y1(i+1)-y1(i))
          if (l2m.gt.zero) cs1=cs1/(l2m*l12)
          if (l2p.gt.zero) cs2=cs2/(l2p*l12)
          if (l1m.gt.zero) cs3=cs3/(l1m*l12)
          if (l1p.gt.zero) cs4=cs4/(l1p*l12)

          lOrtpur(i)=cs2+cs3-cs1-cs4

!!$          propo(i)=  ((g1/l2(i))**2+(g2/(l2(nppol)-l2(i)))**2) &
!!$     &      *(l2(i)-l0(i)/l0(nppol)*l2(nppol))/(pasmin+g1+g2)
          lPropo(i) = 0.0

          if (l2(i).eq.l2(i-1)) then
            lVarr(i) = (pasmin/(l2(i+1)-l2(i)))**2
          else if (l2(i+1).eq.l2(i)) then
            lVarr(i) = (pasmin/(l2(i)-l2(i-1)))**2
          else
            lVarr(i)= (pasmin/(l2(i)-l2(i-1)))**2- &
               &             (pasmin/(l2(i+1)-l2(i)))**2
          endif

          ort(i)= lOrtpur(i) + lVarr(i)

!!$          ort(i)= &
!!$     &       cs2+cs3-cs1-cs4 &
!!$     &      +(pasmin/(l2(i)-l2(i-1)))**2-(pasmin/(l2(i+1)-l2(i)))**2

       enddo
    endif

    ! Optional return arguements for individual criteria
    if (present(ortpur)) ortpur = lOrtpur
    if (present(propo)) propo = lPropo
    if (present(varr)) varr = lVarr

  end subroutine clort

end module carre_criteria
