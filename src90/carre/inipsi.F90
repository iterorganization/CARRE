
!***********************************************************************
      SUBROUTINE INIPSI(equ,nxmax,nymax)
!***********************************************************************
        use carre_types

      implicit none
!
!  initialisation des coefficients d'interpolation pour psi et ses
!  derivees par rapport a x et a y
!
!  arguments
        type(CarreEquilibrium), intent(inout) :: equ

      integer nxmax,nymax
!
!  calculs
      CALL COEFF(nxmax,nymax,equ%nx,equ%ny,equ%x,equ%y,equ%psi,&
          & equ%a00(1,1,1),equ%a10(1,1,1), & 
          & equ%a01(1,1,1),equ%a11(1,1,1))
      CALL COEFF(nxmax,nymax,equ%nx,equ%ny,equ%x,equ%y,equ%psidx,&
          & equ%a00(1,1,2),equ%a10(1,1,2), & 
          & equ%a01(1,1,2),equ%a11(1,1,2))
      CALL COEFF(nxmax,nymax,equ%nx,equ%ny,equ%x,equ%y,equ%psidy,&
          & equ%a00(1,1,3),equ%a10(1,1,3), & 
          & equ%a01(1,1,3),equ%a11(1,1,3))
      return
      end
