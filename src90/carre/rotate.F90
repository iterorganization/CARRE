
!***********************************************************************
      subroutine rotate(x1, x2, phi)
!***********************************************************************
      IMPLICIT NONE

! rotate a vector (x1,x2) by angle phi. Note that the rotation
! is in the counterclocwise direction

!  arguments
      REAL*8 x1, x2, phi

!  internal variables
      REAL*8 tx1, tx2

      tx1 = x1
      tx2 = x2

      x1 = cos(phi) * tx1 - sin(phi) * tx2
      x2 = sin(phi) * tx1 + cos(phi) * tx2

      RETURN
      END
