
!***********************************************************************
      subroutine normalize(x1, x2)
!***********************************************************************
      use KindDefinitions
      IMPLICIT NONE

! Normalize a vector so that || (x1, x2) || = 1

!  arguments
      REAL(rKind) :: x1, x2

!  internal variables
      REAL(rKind) :: l

      l = sqrt( x1**2 + x2**2 )

      x1 = x1 / l
      x2 = x2 / l

      RETURN
      END
