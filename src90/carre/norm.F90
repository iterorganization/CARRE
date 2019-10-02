
!***********************************************************************
      FUNCTION norm(x0,x1)
!***********************************************************************
      use KindDefinitions
      IMPLICIT NONE
      REAL(rKind) :: norm

! Compute euclidean norm of vector

!  arguments
      REAL(rKind) :: x0,x1

!  local variables

      norm = sqrt( x0**2 + x1**2 )

      RETURN
      END
