
************************************************************************
      REAL*8 FUNCTION norm(x0,x1)
************************************************************************
      IMPLICIT NONE

* Compute euclidean norm of vector

*  arguments
      REAL*8 x0,x1
      
*  local variables

      norm = sqrt( x0**2 + x1**2 )

      RETURN
      END
