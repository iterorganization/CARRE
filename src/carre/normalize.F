
************************************************************************
      subroutine normalize(x1, x2)
************************************************************************
      IMPLICIT NONE

* Normalize a vector so that || (x1, x2) || = 1

*  arguments
      REAL*8 x1, x2

*  internal variables
      REAL*8 l

      l = sqrt( x1**2 + x2**2 )

      x1 = x1 / l
      x2 = x2 / l

      RETURN
      END
