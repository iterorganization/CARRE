
!***********************************************************************
      FUNCTION angle(x0,y0,x1,y1,x2,y2)
!***********************************************************************
      use KindDefinitions
      IMPLICIT NONE
      REAL(rKind) :: angle

! For two vectors pointing from (x0,y0) to (x1,y1) and (x2,y2),
! respectively, compute the angle between these vectors.

!  arguments
      REAL(rKind) :: x0,y0,x1,y1,x2,y2

!  local variables
      REAL(rKind) :: dx1,dy1,dx2,dy2

      dx1 = x1 - x0
      dy1 = y1 - y0
      dx2 = x2 - x0
      dy2 = y2 - y0

      angle = acos( ( dx1 * dx2 + dy1 * dy2 ) & 
     &     / ( sqrt( dx1**2 + dy1**2 ) * sqrt( dx2**2 + dy2**2 ) ) )

      RETURN
      END
