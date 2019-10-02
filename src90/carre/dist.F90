
!***********************************************************************
      FUNCTION dist(x0,y0,x1,y1)
!***********************************************************************
      use KindDefinitions
      IMPLICIT NONE
      REAl(rKind) :: dist

! For two points (x0,y0), (x1,y1), compute distance between the points

!  arguments
      REAL(rKind) :: x0,y0,x1,y1

!  local variables
      REAL(rKind) :: dx,dy

      dx = x1 - x0
      dy = y1 - y0

      dist = sqrt( dx**2 + dy**2 )

      RETURN
      END
