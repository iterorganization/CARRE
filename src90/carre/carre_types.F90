module carre_io
  implicit none

  private

#include <CARREDIM.F>

  

  type CarreParameters
!.. nptseg: number of points along differents segments of separatrix
!.. repart: selector for the radial point distribution
!           1=absolute distance, 2=difference in psi
!.. tgarde: guard length for each divertor target
!.. npr   : number of points in radial direction
!.. pntrat: penetration value of the X-point relative to the O-point
!.. deltp1,deltpn: values of the first and last intervals for each sep.
!.. deltr1,deltrn: values of the first and last intervals for each reg.
      INTEGER nptseg(10),repart,npr(nregmx)
      REAL*8 deltp1(10),deltpn(10),deltr1(10),deltrn(10),pntrat, & 
     &       tgarde(4)
  end type CarreParameters

  public CarreParameters
  

!!$  type CarreEquilibrium
!!$  end type CarreEquilibrium
!!$
!!$
!!$  type CarreGrid
!!$  end type CarreGrid
!!$
!!$  type CarreLimiter
!!$  end type CarreLimiter


!!$contains



end module carre_io
