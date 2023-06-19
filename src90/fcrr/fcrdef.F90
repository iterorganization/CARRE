     subroutine fcrdef
!======================================================================
!*** Set some fcrr defaults
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
!======================================================================
     ! standard carre operating mode
     carre_mode = 0
     grid_ext_mode = 0
     equ_ext_mode = 0

     ! no additional vessel structures
     nstrv = 0

     ! 
     lclstr = .false.



!======================================================================
end 
