!=======================================================================
      subroutine pltend
      implicit none
!      integer nplt
      EXTERNAL CLSGKS
!  TERMINATE NCAR
      CALL CLSGKS
      return
      end
