c=======================================================================
      subroutine pltend
      implicit none
C      integer nplt
      EXTERNAL CLSGKS
C  TERMINATE NCAR
      CALL CLSGKS
      return
      end
