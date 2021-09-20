      subroutine agchax (iflg,iaxs,iprt,vils)
!
!  version : 05.04.97 15:50
!
!======================================================================
      implicit none
      integer iflg,iaxs,iprt
      real vils
      external gslwsc
!  variables en common
#include <COMQUA.F>
!======================================================================
! The routine AGCHAX is called by AGAXIS just before and just after each
! of a selected set of objects on the axes are drawn.  A user may supply
! a version to change the appearance of these objects.  The arguments
! are as follows:
!
! - IFLG is zero if a particular object is about to be drawn, non-zero
!   if it has just been drawn.
!
! - IAXS is the number of the axis in question.  The values 1, 2, 3, and
!   4 imply the right, left, bottom, and top axes, respectively.
!
! - IPRT is an integer implying which part of the axis is being drawn.
!   The value 1 implies the line itself, 2 a major tick, 3 a minor tick,
!   4 the mantissa of a label, and 5 the exponent of a label.
!
! - VILS is the value, in the label coordinate system along the axis,
!   associated with the position of the object being drawn.  IPRT=1
!   implies VILS=0.
!
! Done.
!
!  L'appel a la routine GSLWSC permet de modifier l'epaisseur du trait
!  utilise pour tracer le cadre.

      if(qualit.eq.1) then
        call gslwsc(8.)
      else
        call gslwsc(1.)
      endif

      return
!
      end
