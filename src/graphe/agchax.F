      subroutine agchax (iflg,iaxs,iprt,vils)
c
c  version : 05.04.97 15:50
c
c======================================================================
c  variables en common
#include <COMQUA.F>
c======================================================================
C The routine AGCHAX is called by AGAXIS just before and just after each
C of a selected set of objects on the axes are drawn.  A user may supply
C a version to change the appearance of these objects.  The arguments
C are as follows:
C
C - IFLG is zero if a particular object is about to be drawn, nonzero
C   if it has just been drawn.
C
C - IAXS is the number of the axis in question.  The values 1, 2, 3, and
C   4 imply the right, left, bottom, and top axes, respectively.
C
C - IPRT is an integer implying which part of the axis is being drawn.
C   The value 1 implies the line itself, 2 a major tick, 3 a minor tick,
C   4 the mantissa of a label, and 5 the exponent of a label.
C
C - VILS is the value, in the label coordinate system along the axis,
C   associated with the position of the object being drawn.  IPRT=1
C   implies VILS=0.
C
C Done.
C
C  L'appel a la routine GSLWSC permet de modifier l'epaisseur du trait
C  utilise pour tracer le cadre.

      if(qualit.eq.1) then
        call gslwsc(8.)
      else
        call gslwsc(1.)
      endif

      return
c
      end
