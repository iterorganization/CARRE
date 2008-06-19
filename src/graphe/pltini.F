      subroutine pltini
c
c  version : 05.04.97 16:00
c
c======================================================================
      implicit none
* Initialization routine for GRAFLIB
c
c  variables en common
#include <COMQUA.F>
c
c  variables locales
      character titre*20
c
c  procedures
      EXTERNAL OPNGKS
c======================================================================
C  INITIALIZE NCAR
      CALL OPNGKS
c
c  lecture du fichier configuration
      open (unit=19,file='ncar.cfg',status='unknown')
      rewind 19
      read(19,100,end=2)titre
100   format(a)
      if(titre(1:1).ne.' ') then
        qualit=1
        return
      endif
2     continue
      qualit=0
      return
      end
