      subroutine pltini
!
!  version : 05.04.97 16:00
!
!======================================================================
      use comqua
      implicit none
! Initialization routine for GRAFLIB
!
!  variables locales
      character titre*20
!
!  procedures
      EXTERNAL OPNGKS
!======================================================================
!  INITIALIZE NCAR
      CALL OPNGKS
!
!  lecture du fichier configuration
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
