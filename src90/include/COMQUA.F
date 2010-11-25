!  version : 05.04.97 15:47
!dps 20051115 This variable appears to be used as an
!*** integer everywhere and not a real.  Change to
!*** integer to avoid memory problems with -r8 flags.
!
!      real qualit
      integer qualit
      common/comqua/qualit
