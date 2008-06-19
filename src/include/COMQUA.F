c  version : 05.04.97 15:47
cdps 20051115 This variable appears to be used as an
c*** integer everywhere and not a real.  Change to
c*** integer to avoid memory problems with -r8 flags.
c
c      real qualit
      integer qualit
      common/comqua/qualit
