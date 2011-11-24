# The original files in this directory are maintained in the USOL repository on solps-mdsplus.
# Use this script to update them. This is done to avoid an svn:externals (which would cause 
# quite some pain in the usol repository).

# Changes should be brought back into usol, there is an equivalent export script there.

#!/bin/sh

svn export https://solps-mdsplus.aug.ipp.mpg.de/repos/HMK/usol/src/common/KindDefinitions.f90
svn export https://solps-mdsplus.aug.ipp.mpg.de/repos/HMK/usol/src/common/Logging.f90
svn export https://solps-mdsplus.aug.ipp.mpg.de/repos/HMK/usol/src/tools/silo/SiloIO.f90
svn export https://solps-mdsplus.aug.ipp.mpg.de/repos/HMK/usol/src/common/Helper.f90
