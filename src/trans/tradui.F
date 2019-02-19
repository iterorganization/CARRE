      program tradui
c
c  version : 12.01.99 13:28
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

c*** This program translates the grid created with carre into other
c*** grid formats
c======================================================================
      implicit none

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

c  variables locales
      integer nxmx,nymx,ncutmx,nisomx
      parameter(nxmx=npmamx,nymx=nrmamx,ncutmx=4,nisomx=1)
      integer nin,nout,nfin,nreg,isel,nppol(nregmx),nprad(nregmx),
     ,        nptseg(10),repart,npr(10),ifail,nx,ny,
     ,        ncut,nxcut(ncutmx),nycut(ncutmx),niso,nxiso(nisomx+1)
      real(kind=R8) r(npmamx,nrmamx,nregmx),z(npmamx,nrmamx,nregmx),
     .  psi(npmamx,nrmamx,nregmx),psidxm(npmamx,nrmamx,nregmx),
     .  psidym(npmamx,nrmamx,nregmx),deltp1(10),deltpn(10),deltr1(10),
     .  deltrn(10),tgarde(4),distxo,psi_X(2),psi_O,pntrat,delpsi
      real(kind=R8) crx(-1:nxmx,-1:nymx,0:3),cry(-1:nxmx,-1:nymx,0:3),
     .  bb(-1:nxmx,-1:nymx,0:3),b0r0,
     .  fpsi(-1:nxmx,-1:nymx,0:3),ffbz(-1:nxmx,-1:nymx,0:3),
     .  psidx(-1:nxmx,-1:nymx,0:3),psidy(-1:nxmx,-1:nymx,0:3)
      character nom*80

c  procedures
      external limail, change, ecrim1, b2agfz, b2agbb, ecrim2, ecrim3,
     ,         ecrim4
c======================================================================
c*** nregmx: maximum number of regions
c*** npmamx: maximum number of points in poloidal direction for a data
c***         region
c*** nrmamx: maximum number of points in radial direction for a data
c***         region
c*** nxmx  : maximum number of poloidal zones in a grid of B2 type
c*** nymx  : maximum number of radial zones in a grid of B2 type
c*** psidxm: psi derivatives in x in a carre grid
c*** psidx : psi derivatives in x in a grid of type B2.5
c*** ncutmx: max. number of the regular cuts in the B2 grid
c*** nisomx: max. number of the isolating cuts in the B2 grid
c*** ncut  : actual number of the regular cuts in the B2 grid
c*** niso  : actual number of the isolating cuts in the B2 grid
c*** nxcut : location of the regular cuts in the B2 grid
c*** nycut : length of the regular cuts in the B2 grid
c*** nxiso : location of the isolating cuts in the B2 grid
c======================================================================

      nin=1
      nout=7
      nfin=8

c* 1.   Open the files

      write(6,*) 'Name of the file containing the carre grid'
      read(5,101) nom
 101  format(a)
      write(6,*) nom
      open(unit=nin,file=nom,status='old')
      open(unit=nout,status='scratch')
c     open(unit=nout,file='traduit.tmp',status='unknown')
      open(unit=nfin,file='traduit.out',status='unknown')

c* 2.   Read the parameters used to create the grid

      distxo=1.0e30
      psi_X=-1.0e30
      psi_O=-1.0e30
      call change(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .            pntrat,tgarde,distxo,psi_X,psi_O,nin,nout,ifail)
      if (abs(psi_X(1)-psi_O).le.abs(psi_X(2)-psi_O)) then
        delpsi=psi_X(1)-psi_O
      else
        delpsi=psi_X(2)-psi_O
      end if

c* 3.   Read the grid

      call limail(nin,nreg,nppol,nprad,r,z,psi,psidxm,psidym,
     .  npmamx,nrmamx)

c* 4.   Select the output format

      write(6,*) 'Select the output format'
      write(6,*) '1: standard mailtri format'
      write(6,*) '2: format B2.5'
      write(6,*) '3: format SONNET-DIVIMP'
      write(6,*) '4: format DG-SONNET-B2-EIRENE'
      write(6,*) '5: revised DIVIMP with'//
     > ' grid parameters and PSI values'
      read(5,*) isel
      write(6,*) 'The format chosen is :',isel

c* 5.   Write the translated mesh to the output file
c
c Mailtri format
c
      if(isel.eq.1) then
c
        call ecrim1(nout,nfin,r,z,nptseg,nreg,nppol,nprad,npmamx,
     .  nrmamx)
c
c B2.5 Format
c
      elseif(isel.eq.2) then
c
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,nxmx,nymx,
     .    r,z,nreg,nppol,nprad,npmamx,nrmamx,
     .    nptseg,psidx,psidy,psi,psidxm,psidym,b0r0,
     ,    ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso)
        call b2agbb (nx,ny,fpsi,ffbz,bb,
     .    crx,cry,psidx,psidy,nxmx,nymx)
        call ecrim2(nfin,nx,ny,crx,cry,bb,fpsi,ffbz,delpsi,nxmx,nymx)
c
c Original SONNET/DIVIMP format
c
      elseif(isel.eq.3) then
c
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,nxmx,nymx,
     .    r,z,nreg,nppol,nprad,npmamx,nrmamx,
     .    nptseg,psidx,psidy,psi,psidxm,psidym,b0r0,
     ,    ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso)
        call b2agbb (nx,ny,fpsi,ffbz,bb,
     .    crx,cry,psidx,psidy,nxmx,nymx)
        call ecrim3(nfin,nx,ny,crx,cry,bb,nxmx,nymx)
c
c*** B2-Sonnet-DG format
c
      elseif(isel.eq.4) then
c
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,nxmx,nymx,
     .    r,z,nreg,nppol,nprad,npmamx,nrmamx,
     .    nptseg,psidx,psidy,psi,psidxm,psidym,b0r0,
     ,    ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso)
c<<<
c        write(0,*) 'call b2agbb'
c>>>
        call b2agbb (nx,ny,fpsi,ffbz,bb,
     .    crx,cry,psidx,psidy,nxmx,nymx)
c<<<
c        write(0,*) 'call ecrim4: nx,ny = ',nx,ny
c>>>
        call ecrim4(nfin,nx,ny,crx,cry,bb,b0r0,fpsi,ffbz,delpsi,
     ,                          nxmx,nymx,ncut,nxcut,nycut,niso,nxiso)
c<<<
c        write(0,*) 'after ecrim4'
c>>>
c
c Revised DIVIMP format with additional grid information and PSI values
c
      elseif(isel.eq.5) then
c
        call b2agfz(nx,ny,crx,cry,fpsi,ffbz,nxmx,nymx,
     .   r,z,nreg,nppol,nprad,npmamx,nrmamx,
     .   nptseg,psidx,psidy,psi,psidxm,psidym,b0r0,
     ,   ncutmx,ncut,nxcut,nycut,nisomx,niso,nxiso)
        call b2agbb (nx,ny,fpsi,ffbz,bb,
     .   crx,cry,psidx,psidy,nxmx,nymx)
c jdemod - added fpsi to call to ecrim5
        call ecrim5(nfin,nx,ny,crx,cry,bb,b0r0,fpsi,nxmx,nymx)
c
      else
        write(6,*) 'Wrong value (must be 1 to 5): isel=',isel
        stop
      endif
c======================================================================
      end
