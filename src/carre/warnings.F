
      SUBROUTINE WARNINGS(separx,separy,nivx,nivy,
     &  distnv,xpto,ypto)
c****************************************************************
C written by Heimo Buerbaumer December 1998
C Calculation of the distance separatrix-outer border limit
C in the SOL-region and output on the file 'warnings.dat'
C for the use in the automatic grid generator AGG
c****************************************************************
#include <CARRETYPES.F>
#include <CARREDIM.F>
      REAL(KIND=R8)
     & nivx(npnimx,nivmx),nivy(npnimx,nivmx),nxu,nyu,nxo,nyo
      REAL(KIND=R8)
     & separx(npnimx,4,npxmx),separy(npnimx,4,npxmx),xpto,ypto,
     & distnv
      
      open (33,file='warnings.dat')
      rewind 33
      
      adzu=100.
      adzo=100.
      bdzu=100.
      bdzo=100.
      do i=1,npnimx
         do j=1,4
            if (separx(i,j,1).GT.xpto) then
               dz=ypto-separy(i,j,1)
               adz=ABS(dz)
               if ((dz.GE.0.).AND.(adz.LT.adzu)) then
               adzu=adz
               sxu=separx(i,j,1)
               syu=separy(i,j,1)
               endif
               if ((dz.LT.0.).AND.(adz.LT.adzo)) then
               adzo=adz
               sxo=separx(i,j,1)
               syo=separy(i,j,1)
               endif
      
            endif
          enddo
      if (nivx(i,1).GT.xpto) then
         dz=ypto-nivy(i,1)
         adz=ABS(dz)
         if ((dz.GE.0.).AND.(adz.LT.bdzu)) then
            bdzu=adz
            nxu=nivx(i,1)
            nyu=nivy(i,1)
         endif
         if ((dz.LT.0.).AND.(adz.LT.bdzo)) then
            bdzo=adz
            nxo=nivx(i,1)
            nyo=nivy(i,1)
         endif
C      write (33,*) i,bdzu,nxu,nyu,bdzo,nxo,nyo
      endif
      enddo

      z=ABS(syo-syu)
      if (z.LT.0.000001) then
         rs=syo
      else
         rs=(ypto-syo)*(sxu-sxo)/(syu-syo)+sxo
      endif
      z=ABS(nxo-nxu)
      if (z.LT.0.000001) then
         rn=nyo
      else
         rn=(ypto-nyo)*(nxu-nxo)/(nyu-nyo)+nxo
      endif
      distsn=100.*(rn-rs)
      distnv=100.*distnv
       write (33,120) distnv,distsn
       write (33,*) rs,rn,syo,syu,nxo,nyo,xpto,ypto
       
      
      close(33)
 120   FORMAT(1X,F8.2,1X,F8.2)
      RETURN
      END
