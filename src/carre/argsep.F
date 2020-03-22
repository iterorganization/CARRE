      SUBROUTINE ARGSEP(npx,ptx,pty,fctpx,separx,separy,indplq,nptot,
     .                 npnimx,ptsep,racord,ptxint,ypto,nbdef,inddef)
c
c  version : 23.06.98 20:28
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

c*** This subroutine arranges the separatrices in the required way
c*** and indexes them in correct order

*..  Cette sous-routine arrange les separatrices de la facon demandee
*  et les ordonnes selon un ordre precis.
c======================================================================
      IMPLICIT NONE
#include <CARRETYPES.F>
#include <COMLAN.F>

*  arguments
      INTEGER npnimx,npx,indplq(4,npx),nptot(4,npx),ptsep(4,npx),
     ,                                       ptxint,nbdef,inddef(nbdef)
      REAL(kind=R8) ptx(npx),pty(npx),separx(npnimx,4,npx),
     .       separy(npnimx,4,npx),fctpx(npx+1),ypto
      LOGICAL racord

*  variables locales
      INTEGER kkmax
      PARAMETER (kkmax=10)
      real(kind=R8) wx(4),wy(4),zero
      parameter(zero=0.0_R8)
      INTEGER ipx, ipx2, j, i, k0, kk(kkmax), kkp, k, ind,
     ,        i1, i2, ipi, j1, j2, k1, k2, l1, l2

*  procedures
      INTEGER coinci
      real(kind=R8) aire
      LOGICAL tsplaq
      INTRINSIC MOD, ABS
      EXTERNAL coinci,tsplaq,aire

c======================================================================
c*** Input
c.. npx   : number of the X-points
c.. ptx,pty: x and y coordinates of the X-points
c.. fctpx: the psi values at each X- or O-point
c.. separx,separy: coordinates of the points of the parametrised
c                  separatrices
c                  (point index, separatrix index, X-point index)
c.. nptot : number of parametrisation points for each separatrix
c           (separatrix index, X-point index)
c.. npnimx: maximum number of tracing points on a curve
c.. racord: determines whether the X-points are connected
c.. ypto  : vertical coordinate of the magnetic axis (the O-point)
c.. nbdef : number of divertor targets
c.. inddef: index of the target structures
c
c*** Output
c.. ptsep : separatrix pointer, used for indexation
c           (separatrix index, X-point index)
c.. ptxint: index of the X-point which is inner with respect to the
c           other in case of disconnected double-null
c.. indplq: structure indices; 0 if it is not a divertor plate
c           (separatrix index, X-point index)
c
c*** Internal variables
c.. ind,i,j: indices
c.. k0    : counter
c.. kkmax : maximum number which the counter can reach
c.. kk    : pointers of separatrices having to be copied in ptsep
c.. kkp   : separatrix pointer
c.. k     : index of the point where 2 separatrices coincide
c.. ipx,ipx2: indices of each X-point
c======================================================================
#ifdef DBG
      write(0,*) '=== Entering argsep. npx,racord=',npx,racord
      write(0,*) 'nbdef,inddef: ',nbdef,inddef
      write(0,'(1x,a,8i5)') 'nptot : ',nptot
      write(0,*)
      write(0,'(1x,a16,1p,4e12.4)') 'separx(i,1)',
     ,                     (separx(nptot(i,1),i,1),i=1,4)
      write(0,'(1x,a16,1p,4e12.4)') 'separy(i,1)',
     ,                     (separy(nptot(i,1),i,1),i=1,4)
      write(0,*)
      if (npx.ge.2) then
        write(0,'(1x,a16,1p,4e12.4)') 'separx(i,2)',
     ,                     (separx(nptot(i,2),i,2),i=1,4)
        write(0,'(1x,a16,1p,4e12.4)') 'separy(i,2)',
     ,                     (separy(nptot(i,2),i,2),i=1,4)
        write(0,*)
      end if
#endif
*..Initialisation.

      ptxint = 0

*..To have angles of 90 degrees at the X-points, one must drop the
*  second point from each separatrix

      do 1 i=1,npx
         do 2 j=1, 4
            do 3 k=2, nptot(j,i)-1
               separx(k,j,i) = separx(k+1,j,i)
               separy(k,j,i) = separy(k+1,j,i)
    3       CONTINUE
            nptot(j,i) = nptot(j,i) - 1
    2    CONTINUE
    1 CONTINUE
#ifdef DBG
      write(0,*) '** after 1 continue'
      write(0,'(a13,8i5)') ' nptot : ',nptot
      write(0,'(a13,8i5)') ' indplq in : ',indplq
#endif

*..Find the separatrices which go directly to a target
      do 10 ipx=1, npx
cc<<<
c        write(0,*) '** do 10  ipx = ',ipx
cc>>>
        IF (npx .EQ. 1) then
          do 14 j=1, 4
            IF (.NOT.(tsplaq(separx(1,j,ipx),separy(1,j,ipx),
     .           nptot(j,ipx),npx,ptx(ipx),pty(ipx)))) then
              indplq(j,ipx) = 0
            end if
   14     CONTINUE
        else
          ipx2 = MOD(ipx,2) + 1
          do 15 j=1, 4
            IF (.NOT.(tsplaq(separx(1,j,ipx),separy(1,j,ipx),
     .          nptot(j,ipx),npx,ptx(ipx2),pty(ipx2)))) then
              indplq(j,ipx) = 0
            end if
   15     CONTINUE
        end if
#ifdef DBG
        write(0,'(a13,8i5)') ' indplq out: ',indplq
#endif
        k0 = 0
        do 20 j=1, 4
          IF (indplq(j,ipx) .NE. 0) then
            k0 = k0 + 1
            kk(k0) = j
          end if
   20   CONTINUE
cc<<<
c        write(0,*) 'ipx,k0,kk : ',ipx,k0,' ',kk
cc>>>
*..Check whether there are exactly 2 separatrices which go to the plate
        IF (npx .EQ. 1 .AND. k0 .NE. 2) then
          PRINT *, 'Error in argsep. k0=', k0
          call pltend
          STOP
        end if
*..Index the separatrices which go to the targets (right=1, left=2)
        wx(1)=separx(1,kk(1),ipx)
        wy(1)=separy(1,kk(1),ipx)
        wx(2)=separx(nptot(kk(1),ipx),kk(1),ipx)
        wy(2)=separy(nptot(kk(1),ipx),kk(1),ipx)
        wx(3)=separx(nptot(kk(2),ipx),kk(2),ipx)
        wy(3)=separy(nptot(kk(2),ipx),kk(2),ipx)
        wx(4)=wx(1)
        wy(4)=wy(1)
#ifdef DBG
        write(0,'(a17,1p,4e12.4)') ' ypto,pty,aire = ',
     ,                               ypto,pty,aire(wx,wy,4)
        write(0,'(a5,1p,4e12.4)') ' wx: ',wx
        write(0,'(a5,1p,4e12.4)') ' wy: ',wy
#endif
        if((pty(ipx)-ypto)*aire(wx,wy,4).ge.zero) then
          ptsep(1,ipx)=kk(1)
          ptsep(2,ipx)=kk(2)
        else
          ptsep(1,ipx)=kk(2)
          ptsep(2,ipx)=kk(1)
        end if
   10 CONTINUE
#ifdef DBG
      write(0,*) '** After 10 continue. npx = ',npx
      write(0,'(1x,a7,5x,8i5)') 'ptsep: ',ptsep
#endif
*..First case: ONE X-point
      IF (npx .EQ. 1) then
c----------------------------------------------------------------------
#ifdef DBG
        write(0,*) '-- Single null'
#endif
*..Initialisation
         ipx = 1
         k0 = 0
*..Identify the separatrices which do not go to the targets
         do 25 j=1, 4
            IF (indplq(j,ipx) .EQ. 0) then
               k0 = k0 + 1
               kk(k0) = j
            end if
   25    CONTINUE
*..Index the separatrices which do not go to the targets
         wx(1)=separx(1,kk(1),ipx)
         wy(1)=separy(1,kk(1),ipx)
         wx(2)=separx(2,kk(1),ipx)
         wy(2)=separy(2,kk(1),ipx)
         wx(3)=separx(2,kk(2),ipx)
         wy(3)=separy(2,kk(2),ipx)
         wx(4)=wx(1)
         wy(4)=wy(1)
         if((pty(ipx)-ypto)*aire(wx,wy,4).le.zero) then
           ptsep(3,ipx) = kk(1)
           kkp = kk(2)
         else
           ptsep(3,ipx) = kk(2)
           kkp = kk(1)
         end if
*..Parametrise the third separatrix (which meets itself)
         k = coinci(separx(1,ptsep(3,ipx),ipx),separy(1,ptsep(3,ipx),
     .              ipx),nptot(ptsep(3,ipx),ipx),separx(2,kkp,ipx),
     .              separy(2,kkp,ipx)) + 1
         nptot(ptsep(3,ipx),ipx) = k
         separx(k,ptsep(3,ipx),ipx) = separx(1,1,ipx)
         separy(k,ptsep(3,ipx),ipx) = separy(1,1,ipx)
*..Cancel the separatrix which is not used (number of points
*  equal to 1 means that the separatrix is not traced)
         ptsep(4,ipx) = kkp
         nptot(ptsep(4,ipx),ipx) = 1
         separx(1,ptsep(4,ipx),ipx) = separx(1,1,ipx)
         separy(1,ptsep(4,ipx),ipx) = separy(1,1,ipx)
c----------------------------------------------------------------------
*..Second case: TWO X-points
      else if (npx .EQ. 2) then
*..If the X-points are connected...
         IF (racord) then
c----------------------------------------------------------------------
#ifdef DBG
            write(0,*) '-- Connected double null'
#endif
*..Identification of each curve which does not go to a plate
            k0 = 0
            do 30 ipx=1, npx
               do 35 j=1, 4
                  IF (indplq(j,ipx) .EQ. 0) then
                     k0 = k0 + 1
                     kk(k0) = j
                  end if
   35          CONTINUE
   30       CONTINUE
*..   Index each curve not going to a plate (curve connecting the X-point
*     from right: 3, from left: 4)
            do 40 ipx=1, npx
               wx(1)=separx(1,kk(2*(ipx-1)+1),ipx)
               wy(1)=separy(1,kk(2*(ipx-1)+1),ipx)
               wx(2)=separx(2,kk(2*(ipx-1)+1),ipx)
               wy(2)=separy(2,kk(2*(ipx-1)+1),ipx)
               wx(3)=separx(2,kk(2*(ipx-1)+2),ipx)
               wy(3)=separy(2,kk(2*(ipx-1)+2),ipx)
               wx(4)=wx(1)
               wy(4)=wy(1)
               if((pty(ipx)-ypto)*aire(wx,wy,4).le.zero) then
                 ptsep(3,ipx) = kk(2*(ipx-1)+1)
                 ptsep(4,ipx) = kk(2*(ipx-1)+2)
               else
                 ptsep(3,ipx) = kk(2*(ipx-1)+2)
                 ptsep(4,ipx) = kk(2*(ipx-1)+1)
               end if
   40       CONTINUE
*..Connect the end of the separatrices to the other X-point
            ipx = 1
            do 50 ind=3, 4
               k = coinci(separx(1,ptsep(ind,ipx),ipx),
     .                    separy(1,ptsep(ind,ipx),ipx),
     .                    nptot(ptsep(ind,ipx),ipx),
     .                    separx(2,ptsep(ind,ipx+1),ipx+1),
     .                    separy(2,ptsep(ind,ipx+1),ipx+1)) + 1
               nptot(ptsep(ind,ipx),ipx) = k
               separx(k,ptsep(ind,ipx),ipx) = separx(1,1,ipx+1)
               separy(k,ptsep(ind,ipx),ipx) = separy(1,1,ipx+1)
   50       CONTINUE
*..Cancel the separatrices which are not used (number of points
*  equal to 1 means that the separatrix is not traced)
            ipx = 2
            nptot(ptsep(3,ipx),ipx) = 1
            separx(1,ptsep(3,ipx),ipx) = separx(1,1,ipx)
            separy(1,ptsep(3,ipx),ipx) = separy(1,1,ipx)
            nptot(ptsep(4,ipx),ipx) = 1
            separx(1,ptsep(4,ipx),ipx) = separx(1,1,ipx)
            separy(1,ptsep(4,ipx),ipx) = separy(1,1,ipx)
c----------------------------------------------------------------------
*..If the X-points are not connected...
         else if (.NOT.(racord)) then
c----------------------------------------------------------------------
#ifdef DBG
            write(0,*) '-- Disconnected double null'
#endif
*..Determine the X-point which is inner to the other.
*  Inner X-point is that which has the value of the psi closer to
*  that of psi on the axis
            IF (abs(fctpx(1)-fctpx(npx+1)) .lt.
     .          abs(fctpx(2)-fctpx(npx+1))) then
               ptxint = 1
               ipx = ptxint
            else if (abs(fctpx(1)-fctpx(npx+1)) .gt.
     .          abs(fctpx(2)-fctpx(npx+1))) then
               ptxint = 2
               ipx = ptxint
            else
               if (sellan(1:8).eq.'francais') then
                  WRITE(0,*) 'Double nul deconnecte mais valeur de '//
     .             ' fonction egale pour chaque point X. PROBLEME !!'
               else
                  WRITE(0,*) 'Equal psi values at both X-points '//
     ,                       'for the disconnected double null'
               endif
               call pltend
               STOP
            end if
*..Initialisation
            k0 = 0
*..Identify the separatrices which do not go to the targets
            do 60 j=1, 4
               IF (indplq(j,ipx) .EQ. 0) then
                  k0 = k0 + 1
                  kk(k0) = j
               end if
   60       CONTINUE
cc<<<
c        write(0,*) '** after 60 continue. ptxint = ',ptxint
c        write(0,*) 'ipx,k0,kk : ',ipx,k0,' ',kk
cc>>>
*..Index the inner separatrices
            wx(1)=separx(1,kk(1),ipx)
            wy(1)=separy(1,kk(1),ipx)
            wx(2)=separx(2,kk(1),ipx)
            wy(2)=separy(2,kk(1),ipx)
            wx(3)=separx(2,kk(2),ipx)
            wy(3)=separy(2,kk(2),ipx)
            wx(4)=wx(1)
            wy(4)=wy(1)
#ifdef DBG
            write(0,'(a17,1p,4e12.4)') ' ypto,pty,aire = ',
     ,                                   ypto,pty,aire(wx,wy,4)
            write(0,'(a5,1p,4e12.4)') ' wx: ',wx
            write(0,'(a5,1p,4e12.4)') ' wy: ',wy
#endif
            if((pty(ipx)-ypto)*aire(wx,wy,4).le.zero) then
              ptsep(3,ipx) = kk(1)
              kkp = kk(2)
            else
              ptsep(3,ipx) = kk(2)
              kkp = kk(1)
            end if
*..Parametrise the inner separatrix
            k = coinci(separx(1,ptsep(3,ipx),ipx),separy(1,ptsep(3,ipx),
     .              ipx),nptot(ptsep(3,ipx),ipx),separx(2,kkp,ipx),
     .              separy(2,kkp,ipx)) + 1
            nptot(ptsep(3,ipx),ipx) = k
            separx(k,ptsep(3,ipx),ipx) = separx(1,1,ipx)
            separy(k,ptsep(3,ipx),ipx) = separy(1,1,ipx)
*..Cancel the separatrix which is not used (number of points
*  equal to 1 means that the separatrix is not traced)
            ptsep(4,ipx) = kkp
            nptot(ptsep(4,ipx),ipx) = 1
            separx(1,ptsep(4,ipx),ipx) = separx(1,1,ipx)
            separy(1,ptsep(4,ipx),ipx) = separy(1,1,ipx)

*..Arrange the separatrices which surround the inner one

*..Consider the outer X-point

            ipx = MOD(ipx,2) + 1

c*** Find the targets not connected to the inner X-point

          i1=0
          i2=0
          ipi=mod(ipx,2)+1
          do i=1,nbdef
            k=inddef(i)
            do j=1,4
              if(k.eq.indplq(j,ipi)) k=0
            end do
            if(k.ne.0) then
              if(i1.eq.0) then
                i1=k
              else if(i2.eq.0) then
                i2=k
              else
                write(0,*) 'Error detected in argsep: more than two ',
     ,                  'targets not intersecting the inner separatrix'
                call pltend
                STOP
              end if
            end if
          end do
          if(i2.eq.0) then
            write(0,*) 'Error detected in argsep: less than two (',i1,
     ,               ' ) targets not intersecting the inner separatrix'
            call pltend
            STOP
          end if

          j1=0
          j2=0
          do i=1,4
            if(i1.eq.indplq(i,ipx)) j1=i
            if(i2.eq.indplq(i,ipx)) j2=i
          end do
          if(j1.eq.0 .or. j2.eq.0) then
            write(0,*) 'Error detected in argsep: primary targets for',
     ,                                ' the outer separatrix not found'
            write(0,*) 'i1,i2,inddef: ',i1,i2,' ',inddef
            write(0,*) 'indplq :',indplq
            call pltend
            STOP
          end if

          k1=0
          k2=0
          do i=1,4
            if(i.ne.j1 .and. i.ne.j2) then
              if(k1.eq.0) then
                k1=i
              else
                k2=i
              end if
            end if
          end do
          l1=0
          l2=0
          do i=1,4
            k=inddef(i)
            if(k.eq.indplq(k1,ipx)) then
              if(l1.eq.0) then
                l1=k1
              else
                l2=k1
              end if
            end if
            if(k.eq.indplq(k2,ipx)) then
              if(l1.eq.0) then
                l1=k2
              else
                l2=k2
              end if
            end if
          end do
cc<<<
c          write(0,*) 'i1,i2, j1,j2, k1,k2, l1,l2: ',
c     ,                              i1,i2,' ',j1,j2,' ',k1,k2,' ',l1,l2
cc>>>
c
c*** Index the curves which skirt the inner separatrix
c*** (each of them must intersect a target)

          ptsep(1,ipx) = j1
          ptsep(2,ipx) = j2

          ptsep(3,ipx) = l1
          ptsep(4,ipx) = l2
cank
c----------------------------------------------------------------------
         end if
      end if
#ifdef DBG
      write(0,'(1x,a,8i5)') 'Leaving argsep. ptsep :',ptsep
#endif
      RETURN
c======================================================================
      END
