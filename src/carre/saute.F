      SUBROUTINE SAUTE(xst,yst,npst,x1,y1,psi1,x2,y2,pas,sens,repart,
     ,                               nx,ny,x,y,a00,a10,a01,a11,nm1,nm2)
c
c  version : 01.07.98 14:06
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

c*** This subroutine finds the next point on the structure for a
c*** starting point and the step value
c======================================================================
      IMPLICIT NONE

*  arguments
      INTEGER npst,sens,repart,nx,ny,nm1,nm2
      REAL*8 x1,y1,psi1,x2,y2,pas,xst(npst),yst(npst),x(nx),y(ny),
     .       a00(nm1,nm2,3),a10(nm1,nm2,3),a01(nm1,nm2,3),a11(nm1,nm2,3)

*  variables en common

#include <COMLAN.F>

*  variables locales
      INTEGER n,ind1,ind2,ii,jj, compt
      REAL*8 zpas,dist,x0,y0,psi0,psi2,psi3,x3,y3,psimin,psimi2
      PARAMETER (psimin=1.E-08,psimi2=1.E-07)
      logical l_dbg /.false./
      integer n_call /0/

*  procedures
      INTEGER indsgm,ifind
      INTRINSIC MOD,SQRT,MAX,MIN
      EXTERNAL indsgm,ifind
      intrinsic abs

c======================================================================
c*** Input
c.. xst,yst: corner points of the structure in question
c.. npst  : number of points on this structure
c.. x1,y1 : starting point
c.. psi1  : psi value at the starting point
c.. pas   : the step value in terms of either distance along the
c..         leading target (repart=1) or the new psi value (repart=2)
c.. sens  : direction of advancing
c.. x,y   : source ordinates for the psi interpolation
c.. nx,ny : their dimensions
c.. a00..a11: interpolation coefficients
c.. nm1,nm2: their dimensions

c*** Output
c.. x2, y2: the next point
c======================================================================

      n_call=n_call+1
cc<<<
c      l_dbg=n_call.gt.225
c      if(l_dbg) then
c        write(0,*) '===> Entering SAUTE. n_call = ',n_call
c        write(0,'(1x,6a5,a9,3a12)') 'npst','sens','nx','ny',
c     ,                               'nm1','nm2','x1','y1','psi1','pas'
c        write(0,'(6i5,1p,4e12.4)') npst,sens,nx,ny,nm1,nm2,
c     ,                                                   x1,y1,psi1,pas
c        write(0,'(1x,a9,5a12)')
c     -                      'x(1)','y(1)','x(2)','y(2)','x(nx)','y(ny)'
c        write(0,'(1x,1p,6e12.4)') x(1),y(1),x(2),y(2),x(nx),y(nx)
c        write(0,*) 'xst'
c        write(0,'(1p,10e12.4)') xst
c        write(0,*) 'yst'
c        write(0,'(1p,10e12.4)') yst
c        call trc_stk
c      end if
cc>>>

*..Initialisation.

      compt = 0

*..Go along the structure which is supposed to be closed

      n=npst-1

*..Determine the segment index

      ind1=indsgm(xst,yst,npst,x1,y1)
      IF (sens .EQ. 1) then
        ind2 = MOD(ind1,n)+sens
      else
        ind2 = ind1
      end if

      x0=x1
      y0=y1
      psi0 = psi1
cc<<<
c      if(l_dbg) then
c        write(0,'(1x,4a5,a9,5a12)') 'sens','npst','ind1','ind2',
c     ,              'x1','y1','xst(i1)','yst(i1)','xst(i2)','yst(i2)'
c        write(0,'(1x,4i5,1p,6e12.2)') sens,npst,ind1,ind2,
c     ,                    x1,y1,xst(ind1),yst(ind1),xst(ind2),yst(ind2)
c      end if
cc>>>

*..Make a step according to selected distribution of the grid points

      IF (repart .EQ. 1) then

*..Go around the structure

        zpas=pas
c---------------------------------------------------------------------{
    1   CONTINUE

        dist = SQRT((xst(ind2)-x0)**2+(yst(ind2)-y0)**2)

        IF (dist.gt.zpas) then
          x2=x0+zpas/dist*(xst(ind2)-x0)
          y2=y0+zpas/dist*(yst(ind2)-y0)

cc<<<
c          if(l_dbg) then
c            write(0,*) '<=== Leaving SAUTE (dist found). x2,y2 = ',
c     ,                                                            x2,y2
c          end if
cc>>>
          RETURN

        else
          zpas=zpas-dist
          ind1=ind2
          IF (sens .EQ. 1) then
            ind2=MOD(ind1,n)+sens
          else
            ind2=MOD(ind1,npst)+sens
            IF (ind2 .EQ. 0) ind2=n
          end if

          x0=xst(ind1)
          y0=yst(ind1)

          GO TO 1
c---------------------------------------------------------------------}
        end if

      else if (repart .EQ. 2) then

cc<<<
c        if(l_dbg) then
c          write(0,'(2x,2a4,a9,3a12)') 'ii','jj','x','y','psi','pas'
c        end if
cc>>>
c---------------------------------------------------------------------{
   10   CONTINUE

        x3 = MIN(MAX(xst(ind2),x(1)),x(nx))
        y3 = MIN(MAX(yst(ind2),y(1)),y(ny))


        ii = ifind(x3,x,nx,1)
        jj = ifind(y3,y,ny,1)

        psi3 = a00(ii,jj,1) + a10(ii,jj,1)*x3 + a01(ii,jj,1)*y3 +
     .         a11(ii,jj,1)*x3*y3

cc<<<
c        if(l_dbg) then
c          write(0,'(1x,2i4,1p,4e12.4)') ii,jj,x3,y3,psi3,pas
c        end if
cc>>>
        IF (ABS(psi3 - pas) .LT. psimi2) then

          x2 = x3
          y2 = y3

cc<<<
c          if(l_dbg) then
c            write(0,'(2a,1p,3e12.4)')
c     -                   ' <=== Leaving SAUTE (psi3 found). ',
c     ,                                        'x2,y2,psi3 = ',x2,y2,psi
c          end if
cc>>>
          RETURN

        end if

        IF ((MIN(psi0,psi3).LE.pas) .AND. (MAX(psi0,psi3).GE.pas)) then

c---------------------------------------------------------------------{
   20     CONTINUE

          x2 = x0 - (psi0-pas)/(psi3 - psi0)*(x3 - x0)
          y2 = y0 - (psi0-pas)/(psi3 - psi0)*(y3 - y0)

          ii = ifind(x2,x,nx,1)
          jj = ifind(y2,y,ny,1)

          psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 +
     .           a11(ii,jj,1)*x2*y2

cc<<<
c        if(l_dbg) then
c          write(0,'(1x,2i4,1p,4e12.4)') ii,jj,x2,y2,psi2,pas
c        end if
cc>>>
          IF (ABS(psi2-pas) .LT. psimin) then

cc<<<
c            if(l_dbg) then
c              write(0,'(2a,1p,3e12.4)')
c     -                   ' <=== Leaving SAUTE (psi2 found). ',
c     ,                                        'x2,y2,psi3 = ',x2,y2,psi
c            end if
cc>>>
            RETURN

          else if ((MIN(psi0,psi2).LE.pas) .AND.
     .                         (MAX(psi0,psi2).GE.pas)) then
            x3 = x2
            y3 = y2
            psi3 = psi2
          else
            x0 = x2
            y0 = y2
            psi0 = psi2
          end if

          GO TO 20
c---------------------------------------------------------------------}
        else
          IF (compt .EQ. npst) then

c            PRINT *,'On a fait le tour de la structure sans trouver'
c     .                ,' la fonction dans SAUTE'

            if(sellan(1:8).eq.'francais') then
            write(*,*) 'On a fait le tour de la structure sans trouver'
     .                ,' la fonction dans SAUTE'
            else
            write(*,*) 'Error in saute: the psi value not found ',
     ,                                  'on the structure.  pas = ',pas
            write(*,*) '==> Check whether all four targets ',
     ,                                 'intersect the outer separatrix'
            endif
            call trc_stk
            call pltend
            STOP

          end if

          compt = compt + 1
          ind1=ind2
          IF (sens .EQ. 1) then
            ind2=MOD(ind1,n)+sens
          else
            ind2=MOD(ind1,npst)+sens
            IF (ind2 .EQ. 0) ind2=n
          end if

          x0=xst(ind1)
          y0=yst(ind1)
          GO TO 10
c---------------------------------------------------------------------}
        end if
      end if
c======================================================================
      END
