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
#include <CARRETYPES.F>

*  arguments
      INTEGER npst,sens,repart,nx,ny,nm1,nm2
      REAL(KIND=R8)
     .       x1,y1,psi1,x2,y2,pas,xst(npst),yst(npst),x(nx),y(ny),
     .       a00(nm1,nm2,3),a10(nm1,nm2,3),a01(nm1,nm2,3),a11(nm1,nm2,3)

*  variables en common

#include <COMLAN.F>

*  variables locales
      INTEGER n,ind1,ind2,ii,jj, compt
      REAL(KIND=R8) zpas,dist,x0,y0,psi0,psi2,psi3,x3,y3,psimin,psimi2
      REAL(KIND=R8) fracx,fracy
      PARAMETER (psimin=1.0E-08_R8,psimi2=1.0E-07_R8)
      logical l_dbg, print_dbg
      integer n_call
#ifdef DBG
      data l_dbg /.true./
#else
      data l_dbg /.false./
#endif
      data n_call /0/
      save n_call

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
      print_dbg=l_dbg.and.n_call.ge.165
      if(print_dbg) then
        write(0,*) '===> Entering SAUTE. n_call = ',n_call
        write(0,'(1x,6a5,a8,3a12)') 'npst','sens','nx','ny',
     ,                              'nm1','nm2','x1','y1','psi1','pas'
        write(0,'(1x,6i5,1p,4e12.4)') npst,sens,nx,ny,nm1,nm2,
     ,                                                   x1,y1,psi1,pas
        write(0,'(a9,5a12)')
     -                    'x(1)','y(1)','x(2)','y(2)','x(nx)','y(ny)'
        write(0,'(1p,6e12.4)') x(1),y(1),x(2),y(2),x(nx),y(ny)
        write(0,*) 'xst'
        write(0,'(1p,10e12.4)') xst
        write(0,*) 'yst'
        write(0,'(1p,10e12.4)') yst
        call trc_stk
      end if
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
      if(print_dbg) then
        write(0,'(1x,4a5,a8,a12,3x,4a12)') 'sens','npst','ind1','ind2',
     ,              'x1','y1','xst(i1)','yst(i1)','xst(i2)','yst(i2)'
        write(0,'(1x,4i5,1p,6e12.2)') sens,npst,ind1,ind2,
     ,               x1, y1, xst(ind1),yst(ind1),xst(ind2),yst(ind2)
      end if
cc>>>

*..Make a step according to selected distribution of the grid points

      IF (repart .EQ. 1) then

*..Go around the structure

        zpas=pas
c---------------------------------------------------------------------{
    1   CONTINUE

        dist = SQRT((xst(ind2)-x0)**2+(yst(ind2)-y0)**2)

cc<<<
        if(print_dbg) then
          write(0,'(1x,a,2i3,1p,6e12.4)')
     .     'ind1,ind2,zpas,dist,x0,y0,xst,yst',
     .      ind1,ind2,zpas,dist,x0,y0,xst(ind2),yst(ind2)
        end if
cc>>>
        IF (dist.gt.zpas) then
          x2=x0+zpas/dist*(xst(ind2)-x0)
          y2=y0+zpas/dist*(yst(ind2)-y0)

cc<<<
          if(print_dbg) then
            write(0,'(1x,a,2i4,1p,2e14.6)')
     ,       '<=== Leaving SAUTE (dist found). ind1,ind2,x2,y2 = ',
     ,                                         ind1,ind2,x2,y2
          end if
cc>>>
          RETURN

        else
          zpas=zpas-dist
          ind1=ind2
          IF (sens .EQ. 1) then
            ind2=MOD(ind1,n)+sens
          else
            ind2=MOD(ind1+sens,npst)
            IF (ind2 .EQ. 0) ind2=n
          end if

          x0=xst(ind1)
          y0=yst(ind1)

          GO TO 1
c---------------------------------------------------------------------}
        end if

      else if (repart .EQ. 2) then

cc<<<
        if(print_dbg) then
          write(0,'(2x,1x,2(a4,1x),a9,3a16,a14,a12)')
     .     'ii','jj','x','y','psi','pas','diff','min'
        end if
cc>>>
c---------------------------------------------------------------------{
   10   CONTINUE

cxpb  We make sure that the end of the structure lies within the 
cxpb  rectangle where the psi function is defined. Otherwise we
cxpb  must find the point where the structure exits this rectangle.
        if (xst(ind2).ge.x(1) .and. xst(ind2).le.x(nx)) then
          if (yst(ind2).ge.y(1) .and. yst(ind2).le.y(ny)) then
            x3 = xst(ind2)
            y3 = yst(ind2)
          else if (yst(ind2).lt.y(1)) then
            x3 = x1+(xst(ind2)-x1)*(y1-y(1))/(y1-yst(ind2))
            y3 = y(1)
          else if (yst(ind2).gt.y(ny)) then
            x3 = x1+(xst(ind2)-x1)*(y(ny)-y1)/(yst(ind2)-y1)
            y3 = y(ny)
          endif
        else if (xst(ind2).lt.x(1)) then
          if (yst(ind2).ge.y(1) .and. yst(ind2).le.y(ny)) then
            x3 = x(1)
            y3 = y1+(yst(ind2)-y1)*(x1-x(1))/(x1-xst(ind2))
          else if (yst(ind2).lt.y(1)) then
            fracx = (x1-x(1))/(x1-xst(ind2))
            fracy = (y1-y(1))/(y1-yst(ind2))
            if (fracx.lt.fracy) then
              x3 = x(1)
              y3 = y1+(yst(ind2)-y1)*fracx
            else if (fracx.eq.fracy) then
              x3 = x(1)
              y3 = y(1)
            else if (fracx.gt.fracy) then
              x3 = x1+(xst(ind2)-x1)*fracy
              y3 = y(1)
            endif
          else if (yst(ind2).gt.y(ny)) then
            fracx = (x1-x(1))/(x1-xst(ind2))
            fracy = (y(ny)-y1)/(yst(ind2)-y1)
            if (fracx.lt.fracy) then
              x3 = x(1)
              y3 = y1+(yst(ind2)-y1)*fracx
            else if (fracx.eq.fracy) then
              x3 = x(1)
              y3 = y(ny)
            else if (fracx.gt.fracy) then
              x3 = x1+(x1-x(1))*fracy
              y3 = y(ny)
            endif
          endif
        else if (xst(ind2).gt.x(nx)) then
          if (yst(ind2).ge.y(1) .and. yst(ind2).le.y(ny)) then
            x3 = x(nx)
            y3 = y1+(yst(ind2)-y1)*(x(nx)-x1)/(xst(ind2)-x1)
          else if (yst(ind2).lt.y(1)) then
            fracx = (x(nx)-x1)/(xst(ind2)-x1)
            fracy = (y1-y(1))/(y1-yst(ind2))
            if (fracx.lt.fracy) then
              x3 = x(nx)
              y3 = y1+(y1-yst(ind2))*fracx
            else if (fracx.eq.fracy) then
              x3 = x(nx)
              y3 = y(1)
            else if (fracx.gt.fracy) then
              x3 = x1+(x(nx)-x1)*fracy
              y3 = y(1)
            endif
          else if (yst(ind2).gt.y(ny)) then
            fracx = (x(nx)-x1)/(xst(ind2)-x1)
            fracy = (y(ny)-y1)/(yst(ind2)-y1)
            if (fracx.lt.fracy) then
              x3 = x(nx)
              y3 = y1+(yst(ind2)-y1)*fracx
            else if (fracx.eq.fracy) then
              x3 = x(nx)
              y3 = y(ny)
            else if (fracx.gt.fracy) then
              x3 = x1+(x(nx)-x1)*fracy
              y3 = y(ny)
            endif
          endif
        endif            

        ii = ifind(x3,x,nx,1)
        jj = ifind(y3,y,ny,1)

        psi3 = a00(ii,jj,1) + a10(ii,jj,1)*x3 + a01(ii,jj,1)*y3 +
     .         a11(ii,jj,1)*x3*y3

cc<<<
        if(print_dbg) then
          write(0,'(a2,2(1x,i4),1p,4e16.8,2e12.4)') '3:',
     .     ii,jj,x3,y3,psi3,pas,abs(psi3-pas),psimi2
        end if
cc>>>
        IF (ABS(psi3 - pas) .LT. psimi2) then

          x2 = x3
          y2 = y3

cc<<<
          if(print_dbg) then
            write(0,'(2a,1p,3e12.4)')
     -              ' <=== Leaving SAUTE (psi3 found). ',
     ,                                   'x2,y2,psi3 = ',x2,y2,psi3
          end if
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
          if(print_dbg) then
            write(0,'(a2,2(1x,i4),1p,4e16.8,2e12.4)') '2:',
     .       ii,jj,x2,y2,psi2,pas,abs(psi2-pas),psimin
          end if
cc>>>
          IF (ABS(psi2-pas) .LT. psimin .or.
     .       (x2.eq.x3 .and. y2.eq.y3)) then

cc<<<
            if(print_dbg) then
              write(0,'(2a,1p,3e12.4)')
     -                ' <=== Leaving SAUTE (psi2 found). ',
     ,                                     'x2,y2,psi3 = ',x2,y2,psi3
            end if
cc>>>
            RETURN

          else if ((MIN(psi0,psi2).LE.pas) .AND.
     .             (MAX(psi0,psi2).GE.pas)) then
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

            if(sellan(1:8).eq.'francais') then
              write(*,*) 'On a fait le tour de la structure ',
     .                   'sans trouver la fonction dans SAUTE'
            else
              write(*,'(a,1p,1e15.8)')
     ,                   'Error in saute: the psi value not found '//
     ,                   'on the structure.  pas = ',pas
              write(*,'(a,1p,2e15.8)') 'psi0, psi3 = ', psi0, psi3
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
            ind2=MOD(ind1+sens,npst)
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
