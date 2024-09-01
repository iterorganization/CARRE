SUBROUTINE SAUTE(xst,yst,npst,x1,y1,psi1,x2,y2,pas,sens,repart, &
     & nx,ny,x,y,a00,a10,a01,a11,nm1,nm2)

  !*** This subroutine finds the next point on the structure for a
  !*** starting point and the step value
  !======================================================================
  use carre_target
  use trc_stk_mod
  use Logging
  use comlan

  IMPLICIT NONE

  !  arguments
  INTEGER npst,sens,repart,nx,ny,nm1,nm2
  REAL(rKind) :: x1,y1,psi1,x2,y2,pas,xst(npst),yst(npst),x(nx),y(ny), &
       &       a00(nm1,nm2,3),a10(nm1,nm2,3),a01(nm1,nm2,3),a11(nm1,nm2,3)

  !  variables locales
  INTEGER ind1,ind2,ii,jj, compt
  REAL(rKind) :: zpas,dist,x0,y0,psi0,psi2,psi3,x3,y3,psimin,psimi2
  REAL(rKind) :: fracx,fracy
  double precision :: bestX, bestY, bestPsi
  PARAMETER (psimin=1.E-08,psimi2=1.E-07)
  logical closed
  integer n_call
  data n_call /0/

  !  procedures
  INTEGER ifind
  INTRINSIC SQRT,MAX,MIN
  EXTERNAL ifind,pltend
  intrinsic abs

  !======================================================================
  !*** Input
  !.. xst,yst: corner points of the structure in question
  !.. npst  : number of points on this structure
  !.. x1,y1 : starting point
  !.. psi1  : psi value at the starting point
  !.. pas   : the step value in terms of either distance along the
  !..         leading target (repart=1) or the new psi value (repart=2)
  !.. sens  : direction of advancing
  !.. x,y   : source ordinates for the psi interpolation
  !.. nx,ny : their dimensions
  !.. a00..a11: interpolation coefficients
  !.. nm1,nm2: their dimensions

  !*** Output
  !.. x2, y2: the next point
  !======================================================================

  n_call=n_call+1

  !..Initialisation.

  compt = 0

  !.. is the structure closed?

  closed =  xst(1) == xst(npst) .and. yst(1) == yst(npst)

  !..Determine the segment index
  ind1=indsgm(xst,yst,npst,x1,y1)
  ! Determine next node on structure in stepping direction
  IF (sens .EQ. 1) then
      ind2 = ind1 + sens
      if (closed .and. ind2 == npst) ind2 = 1
  else
      ind2 = ind1
  end if

  x0=x1
  y0=y1
  psi0 = psi1

  !..Make a step according to selected distribution of the grid points

  IF (repart .EQ. 1) then

      !..Go around the structure

      zpas=pas
      !---------------------------------------------------------------------{
      do

          dist = SQRT((xst(ind2)-x0)**2+(yst(ind2)-y0)**2)

          IF (dist.gt.zpas) then
              x2=x0+zpas/dist*(xst(ind2)-x0)
              y2=y0+zpas/dist*(yst(ind2)-y0)

              RETURN

          else
              zpas=zpas-dist
              ind1=ind2
              ind2 = ind1 + sens
              ! handle wraparound
              if (closed) then
                  if (ind2 == 0) ind2 = npst - 1
                  if (ind2 == npst) ind2 = 1
              else
                  ! We fell off the structure. Return the last point of the structure.
                  if (ind2 == 0) then
                      x2 = xst(1)
                      y2 = yst(1)
                      call logmsg(LOGWARNING, "saute: fell off structure at first point (repart=1)")
                      return
                  end if
                  if (ind2 > npst) then
                      x2 = xst(npst)
                      y2 = yst(npst)
                      call logmsg(LOGWARNING, "saute: fell off structure at last point (repart=1)")
                      return
                  end if
              end if

              x0=xst(ind1)
              y0=yst(ind1)
          end IF


      end do
      !---------------------------------------------------------------------}

  else if (repart .EQ. 2) then

      !---------------------------------------------------------------------{

      bestPsi = huge(bestPsi)

      do

          !xpb  We make sure that the end of the structure lies within the
          !xpb  rectangle where the psi function is defined. Otherwise we
          !xpb  must find the point where the structure exits this rectangle.
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

          psi3 = a00(ii,jj,1) + a10(ii,jj,1)*x3 + a01(ii,jj,1)*y3 + &
               &         a11(ii,jj,1)*x3*y3

          IF (ABS(psi3 - pas) .LT. psimi2) then

              x2 = x3
              y2 = y3

              RETURN

          end if

          ! Note point closest to requested psi value found so far
          ! x3, y3, psi3

          if ( abs(psi3 - pas) < abs(bestPsi - pas)) then
              bestX = x3
              bestY = y3
              bestPsi = psi3
          end if

          IF ((MIN(psi0,psi3).LE.pas) .AND. (MAX(psi0,psi3).GE.pas)) then
              ! The current psi value is still in the allowed range.
              ! Take a step along the target towards the value pas.

              !---------------------------------------------------------------------{
              do

                  x2 = x0 - (psi0-pas)/(psi3 - psi0)*(x3 - x0)
                  y2 = y0 - (psi0-pas)/(psi3 - psi0)*(y3 - y0)

                  ii = ifind(x2,x,nx,1)
                  jj = ifind(y2,y,ny,1)

                  psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + &
                       &           a11(ii,jj,1)*x2*y2

                  IF (ABS(psi2-pas) .LT. psimin) then
                      RETURN
                  else if ((MIN(psi0,psi2).LE.pas) .AND. &
                       &             (MAX(psi0,psi2).GE.pas)) then
                      x3 = x2
                      y3 = y2
                      psi3 = psi2
                  else
                      x0 = x2
                      y0 = y2
                      psi0 = psi2
                  end if

              end do
              !---------------------------------------------------------------------}
          else

              IF (compt .EQ. npst) then

                  ! EXPERIMENTAL: return best point
                  x2 = bestX
                  y2 = bestY
                  return ! FIXME: re-enable below error condition for non-extended grid

                  if(sellan(1:8).eq.'francais') then
                      write(*,*) 'On a fait le tour de la structure sans trouver' &
                           &                ,' la fonction dans SAUTE'
                  else
                      write(*,*) 'Error in saute: the psi value not found ', &
                           &                                  'on the structure.  pas = ',pas
                      write(*,*) '==> Check whether all four targets ', &
                           &                                 'intersect the outer separatrix'
                  endif
                  call trc_stk
                  call pltend
                  STOP

              end if

              compt = compt + 1


              ind1=ind2
              ind2 = ind1 + sens
              ! handle wraparound
              if (closed) then
                  if (ind2 == 0) ind2 = npst - 1
                  if (ind2 == npst) ind2 = 1
              else
                  ! We fell off the structure. Return last point of structure.
                  if (ind2 == 0) then
                      x2 = xst(1)
                      y2 = yst(1)
                      call logmsg(LOGWARNING, "saute: fell off structure at first point (repart=2)")
                      return
                  end if
                  if (ind2 > npst) then
                      x2 = xst(npst)
                      y2 = yst(npst)
                      call logmsg(LOGWARNING, "saute: fell off structure at last point (repart=2)")
                      return
                  end if
              end if

              x0=xst(ind1)
              y0=yst(ind1)
              !---------------------------------------------------------------------}
          end if ! c.f. line 234
      end do
  end if ! c.f. line 85

END SUBROUTINE SAUTE
