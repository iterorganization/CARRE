subroutine sptris(nx,ny,x,y,psi,npx,ptx,pty, &
     &           iptx,jptx,fctpx,separx,separy,nptot, &
     &           nstruc,npstru,xstruc,ystruc,indplq,inddef,nbdef, &
     &           a00,a10,a01,a11,struct)

  !=======================================================================
  !*** This subroutine finds first the second starting point for each
  !*** separatrix branch (the first point is the X-point).
  !*** Once such a point is found, a subroutine CRBNIV is called, which
  !*** traces this branch until it strikes a target or comes to an X-point
  !*** and stores the trace co-ordinates for plotting.
  !*** Finally, a subroutine ORDDEF is called to order the targets
  !*** according to the CARRE conventions, i.e. from right to left, from
  !*** top to bottom.
  !=======================================================================
  use carre_dimensions
  use carre_niveau
  use carre_target
  use carre_types

  implicit none

  !  arguments
  integer nx,ny,npx,iptx(npxmx),jptx(npxmx), &
       &     nptot(4,npxmx),nstruc,npstru(strumx), &
       &     indplq(4,npxmx),inddef(nbdmx),nbdef
  real(rKind) :: x(nxmax),y(nymax),psi(nxmax,nymax),ptx(npxmx),pty(npxmx), &
       &     fctpx(npxmx),separx(npnimx,4,npxmx),separy(npnimx,4,npxmx), &
       &     xstruc(npstmx,strumx),ystruc(npstmx,strumx), &
       &     a00(nxmax,nymax,3),a10(nxmax,nymax,3), &
       &     a01(nxmax,nymax,3),a11(nxmax,nymax,3)
  type(CarreStructures), intent(inout) :: struct
  ! FIXME/CLEAN-UP: remove/replace arguments also contained in struct

  !  variables locales
  integer ipx,ix,jx,isep,i1,i2,j1,j2,iref,jref,k,idir, &
       &  indstr,plaque,nbdfav,nt(2),ixp_hlp(nbdmx),idirloop
  double precision :: xin, yin, delta
  real(rKind) :: xsttmp(nbdmx),ysttmp(nbdmx)
  integer i,j     !!!
  integer incri(4),incrj(4),incrir(4),incrjr(4),ic
  data incri  /0, -1,  0,  1/,  incrj  / 1, 0, -1,  0/
  data incrir /0,  0, -1, -1/,  incrjr /-1, 0,  0, -1/

  !  procedures
  logical milieu
  real(rKind) :: interp
  external insert,interp,milieu,orddef,trgarng
  !=======================================================================

  !..Initialisation
  !<<<
  write(0,*)
  write(0,*) 'Entering sptris: npx=',npx
  !>>>

  plaque = 0
  nbdef = 0
  nt(1)=0
  nt(2)=0

  !..Loop over the X-points

  do ipx=1,npx !{
      ix = iptx(ipx)
      jx = jptx(ipx)
      !<<<
      write(0,*)
      write(0,"(a,3i6,2e16.8)") 'ipx,ix,jx,ptx,pty =', &
           &                     ipx,ix,jx,ptx(ipx),pty(ipx)
      write(0,"(a,1p,4e16.8)") 'psi(ix,jx),CCW =',psi(ix,jx),psi(ix+1,jx), &
           &                                      psi(ix+1,jx+1),psi(ix,jx+1)
      !>>>

      !..Calculate the flux at the X-point

      fctpx(ipx) = a00(ix,jx,1) + a10(ix,jx,1)*ptx(ipx) &
           &                            + a01(ix,jx,1)*pty(ipx) &
           &                            + a11(ix,jx,1)*ptx(ipx)*pty(ipx)
      write(0,"(a,1p,1e16.8)") 'fctpx =',fctpx(ipx)

      !..Initialise the co-ordinates of each separatrix curve

      do isep=1, 4 !{
          separx(1,isep,ipx) = ptx(ipx)
          separy(1,isep,ipx) = pty(ipx)
      end do !}

      !..Loop over the directions for each X-point

      isep = 0

      !ank-20000720 { here we change the algorithm.
      !*** Originally, the perimeter of the cell containing the X-point was
      !*** searched for the intersections with the separatrix branches - that
      !*** is, for the proper PSI value at each cell boundary. If at some
      !*** boundary no intersection was found, then the adjacent cell was also
      !*** examined in a similar way. Due to the numerical noise (?), this
      !*** often lead to a confusion when not all the separatrix branches
      !*** could be identified properly.
      !*** Now we check the perimeter of the rim of cells (3x3) surrounding
      !*** the one with X-point to reduce the effect of noise.

      i2=ix+2
      j2=jx-1
      do idirloop=1,4 !{
          idir=idirloop
          nbdfav=nbdef
          !<<<
          write(0,*)
          write(0,*) 'idir,nbdfav,isep =',idir,nbdfav,isep
          !>>>

          do ic=1,3 !{
              i1 = i2
              i2 = i2+incri(idir)
              j1 = j2
              j2 = j2+incrj(idir)
              iref = i2+incrir(idir)
              jref = j2+incrjr(idir)
              !<<<
              write(0,'(a,6i5)') 'i1,i2,j1,j2,iref,jref =',i1,i2,j1,j2,iref,jref
              write(0,'(a,3e16.8)') 'psi(i1,j1),psi(i2,j2),fctpx(ipx)=', &
                   &                 psi(i1,j1),psi(i2,j2),fctpx(ipx)
              !>>>

              !..Check for crossing the segment
              if (milieu(psi(i1,j1),psi(i2,j2),fctpx(ipx))) then !{
                  isep = isep + 1
                  k = 2
                  if (idir.eq.1 .or. idir.eq.3) then !{
                      separx(k,isep,ipx) = x(i1)
                      separy(k,isep,ipx) = interp(y(j1),y(j2),psi(i1,j1), &
                           &                                psi(i2,j2),fctpx(ipx))
                  else !}{
                      separx(k,isep,ipx) = interp(x(i1),x(i2),psi(i1,j1), &
                           &                                psi(i2,j2),fctpx(ipx))
                      separy(k,isep,ipx) = y(j1)
                  endif !}

                  !<<<
                  write(0,'(a,3i6,2e16.8)') 'Cell crossing: k,isep,ipx,separx,separy =', &
                       &                  k,isep,ipx,separx(k,isep,ipx),separy(k,isep,ipx)
                  !>>>

                  !..Call the routine which traces the line and stores the points

                  call crbniv(iref,jref,k,idir,nx,ny,x,y,psi, &
                       &                fctpx(ipx),separx(1,isep,ipx),separy(1,isep,ipx), &
                       &                nstruc,npstru,xstruc, &
                       &                ystruc,indstr,xstruc,ystruc,nt,1,plaque, &
                       &                ptx(ipx),pty(ipx))

                  nptot(isep,ipx) = k
                  call insert(indstr,inddef,nbdef,ipx)
                  indplq(isep,ipx) = indstr
                  if(nbdfav.eq.nbdef-1) then !{
                      nbdfav=nbdef
                      ixp_hlp(nbdef)=ipx
                      xsttmp(nbdef)=separx(k,isep,ipx)
                      ysttmp(nbdef)=separy(k,isep,ipx)
                  endif !}

                  ! If we hit a structure,
                  ! from separatrix incidence direction derive "internal" and "external" side of target
                  ! (used for gridding with open target structures)

                  ! compute an internal point by moving a small step from the strike point towards the X-point
                  if (indStr /= 0) then
                      delta = (x(2) - x(1)) * 0.1
                      xin = separx(k, isep, ipx) + (separx(k-1, isep, ipx) - separx(k, isep, ipx)) * delta
                      yin = separy(k, isep, ipx) + (separy(k-1, isep, ipx) - separy(k, isep, ipx)) * delta
                      struct%internalSide(indStr) = onWhichSideOfStructure(xin, yin, struct, indstr)
                  end if

              endif !}
          end do !}
      end do !}
      write(0,*)
      write(0,'(a,4i5)') 'After idir loop: isep,ipx,nbdef,nbdfav = ', &
           &                                        isep,ipx,nbdef,nbdfav !###
      !ank}
  end do !}
  !<<<
  write(0,*)
  write(0,'(a,4i5)') 'After X-point loop: isep,npx,nbdef,nbdfav = ', &
       &                                        isep,npx,nbdef,nbdfav !###
  if(nbdef.gt.0) then !{
      write(0,'(a,8i5)') 'ixp_hlp :',(ixp_hlp(i),i=1,nbdef)
      write(0,'(a,1p,8f12.4)') 'xsttmp :',(xsttmp(i),i=1,nbdef)
      write(0,'(a,1p,8f12.4)') 'ysttmp :',(ysttmp(i),i=1,nbdef)
  end if !}
  if(npx.gt.0 .and. isep.gt.0) then !{
      write(0,*) 'nptot'
      do j=1,npx !{
          write(0,'(2x,20i5)') (nptot(i,j),i=1,isep)
      end do !}
      write(0,*) 'indplq'
      do j=1,npx !{
          write(0,'(2x,20i5)') (indplq(i,j),i=1,isep)
      end do !}
  end if !}
  !>>>

  !   Calculation of psi at the O-point using the fact that the O-point
  !  coordinates are at the end of ptx and pty (see routine SELPTX)

  ipx=npx+1
  ix = iptx(ipx)
  jx = jptx(ipx)

  fctpx(ipx) = a00(ix,jx,1) + a10(ix,jx,1)*ptx(ipx) &
       &           + a01(ix,jx,1)*pty(ipx) &
       &           + a11(ix,jx,1)*ptx(ipx)*pty(ipx)

  !*** Correct the actual number of targets and corresponding lists

  !<<<
  !      write(0,*) 'before trgarng'
  !>>>
  call trgarng(inddef,ixp_hlp,xsttmp,ysttmp,nbdef,npx)

  !..Call the subroutine which orders the divertor plates according to the
  !   specific order

  !<<<
  !      write(0,*) 'before orddef'
  !>>>
  call orddef(inddef,nbdef,xsttmp,ysttmp,ptx,pty,npx)

  !<<<
  write(0,*) 'Leaving sptris...'
  write(0,*)
  !>>>
  return
end subroutine sptris
