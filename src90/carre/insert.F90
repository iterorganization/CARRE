      subroutine insert(indstr,inddef,nbdef,ipx)
!
!  version: 17.11.99 23:56
!
!=======================================================================
      use KindDefinitions
      implicit none
#include <CARREDIM.F>

!.. This routine creates a vector of indices of the divertor targets

!  arguments
      integer indstr,nbdef,inddef(nbdmx),indxpt(nbdmx),ipx,npx
      real(rKind) :: xst(nbdmx),yst(nbdmx),xtt(nbdmx),ytt(nbdmx)

!  variables locales
      integer npxmx4
      parameter (npxmx4=4*npxmx)
      integer i,j,k,l,m,iipx(npxmx4), & 
     &  ii(strumx),jj(nbdmx),kk(npxmx),ll(nbdmx),mm(npxmx)

      data iipx /npxmx4*0/
      save iipx

!=======================================================================
!.. indstr: structure index
!.. inddef: table of target indices
!.. nbdef:  number of targets
!=======================================================================

!*** If the structure index is non-zero, it points to a structure
!*** intersecting the separatrix branch. Store all such cases for a
!*** while - we will select the primary strikepoints after all X-points
!*** have been treated.
!ank-19990727: symptomatic treatment
!*** ... However, multiple intersections of separatrix branches starting
!*** at the same X-point with the same target should be screened

      write(0,*) 'insert: indstr,nbdef,ipx',indstr,nbdef,ipx !###
      if (indstr .gt. 0)  then !{
        do i=1, nbdef !{
          if (inddef(i).eq.indstr .and. ipx.eq.iipx(i)) return
        end do !}
        iipx(i)=ipx
        nbdef = nbdef + 1
        inddef(nbdef) = indstr
      end if !}
      return

!=======================================================================

      entry trgarng(inddef,indxpt,xst,yst,nbdef,npx)
!=======================================================================
!*** Here, we determine the "primary" strikepoints - i.e., those
!*** connected to the innermost X-point for each target, and check for
!*** the consistency of the strike-points and targets.
!*** Each X-point must be connected to two "primary" strike-points.
!***
!*** Input:
!***  inddef  - list of targets (pieces of structure having at least one
!***            intersection with a separatrix branch (strike-point)
!***  indxpt  - list of X-points connected to the corresponding targets
!***  xst,yst - strike-point co-ordinates
!***  nbdef   - number of strike-points
!***  npx     - number of X-points
!***
!*** Output:
!***  the same arrays corrected so that only the "primary" strike-points
!***  are included and counted
!***
!*** Internal arrays:
!***  ii  - strike-point counters for each target
!***  jj  - index of targets being "private" - i.e., having only one
!***        strike-point
!***  kk  - index of X-points connected to two "private" targets
!***  ll  - index of X-points corresponding to the "private" targets
!***  mm  - number of "private" targets for each X-point
!=======================================================================
!<<<
      write(0,*) 'Entering trgarng: nbdef=',nbdef
      if(nbdef.gt.0) then !{
        write(0,'(1x,a8,i6,9i10)') 'inddef: ',(inddef(i),i=1,nbdef)
        write(0,'(1x,a8,i6,9i10)') 'indxpt: ',(indxpt(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10e10.3)') 'xst:',(xst(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10e10.3)') 'yst:',(yst(i),i=1,nbdef)
      end if !}
!>>>
!*** Select the primary X-points and correct the number of targets
!*** First, check which targets are intersected only once

      do i=1,nbdmx !{
        ii(i)=0
      end do !}
      do i=1,nbdef !{
        ii(inddef(i))=ii(inddef(i))+1
      end do !}
      do i=1,npx !{
        mm(i)=0
      end do !}

!*** k is the number of "private" targets
      k=0
      do j=1,nbdef !{
        if(ii(inddef(j)).eq.1) then !{
          k=k+1
          jj(k)=j
          ll(k)=indxpt(j)
          mm(indxpt(j))=mm(indxpt(j))+1
        else if(ii(inddef(j)).gt.2) then !}{
          write(0,*) 'Error found in INSERT (entry TRGARNG): too many ', & 
     &      'intersections with separatrix branches for one target - ', & 
     &      ii(inddef(j)), 'for structure ',inddef(j)
          stop
        end if !}
      end do !}
!<<<
      write(0,*) 'Single intersections : ',k
      if(k.gt.0) then !{
        write(0,*) 'jj : ',(jj(i),i=1,k)
        write(0,*) 'll : ',(ll(i),i=1,k)
      end if !}
!>>>

!*** Check the count of private targets for each X-point
!*** m is the number of X-points having two "private" targets
      m=0
      do j=1,npx !{
        if(mm(j).eq.2) then !{
          m=m+1
          kk(m)=j
        else if(mm(j).gt.2) then !}{
          write(0,*) 'Error found in INSERT (entry TRGARNG): too many ', & 
     &      '"private" targets for one X-point: ',j
          stop '==> Check the targets related to the inner X-point'
        end if !}
      end do !}
!<<<
      write(0,*) 'Complete X-points: ',m
      if(m.gt.0) then !{
        write(0,*) 'kk : ',(kk(i),i=1,m)
      end if !}
!>>>
!*** Now look for targets intersected twice
      do j=1,nbdef !{
        if(ii(inddef(j)).eq.2) then !{
          l=0
          do i=1,m !{
            if(kk(i).eq.indxpt(j)) l=l+1
          end do !}
          if(l.eq.0) then !{
            k=k+1
            jj(k)=j
          end if !}
        end if !}
      end do !}
!<<<
      write(0,*) 'Before re-arrangement. k = ',k
      if(k.gt.0) write(0,*) 'jj : ',(jj(i),i=1,k)
!>>>

      do i=1,k !{
        j=jj(i)
        kk(i)=inddef(j)
        mm(i)=indxpt(j)
        xtt(i)=xst(j)
        ytt(i)=yst(j)
      end do !}
      do i=1,k !{
        inddef(i)=kk(i)
        indxpt(i)=mm(i)
        xst(i)=xtt(i)
        yst(i)=ytt(i)
      end do !}
      nbdef=k
!<<<
      write(0,*) 'After re-arrangement: ',nbdef
      if(nbdef.gt.0) then !{
        write(0,'(1x,a8,i6,9i10)') 'inddef: ',(inddef(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10e10.3)') 'xst:',(xst(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10e10.3)') 'yst:',(yst(i),i=1,nbdef)
      end if !}
!>>>

!*** Check the consistency of the "primary targets" once more

      do i=1,npx !{
        mm(i)=0
      end do !}
      do i=1,nbdef !{
        mm(indxpt(i))=mm(indxpt(i))+1
      end do !}
      m=0
      do i=1,npx !{
        if(mm(i).ne.2) then !{
          m=m+1
          write(0,*) 'Error found in INSERT (entry TRGARNG): wrong ', & 
     &      'number of "private" targets. ipx,n = ',i,mm(i)
        end if !}
      end do !}
      if(m.gt.0) stop '==> check the target specification'
!<<<
      write(0,*) 'Leaving trgarng: ',nbdef
!>>>
!=======================================================================
      end
