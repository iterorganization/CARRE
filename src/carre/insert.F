      subroutine insert(indstr,inddef,nbdef,ipx)
c
c  version: 17.11.99 23:56
c
c=======================================================================
      implicit none
#include <CARRETYPES.F>
#include <CARREDIM.F>

*.. This routine creates a vector of indices of the divertor targets

*  arguments
      integer indstr,nbdef,inddef(nbdmx),indxpt(nbdmx),ipx,npx
      real(kind=R8) xst(nbdmx),yst(nbdmx),xtt(nbdmx),ytt(nbdmx)

*  variables locales
      integer npxmx4
      parameter (npxmx4=4*npxmx)
      integer i,j,k,l,m,n,iipx(npxmx4),
     ,  ii(strumx),jj(nbdmx),kk(npxmx),ll(nbdmx),mm(npxmx)
      
      data iipx /npxmx4*0/
      save iipx

c=======================================================================
c.. indstr: structure index
c.. inddef: table of target indices
c.. nbdef:  number of targets
c=======================================================================

c*** If the structure index is nonzero, it points to a structure
c*** intersecting the separatrix branch. Store all such cases for a 
c*** while - we will select the primary strikepoints after all x-points
c*** have been treated.
cank-19990727: symptomatic treatment
c*** ... However, multiple intersections of separatrix branches starting
c*** at the same x-point with the same target should be screened

      write(0,*) 'insert: indstr,nbdef,ipx',indstr,nbdef,ipx !###
      if (indstr .gt. 0) then !{
        do i=1, nbdef !{
          if (inddef(i).eq.indstr .and. ipx.eq.iipx(i)) return
        end do !}
        iipx(i)=ipx
        nbdef = nbdef + 1
        inddef(nbdef) = indstr
      end if !}
      return

c=======================================================================

      entry trgarng(inddef,indxpt,xst,yst,nbdef,npx)
c=======================================================================
c*** Here, we determine the "primary" strikepoints - i.e., those 
c*** connected to the innermost x-point for each target, and check for  
c*** the consistency of the strike-points and targets. 
c*** Each x-point must be connected to two "primary" strike-points.
c***
c*** Input:
c***  inddef  - list of targets (pieces of structure having at least one
c***            intersection with a separatrix branch: strike-point)
c***  indxpt  - list of x-points connected to the corresponding targets
c***  xst,yst - strike-point coordinates
c***  nbdef   - number of strike-points
c***  npx     - number of x-points
c***
c*** Output:
c***  the same arrays corrected so that only the "primary" strike-points
c***  are included and counted
c***
c*** Internal arrays:
c***  ii  - strike-point counters for each target
c***  jj  - index of targets being "private" - i.e., having only one
c***        strike-point
c***  kk  - index of x-points connected to two "private" targets
c***  ll  - index of x-points corresponding to the "private" targets
c***  mm  - number of "private" targets for each x-point
c=======================================================================
c<<<
      write(0,*) 'Entering trgarng: nbdef=',nbdef
      if(nbdef.gt.0) then !{
        write(0,'(1x,a8,i7,9i12)') 'inddef: ',(inddef(i),i=1,nbdef)
        write(0,'(1x,a8,i7,9i12)') 'indxpt: ',(indxpt(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10e12.4)') 'xst:',(xst(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10e12.4)') 'yst:',(yst(i),i=1,nbdef)
      end if !}
c>>>
c*** Select the primary x-points and correct the number of targets
c*** First, check which targets are intersected only once

      do i=1,nbdmx !{
        ii(i)=0
      end do !}
      do i=1,nbdef !{
        ii(inddef(i))=ii(inddef(i))+1
      end do !}
      do i=1,npx !{
        mm(i)=0
      end do !}

c*** k is the number of "private" targets
      k=0
      do j=1,nbdef !{
        if(ii(inddef(j)).eq.1) then !{
          k=k+1
          jj(k)=j
          ll(k)=indxpt(j)
          mm(indxpt(j))=mm(indxpt(j))+1
        else if(ii(inddef(j)).gt.2) then !}{
          write(0,*) 'Error found in INSERT (entry TRGARNG): too many ',
     ,      'intersections with separatrix branches for one target - ',
     ,      ii(inddef(j)), 'for structure ',inddef(j)
          stop
        end if !}
      end do !}
c<<<
      write(0,*) 'Single intersections : ',k
      if (k.eq.0.and.nbdef.eq.8) then  ! case of a "perfectly symmetric" connected double-null
cxpb We will try throwing away one of each of the separatrix segments
       n=0
       do i=1,nbdef
        do j=i+1,nbdef
         if(inddef(i).eq.inddef(j).and.i.ne.j.and.
     &      indxpt(i).ne.indxpt(j)) then
          n=n+1
          k=k+1
          if(n.le.2) then ! assign to the first X-point
            ii(inddef(j))=ii(inddef(j))-1
            jj(k)=j
            ll(k)=indxpt(j)
            mm(indxpt(j))=mm(indxpt(j))+1
          else ! assign to the second X-point
            ii(inddef(i))=ii(inddef(i))-1
            jj(k)=i
            ll(k)=indxpt(i)
            mm(indxpt(i))=mm(indxpt(i))+1
          endif
         endif
        end do
       end do
       write(0,*) 'Single intersections after duplicates removal : ',k
      end if
c
      if(k.gt.0) then !{
        write(0,'(1x,a,8i4)') 'jj : ',(jj(i),i=1,k)
        write(0,'(1x,a,8i4)') 'll : ',(ll(i),i=1,k)
      end if !}
c>>>

c*** Check the count of private targets for each x-point
c*** m is the number of x-points having two "private" targets
      m=0
      do j=1,npx !{
        if(mm(j).eq.2) then !{
          m=m+1
          kk(m)=j
        else if(mm(j).gt.2) then !}{
          write(0,*) 'Error found in INSERT (entry TRGARNG): ',
     ,      'too many "private" targets for one x-point: ',j
          stop '==> Check the targets related to the inner x-point'
        end if !}
      end do !}
c<<<
      write(0,*) 'Complete x-points: ',m
      if(m.gt.0) then !{
        write(0,'(1x,a,4i4)') 'kk : ',(kk(i),i=1,m)
      end if !}
c>>> 
c*** Now look for targets intersected twice
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
c<<<
      write(0,*) 'Before re-arrangement. k = ',k
      if(k.gt.0) write(0,'(1x,a,8i4)') 'jj : ',(jj(i),i=1,k)
c>>>
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
c<<<
      write(0,*) 'After re-arrangement: ',nbdef
      if(nbdef.gt.0) then !{
        write(0,'(1x,a8,i6,9i11)') 'inddef: ',(inddef(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10(e10.3,1x))') 'xst:',(xst(i),i=1,nbdef)
        write(0,'(1x,a4,4x,1p,10(e10.3,1x))') 'yst:',(yst(i),i=1,nbdef)
      end if !}
c>>> 

c*** Check the consistency of the "primary targets" once more
      
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
          write(0,*) 'Error found in INSERT (entry TRGARNG): ',
     ,      'wrong number of "private" targets. ipx,n = ',i,mm(i)
        end if !}
      end do !}
      if(m.gt.0) stop '==> check the target specification' 
c<<<
      write(0,*) 'Leaving trgarng: ',nbdef
c>>> 
c=======================================================================
      end
