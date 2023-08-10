      subroutine construct_vessel_polygons
!======================================================================
!***  Contructs vessel polygons
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      integer(Short) :: i, j, k, l, ipt, npts, npth, nptt, ilbl
      integer(Short) :: fclblmin, fclblmax
      integer(Short), allocatable :: fclbls(:)
      real(rKind) :: xsrt, ysrt, xend, yend
      real(rKind) :: xhead(nmstr), yhead(nmstr), xtail(nmstr), ytail(nmstr)
      logical lfound, ldir, lused(nvess)
      logical points_match
      external points_match
      intrinsic min, max
!======================================================================

      ! init
      nstrv = 0
      lstrv = 0
      lclstrv = .false.
      xstrv = 0.0_rKind
      ystrv = 0.0_rKind

      xhead = 0.0_rKind
      yhead = 0.0_rKind
      xtail = 0.0_rKind
      ytail = 0.0_rKind

      ! check for presence at least one vessel segment
      if (nvess.le.0) then
        ! no vessel elements present - nothing to be done
        write (*,*) 'No vessel elements defined'
        return
      end if

      ! check which values of fclbl are present
      fclblmin = 1e5
      fclblmax = 0
      j = 0
      do i = 1, nvess
        fclblmin = min(fclbl(vess_elm(i)),fclblmin)
        fclblmax = max(fclbl(vess_elm(i)),fclblmax)
      end do

      ! consistency checks
      if (fclblmin.ne.1.and..not.(fclblmin.eq.0.and.fclblmax.eq.0)) then
        write (*,*) 'Warning: possibly inconsistent fclbl definition in DG model'
        do i = 1, nvess
          if (fclbl(vess_elm(i)).eq.0) then
            write (*,*) 'Element ', vess_elm(i), ' has fclbl 0.'
          endif
        end do
        stop
      elseif (fclblmin.eq.0.and.fclblmax.eq.0) then
        write (*,*) 'All vessel elements have fclbl = 0. Labels will be automatically assigned to individual polygon pieces.'
      end if
      allocate(fclbls(fclblmax-fclblmin+1))
      fclbls = [(i, i = fclblmin, fclblmax)]

!      write (*,*) 'construct_vessel_polygons: fclbl'
!      write (*,*) 'min and max label: ', fclblmin, fclblmax
!      do i = 1,nvess
!        write (*,*) 'segment i =',i,', label ',fclbl(vess_elm(i))
!      end do

      

      ! sort elements into polygons
      lused = .false.
      npts  = 0
      ilbl  = 0
      do while (.not.all(lused))

        nstrv = nstrv + 1
        ilbl  = ilbl + 1

        if (ilbl .gt. (fclblmax - fclblmin + 1)) then
          write (*,*) 'Not all vessel elements assigned to a polygon.'
          write (*,*) 'Check for inconsistent fclbls of the elements.'
          write (*,*) 'Elements with the same fclbl must form a single'
          write (*,*) 'open or closed polygon (no gaps).'
          stop ' ==> Check DG model'
        endif

        ! find suitable unused element to start search
        lfound = .false.
        i = 0
        do while (.not.lfound)
          i = i + 1
          if (.not.lused(i).and.fclbl(vess_elm(i)).eq.fclbls(ilbl)) lfound = .true.
          if (i.gt.nvess) then
            write (*,*) 'Found no elements with fclbl = ',fclbls(ilbl), '.'
            write (*,*) 'Min fclbl = ', fclblmin
            write (*,*) 'Max fclbl = ', fclblmax
            stop ' ==> Check DG model'
          end if
        end do
 
        ! set the initial element
        lused(i) = .true.
        xsrt = p1(1,vess_elm(i))
        ysrt = p1(2,vess_elm(i))
        xend = p2(1,vess_elm(i))
        yend = p2(2,vess_elm(i))

        xhead(1) = xsrt
        yhead(1) = ysrt
        xtail(1) = xend
        ytail(1) = yend

        ! first pass: find end point of the polygon
        ldir = .true.
        ipt = 1
        do while (ldir.and..not.all(lused))
          lfound = .false.
          i = 0
          do while (ldir.and..not.lfound)
            i = i + 1
            if (i.gt.nvess) then
               ! no further point found in this direction
               ! switch to other side
               ldir = .false.
            elseif (.not.lused(i).and.fclbl(vess_elm(i)).eq.fclbls(ilbl)) then
              if (points_match (xend, yend, p1(1,vess_elm(i)), p1(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xend = p2(1,vess_elm(i))
                yend = p2(2,vess_elm(i))
                xtail(ipt) = p2(1,vess_elm(i))
                ytail(ipt) = p2(2,vess_elm(i))
              elseif (points_match (xend, yend, p2(1,vess_elm(i)), p2(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xend = p1(1,vess_elm(i))
                yend = p1(2,vess_elm(i))
                xtail(ipt) = p1(1,vess_elm(i))
                ytail(ipt) = p1(2,vess_elm(i))
              end if
            endif
          end do
        end do
        nptt = ipt

        ! second pass: find start point of polygon
        ipt = 1
        do while (.not.ldir.and..not.all(lused))
          lfound = .false.
          i = 0
          do while (.not.ldir.and..not.lfound)
            i = i + 1
            if (i.gt.nvess) then
               ! no further point found in this direction
               ! terminate search for this polygon
               ldir = .true.
            elseif (.not.lused(i).and.fclbl(vess_elm(i)).eq.fclbls(ilbl)) then
              if (points_match (xsrt, ysrt, p1(1,vess_elm(i)), p1(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xsrt = p2(1,vess_elm(i))
                ysrt = p2(2,vess_elm(i))
                xhead(ipt) = p2(1,vess_elm(i))
                yhead(ipt) = p2(2,vess_elm(i))
              elseif (points_match (xsrt, ysrt, p2(1,vess_elm(i)), p2(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xsrt = p1(1,vess_elm(i))
                ysrt = p1(2,vess_elm(i))
                xhead(ipt) = p1(1,vess_elm(i))
                yhead(ipt) = p1(2,vess_elm(i))
              end if
            endif
          end do
        end do
        npth = ipt

        ! stitch pieces together
        if (points_match(xhead(npth), yhead(npth), xtail(nptt), ytail(nptt))) then
          ! closed polygon; remove duplicate start/end point here
          ! (will be added again during writing structure.dat file)
          lclstrv(nstrv) = .true.
          lstrv(nstrv) = nptt + npth - 1
          xstrv(npts+1:npts+npth) = xhead(npth:1:-1)
          ystrv(npts+1:npts+npth) = yhead(npth:1:-1)
          xstrv(npts+npth+1:npts+lstrv(nstrv)) = xtail(1:nptt-1)
          ystrv(npts+npth+1:npts+lstrv(nstrv)) = ytail(1:nptt-1)
        else
          ! open polygon
          lclstrv(nstrv) = .false.
          lstrv(nstrv) = nptt + npth
          xstrv(npts+1:npts+npth) = xhead(npth:1:-1)
          ystrv(npts+1:npts+npth) = yhead(npth:1:-1)
          xstrv(npts+npth+1:npts+lstrv(nstrv)) = xtail(1:nptt)
          ystrv(npts+npth+1:npts+lstrv(nstrv)) = ytail(1:nptt)
        endif
        npts = npts + nptt + npth

      end do

      deallocate(fclbls)

      ! Some consistency checks
      ! To be added: check that complete vessel is closed in case of multiple (open) polygons
      if (fclblmin.eq.0.and.fclblmax.eq.0) then
        !if (nstrv.ne.1) then
        !  write (*,*) 'Problem with definition vessel elements.'
        !elseif (.not.lclstrv(1)) then
        !  write (*,*) 'Vessel polygon not closed'
        !end if
      end if
       

      write (*,*) 'construct_vessel_polygons -- built ', nstrv, ' vessel polygons'

!======================================================================
      end 

      real(rKind) function points_dist( x1, y1, x2, y2)
        use KindDefinitions
        implicit none
        real(rKind), intent(in) :: x1, y1, x2, y2

        points_dist = sqrt( (x1-x2)**2 + (y1-y2)**2 )

      end function points_dist

      logical function points_match(x1 , y1 , x2 ,y2)
        use KindDefinitions
        implicit none
        real(rKind), intent(in) :: x1, y1, x2, y2
        real(rKind) :: points_dist
        external points_dist

        points_match = points_dist(x1, y1, x2, y2).lt.1.0e-6_rKind

      end function points_match
        
