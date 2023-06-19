      subroutine construct_vessel_polygons
!======================================================================
!***  Contructs vessel polygons
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      integer(Short) :: i, j, k, l, ipt, npts, npth, nptt
      real(rKind) :: xsrt, ysrt, xend, yend
      real(rKind) :: xhead(nmstr), yhead(nmstr), xtail(nmstr), ytail(nmstr)
      logical lfound, ldir, lused(nvess)
      logical points_match
      external points_match
!======================================================================

      ! init
      nstrv = 0
      lstrv = 0
      lclstrv = .false.
      xstrv = 0.0_rKind
      ystrv = 0.0_rKind

      ! check for presence at least one vessel segment
      if (nvess.le.0) then
        ! no vessel elements present - nothing to be done
        write (*,*) 'No vessel elements defined'
        return
      end if

      ! sort elements into polygons
      lused = .false.
      npts  = 0
      do while (.not.all(lused))

        nstrv = nstrv + 1

        ! find suitable unused element to start search
        lfound = .false.
        i = 0
        do while (.not.lfound)
          i = i + 1
          if (.not.lused(i)) lfound = .true.
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
        xtail(1) = xend

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
            elseif (.not.lused(i)) then
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
            elseif (.not.lused(i)) then
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
        
