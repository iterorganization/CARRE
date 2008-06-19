      subroutine percut(nx,ny,crx,cry,fpsi,ffbz,psidx,psidy,del,
     ,                                     nxmax,nymax,ix1,ix2,iy1,iy2)
c======================================================================
c*** introduce guard cells at a location defined by ix1, ix2, iy1, iy2.
c*** if abs(ix2-ix1).eq.1, the guard cell is placed at ix2, with iy
c*** varying from iy1 to iy2.
c======================================================================
      implicit none

c  arguments
      integer nx,ny,nxmax,nymax,ix1,ix2,iy1,iy2
      real crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  fpsi(-1:nxmax,-1:nymax,0:3),ffbz(-1:nxmax,-1:nymax,0:3),del,
     .  psidx(-1:nxmax,-1:nymax,0:3),psidy(-1:nxmax,-1:nymax,0:3)

c  local variables
      integer ix,iy,iy0,ix0
c======================================================================
c  calculs

      if(abs(iy1-iy2).eq.1) then
        iy0=iy1-(iy2-iy1)
        do ix=ix1,ix2
        crx(ix,iy2,0)=crx(ix,iy1,0)+del*(crx(ix,iy1,0)-crx(ix,iy0,0))
        crx(ix,iy2,1)=crx(ix,iy1,1)+del*(crx(ix,iy1,1)-crx(ix,iy0,1))
        crx(ix,iy2,2)=crx(ix,iy1,0)
        crx(ix,iy2,3)=crx(ix,iy1,1)

        cry(ix,iy2,0)=cry(ix,iy1,0)+del*(cry(ix,iy1,0)-cry(ix,iy0,0))
        cry(ix,iy2,1)=cry(ix,iy1,1)+del*(cry(ix,iy1,1)-cry(ix,iy0,1))
        cry(ix,iy2,2)=cry(ix,iy1,0)
        cry(ix,iy2,3)=cry(ix,0,1)

        fpsi(ix,iy2,0)=fpsi(ix,iy1,0)
     .    +del*(fpsi(ix,iy1,0)-fpsi(ix,iy0,0))
        fpsi(ix,iy2,1)=fpsi(ix,iy1,1)
     .    +del*(fpsi(ix,iy1,1)-fpsi(ix,iy0,1))
        fpsi(ix,iy2,2)=fpsi(ix,iy1,0)
        fpsi(ix,iy2,3)=fpsi(ix,iy1,1)

        ffbz(ix,iy2,0)=ffbz(ix,iy1,0)
     .    +del*(ffbz(ix,iy1,0)-ffbz(ix,iy0,0))
        ffbz(ix,iy2,1)=ffbz(ix,iy1,1)
     .    +del*(ffbz(ix,iy1,1)-ffbz(ix,iy0,1))
        ffbz(ix,iy2,2)=ffbz(ix,iy1,0)
        ffbz(ix,iy2,3)=ffbz(ix,iy1,1)

        psidx(ix,iy2,0)=psidx(ix,0,0)
     .    +del*(psidx(ix,iy1,0)-psidx(ix,iy0,0))
        psidx(ix,iy2,1)=psidx(ix,0,1)
     .    +del*(psidx(ix,iy1,1)-psidx(ix,iy0,1))
        psidx(ix,iy2,2)=psidx(ix,iy1,0)
        psidx(ix,iy2,3)=psidx(ix,iy1,1)

        psidy(ix,iy2,0)=psidy(ix,iy1,0)
     .    +del*(psidy(ix,iy1,0)-psidy(ix,iy0,0))
        psidy(ix,iy2,1)=psidy(ix,iy1,1)
     .    +del*(psidy(ix,iy1,1)-psidy(ix,iy0,1))
        psidy(ix,iy2,2)=psidy(ix,iy1,0)
        psidy(ix,iy2,3)=psidy(ix,iy1,1)
        enddo

      elseif(abs(ix1-ix2).eq.1) then
        ix0=ix1-(ix2-ix1)
        do iy=iy1,iy2
        crx(ix2,iy,0)=crx(ix1,iy,1)
        crx(ix2,iy,1)=crx(ix1,iy,1)+del*(crx(ix1,iy,1)-crx(ix0,iy,1))
        crx(ix2,iy,2)=crx(ix1,iy,3)
        crx(ix2,iy,3)=crx(ix1,iy,3)+del*(crx(ix1,iy,3)-crx(ix0,iy,3))

        cry(ix2,iy,0)=cry(ix1,iy,1)
        cry(ix2,iy,1)=cry(ix1,iy,1)+del*(cry(ix1,iy,1)-cry(ix0,iy,1))
        cry(ix2,iy,2)=cry(ix1,iy,3)
        cry(ix2,iy,3)=cry(ix1,iy,3)+del*(cry(ix1,iy,3)-cry(ix0,iy,3))

        fpsi(ix2,iy,0)=fpsi(ix1,iy,1)
        fpsi(ix2,iy,1)=fpsi(ix1,iy,1)
     .    +del*(fpsi(ix2-1,iy,1)-fpsi(ix0,iy,1))
        fpsi(ix2,iy,2)=fpsi(ix1,iy,3)
        fpsi(ix2,iy,3)=fpsi(ix1,iy,3)
     .    +del*(fpsi(ix2-1,iy,3)-fpsi(ix0,iy,3))

        ffbz(ix2,iy,0)=ffbz(ix1,iy,1)
        ffbz(ix2,iy,1)=ffbz(ix1,iy,1)
     .    +del*(ffbz(ix2-1,iy,1)-ffbz(ix0,iy,1))
        ffbz(ix2,iy,2)=ffbz(ix1,iy,3)
        ffbz(ix2,iy,3)=ffbz(ix1,iy,3)
     .    +del*(ffbz(ix2-1,iy,3)-ffbz(ix0,iy,3))

        psidx(ix2,iy,0)=psidx(ix1,iy,1)
        psidx(ix2,iy,1)=psidx(ix1,iy,1)
     .    +del*(psidx(ix2-1,iy,1)-psidx(ix0,iy,1))
        psidx(ix2,iy,2)=psidx(ix1,iy,3)
        psidx(ix2,iy,3)=psidx(ix1,iy,3)
     .    +del*(psidx(ix2-1,iy,3)-psidx(ix0,iy,3))

        psidy(ix2,iy,0)=psidy(ix1,iy,1)
        psidy(ix2,iy,1)=psidy(ix1,iy,1)
     .    +del*(psidy(ix2-1,iy,1)-psidy(ix0,iy,1))
        psidy(ix2,iy,2)=psidy(ix1,iy,3)
        psidy(ix2,iy,3)=psidy(ix1,iy,3)
     .    +del*(psidy(ix2-1,iy,3)-psidy(ix0,iy,3))
        enddo
      endif
c***
c       do ix=-1,nx
c       do iy=-1,ny
c         write(6,*)'ix, iy=',ix,iy
c         write(6,404)crx(ix,iy,2),cry(ix,iy,2),crx(ix,iy,3),
c    .      cry(ix,iy,3)
c         write(6,404)crx(ix,iy,0),cry(ix,iy,0),crx(ix,iy,1),
c    .      cry(ix,iy,1)
c404      format(1x,2f10.6,5x,2f10.6)
c       enddo
c       enddo
c***
      return
      end
