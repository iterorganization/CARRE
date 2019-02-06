      subroutine periph(nx,ny,crx,cry,fpsi,ffbz,psidx,psidy,del,
     ,                                                     nxmax,nymax)
c
c  version : 24.06.98 14:45
c
c======================================================================
c*** This routine apparently creates the "guard cells" around the mesh
c======================================================================
      implicit none
#include <CARRETYPES.F>
c
c  arguments
      integer nx,ny,nxmax,nymax
      real(kind=R8)
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  fpsi(-1:nxmax,-1:nymax,0:3),ffbz(-1:nxmax,-1:nymax,0:3),del,
     .  psidx(-1:nxmax,-1:nymax,0:3),psidy(-1:nxmax,-1:nymax,0:3)
c
c  local variables
      integer ix,iy
c======================================================================
c  calculs

        do ix=0,nx-1
          crx(ix,-1,0)=crx(ix,0,0)+del*(crx(ix,0,0)-crx(ix,1,0))
          crx(ix,-1,1)=crx(ix,0,1)+del*(crx(ix,0,1)-crx(ix,1,1))
          crx(ix,-1,2)=crx(ix,0,0)
          crx(ix,-1,3)=crx(ix,0,1)
          crx(ix,ny,0)=crx(ix,ny-1,2)
          crx(ix,ny,1)=crx(ix,ny-1,3)
          crx(ix,ny,2)=crx(ix,ny-1,2)+
     +                              del*(crx(ix,ny-1,2)-crx(ix,ny-2,2))
          crx(ix,ny,3)=crx(ix,ny-1,3)+
     +                              del*(crx(ix,ny-1,3)-crx(ix,ny-2,3))
c***
c       print*,'ix=',ix
c       print*,crx(ix,-1,0),crx(ix,-1,1)
c       print*,crx(ix,-1,2),crx(ix,-1,3)
c       print*,crx(ix,ny,0),crx(ix,ny,1)
c       print*,crx(ix,ny,2),crx(ix,ny,3)
c***

          cry(ix,-1,0)=cry(ix,0,0)+del*(cry(ix,0,0)-cry(ix,1,0))
          cry(ix,-1,1)=cry(ix,0,1)+del*(cry(ix,0,1)-cry(ix,1,1))
          cry(ix,-1,2)=cry(ix,0,0)
          cry(ix,-1,3)=cry(ix,0,1)
          cry(ix,ny,0)=cry(ix,ny-1,2)
          cry(ix,ny,1)=cry(ix,ny-1,3)
          cry(ix,ny,2)=cry(ix,ny-1,2)+
     +                              del*(cry(ix,ny-1,2)-cry(ix,ny-2,2))
          cry(ix,ny,3)=cry(ix,ny-1,3)+
     +                              del*(cry(ix,ny-1,3)-cry(ix,ny-2,3))

          fpsi(ix,-1,0)=fpsi(ix,0,0)+del*(fpsi(ix,0,0)-fpsi(ix,1,0))
          fpsi(ix,-1,1)=fpsi(ix,0,1)+del*(fpsi(ix,0,1)-fpsi(ix,1,1))
          fpsi(ix,-1,2)=fpsi(ix,0,0)
          fpsi(ix,-1,3)=fpsi(ix,0,1)
          fpsi(ix,ny,0)=fpsi(ix,ny-1,2)
          fpsi(ix,ny,1)=fpsi(ix,ny-1,3)
          fpsi(ix,ny,2)=fpsi(ix,ny-1,2)+
     +                            del*(fpsi(ix,ny-1,2)-fpsi(ix,ny-2,2))
          fpsi(ix,ny,3)=fpsi(ix,ny-1,3)+
     +                            del*(fpsi(ix,ny-1,3)-fpsi(ix,ny-2,3))

          ffbz(ix,-1,0)=ffbz(ix,0,0)+del*(ffbz(ix,0,0)-ffbz(ix,1,0))
          ffbz(ix,-1,1)=ffbz(ix,0,1)+del*(ffbz(ix,0,1)-ffbz(ix,1,1))
          ffbz(ix,-1,2)=ffbz(ix,0,0)
          ffbz(ix,-1,3)=ffbz(ix,0,1)
          ffbz(ix,ny,0)=ffbz(ix,ny-1,2)
          ffbz(ix,ny,1)=ffbz(ix,ny-1,3)
          ffbz(ix,ny,2)=ffbz(ix,ny-1,2)+
     +                            del*(ffbz(ix,ny-1,2)-ffbz(ix,ny-2,2))
          ffbz(ix,ny,3)=ffbz(ix,ny-1,3)+
     +                            del*(ffbz(ix,ny-1,3)-ffbz(ix,ny-2,3))

          psidx(ix,-1,0)=psidx(ix,0,0)+
     +                                del*(psidx(ix,0,0)-psidx(ix,1,0))
          psidx(ix,-1,1)=psidx(ix,0,1)+
     +                                del*(psidx(ix,0,1)-psidx(ix,1,1))
          psidx(ix,-1,2)=psidx(ix,0,0)
          psidx(ix,-1,3)=psidx(ix,0,1)
          psidx(ix,ny,0)=psidx(ix,ny-1,2)
          psidx(ix,ny,1)=psidx(ix,ny-1,3)
          psidx(ix,ny,2)=psidx(ix,ny-1,2)+
     +                          del*(psidx(ix,ny-1,2)-psidx(ix,ny-2,2))
          psidx(ix,ny,3)=psidx(ix,ny-1,3)+
     +                            del*(ffbz(ix,ny-1,3)-ffbz(ix,ny-2,3))

          psidy(ix,-1,0)=psidy(ix,0,0)+
     +                                del*(psidy(ix,0,0)-psidy(ix,1,0))
          psidy(ix,-1,1)=psidy(ix,0,1)+
     +                                del*(psidy(ix,0,1)-psidy(ix,1,1))
          psidy(ix,-1,2)=psidy(ix,0,0)
          psidy(ix,-1,3)=psidy(ix,0,1)
          psidy(ix,ny,0)=psidy(ix,ny-1,2)
          psidy(ix,ny,1)=psidy(ix,ny-1,3)
          psidy(ix,ny,2)=psidy(ix,ny-1,2)+
     +                          del*(psidy(ix,ny-1,2)-psidy(ix,ny-2,2))
          psidy(ix,ny,3)=psidy(ix,ny-1,3)+
     +                            del*(ffbz(ix,ny-1,3)-ffbz(ix,ny-2,3))
        end do

        do iy=0,ny-1
          crx(-1,iy,0)=crx(0,iy,0)+del*(crx(0,iy,0)-crx(1,iy,0))
          crx(-1,iy,1)=crx(0,iy,0)
          crx(-1,iy,2)=crx(0,iy,2)+del*(crx(0,iy,2)-crx(1,iy,2))
          crx(-1,iy,3)=crx(0,iy,2)
          crx(nx,iy,0)=crx(nx-1,iy,1)
          crx(nx,iy,1)=crx(nx-1,iy,1)+
     +                              del*(crx(nx-1,iy,1)-crx(nx-2,iy,1))
          crx(nx,iy,2)=crx(nx-1,iy,3)
          crx(nx,iy,3)=crx(nx-1,iy,3)+
     +                              del*(crx(nx-1,iy,3)-crx(nx-2,iy,3))

          cry(-1,iy,0)=cry(0,iy,0)+del*(cry(0,iy,0)-cry(1,iy,0))
          cry(-1,iy,1)=cry(0,iy,0)
          cry(-1,iy,2)=cry(0,iy,2)+del*(cry(0,iy,2)-cry(1,iy,2))
          cry(-1,iy,3)=cry(0,iy,2)
          cry(nx,iy,0)=cry(nx-1,iy,1)
          cry(nx,iy,1)=cry(nx-1,iy,1)+
     +                              del*(cry(nx-1,iy,1)-cry(nx-2,iy,1))
          cry(nx,iy,2)=cry(nx-1,iy,3)
          cry(nx,iy,3)=cry(nx-1,iy,3)+
     +                              del*(cry(nx-1,iy,3)-cry(nx-2,iy,3))

          fpsi(-1,iy,0)=fpsi(0,iy,0)+del*(fpsi(0,iy,0)-fpsi(1,iy,0))
          fpsi(-1,iy,1)=fpsi(0,iy,0)
          fpsi(-1,iy,2)=fpsi(0,iy,2)+del*(fpsi(0,iy,2)-fpsi(1,iy,2))
          fpsi(-1,iy,3)=fpsi(0,iy,2)
          fpsi(nx,iy,0)=fpsi(nx-1,iy,1)
          fpsi(nx,iy,1)=fpsi(nx-1,iy,1)+
     +                            del*(fpsi(nx-1,iy,1)-fpsi(nx-2,iy,1))
          fpsi(nx,iy,2)=fpsi(nx-1,iy,3)
          fpsi(nx,iy,3)=fpsi(nx-1,iy,3)+
     +                            del*(fpsi(nx-1,iy,3)-fpsi(nx-2,iy,3))

          ffbz(-1,iy,0)=ffbz(0,iy,0)+del*(ffbz(0,iy,0)-ffbz(1,iy,0))
          ffbz(-1,iy,1)=ffbz(0,iy,0)
          ffbz(-1,iy,2)=ffbz(0,iy,2)+del*(ffbz(0,iy,2)-ffbz(1,iy,2))
          ffbz(-1,iy,3)=ffbz(0,iy,2)
          ffbz(nx,iy,0)=ffbz(nx-1,iy,1)
          ffbz(nx,iy,1)=ffbz(nx-1,iy,1)+
     +                            del*(ffbz(nx-1,iy,1)-ffbz(nx-2,iy,1))
          ffbz(nx,iy,2)=ffbz(nx-1,iy,3)
          ffbz(nx,iy,3)=ffbz(nx-1,iy,3)+
     +                            del*(ffbz(nx-1,iy,3)-ffbz(nx-2,iy,3))

          psidx(-1,iy,0)=psidx(0,iy,0)+
     +                                del*(psidx(0,iy,0)-psidx(1,iy,0))
          psidx(-1,iy,1)=psidx(0,iy,0)
          psidx(-1,iy,2)=psidx(0,iy,2)+
     +                                del*(psidx(0,iy,2)-psidx(1,iy,2))
          psidx(-1,iy,3)=psidx(0,iy,2)
          psidx(nx,iy,0)=psidx(nx-1,iy,1)
          psidx(nx,iy,1)=psidx(nx-1,iy,1)+
     +                          del*(psidx(nx-1,iy,1)-psidx(nx-2,iy,1))
          psidx(nx,iy,2)=psidx(nx-1,iy,3)
          psidx(nx,iy,3)=psidx(nx-1,iy,3)+
     +                          del*(psidx(nx-1,iy,3)-psidx(nx-2,iy,3))

          psidy(-1,iy,0)=psidy(0,iy,0)+
     +                                del*(psidy(0,iy,0)-psidy(1,iy,0))
          psidy(-1,iy,1)=psidy(0,iy,0)
          psidy(-1,iy,2)=psidy(0,iy,2)+
     +                                del*(psidy(0,iy,2)-psidy(1,iy,2))
          psidy(-1,iy,3)=psidy(0,iy,2)
          psidy(nx,iy,0)=psidy(nx-1,iy,1)
          psidy(nx,iy,1)=psidy(nx-1,iy,1)+
     +                          del*(psidy(nx-1,iy,1)-psidy(nx-2,iy,1))
          psidy(nx,iy,2)=psidy(nx-1,iy,3)
          psidy(nx,iy,3)=psidy(nx-1,iy,3)+
     +                          del*(psidy(nx-1,iy,3)-psidy(nx-2,iy,3))
        end do
c  lower left corner
        crx(-1,-1,0)=(1.+del)*crx(-1,0,0)-del*crx(-1,1,0)
        crx(-1,-1,1)=crx(0,-1,0)
        crx(-1,-1,2)=crx(-1,0,0)
        crx(-1,-1,3)=crx(0,0,0)

        cry(-1,-1,0)=(1.+del)*cry(-1,0,0)-del*cry(-1,1,0)
        cry(-1,-1,1)=cry(0,-1,0)
        cry(-1,-1,2)=cry(-1,0,0)
        cry(-1,-1,3)=cry(0,0,0)

        fpsi(-1,-1,0)=(1.+del)*fpsi(-1,0,0)-del*fpsi(-1,1,0)
        fpsi(-1,-1,1)=fpsi(0,-1,0)
        fpsi(-1,-1,2)=fpsi(-1,0,0)
        fpsi(-1,-1,3)=fpsi(0,0,0)

        ffbz(-1,-1,0)=(1.+del)*ffbz(-1,0,0)-del*ffbz(-1,1,0)
        ffbz(-1,-1,1)=ffbz(0,-1,0)
        ffbz(-1,-1,2)=ffbz(-1,0,0)
        ffbz(-1,-1,3)=ffbz(0,0,0)

        psidx(-1,-1,0)=(1.+del)*psidx(-1,0,0)-del*psidx(-1,1,0)
        psidx(-1,-1,1)=psidx(0,-1,0)
        psidx(-1,-1,2)=psidx(-1,0,0)
        psidx(-1,-1,3)=psidx(0,0,0)

        psidy(-1,-1,0)=(1.+del)*psidy(-1,0,0)-del*psidy(-1,1,0)
        psidy(-1,-1,1)=psidy(0,-1,0)
        psidy(-1,-1,2)=psidy(-1,0,0)
        psidy(-1,-1,3)=psidy(0,0,0)
c  lower right corner
        crx(nx,-1,0)=crx(nx-1,-1,1)
        crx(nx,-1,1)=(1.+del)*crx(nx,0,1)-del*crx(nx,1,1)
        crx(nx,-1,2)=crx(nx,0,0)
        crx(nx,-1,3)=crx(nx,0,1)

        cry(nx,-1,0)=cry(nx-1,-1,1)
        cry(nx,-1,1)=(1.+del)*cry(nx,0,1)-del*cry(nx,1,1)
        cry(nx,-1,2)=cry(nx,0,0)
        cry(nx,-1,3)=cry(nx,0,1)

        fpsi(nx,-1,0)=fpsi(nx-1,-1,1)
        fpsi(nx,-1,1)=(1.+del)*fpsi(nx,0,1)-del*fpsi(nx,1,1)
        fpsi(nx,-1,2)=fpsi(nx,0,0)
        fpsi(nx,-1,3)=fpsi(nx,0,1)

        ffbz(nx,-1,0)=ffbz(nx-1,-1,1)
        ffbz(nx,-1,1)=(1.+del)*ffbz(nx,0,1)-del*ffbz(nx,1,1)
        ffbz(nx,-1,2)=ffbz(nx,0,0)
        ffbz(nx,-1,3)=ffbz(nx,0,1)

        psidx(nx,-1,0)=psidx(nx-1,-1,1)
        psidx(nx,-1,1)=(1.+del)*psidx(nx,0,1)-del*psidx(nx,1,1)
        psidx(nx,-1,2)=psidx(nx,0,0)
        psidx(nx,-1,3)=psidx(nx,0,1)

        psidy(nx,-1,0)=psidy(nx-1,-1,1)
        psidy(nx,-1,1)=(1.+del)*psidy(nx,0,1)-del*psidy(nx,1,1)
        psidy(nx,-1,2)=psidy(nx,0,0)
        psidy(nx,-1,3)=psidy(nx,0,1)
c  upper left corner
        crx(-1,ny,0)=crx(-1,ny-1,2)
        crx(-1,ny,1)=crx(0,ny,0)
        crx(-1,ny,2)=(1.+del)*crx(-1,ny-1,2)-del*crx(-1,ny-2,2)
        crx(-1,ny,3)=crx(0,ny,2)

        cry(-1,ny,0)=cry(-1,ny-1,2)
        cry(-1,ny,1)=cry(0,ny,0)
        cry(-1,ny,2)=(1.+del)*cry(-1,ny-1,2)-del*cry(-1,ny-2,2)
        cry(-1,ny,3)=cry(0,ny,2)

        fpsi(-1,ny,0)=fpsi(-1,ny-1,2)
        fpsi(-1,ny,1)=fpsi(0,ny,0)
        fpsi(-1,ny,2)=(1.+del)*fpsi(-1,ny-1,2)-del*fpsi(-1,ny-2,2)
        fpsi(-1,ny,3)=fpsi(0,ny,2)

        ffbz(-1,ny,0)=ffbz(-1,ny-1,2)
        ffbz(-1,ny,1)=ffbz(0,ny,0)
        ffbz(-1,ny,2)=(1.+del)*ffbz(-1,ny-1,2)-del*ffbz(-1,ny-2,2)
        ffbz(-1,ny,3)=ffbz(0,ny,2)

        psidx(-1,ny,0)=psidx(-1,ny-1,2)
        psidx(-1,ny,1)=psidx(0,ny,0)
        psidx(-1,ny,2)=(1.+del)*psidx(-1,ny-1,2)-del*psidx(-1,ny-2,2)
        psidx(-1,ny,3)=psidx(0,ny,2)

        psidy(-1,ny,0)=psidy(-1,ny-1,2)
        psidy(-1,ny,1)=psidy(0,ny,0)
        psidy(-1,ny,2)=(1.+del)*psidy(-1,ny-1,2)-del*psidy(-1,ny-2,2)
        psidy(-1,ny,3)=psidy(0,ny,2)
c  upper right corner
        crx(nx,ny,0)=crx(nx-1,ny,1)
        crx(nx,ny,1)=crx(nx,ny-1,3)
        crx(nx,ny,2)=crx(nx-1,ny,3)
        crx(nx,ny,3)=(1.+del)*crx(nx-1,ny,3)-del*crx(nx-2,ny,3)

        cry(nx,ny,0)=cry(nx-1,ny,1)
        cry(nx,ny,1)=cry(nx,ny-1,3)
        cry(nx,ny,2)=cry(nx-1,ny,3)
        cry(nx,ny,3)=(1.+del)*cry(nx-1,ny,3)-del*cry(nx-2,ny,3)

        fpsi(nx,ny,0)=fpsi(nx-1,ny,1)
        fpsi(nx,ny,1)=fpsi(nx,ny-1,3)
        fpsi(nx,ny,2)=fpsi(nx-1,ny,3)
        fpsi(nx,ny,3)=(1.+del)*fpsi(nx-1,ny,3)-del*fpsi(nx-2,ny,3)

        ffbz(nx,ny,0)=ffbz(nx-1,ny,1)
        ffbz(nx,ny,1)=ffbz(nx,ny-1,3)
        ffbz(nx,ny,2)=ffbz(nx-1,ny,3)
        ffbz(nx,ny,3)=(1.+del)*ffbz(nx-1,ny,3)-del*ffbz(nx-2,ny,3)

        psidx(nx,ny,0)=psidx(nx-1,ny,1)
        psidx(nx,ny,1)=psidx(nx,ny-1,3)
        psidx(nx,ny,2)=psidx(nx-1,ny,3)
        psidx(nx,ny,3)=(1.+del)*psidx(nx-1,ny,3)-del*psidx(nx-2,ny,3)

        psidy(nx,ny,0)=psidy(nx-1,ny,1)
        psidy(nx,ny,1)=psidy(nx,ny-1,3)
        psidy(nx,ny,2)=psidy(nx-1,ny,3)
        psidy(nx,ny,3)=(1.+del)*psidy(nx-1,ny,3)-del*psidy(nx-2,ny,3)
c***
c       do ix=-1,nx
c         do iy=-1,ny
c           write(6,*)'ix, iy=',ix,iy
c           write(6,404)crx(ix,iy,2),cry(ix,iy,2),crx(ix,iy,3),
c    .        cry(ix,iy,3)
c           write(6,404)crx(ix,iy,0),cry(ix,iy,0),crx(ix,iy,1),
c    .        cry(ix,iy,1)
c404        format(1x,2f10.6,5x,2f10.6)
c         enddo
c       enddo
c***
      return
c======================================================================
      end
