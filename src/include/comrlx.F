      module comrlx
      use carre_types
      use carre_dimensions

      integer, save :: nrelax,ipol1,ipoln
      real(kind=R8), save ::
     .       l0(npmamx),l1(npmamx),l2(npmamx),ort1(npmamx),ort2(npmamx),
     .       relax,pasmin,del,rlcept,ortmax,ortpur1(npmamx),
     .       ortpur2(npmamx),propo1(npmamx),propo2(npmamx),
     .       varr1(npmamx),varr2(npmamx),tot1(npmamx),tot2(npmamx)

      end module comrlx
