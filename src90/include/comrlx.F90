      module comrlx
      use KindDefinitions
      use carre_dimensions

      integer, save :: nrelax,ipol1,ipoln
      real(rKind), save :: &
     &  l0(npmamx),l1(npmamx),l2(npmamx), &
     &  relax,pasmin,del,rlcept,ortmax

      end module comrlx
