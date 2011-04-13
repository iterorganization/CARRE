module itmcarre

  use 

  implicit none
  


contains


  subroutine itmcarre()! equilibrium, limiter, edge )
    
    ! internal
    integer :: ifail

    ! 1. Read code parameters
    type(CarreParameters) :: par
    OPEN(UNIT=9, FILE='carre.dat', STATUS='unknown')
    CALL CHANGE(par,9,0,ifail)
    if ( ifail /= 0 ) then
        stop 'itmcarre: error reading carre.dat'
    end if

    ! 1. Read equilibrium

    ! 2. Read limiter






  end subroutine itmcarre
  

end module itmcarre
