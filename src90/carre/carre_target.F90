module carre_target

  ! Helper functions for handling target structures.

  use carre_types

  implicit none

  integer, parameter :: INDSGM_MODE_ANGLE = 1
  integer, parameter :: INDSGM_MODE_DISTANCE = 2

contains

  ! Check on which side of structure with index iStruct a point is when
  ! walking along the structure segments.
  ! Result can either be STRUCT_LEFT (point is left/counterclockwise) 
  ! or STRUCT_RIGHT (point is on right/clockwise direction)
  integer function onWhichSideOfStructure(x, y, struct, iStruct) result(direction)
    double precision, intent(in) :: x, y
    type(CarreStructures), intent(in) :: struct
    integer, intent(in) :: iStruct

    ! internal
    integer :: ind
    double precision :: segx, segy, conx, cony, cp

    ! Find index of closest segment
    ind=indsgm(struct%xstruc(1:struct%npstru(iStruct), iStruct),&
         & struct%ystruc(1:struct%npstru(iStruct), iStruct),&
         & struct%npstru(iStruct), x, y, mode = INDSGM_MODE_DISTANCE)

    ! compute cross product between segment and vector connecting
    ! the start point of the segment to the point

    segx = struct%xstruc(ind + 1, iStruct) - struct%xstruc(ind, iStruct)
    segy = struct%ystruc(ind + 1, iStruct) - struct%ystruc(ind, iStruct)
    conx = x - struct%xstruc(ind, iStruct)
    cony = y - struct%ystruc(ind, iStruct)
    
    cp = segx * cony - conx * segy

    ! cp > 0.0: segment vector is clockwise from connection vector: point is on left side of segment        
    ! cp < 0.0: segment vector is counterclockwise from connection vector: point is on right side of segment
    ! cp == 0.0: vectors are collinear, point is on structure segment
    if ( cp == 0.0d0 ) then
        ! in this unlikely event count point to be internal
        direction = GRID_UNDEFINED
    else if ( cp > 0.0d0 ) then
        direction = STRUCT_LEFT
    else 
        direction = STRUCT_RIGHT
    end if
  end function onWhichSideOfStructure


  ! Check whether a point is on the internal or externals side
  ! the structure with index iStruct. For this to work, the
  ! external normal information for the structure has to be specified.
  ! (For the target plates, this is done in the routine sptris.
  ! If the point is exactly on the segment, it is counted as not internal.
  logical function onInternalSideOfStructure(x, y, struct, iStruct) result(isInternal)
    double precision, intent(in) :: x, y
    type(CarreStructures), intent(in) :: struct
    integer, intent(in) :: iStruct

    ! internal
    integer :: ind
    double precision :: segx, segy, conx, cony, cp

    if ( struct%internalSide(iStruct) == GRID_UNDEFINED ) then
        stop "onInternalSideOfStructure: don't know internal side for this structure"
    end if

    isInternal = (onWhichSideOfStructure(x, y, struct, iStruct) == struct%internalSide(iStruct))

  end function onInternalSideOfStructure

  ! For a closed structure, check whether a point is inside the structure.
  ! Cette fonction determine si un point est a l'interieur d'une
  ! structure fermee.
  LOGICAL FUNCTION inStruct(x,y,xstr,ystr,n)

    !  arguments
    INTEGER n
    REAL*8 x, y, xstr(abs(n)), ystr(abs(n))

    !  variables locales
    INTEGER i
    REAL*8 angtot,theta,prdvec,a
    logical :: ouvert

    !  procedures
    INTRINSIC ACOS,ABS

    !=========================
    !.. xst,yst: tableaux des coordonnees des points de la structure.
    !.. n  : nombre de points de la structure.
    !.. x,y: point ou il faut verifier.
    !.. theta
    !.. theta: angle entre les vecteurs allant du point au debut d'un
    !          segment et du allant du point a la fin de ce segment.
    !.. prdvec: produit vectoriel entre les vecteurs.
    !.. angtot: somme des angles.
    !=========================

    ouvert = xstr(1).ne.xstr(n) .or. ystr(1).ne.ystr(n)
    if (ouvert) then
        ! We are looking at an open structure.
        inStruct = .false.
        return
    end if

    !  Le produit scalaire est utilise pour trouver l'angle et le
    !  produit vectoriel, pour voir si cet angle sera additionne
    !  ou soustrait.
    angtot=0.
    DO i=1, n-1
        prdvec = (xstr(i)-x)*(ystr(i+1)-y) - (ystr(i)-y)*(xstr(i+1)-x)

        if (prdvec.ne.0.) then
            theta = acos(((xstr(i)-x)*(xstr(i+1)-x) + & 
                 &                   (ystr(i)-y)*(ystr(i+1)-y))/ & 
                 &                   (sqrt(((xstr(i)-x)**2+(ystr(i)-y)**2)* & 
                 &                   ((xstr(i+1)-x)**2+(ystr(i+1)-y)**2))))
        else
            theta = 3.14159
        endif

        IF (prdvec.lt.0.) THEN
            a = -1.
        ELSE
            a = 1.
        ENDIF

        angtot = angtot + a*theta
    end do

    IF (ABS((ABS(angtot)-6.28318)).LT.0.001) THEN
        inStruct = .TRUE.
    ELSE
        inStruct = .FALSE.
    ENDIF
    RETURN
  END FUNCTION inStruct


  INTEGER FUNCTION drctio(xst,yst,npst,x1,y1,x2,y2,doug)

    !..Determination de la direction anti-horaire (dir=+1) ou horaire (-1)
    !  qui correspond a un deplacement vers la droite ou vers la gauche
    !  le long d'une structure, a partir d'un point x,y

    !  arguments
    INTEGER npst
    REAL*8 xst(npst),yst(npst),x1,y1
    double precision, intent(in) :: x2, y2
    CHARACTER*(*) doug

    !  variables locales
    INTEGER ind,nrot,irot
    REAL*8 sg,delta,xin,yin,xin1,yin1,zero,norm,pi,cs,sn
    parameter(delta=1.e-4,zero=0.,nrot=10,pi=3.141592654)
    logical :: found

    !  procedures
    intrinsic sqrt,sin,cos

    integer n_call
    logical l_dbg
    data n_call /0/, l_dbg /.false./

    !=========================
    !.. xst,yst: tableaux des coordonnees des points de la structure.
    !.. npst: nombre de points de la structure.
    !.. x1,y1: point de depart sur la structure.
    !.. x2,y2: additional point outside the structure to interpret the 
    !.. points away from the structure
    !.. doug: drote ou gauche
    !.. ind: indice du segment de structure.
    !=========================
    !  calculs

    n_call=n_call+1

    cs=cos((2.*pi)/nrot)
    sn=sin((2.*pi)/nrot)

    ind=indsgm(xst,yst,npst,x1,y1)

    IF (doug(1:1).EQ.'d' .OR. doug(1:1).EQ.'D') then
        sg=+1
    else
        sg=-1
    end if
    if(y1.gt.zero) sg=-sg

    ! If structure is closed, use ("old") supposedly more robust
    ! algorithm that finds a point inside the structure
    if ( xst(1) == xst(npst) .and. yst(1) == yst(npst) ) then

        IF (xst(ind).NE.x1 .or. yst(ind).ne.y1) then
            !  on trouve d'abord les coordonnees d'un point interieur
            xin=y1-yst(ind)
            yin=xst(ind)-x1
            norm=sqrt(xin*xin+yin*yin)
            xin=delta*xin/norm
            yin=delta*yin/norm

            do irot=1,nrot
                found = inStruct(x1+xin,y1+yin,xst,yst,npst)
                if (found) exit
                norm=xin
                xin=xin*cs-yin*sn
                yin=norm*sn+yin*cs
            end do

            if (.not. found) then
                !        write(0,*)'on ne trouve pas de point interieur dans drctio'
                write(0,*) 'Interior point not found in drctio ', & 
                     &                                  '- an internal error in Carre?'
                write(0,*) '*2: n_call,x1,y1=',n_call,x1,y1
                write(0,'(5h xst:,1p,7e12.4/(5x,7e12.4))') xst
                write(0,'(5h yst:,1p,7e12.4/(5x,7e12.4))') yst
                call trc_stk
                write(0,*) '==> Check whether the structures representing ', & 
                     &                                         'the targets are closed'
                call pltend
                stop
            end if

            !  on teste maintenant l'orientation
            if(sg*(xin*(y1-yst(ind))-yin*(x1-xst(ind))).gt.zero) then
                drctio=1
            else
                drctio=-1
            end if
        else if(xst(ind+1).NE.x1 .or. yst(ind+1).ne.y1) then
            xin1=xst(ind+1)-x1
            yin1=yst(ind+1)-y1
            norm=sqrt(xin1*xin1+yin1*yin1)
            xin1=xin1/norm
            yin1=yin1/norm
            if(ind.eq.1) then
                irot=npst-1
            else
                irot=ind-1
            end if
            xin=xst(irot)-x1
            yin=yst(irot)-y1
            norm=sqrt(xin*xin+yin*yin)
            xin=xin/norm+xin1
            yin=yin/norm+yin1
            norm=sqrt(xin*xin+yin*yin)
            xin=delta*xin/norm
            yin=delta*yin/norm

            do irot=1,nrot
                found = inStruct(x1+xin,y1+yin,xst,yst,npst)
                if (found) exit
                norm=xin
                xin=xin*cs-yin*sn
                yin=norm*sn+yin*cs
            end do
            if (.not. found) then

                write(0,*) 'Interior point not found in drctio ', & 
                     &                                  '- an internal error in Carre?'
                write(0,*) '*6: n_call,x1,y1=',n_call,x1,y1
                write(0,'(5h xst:,1p,7e12.4/(5x,7e12.4))') xst
                write(0,'(5h yst:,1p,7e12.4/(5x,7e12.4))') yst
                call trc_stk
                write(0,*) '==> Check whether the structures representing ', & 
                     &                                         'the targets are closed'
                call pltend
                stop
            end if

            !  on teste maintenant l'orientation
            if(sg*(xin*(y1-yst(ind))-yin*(x1-xst(ind))).gt.zero) then
                drctio=1
            else
                drctio=-1
            end if
        else
            call pltend
            STOP 'Degenerated segment detected in drctio: the program stops'
        end if
    else
        ! For an open structure, use new algorithm that exploits additional supplied
        ! external point

        xin=x1-x2
        yin=y1-y2
        norm=sqrt(xin**2+yin**2)
        !xin = (xst(ind) + xst(ind+1)) / 2.0 + delta * xin / norm
        !yin = (yst(ind) + yst(ind+1)) / 2.0 + delta * yin / norm
        xin = delta * xin / norm
        yin = delta * yin / norm
        
        if (sg*(xin*(yst(ind+1)-yst(ind))-yin*(xst(ind+1)-xst(ind))).gt.zero) then
            drctio=1
            !if (drctio /= 1) stop "drctio: algorithms differ (1)"
        else
            drctio=-1
            !if (drctio /= -1) stop "drctio: algorithms differ (2)"
        end if
    end if


  END FUNCTION drctio



  REAL*8 FUNCTION plqdst(x0,y0,x2,y2,xst,yst,npst,doug)

    !..Cette sous-routine calcule la distance entre deux points sur une meme
    !  structure, en longeant les segments de structure.

    !  arguments
    INTEGER npst
    REAL*8 x0,y0,x2,y2,xst(npst),yst(npst)
    CHARACTER*(*) doug

    !  variables locales
    INTEGER ind,ind1,ind2,sens
    REAL*8 x1,y1,x3,y3,dist

    !  procedures
    INTRINSIC MOD,SQRT
    EXTERNAL trc_stk_in,trc_stk_out

    !=========================
    !.. xst,yst: tableaux des coordonnees des points de la structure.
    !.. npst: nombre de points de la structure.
    !.. x0,y0: point de depart sur la structure.
    !.. x2,y2: point d'arrivee sur la structure.
    !.. doug: droite ou gauche
    !.. ind2: indice du segment sur lequel se trouve le point 2.
    !.. sens: sens dans lequel il faut partir pour la recherche,
    !         1 = sens des points de la structure, -1 = sens contraire.
    !.. x1,y1,x3,y3: points intermediaires.
    !=========================

    !..Copie d'arguments en variables locales
    x1 = x0
    y1 = y0

    ! FIXME/CLEANUP: simplify this inefficient mess

    !..Recherche du sens et indices de segment pour les deux points.
    ind1 = indsgm(xst,yst,npst,x1,y1)
    ind2 = indsgm(xst,yst,npst,x2,y2)

    !..On verifie si les points sont sur le meme segment.
    IF (ind1 .EQ. ind2) THEN
        plqdst = SQRT((x1-x2)**2 + (y1-y2)**2)
        RETURN
    ELSE

        !..Sinon on calcule la distance de coin en coin jusqu'a ce qu'on soit
        !  sur le segment du point d'arrivee.
        ! To avoid the quite unnecessary call to drctio, we just compute
        ! the distance in both directions and take the smaller one.

        plqdst = huge(plqdst)

        do sens = -1, 1, 2

            x1 = x0
            x1 = x0
            ind1 = indsgm(xst,yst,npst,x1,y1)
            ind2 = indsgm(xst,yst,npst,x2,y2)

            dist = 0.0

            do
                IF (sens .EQ. 1) THEN
                    ind = MOD(ind1,npst-1) + 1
                ELSE
                    ind = ind1
                ENDIF

                x3 = xst(ind)
                y3 = yst(ind)

                dist = dist + SQRT((x1-x3)**2 + (y1-y3)**2)

                IF (sens .EQ. 1) THEN

                    IF (ind .NE. ind2) THEN
                        x1 = x3
                        y1 = y3
                        ind1 = ind
                        cycle
                    ELSE
                        dist = dist + SQRT((x2-x3)**2 + (y2-y3)**2)
                        exit
                    ENDIF
                ELSE
                    IF (ind .NE. MOD(ind2,abs(npst)-1) + 1) THEN
                        x1 = x3
                        y1 = y3
                        ind1 = ind1 - 1
                        IF (ind1 .EQ. 0) ind1 = abs(npst) - 1
                        cycle
                    ELSE
                        dist = dist + SQRT((x2-x3)**2 + (y2-y3)**2)
                        exit
                    ENDIF
                ENDIF
            end do

            plqdst = min(plqdst, dist)
        end do ! direction loop

    ENDIF

  END FUNCTION plqdst



  INTEGER FUNCTION indsgm(xst, yst, n, xx, yy, mode)

    !..  Cette fonction trouve le segment d'une structure auquel est associe
    !  un point de contact (xx,yy). Les erreurs numeriques font que ce point
    !  n'est pas exactement sur le segment. 

    !  Default algorithm: mode = INDSGM_MODE_ANGLE: Pour trouver le segment, on
    !  effectue (pour chaque segment) un produit scalaire entre les 2
    !  vecteurs suivants: 1) du point jusqu'a une des extremites du segment;
    !  2) du point vers l'autre extremite. Ainsi l'angle qu'on mesurera pour le
    !  segment de contact sera pres de 180 degrees et pour chaque autre on
    !  aura un angle moindre. Le segment de contact sera celui qui
    !  presentera l'angle le plus grand.
    !
    ! NOTE: do not use this algorithm for points that are not very close to/on the
    !       structure. The further away from the structure, the higher the danger
    !       that the variations in size of the structure segments break the algorithm.

    ! Alternative algorithm: mode = INDSGM_MODE_ANGLE. Finds the line segment to which
    ! the point is closest to. 
    
    ! If mode is omitted, defaults to mode = INDSGM_MODE_ANGLE.

    !  arguments
    INTEGER n
    REAL*8 xst(n), yst(n), xx, yy
    integer, intent(in), optional :: mode


    !  variables locales
    INTEGER i
    REAL*8 mumin, mu, ax, ay, bx, by, eps, dist, distToSeg
    PARAMETER (eps=1.E-6)
    integer :: lMode = INDSGM_MODE_ANGLE

    !=========================
    !.. xst,yst: tableaux des coordonnees des points de la structure.
    !.. n  : nombre de points de la structure.
    !.. xx,yy: point de contact sur un segment de structure.
    !.. ax,ay: vecteur allant du point de contact au debut d'un segment.
    !.. bx,by: vecteur allant du point de contact au debut du segment
    !          suivant.
    !.. mu, mumin: cosinus de l'angle et de l'angle maximum
    !=========================
    
    if (present(mode)) lMode = mode

    select case (lMode)
    case (INDSGM_MODE_ANGLE)

        !..Initialisation.

        mumin = 1.

        !..Test pour savoir si le point x,y est egal au premier point parame-
        !  trise.

        ax = xst(1) - xx
        ay = yst(1) - yy

        dist = SQRT(ax*ax + ay*ay)

        IF (dist .LT. eps) THEN

            indsgm = 1
            RETURN

        ENDIF

        !..Boucle sur tous les segments

        DO i=1, n-1

            !..Def. des 2 vecteurs a et b.

            ax = xst(i) - xx
            ay = yst(i) - yy
            bx = xst(i+1) - xx
            by = yst(i+1) - yy

            !..Test pour voir si le point x,y tombe sur le point parametrise suivant

            dist = SQRT(bx*bx + by*by)
            IF (dist .LT. eps) THEN
                IF (i .NE. n-1) THEN
                    indsgm = i + 1
                    RETURN
                ELSE
                    indsgm = i
                    RETURN
                ENDIF
            ELSE

                !..On trouve le cosinus de l'angle entre les 2 par le produit scalaire.

                mu = ((ax*bx) + (ay*by)) & 
                     &                  /SQRT(((ax**2)+(ay**2)) * ((bx**2)+(by**2)))

                !..L'indice de segment correspond au cosinus le plus petit

                IF (mu .lt. mumin) THEN
                    indsgm = i
                    mumin = mu
                ENDIF
            endif
        end do


    case (INDSGM_MODE_DISTANCE) 

        dist = huge(dist)
        !..Boucle sur tous les segments
        do i=1, n-1
            distToSeg = distanceToLineSegment(xx, yy, xst(i), yst(i), xst(i+1), yst(i+1))
            if ( distToSeg < dist ) then
                dist = distToSeg
                indsgm = i
            end if
        end do

    end select


  END FUNCTION indsgm


  ! Compute minimal distance from a point to a line segment. 
  ! Computes the projection of the point on the line defined by the segment.
  ! If projection is on the segment, use this distance. If not, compute 
  ! minimum distance to either start or end point of segment  
  double precision function distanceToLineSegment( x, y, ax, ay, bx, by ) result( minDist )
    double precision, intent(in) :: x, y, ax, ay, bx, by
    
    ! internal
    double precision :: dn, r, s
    double precision :: dist
    external :: dist

    ! Obscure computation of projection of point x, y onto line defined
    ! by line segment a->b
    dn = (bx - ax)**2 + (by - ay)**2
    r = ((x - ax) * (bx - ax) + (y - ay) * (by - ay)) / dn
    s =  ((ay-y)*(bx-ax)-(ax-x)*(by-ay) ) / dn;
    minDist = abs(s) * sqrt(dn);

    ! Projection point would be
    ! px = ax + r * (bx - ax)
    ! py = ay + r * (by - ay)

    if (.not. ((r >= 0.0d0) .and. (r <= 1.0d0))) then
        minDist = min( dist(x, y, ax, ay), dist(x, y, bx, by) )
    end if

  end function distanceToLineSegment

end module carre_target
