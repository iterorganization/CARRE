module carre_target

  ! Helper functions for handling target structures.

  implicit none

contains

  INTEGER FUNCTION drctio(xst,yst,npst,x1,y1,x2,y2,doug)

    !..Determination de la direction anti-horaire (dir=+1) ou horaire (-1)
    !  qui correspond a un deplacement vers la droite ou vers la gauche
    !  le long d'une structure, a partir d'un point x,y

    !  arguments
    INTEGER npst
    REAL*8 xst(npst),yst(npst),x1,y1,x2,y2
    CHARACTER*(*) doug

    !  variables locales
    INTEGER ind,nrot,irot
    REAL*8 sg,delta,xin,yin,xin1,yin1,zero,norm,pi,cs,sn
    parameter(delta=1.e-4,zero=0.,nrot=10,pi=3.141592654)
    logical :: found

    !  procedures
    intrinsic sqrt,sin,cos
    INTEGER indsgm
    logical in
    EXTERNAL indsgm,in & 
         &         ,trc_stk

    integer n_call
    logical l_dbg
    data n_call /0/, l_dbg /.false./

    !=========================
    !.. xst,yst: tableaux des coordonnees des points de la structure.
    !.. npst: nombre de points de la structure.
    !.. x1,y1: point de depart sur la structure.
    !.. doug: drote ou gauche
    !.. ind: indice du segment de structure.
    !=========================
    !  calculs

    n_call=n_call+1

    if (npst < 0) then
        ! structure is not closed
        stop "drctio: structure not closed"
    end if

    cs=cos((2.*pi)/nrot)
    sn=sin((2.*pi)/nrot)

    ind=indsgm(xst,yst,npst,x1,y1)

    IF (doug(1:1).EQ.'d' .OR. doug(1:1).EQ.'D') then
        sg=+1
    else
        sg=-1
    end if
    if(y1.gt.zero) sg=-sg

    IF (xst(ind).NE.x1 .or. yst(ind).ne.y1) then
        !  on trouve d'abord les coordonnees d'un point interieur
        xin=y1-yst(ind)
        yin=xst(ind)-x1
        norm=sqrt(xin*xin+yin*yin)
        xin=delta*xin/norm
        yin=delta*yin/norm

        do irot=1,nrot

            found = in(x1+xin,y1+yin,xst,yst,npst)
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
            found = in(x1+xin,y1+yin,xst,yst,npst)
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

  END FUNCTION drctio



  !***********************************************************************
  REAL*8 FUNCTION plqdst(x0,y0,x2,y2,xst,yst,npst,doug)
    !***********************************************************************
    !
    !  version : 18.06.98 20:20
    !
    IMPLICIT NONE

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
    INTEGER indsgm,drctio
    INTRINSIC MOD,SQRT
    EXTERNAL drctio,indsgm & 
         &        ,trc_stk_in,trc_stk_out

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

    !..Recherche du sens et indices de segment pour les deux points.

    !call trc_stk_in('plqdst','*start')
    !sens = drctio(xst,yst,npst,x1,y1,doug)
    !call trc_stk_out

    ind1 = indsgm(xst,yst,npst,x1,y1)
    ind2 = indsgm(xst,yst,npst,x2,y2)

    !..On verifie si les points sont sur le meme segment.

    IF (ind1 .EQ. ind2) THEN
        plqdst = SQRT((x1-x2)**2 + (y1-y2)**2)
        RETURN
    ELSE

        !..Sinon on calcule la distance de coin en coin jusqu'a ce qu'on soit
        !  sur le segment du point d'arrivee.
        do sens = -1, 1, 2

            dist= 0. 

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
                        plqdst = dist + SQRT((x2-x3)**2 + (y2-y3)**2)
                        RETURN
                    ENDIF
                ELSE
                    IF (ind .NE. MOD(ind2,npst-1) + 1) THEN
                        x1 = x3
                        y1 = y3
                        ind1 = ind1 - 1
                        IF (ind1 .EQ. 0) ind1 = npst - 1
                        cycle
                    ELSE
                        plqdst = dist + SQRT((x2-x3)**2 + (y2-y3)**2)
                        RETURN
                    ENDIF
                ENDIF
            end do

        end do ! structure loop direction

    ENDIF

  END FUNCTION plqdst


end module carre_target
