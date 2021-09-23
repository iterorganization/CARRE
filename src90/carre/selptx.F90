SUBROUTINE SELPTX(npxtot,npx,pointx,pointy,ii,jj,ptx, &
    &                  pty,iptx,jptx,xpto,ypto,racord,limcfg,par)
  !
  !  version : 07.02.99 22:05
  !
  !======================================================================
  use KindDefinitions
  use carre_types

  IMPLICIT NONE

  !..  Cette sous-routine imprime la liste des points ou le gradient
  !  s'annule et nous demande lequel(lesquels) on prendra comme point(s)
  !  X. S'il y a plus d'un point X alors ils sont ordonnes du haut vers le
  !  bas. On demande aussi l'indice du point central (point O), ainsi que,
  !  dans le cas ou il y a 2 points X, si on desire qu'ils soient
  !  raccordes.

  !ank-970702: moved dimensions into a separate file
  !  dimensions
#include <CARREDIM.F>

  !  arguments
  INTEGER npxtot,npx,ii(gradmx),jj(gradmx),iptx(npxmx), &
      &        jptx(npxmx),limcfg
  REAL(rKind) :: pointx(gradmx),pointy(gradmx),ptx(npxmx),pty(npxmx), &
      &       xpto,ypto
  LOGICAL racord
  type(CarreParameters), intent(in) :: par
  external pltend, endpag

  !  variables en common

#include <COMLAN.F>

  !  variables locales
  INTEGER i, j, k, ipx, ipo, iptxtm, jptxtm
  REAL(rKind) :: ptxtmp,ptytmp,eps_hlp
  CHARACTER rep*1
  !*** Tolerance for coinciding X-points
  parameter (eps_hlp=1.e-5)
  !ank-970701 <<<
  !*** Automatic definition of the X- and O-points from DG data
  !***
  !*** The code tries to identify the pre-selected points with those
  !*** provided by Carre. If such an identification is unambiguous,
  !*** then the user is prompted to accept these points. Otherwise, or
  !*** if the user does not accept the selection, the old scheme works.
  !*** The pre-selected points should be specified in a file "selptx.inf"
  !*** in the format suitable for "read *" in the following sequence:
  !***
  !***  xpttol
  !***  nptx
  !***  (xptxm(i),yptxm(i),i=1,nptx)
  !***  xptom,yptom
  !***
  !*** nptx =< 0 means a limiter configuration
  !***
  !***  nptxm         maximum number of the selected X-points ( =< 2 )
  !***  nptx          real number of the selected X-points
  !***  xptxm, yptxm  X-point coordinates read from the file
  !***  xptom, yptom  O-point coordinates read from the file
  !***  xpttol        tolerance parameter - related to the distance
  !***                between the points
  !***  lconn         1: connect X-points, 0: do not, -1: ask user
  !***  lun           unit number for the point specification file
  !
  integer nptxm,nptx,lun,lconn
  parameter (nptxm=2)
  integer kptx(nptxm)
  real(rKind) :: xptxm(nptxm),yptxm(nptxm),xptom,yptom, &
      &                         xpttol,xptdist(gradmx),x1,x2
  !      parameter (xpttol=5.e-2)
  logical ex,op
  logical :: havePredefinedXPoints
  !ank>>>
  !======================================================================
  !.. npxtot: nombre de points ou le gradient est nul.
  !.. pointx,pointy: tableaux des coordonnees en x et y des points ou le
  !                  gradient est nul.
  !.. ii,jj: tableaux des indices des carres ou le gradient est nul.
  !.. npx: nombre de points X.
  !.. ptx,pty: tableaux des coordonnees en x et y des points X.
  !.. iptx,jptx: tableaux des indices des carres ou il y a un point X.
  !.. xpto,ypto: coordonnees du point O.
  !.. racord: determine si oui ou non les points X sont connectes.
  !.. ipx,ipo: indice du point X et du point O.
  !.. iptxtm,jptxtm: tableaux temporaires des indices des carres ou il y
  !                  a un point X.
  !.. ptxtmp,ptytmp: tableaux temporaires des coordonnees en x et y des
  !                  points X.
  !======================================================================
  !..Initialisation

2 npx = 0
  do i=1,nptxm
      kptx(i)=0
  enddo
  lconn=-1

  havePredefinedXPoints = .false.

  ! If the code parameter structure contains X-points, we use them.
  ! Otherwise, we look for the selptx.inf file, or ask the user.
  if (par % xPointNum > 0) then
      nptx = par%xPointNum
      xptxm(1:nptx) = par%xPointX(1:nptx)
      yptxm(1:nptx) = par%xPointY(1:nptx)
      xptom = par%oPointX
      yptom = par%oPointY
      havePredefinedXPoints = .true.
      xpttol = 0.1
  else
      !ank-970701 <<<
      !*** Check whether the point specification file is present
      inquire(file='selptx.inf',exist=ex)
      if(ex) then
          do lun=7,99
              inquire(lun,opened=op)
              if(.not.op) go to 10
          end do
          write(*,*) '*** cannot open selptx.inf: no free unit number'
          lun=0
          ex=.false.
          go to 20
10        continue
          open(lun,file='selptx.inf',err=12)
          rewind(lun)
          read(lun,*,err=14) xpttol ! X-point identification tolerance
          read(lun,*,err=14) nptx   ! number of X-points specified in file
          ex=nptx.le.nptxm
          if(ex) then               ! if at most nptxm X-points given in file continue, else error
              nptx=max0(nptx,0)
              do i=1,nptx
                  read(lun,*,err=14) xptxm(i),yptxm(i)
              end do
              read(lun,*,err=14) xptom,yptom
              havePredefinedXPoints = .true.
              !*** lconn>0 means "connect X-points")
              read(lun,*,end=11, err=111) lconn
              go to 11
111           lconn=-1
              goto 11
          else
              write(*,*) 'Too many X-points specified in selptx.inf: ',nptx
              write(*,*) 'Check the specification or the nptxm parameter ', &
                  &                                                    'in selptx.F'
              ex=.false.
          end if
12        write(*,*) 'selptx: error opening the file selptx.inf'
          ex=.false.
          go to 20
14        write(*,*) 'selptx: error reading the data from selptx.inf'
          ex=.false.
11        continue ! Finished reading selptx.inf file
          close(lun)
      end if ! selptx.inf file exists
  end if


  if (havePredefinedXPoints) then

      !*** Set the absolute tolerance for each candidate point

      do j=1,npxtot
          x1=1.e30
          do i=1,npxtot
              if(i.ne.j) then
                  x2=rdist(pointx(i),pointy(i),pointx(j),pointy(j))
                  if(x2.gt.eps_hlp) x1=min(x1,x2)
              end if
          end do
          xptdist(j)=x1*xpttol
      end do
      !
      !*** Try to identify the pre-specified points
      !
      k=0
      do i=1,npxtot
          if(abs(xptom-pointx(i)).le.xptdist(i) .and. &
              &                         abs(yptom-pointy(i)).le.xptdist(i)) then
              if(k.eq.0) then
                  k=i
              else if(rdist(pointx(i),pointy(i),pointx(k),pointy(k)) &
                  &                                         .gt.eps_hlp) then
                  write(*,*) 'Ambiguous definition of the O-point: ', &
                      &                         'points ',k,'  and ',i,'  are too close'
                  havePredefinedXPoints=.false.
                  go to 20
              end if
          end if
      end do
      if(k.eq.0) then
          write(*,*) 'No O-point identified - ', &
              &                                       'check the specification!'
          havePredefinedXPoints=.false.
          go to 20
      end if
      ipo=k
      do j=1,nptx
          k=0
          do i=1,npxtot
              if(abs(xptxm(j)-pointx(i)).le.xptdist(i) .and. &
                  &                      abs(yptxm(j)-pointy(i)).le.xptdist(i)) then
                  if(k.eq.0) then
                      k=i
                  else if(rdist(pointx(i),pointy(i),pointx(k),pointy(k)) &
                      &                                         .gt.eps_hlp) then
                      write(*,*) 'Ambiguous definition of the X-point', &
                          &                     j,': points ',k,'  and ',i,'  are too close'
                      havePredefinedXPoints=.false.
                      go to 20
                  end if
              end if
          end do
          if(k.eq.0) then
              write(*,*) 'No X-point ',j,' identified - ', &
                  &                                       'check the specification!'
              havePredefinedXPoints=.false.
              go to 20
          end if
          if(k.eq.ipo) then
              write(*,*) 'Ambiguous definition of the X-point', &
                  &                              j,': the same as the plasma center'
              havePredefinedXPoints=.false.
              go to 20
          end if
          do i=1,j-1
              if(k.eq.kptx(j)) then
                  write(*,*) 'Ambiguous definition of the X-point', &
                      &                                  j,': the same as the X-point',i
                  havePredefinedXPoints=.false.
                  go to 20
              end if
          end do
          kptx(j)=k
      end do
      !
      !*** Here the O-point and nptx X-points are identified.
      !*** Ask for confirmation.
      !
      write(*,*) 'Pre-selected points are identified.'
      write(*,'(1x,a9,1p,2e14.4)') &
          &                            'O-point: ',pointx(ipo),pointy(ipo)
      if(nptx.gt.0) then
          write(*,'(1x,a9,1p,2e14.4/(10x,2e14.4))') &
              &           'X-point: ',(pointx(kptx(i)),pointy(kptx(i)),i=1,nptx)
          if(nptx.gt.1) then
              if(lconn.gt.0) then
                  write(*,*) 'Connected double-null'
              else if(lconn.eq.0) then
                  write(*,*) 'Disconnected double-null'
              end if
          end if
      else
          write(*,*) 'No X-point - limiter configuration'
      end if

#ifndef CARRE_NONINTERACTIVE
      write(*,*)
      write(*,*) 'Do you accept the selection (y/n)?'
      read(*,'(a)') rep
      havePredefinedXPoints=rep.eq.'y'.or.rep.eq.'Y'
      if(.not.havePredefinedXPoints) go to 20
#endif

      !
      !*** The configuration is accepted. Set the Carre variables
      !
      if(nptx.le.0) then
          npx=1
          limcfg=1
      else
          limcfg=0
          npx=nptx
          do i=1,npx
              ptx(i)=pointx(kptx(i))
              pty(i)=pointy(kptx(i))
              iptx(i)=ii(kptx(i))
              jptx(i)=jj(kptx(i))
          end do
      end if
      xpto=pointx(ipo)
      ypto=pointy(ipo)
      ptx(npx+1)=xpto
      pty(npx+1)=ypto
      iptx(npx+1)=ii(ipo)
      jptx(npx+1)=jj(ipo)
  end if

20 continue
  if(.not.havePredefinedXPoints) then
      !ank>>>
      !..Affichage des points ou le gradient s'annule.

      !-langue
      if(sellan(1:8).eq.'francais') then
          WRITE(6,25) (i,pointx(i),pointy(i), i=1, npxtot)
25        FORMAT ('     Voici la liste des points de gradient zero:'/ &
              &        '     -------------------------------------------'/ &
              &  '  #',T10,' R=',T25,' Z='/ &
              &         (I3, T10, 1pe12.5, T25, 1pe12.5))
          write(6,*)'  0   pour terminer la selection'
          write(6,*)' -1   pour une configuration limiteur'
          write(6,*)' Pour une configuration deflecteur, selectionnez', &
              &    ' le ou les points X.'

          !       write(6,*)' Choisissez en donnant l''indice approprie.'
      elseif(sellan(1:7).eq.'english') then
          WRITE(6,225) (i,pointx(i),pointy(i), i=1, npxtot)
225       FORMAT ('       List of points where grad-psi vanishes:'/ &
              &        '     -------------------------------------------'/ &
              &  '  #',T10,' R=',T25,' Z='/ &
              &         (I3, T10, 1pe12.5, T25, 1pe12.5))
          write(6,*)'  0   to stop selection'
          write(6,*)' -1   for a limiter configuration'
          write(6,*)' For a divertor configuration, select X-point(s)'
          !       write(6,*)' Choose option by typing the appropriate index.'
      else
          write(6,*) 'Warning: no language specified'
      end if

      !..Indexation des points X.

30    CONTINUE

      READ (5,*, end=31, err=666) ipx
      GO TO 31
666   if(sellan(1:8).eq.'francais') then
          PRINT *, "Erreur de lecture. Veuillez recommencer."
      elseif(sellan(1:7).eq.'english') then
          PRINT *, "Invalid input. Please try again."
      end if
      go to 2
31    continue
      if (ipx.lt.-1 .or. ipx.gt.npxtot) goto 666

      IF (ipx.GT.0) THEN
          npx = npx + 1
          ptx(npx) = pointx(ipx)
          pty(npx) = pointy(ipx)
          iptx(npx) = ii(ipx)
          jptx(npx) = jj(ipx)
          if(npx.lt.2) GO TO 30
      end if

      !
      if(npx.eq.0 .and. ipx.eq.-1) then
          npx=1
          limcfg=1
          !  N.B.: dans ce cas, le point X est defini comme etant le premier point
          !      de contact avec un limiteur. Ce point est calcule dans limfnd.
      else
          limcfg=0
      end if

      !..On identifie le point O.

      IF (npx.GT.0 .or. limcfg.ne.0) THEN

          if(sellan(1:8).eq.'francais') then
              write(6,*)' Indiquez l''indice du point O.'
          elseif(sellan(1:7).eq.'english') then
              write(6,*)' Select index for O-point.'
          end if
          READ(5,*) ipo

          xpto = pointx(ipo)
          ypto = pointy(ipo)
          !  on met les coordonnees du point O a la fin des vecteurs ptx et pty.
          !  ceci servira a distinguer le point X interieur d'un double nul decon-
          !  necte a la fin de la routine sptris
          ptx(npx+1)=xpto
          pty(npx+1)=ypto

      end if

      !ank-970701
  end if

  !..On choisit de raccorder ou ne pas raccorder les points X.

  if (npx .gt. 1) then
      if(lconn.ge.0) then
          racord = lconn.gt.0
      else
          if(sellan(1:8).eq.'francais') then
              write(6,*) &
                  &       ' Voulez-vous que les points X soient raccordes? (o/n)'
          else if(sellan(1:7).eq.'english') then
              write(6,*) 'Do you wish to force connection ', &
                  &                              'of the two X-points (y/n)?'
          end if
          read(5,35) rep
35        format (a)

          if (rep .EQ. 'n' .OR. rep .EQ. 'N') then
              racord = .false.
          else
              racord = .true.
          end if
      end if

      !..On ordonne les points X du haut vers le bas.

      IF (pty(1) .LT. pty(2)) THEN

          ptxtmp = ptx(1)
          ptx(1) = ptx(2)
          ptx(2) = ptxtmp

          ptytmp = pty(1)
          pty(1) = pty(2)
          pty(2) = ptytmp

          iptxtm = iptx(1)
          iptx(1) = iptx(2)
          iptx(2) = iptxtm

          jptxtm = jptx(1)
          jptx(1) = jptx(2)
          jptx(2) = jptxtm

      end if

  ELSE
      racord = .FALSE.
  end if

  !nba  The 'endpag' call below closes the graphics window opened by 'cntour'.
  !nba  L'appel a 'endpag' ci-dessous ferme la fenetre graphique ouverte par
  !nba  'cntour'.
  call endpag

  RETURN

  CONTAINS

  FUNCTION RDIST(X1,Y1,X2,Y2)
  implicit none
  real(rKind) :: Rdist
  real(rKind) :: X1, Y1, X2, Y2

  RDIST = sqrt((x1-x2)**2+(y1-y2)**2)

  RETURN
  END FUNCTION RDIST

END SUBROUTINE SELPTX
