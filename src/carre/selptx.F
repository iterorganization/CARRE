      SUBROUTINE SELPTX(npxtot,npx,pointx,pointy,ii,jj,ptx,
     .                  pty,iptx,jptx,xpto,ypto,racord,limcfg)
c
c  version : 07.02.99 22:05
c
c======================================================================
      IMPLICIT NONE

*..  Cette sous-routine imprime la liste des points ou le gradient
*  s'annule et nous demande lequel(lesquels) on prendra comme point(s)
*  X. S'il y a plus d'un point X alors ils sont ordonnes du haut vers le
*  bas. On demande aussi l'indice du point central (point O), ainsi que,
*  dans le cas ou il y a 2 points X, si on desire qu'ils soient
*  raccordes.

cank-970702: moved dimensions into a separate file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

*  arguments
      INTEGER npxtot,npx,ii(gradmx),jj(gradmx),iptx(npxmx),
     .        jptx(npxmx),limcfg
      REAL(kind=R8) pointx(gradmx),pointy(gradmx),ptx(npxmx),pty(npxmx),
     .       xpto,ypto
      LOGICAL racord

*  variables en common
#include <COMLAN.F>

*  variables locales
      INTEGER i, j, k, ipx, ipo, iptxtm, jptxtm
      REAL(KIND=R8) ptxtmp,ptytmp,eps_hlp
      CHARACTER rep*1
c*** Tolerance for coinciding X-points
      parameter (eps_hlp=1.0e-5_R8)
cank-970701 <<<
c*** Automatic definition of the X- and O-points from DG data
c***
c*** The code tries to identify the pre-selected points with those
c*** provided by Carre. If such an identification is unambiguous,
c*** then the user is prompted to accept these points. Otherwise, or
c*** if the user does not accept the selection, the old scheme works.
c*** The pre-selected points should be specified in a file "selptx.inf"
c*** in the format suitable for "read *" in the following sequence:
c***
c***  xpttol
c***  nptx
c***  (xptxm(i),yptxm(i),i=1,nptx)
c***  xptom,yptom
c***
c*** nptx =< 0 means a limiter configuration
c***
c***  nptxm         maximum number of the selected X-points ( =< 2 )
c***  nptx          real number of the selected X-points
c***  xptxm, yptxm  X-point coordinates read from the file
c***  xptom, yptom  O-point coordinates read from the file
c***  xpttol        tolerance parameter - related to the distance
c***                between the points
c***  lconn         1: connect X-points, 0: do not, -1: ask user
c***  lun           unit number for the point specification file
c
      integer nptxm,nptx,lun,lconn
      parameter (nptxm=2)
      integer kptx(nptxm)
      real(kind=R8) xptxm(nptxm),yptxm(nptxm),xptom,yptom,
     ,                         xpttol,xptdist(gradmx),rdist,x1,x2,y1,y2
c      parameter (xpttol=5.0e-2_R8)
      logical ex,op
cank>>>
c======================================================================
c.. npxtot: nombre de points ou le gradient est nul.
c.. pointx,pointy: tableaux des coordonnees en x et y des points ou le
c                  gradient est nul.
c.. ii,jj: tableaux des indices des carres ou le gradient est nul.
c.. npx: nombre de points X.
c.. ptx,pty: tableaux des coordonnees en x et y des points X.
c.. iptx,jptx: tableaux des indices des carres ou il y a un point X.
c.. xpto,ypto: coordonnees du point O.
c.. racord: determine si oui ou non les points X sont connectes.
c.. ipx,ipo: indice du point X et du point O.
c.. iptxtm,jptxtm: tableaux temporaires des indices des carres ou il y
c                  a un point X.
c.. ptxtmp,ptytmp: tableaux temporaires des coordonnees en x et y des
c                  points X.
c======================================================================
      rdist(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
c======================================================================
*..Initialisation

  2   npx = 0
      do i=1,nptxm
        kptx(i)=0
      enddo
      lconn=-1

cank-970701 <<<
c*** Check whether the point specification file is present
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
 10     continue
        open(lun,file='selptx.inf',err=12)
        rewind(lun)
        read(lun,*,err=14) xpttol
        read(lun,*,err=14) nptx
        ex=nptx.le.nptxm
        if(ex) then
          nptx=max0(nptx,0)
          do i=1,nptx
            read(lun,*,err=14) xptxm(i),yptxm(i)
          end do
          read(lun,*,err=14) xptom,yptom
c*** lconn>0 means "connect x-points")
          read(lun,*,end=11, err=111) lconn
          go to 11
 111      lconn=-1

c*** Set the absolute tolerance for each candidate point

 11         do j=1,npxtot
            x1=1.e30
            do i=1,npxtot
              if(i.ne.j) then
                x2=rdist(pointx(i),pointy(i),pointx(j),pointy(j))
                if(x2.gt.eps_hlp) x1=min(x1,x2)
              end if
            end do
            xptdist(j)=x1*xpttol
          end do
c
c*** Try to identify the pre-specified points
c
          k=0
          do i=1,npxtot
            if(abs(xptom-pointx(i)).le.xptdist(i) .and.
     .                         abs(yptom-pointy(i)).le.xptdist(i)) then
              if(k.eq.0) then
                k=i
              else if(rdist(pointx(i),pointy(i),pointx(k),pointy(k))
     .                       .gt.eps_hlp) then
                write(*,*) 'Ambiguous definition of the O-point: ',
     ,                         'points ',k,'  and ',i,'  are too close'
                ex=.false.
                go to 20
              end if
            end if
          end do
          if(k.eq.0) then
            write(*,*) 'No O-point identified - ',
     ,                                       'check the specification!'
            ex=.false.
            go to 20
          end if
          ipo=k
          do j=1,nptx
            k=0
            do i=1,npxtot
              if(abs(xptxm(j)-pointx(i)).le.xptdist(i) .and.
     ,                      abs(yptxm(j)-pointy(i)).le.xptdist(i)) then
                if(k.eq.0) then
                  k=i
              else if(rdist(pointx(i),pointy(i),pointx(k),pointy(k))
     .                       .gt.eps_hlp) then
                  write(*,*) 'Ambiguous definition of the X-point',
     ,                     j,': points ',k,'  and ',i,'  are too close'
                  ex=.false.
                  go to 20
                end if
              end if
            end do
            if(k.eq.0) then
              write(*,*) 'No X-point ',j,' identified - ',
     ,                                       'check the specification!'
              ex=.false.
              go to 20
            end if
            if(k.eq.ipo) then
              write(*,*) 'Ambiguous definition of the X-point',
     ,                              j,': the same as the plasma center'
              ex=.false.
              go to 20
            end if
            do i=1,j-1
              if(k.eq.kptx(j)) then
                write(*,*) 'Ambiguous definition of the X-point',
     ,                                  j,': the same as the X-point',i
                ex=.false.
                go to 20
              end if
            end do
            kptx(j)=k
          end do
c
c*** Here the o-point and nptx x-points are identified.
c*** Ask for confirmation.
c
          write(*,*) 'Pre-selected points are identified.'
          write(*,'(1x,a9,1p,2e14.4)')
     -                            'O-point: ',pointx(ipo),pointy(ipo)
          if(nptx.gt.0) then
            write(*,'(1x,a9,1p,2e14.4/(10x,2e14.4))')
     -           'X-point: ',(pointx(kptx(i)),pointy(kptx(i)),i=1,nptx)
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

          write(*,*)
          write(*,*) 'Do you accept the selection (y/n)?'
          read(*,'(a)') rep
          ex=rep.eq.'y'.or.rep.eq.'Y'
          if(.not.ex) go to 20
c
c*** The configuration is accepted. Set the Carre variables
c
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
        else
          write(*,*) 'Too many X-points specified in selptx.inf: ',nptx
          write(*,*) 'Check the specification or the nptxm parameter ',
     ,                                                    'in selptx.F'
          ex=.false.
        end if
        close(lun)
      end if
      go to 20
 12   write(*,*) 'selptx: error opening the file selptx.inf'
      ex=.false.
      go to 20
 14   write(*,*) 'selptx: error reading the data from selptx.inf'
      ex=.false.
 20   continue
      if(.not.ex) then
cank>>>
*..Affichage des points ou le gradient s'annule.

c-langue
        if(sellan(1:8).eq.'francais') then
          WRITE(6,25) (i,pointx(i),pointy(i), i=1, npxtot)
   25     FORMAT ('     Voici la liste des points de gradient zero:'/
     .        '     -------------------------------------------'/
     .  '  #',T10,' R=',T25,' Z='/
     .         (I3, T10, 1pe12.5, T25, 1pe12.5))
          write(6,*)'  0   pour terminer la selection'
          write(6,*)' -1   pour une configuration limiteur'
          write(6,*)' Pour une configuration deflecteur, selectionnez',
     .    ' le ou les points X.'

c       write(6,*)' Choisissez en donnant l''indice approprie.'
        elseif(sellan(1:7).eq.'english') then
          WRITE(6,225) (i,pointx(i),pointy(i), i=1, npxtot)
  225     FORMAT ('       List of points where grad-psi vanishes:'/
     .        '     -------------------------------------------'/
     .  '  #',T10,' R=',T25,' Z='/
     .         (I3, T10, 1pe12.5, T25, 1pe12.5))
          write(6,*)'  0   to stop selection'
          write(6,*)' -1   for a limiter configuration'
          write(6,*)' For a divertor configuration, select X-point(s)'
c       write(6,*)' Choose option by typing the appropriate index.'
        end if

*..Indexation des points X.

   30   CONTINUE

        READ (5,*, end=31, err=666) ipx
        GO TO 31
  666   if(sellan(1:8).eq.'francais') then
          PRINT *, "Erreur de lecture. Veuillez recommencer."
        elseif(sellan(1:7).eq.'english') then
          PRINT *, "Invalid input. Please try again."
        end if
        go to 2
   31   continue
        if (ipx.lt.-1 .or. ipx.gt.npxtot) goto 666

        IF (ipx.GT.0) THEN
          npx = npx + 1
          ptx(npx) = pointx(ipx)
          pty(npx) = pointy(ipx)
          iptx(npx) = ii(ipx)
          jptx(npx) = jj(ipx)
          if(npx.lt.2) GO TO 30
        end if

c
        if(npx.eq.0 .and. ipx.eq.-1) then
          npx=1
          limcfg=1
c  N.B.: dans ce cas, le point X est defini comme etant le premier point
c      de contact avec un limiteur. Ce point est calcule dans limfnd.
        else
          limcfg=0
        end if

*..On identifie le point O.

        IF (npx.GT.0 .or. limcfg.ne.0) THEN

           if(sellan(1:8).eq.'francais') then
             write(6,*)' Indiquez l''indice du point O.'
           elseif(sellan(1:7).eq.'english') then
             write(6,*)' Select index for O-point.'
           end if
           READ(5,*) ipo

           xpto = pointx(ipo)
           ypto = pointy(ipo)
c  on met les coordonnees du point O a la fin des vecteurs ptx et pty.
c  ceci servira a distinguer le point X interieur d'un double nul decon-
c  necte a la fin de la routine sptris
           ptx(npx+1)=xpto
           pty(npx+1)=ypto

        end if

cank-970701
      end if

*..On choisit de raccorder ou ne pas raccorder les points X.

      if (npx .gt. 1) then
        if(lconn.ge.0) then
          racord = lconn.gt.0
        else
          if(sellan(1:8).eq.'francais') then
            write(6,*)
     .       ' Voulez-vous que les points X soient raccordes? (o/n)'
          else if(sellan(1:7).eq.'english') then
            write(6,*) 'Do you wish to force connection ',
     .                 'of the two X-points (y/n)?'
          end if
          read(5,35) rep
   35     format (a)

          if (rep .EQ. 'n' .OR. rep .EQ. 'N') then
            racord = .false.
          else
            racord = .true.
          end if
        end if

*..On ordonne les points X du haut vers le bas.

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

cnba  The 'endpag' call below closes the graphics window opened by 'cntour'.
cnba  L'appel a 'endpag' ci-dessous ferme la fenetre graphique ouverte par
cnba  'cntour'.
      call endpag

      RETURN
      END
