module carre_parameter_io

  ! Module for parameter I/O routines that handle the carre.dat input file. 

  ! TODO: The functions for this are still scattered throughout
  ! the code (maille, lecle, change, ...). Collect them here.

  use carre_types
  use Logging
  use Helper

  implicit none

  private
  public read_code_parameters_noninteractive, change

!  dimensions
#include <CARREDIM.F>
!  variables en common
#include <COMLAN.F>
#include <COMRLX.F>

CONTAINS

  !> In noninteractive mode, the code parameters are initialized 
  !> at the entry into the ITMCARRE main subroutine using this routine.
  subroutine read_code_parameters_noninteractive(par)
    implicit none
    type(CarreParameters), intent(inout) :: par

    ! internal
    integer :: ient,isor,ifail

#ifdef CARRE_NONINTERACTIVE
    !..1.1  Read all the necessary data from the file    
    ient = 9
    isor = 0
    CALL CHANGE(par,ient,isor,ifail)
    
    !..Check whether all the data have been read from the file
    IF (ifail .EQ. 1) THEN
       ! In this routine we do not care whether some parameters are broken
       call logmsg( LOGWARNING, "Input values read from carre.dat are inconsistent")
    ENDIF
#endif

  end subroutine read_code_parameters_noninteractive




!***********************************************************************
!..  Cette sous-routine remplace la valeur d'une variable par une
!  nouvelle valeur desiree. Elle est appelee pour la lecture des parame-
!  tres dans un fichier (unite d'entree correspond a l'unite du fichier)
!  ainsi que pour la modification au clavier (unite d'entree = 5).

      SUBROUTINE CHANGE(par,ient,isor,ifail)
!
!  version : 07.07.97 18:38
!
!***********************************************************************
      use carre_types
      IMPLICIT NONE

!ank-970707: dimensions from the file

!  arguments
      INTEGER ient,isor,ifail
      type(CarreParameters), intent(inout) :: par

!  variables locales
      INTEGER ieg,ierror,ipos
      CHARACTER*80 vari, novlan

!  procedures
      INTRINSIC INDEX, MIN

!=========================
!.. ient: indice correspondant a l'unite d'entree.
!.. isor: indice correspondant a l'unite de sortie.
!.. ifail: test qui nous dit si le fichier contenant les donnees est
!          acceptable, donc si on n'a pas besoin de demander d'entrer
!          les donnees par clavier avec la routine LECCLE ou LECCLF.
!          (0= acceptable, 1= non acceptable)
!.. vari: chaine de caracteres qui est lue.
!.. ieg: position du signe "=" dans la chaine de caracteres lue.
!=========================

!..Initialisation.

      ifail = 0

      if(isor.eq.6) then
!-langue
        if(sellan(1:8).eq.'francais') then
          WRITE(isor,*)'Entrez le nom de la variable que vous voulez', & 
     &        ' changer ainsi que sa nouvelle valeur.'
          WRITE(isor,*)'Exemple: nptseg(2) = 32 (enter)'
          WRITE(isor,*)'Pour arreter, inscrivez "fin".'
        elseif(sellan(1:7).eq.'english') then
          write(isor,*)'Type the name of the variable to be changed', & 
     &    ' followed by ''='', and its numerical value.'
          write(isor,*)'For example: nptseg(2)=32 (return)'
          write(isor,*)'Type "end" to stop.'
        endif
      endif
!
!  on trouve la position du debut des parametres quand on lit
!  d'un fichier
      if(ient.ne.5) then
   5    continue
        read(ient,2,end=99)vari
        if(index(vari,'output format').gt.0) then
            ipos=index(vari,':')
            read(vari(ipos+1:min(ipos+8,len(vari))),'(a8)') par%carre_format
        end if
        if(index(vari,'$paramet').eq.0) go to 5
      endif

   10 CONTINUE

!..Lecture de la chaine de caracteres et association de la valeur a la
!  variable lue.

      READ(ient,2,END=99) vari
    2 FORMAT(A)
      ieg = len(vari)
      do while (vari(1:1).eq.' '.and.ieg.ge.1)
        vari(1:ieg-1)=vari(2:ieg)
        ieg = ieg-1
      end do

      if (index(vari(1:4),'$fin')+index(vari(1:4),'$end').gt.0 .and. & 
     &                                                ient.ne.5) return

      ieg = INDEX(vari,'=')

      IF (vari(1:6) .EQ. 'repart')THEN
!        READ(vari(ieg+1:80),*,err=98)repart
         call rdfrin(11,vari(ieg+1:80),par%repart,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'pntrat')THEN
!        READ(vari(ieg+1:80),*,err=98)pntrat
         call rdfrre(11,vari(ieg+1:80),par%pntrat,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(1)')THEN
!        READ(vari(ieg+1:80),*,err=98)tgarde(1)
         call rdfrre(11,vari(ieg+1:80),par%tgarde(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(2)')THEN
!        READ(vari(ieg+1:80),*,err=98)tgarde(2)
         call rdfrre(11,vari(ieg+1:80),par%tgarde(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(3)')THEN
!        READ(vari(ieg+1:80),*,err=98)tgarde(3)
         call rdfrre(11,vari(ieg+1:80),par%tgarde(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(4)')THEN
!        READ(vari(ieg+1:80),*,err=98)tgarde(4)
         call rdfrre(11,vari(ieg+1:80),par%tgarde(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(1)') THEN
!        READ(vari(ieg+1:80),*,err=98)nptseg(1)
         call rdfrin(11,vari(ieg+1:80),par%nptseg(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(2)') THEN
!        READ(vari(ieg+1:80),*,err=98)nptseg(2)
         call rdfrin(11,vari(ieg+1:80),par%nptseg(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(3)') THEN
!        READ(vari(ieg+1:80),*,err=98)nptseg(3)
         call rdfrin(11,vari(ieg+1:80),par%nptseg(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(4)') THEN
!        READ(vari(ieg+1:80),*,err=98)nptseg(4)
         call rdfrin(11,vari(ieg+1:80),par%nptseg(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(5)') THEN
!        READ(vari(ieg+1:80),*,err=98)nptseg(5)
         call rdfrin(11,vari(ieg+1:80),par%nptseg(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(6)') THEN
!        READ(vari(ieg+1:80),*,err=98)nptseg(6)
         call rdfrin(11,vari(ieg+1:80),par%nptseg(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(1)') THEN
!        READ(vari(ieg+1:80),*,err=98)npr(1)
         call rdfrin(11,vari(ieg+1:80),par%npr(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(2)') THEN
!        READ(vari(ieg+1:80),*,err=98)npr(2)
         call rdfrin(11,vari(ieg+1:80),par%npr(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(3)') THEN
!        READ(vari(ieg+1:80),*,err=98)npr(3)
         call rdfrin(11,vari(ieg+1:80),par%npr(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(4)') THEN
!        READ(vari(ieg+1:80),*,err=98)npr(4)
         call rdfrin(11,vari(ieg+1:80),par%npr(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(5)') THEN
!        READ(vari(ieg+1:80),*,err=98)npr(5)
         call rdfrin(11,vari(ieg+1:80),par%npr(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(6)') THEN
!        READ(vari(ieg+1:80),*,err=98)npr(6)
         call rdfrin(11,vari(ieg+1:80),par%npr(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(1)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltp1(1)
         call rdfrre(11,vari(ieg+1:80),par%deltp1(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(2)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltp1(2)
         call rdfrre(11,vari(ieg+1:80),par%deltp1(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(3)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltp1(3)
         call rdfrre(11,vari(ieg+1:80),par%deltp1(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(4)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltp1(4)
         call rdfrre(11,vari(ieg+1:80),par%deltp1(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(5)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltp1(5)
         call rdfrre(11,vari(ieg+1:80),par%deltp1(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(6)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltp1(6)
         call rdfrre(11,vari(ieg+1:80),par%deltp1(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(1)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltpn(1)
         call rdfrre(11,vari(ieg+1:80),par%deltpn(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(2)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltpn(2)
         call rdfrre(11,vari(ieg+1:80),par%deltpn(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(3)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltpn(3)
         call rdfrre(11,vari(ieg+1:80),par%deltpn(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(4)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltpn(4)
         call rdfrre(11,vari(ieg+1:80),par%deltpn(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(5)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltpn(5)
         call rdfrre(11,vari(ieg+1:80),par%deltpn(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(6)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltpn(6)
         call rdfrre(11,vari(ieg+1:80),par%deltpn(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(1)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltr1(1)
         call rdfrre(11,vari(ieg+1:80),par%deltr1(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(2)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltr1(2)
         call rdfrre(11,vari(ieg+1:80),par%deltr1(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(3)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltr1(3)
         call rdfrre(11,vari(ieg+1:80),par%deltr1(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(4)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltr1(4)
         call rdfrre(11,vari(ieg+1:80),par%deltr1(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(5)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltr1(5)
         call rdfrre(11,vari(ieg+1:80),par%deltr1(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(6)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltr1(6)
         call rdfrre(11,vari(ieg+1:80),par%deltr1(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(1)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(1)
         call rdfrre(11,vari(ieg+1:80),par%deltrn(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(2)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(2)
         call rdfrre(11,vari(ieg+1:80),par%deltrn(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(3)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(3)
         call rdfrre(11,vari(ieg+1:80),par%deltrn(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(4)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(4)
         call rdfrre(11,vari(ieg+1:80),par%deltrn(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(5)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(5)
         call rdfrre(11,vari(ieg+1:80),par%deltrn(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(6)') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrre(11,vari(ieg+1:80),par%deltrn(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'nrelax') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrin(11,vari(ieg+1:80),nrelax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:5) .EQ. 'relax') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrre(11,vari(ieg+1:80),relax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'pasmin') THEN
!        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrre(11,vari(ieg+1:80),pasmin,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'rlcept') THEN
!        READ(vari(ieg+1:80),*,err=98) deltrn(6)
         call rdfrre(11,vari(ieg+1:80),rlcept,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('carreMode')) .EQ. 'carreMode') THEN
         call rdfrin(11,vari(ieg+1:80),par%carreMode,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('equExtensionMode')) .EQ. 'equExtensionMode') THEN
         call rdfrin(11,vari(ieg+1:80),par%equExtensionMode,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('equDistanceFun')) .EQ. 'equDistanceFun') THEN
         call rdfrin(11,vari(ieg+1:80),par%equDistanceFunction,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('psimin')) .EQ. 'psimin') THEN
         call rdfrre(11,vari(ieg+1:80),par%psimin,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('psimax')) .EQ. 'psimax') THEN
         call rdfrre(11,vari(ieg+1:80),par%psimax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('rmin')) .EQ. 'rmin') THEN
         call rdfrre(11,vari(ieg+1:80),par%rmin,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('rmax')) .EQ. 'rmax') THEN
         call rdfrre(11,vari(ieg+1:80),par%rmax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('zmin')) .EQ. 'zmin') THEN
         call rdfrre(11,vari(ieg+1:80),par%zmin,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('zmax')) .EQ. 'zmax') THEN
         call rdfrre(11,vari(ieg+1:80),par%zmax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('addLeft')) .EQ. 'addLeft') THEN
         call rdfrin(11,vari(ieg+1:80),par%addLeft,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('addRight')) .EQ. 'addRight') THEN
         call rdfrin(11,vari(ieg+1:80),par%addRight,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('addBottom')) .EQ. 'addBottom') THEN
         call rdfrin(11,vari(ieg+1:80),par%addBottom,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('addTop')) .EQ. 'addTop') THEN
         call rdfrin(11,vari(ieg+1:80),par%addTop,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('equExpansionFactor')) .EQ. 'equExpansionFactor') THEN
         call rdfrre(11,vari(ieg+1:80),par%equExpansionFactor,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('gridExtensionMode')) .EQ. 'gridExtensionMode') THEN
         call rdfrin(11,vari(ieg+1:80),par%gridExtensionMode,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('targetResolution')) .EQ. 'targetResolution') THEN
         call rdfrre(11,vari(ieg+1:80),par%targetRes,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('maxResJump')) .EQ. 'maxResJump') THEN
         call rdfrre(11,vari(ieg+1:80),par%maxResJump,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('cleanupPasmin')) .EQ. 'cleanupPasmin') THEN
         call rdfrre(11,vari(ieg+1:80),par%cleanupPasmin,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('nVirtualStructs')) .EQ. 'nVirtualStructs') THEN
         call rdfrin(11,vari(ieg+1:80),par%nVirtualStructs,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('nRefineAtStructs')) .EQ. 'nRefineAtStructs') THEN
         call rdfrin(11,vari(ieg+1:80),par%nRefineAtStructs,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('refineAtStructs')) .EQ. 'refineAtStructs') THEN
         call rdfrinarray(11,vari(ieg+1:80),par%refineAtStructs(1:par%nRefineAtStructs),&
              &par%nRefineAtStructs,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('loglevel')) .EQ. 'loglevel') THEN
         call rdfrin(11,vari(ieg+1:80),par%logLevel,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:len('distxo')) .EQ. 'distxo') THEN
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'sellan') THEN
         call rdfrch(11,vari(ieg+1:80),novlan,ierror)
         if(ierror.eq.1) go to 98
         if(novlan(1:8).eq.'francais'.or.novlan(1:7).eq.'english') then
           sellan=novlan
         else
           if(sellan(1:8).eq.'francais') then
             write(6,*)'Erreur de format. Veuillez recommencer.'
           elseif(sellan(1:7).eq.'english') then
             write(6,*)'Format error. Please try again.'
           endif
         endif
         GO TO 10
      ELSE IF (vari(1:3).EQ.'fin' .or. vari(1:3).eq.'end' .or. & 
     &       index(vari(1:4),'$fin')+index(vari(1:4),'$end').gt.0) then
        return
      ELSE
         if(sellan(1:8).eq.'francais') then
           write(6,*)'Variable inexistante ou inchangeable.'
         elseif(sellan(1:7).eq.'english') then
           write(6,*)'Invalid assignment. Please try again. Input line was: "', vari, '"'
         endif
         GO TO 10
      ENDIF
98    continue
!-langue
      if(sellan(1:8).eq.'francais') then
        write(6,*)'Erreur de format. Veuillez recommencer.'
      elseif(sellan(1:7).eq.'english') then
        write(6,*)'Format error. Please try again.'
      endif
      go to 10

   99 CONTINUE
      ifail = 1

      RETURN

    END SUBROUTINE CHANGE


    subroutine rdfrinarray(iunit,vari,ii,nii,ierror)
      !  version : 03.07.2000 22:59
      implicit none
      !  read an array of integer numbers from a chain of characters vari
      !
      !  arguments
      integer iunit,ii(nii),nii,ierror
      character vari*(*)
      !  iunit: unit of temporary file
      !  vari: chain of characters from which to read
      !  ii: integer read from vari (output)
      !  ierror: error flag: 0 for no error, 1 when there is an error.
      !
      !  calculations
      !      rewind iunit
      !      write(iunit,100)vari
      !100   format(a)
      !      rewind iunit
      !      read(iunit,*,err=99)ii
      !      write (*,*) 'rdrfin: vari', vari
#ifdef READOPT
      read(vari,err=99)ii
#else
      read(vari,*,err=99)ii
#endif
      ierror=0
      return
99    ierror=1
      return
    end subroutine rdfrinarray



end module carre_parameter_io
