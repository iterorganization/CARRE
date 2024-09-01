      SUBROUTINE AGCHIL (IFLG,LBNM,LNNO)
!
!  version : 05.04.97 15:51
!
!======================================================================
!
      use comqua
      IMPLICIT NONE
      INTEGER IFLG, LNNO
      CHARACTER*(*) LBNM
      EXTERNAL GSTXFP
      INTEGER ICAR
!======================================================================
! The routine AGCHIL is called by AGLBLS just before and just after each
! informational-label line of text is drawn.  The default version does
! nothing.  A user may supply a version to change the appearance of the
! text lines.  The arguments are as follows:
!
! - IFLG is zero if a text line is about to be drawn, non-zero if one
!   has just been drawn.
!
! - LBNM is the name of the label containing the line in question.
!
! - LNNO is the number of the line.
!
! Done.
!

!  Cette sous-routine permet de modifier tous les caracteres qui sont
!  produits par AUTOGRAPH. L'appel a la routine GKS avec ICAR=-13 donne
!  un jeu de caracteres plus epais que la valeur par defaut. [Voir le
!  manuel pour la definition des jeux de caracteres].
!======================================================================

      if(qualit.eq.1) then
        ICAR = -13
      else
        icar=1
      endif
      CALL GSTXFP (ICAR,0)
      RETURN
!
      END
