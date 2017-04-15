      SUBROUTINE AGCHIL (IFLG,LBNM,LNNO)
c
c  version : 05.04.97 15:51
c
c======================================================================
C
      CHARACTER*(*) LBNM
c  variables en common
#include <COMQUA.F>
c======================================================================
C The routine AGCHIL is called by AGLBLS just before and just after each
C informational label line of text is drawn.  The default version does
C nothing.  A user may supply a version to change the appearance of the
C text lines.  The arguments are as follows:
C
C - IFLG is zero if a text line is about to be drawn, nonzero if one
C   has just been drawn.
C
C - LBNM is the name of the label containing the line in question.
C
C - LNNO is the number of the line.
C
C Done.
C

C  Cette sous-routine permet de modifier tous les caracteres qui sont
C  produits par AUTOGRAPH. L'appel a la routine GKS avec ICAR=-13 donne
C  un jeu de caracteres plus epais que la valeur par defaut. [Voir le
C  manuel pour la definition des jeux de caracteres].
c======================================================================

      if(qualit.eq.1) then
        ICAR = -13
      else
        icar=1
      endif
      CALL GSTXFP (ICAR,0)
      RETURN
C
      END
