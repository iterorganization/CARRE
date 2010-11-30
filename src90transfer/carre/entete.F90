!***********************************************************************
      subroutine entete(nin,chaine,iflag)
!***********************************************************************
!
!  version : 04.04.97 21:21
!
      implicit none
!  cette routine lit jusqu'a la ligne qui contient la chaine de
!  caracteres specifiee dans chaine. si iflag =1 a l'entree, le fichier
!  est rembonine avant la lecture.
!
!  nin: unite de lecture
!  chaine: chaine de caracteres qu'il faut trouver
!  iflag:
!    entree: 1 pour commencer la lecture a partir du debut du fichier
!            2 comme 1, mais le programme arrete en cas d'echec
!            0 pour commencer la lecture a partir de la position
!              actuelle
!           -1 comme 0 mais le programme arrete en cas d'echec
!    sortie: 0 succes
!            1 echec
!
!  arguments
      integer nin,iflag
      character chaine*(*)
!
!  variables locales
      character ligne*200
!
!  procedures
      intrinsic index
!
!  calculs
      if(nin.eq.5) print *,'starting entete 5'
      if(iflag.ge.1) rewind nin
1     continue
      read(nin,100,end=99)ligne
100   format(a)
      if(index(ligne,chaine).eq.0) go to 1
!
!  chaine trouvee
      iflag=0
      return
!
!  chaine non trouvee
99    continue
      if(iflag.eq.-1 .or. iflag.eq.2) then
        write(6,*)'Incapable de trouver l''entete suivante:'
        write(6,*)chaine
        write(6,*)'Arret du programme.'
        stop
      endif
      iflag=1
      return
      end
