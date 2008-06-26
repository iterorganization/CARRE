************************************************************************
      subroutine entete(nin,chaine,iflag)
************************************************************************
c
c  version : 04.04.97 21:21
c
      implicit none
c  cette routine lit jusqu'a la ligne qui contient la chaine de
c  caracteres specifiee dans chaine. si iflag =1 a l'entree, le fichier
c  est rembonine avant la lecture.
c
c  nin: unite de lecture
c  chaine: chaine de caracteres qu'il faut trouver
c  iflag:
c    entree: 1 pour commencer la lecture a partir du debut du fichier
c            2 comme 1, mais le programme arrete en cas d'echec
c            0 pour commencer la lecture a partir de la position
c              actuelle
c           -1 comme 0 mais le programme arrete en cas d'echec
c    sortie: 0 succes
c            1 echec
c
c  arguments
      integer nin,iflag
      character chaine*(*)
c
c  variables locales
      character ligne*200
c
c  procedures
      intrinsic index
c
c  calculs
      if(nin.eq.5) print *,'starting entete 5'
      if(iflag.ge.1) rewind nin
1     continue
      read(nin,100,end=99)ligne
100   format(a)
      if(index(ligne,chaine).eq.0) go to 1
c
c  chaine trouvee
      iflag=0
      return
c
c  chaine non trouvee
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
