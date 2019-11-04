************************************************************************
      SUBROUTINE CHANGE(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .                  pntrat,tgarde,distxo,psi_X,psi_O,
     .                  ient,isor,ifail)
c
c  version : 07.07.97 18:38
c
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette sous-routine remplace la valeur d'une variable par une
*  nouvelle valeur desiree. Elle est appelee pour la lecture des parame-
*  tres dans un fichier (unite d'entree correspond a l'unite du fichier)
*  ainsi que pour la modification au clavier (unite d'entree = 5).

cank-970707: dimensions from the file
*  dimensions
#include <CARREDIM.F>

*  arguments
      INTEGER nptseg(10),repart,npr(nregmx),ient,isor,ifail
      REAL(kind=R8) deltp1(10),deltpn(10),deltr1(10),deltrn(10),pntrat
     .       ,tgarde(4),distxo,psi_X(2),psi_O

*  variables locales
      INTEGER ieg,ierror,ipx,npx
      REAL(KIND=R8) x, y
      CHARACTER*80 vari, novlan

*  variables en common
c
#include <COMLAN.F>
#include <COMRLX.F>
c     save relax,pasmin,rlcept,nrelax

*  procedures
      INTEGER lnblnk
      EXTERNAL lnblnk
      INTRINSIC INDEX, MIN

c=========================
c.. ient: indice correspondant a l'unite d'entree.
c.. isor: indice correspondant a l'unite de sortie.
c.. ifail: test qui nous dit si le fichier contenant les donnees est
c          acceptable, donc si on n'a pas besoin de demander d'entrer
c          les donnees par clavier avec la routine LECCLE ou LECCLF.
c          (0= acceptable, 1= non acceptable)
c.. vari: chaine de caracteres qui est lue.
c.. ieg: position du signe "=" dans la chaine de caracteres lue.
c=========================

*..Initialisation.

      ifail = 0

      if(isor.eq.6) then
c-langue
        if(sellan(1:8).eq.'francais') then
          WRITE(isor,*)'Entrez le nom de la variable que vous voulez',
     .        ' changer ainsi que sa nouvelle valeur.'
          WRITE(isor,*)'Exemple: nptseg(2) = 32 (enter)'
          WRITE(isor,*)'Pour arreter, inscrivez "fin".'
        elseif(sellan(1:7).eq.'english') then
          write(isor,*)'Type the name of the variable to be changed',
     .    ' followed by ''='', and its numerical value.'
          write(isor,*)'For example: nptseg(2)=32 (return)'
          write(isor,*)'Type "end" to stop.'
        endif
      endif
c
c  on trouve la position au debut des parametres quand on lit
c  d'un fichier
      if(ient.ne.5) then
   5    continue
        read(ient,2,end=99)vari
        if(index(vari,'Number').ne.0.or.index(vari,'Nombre').ne.0) then
          ieg = index(vari,':')
          read(vari(ieg+1:lnblnk(vari)),*) npx
          read(ient,1028,end=99)
          do ipx = 1, npx
            read(ient,*,end=99) x, y, psi_X(ipx)
          end do
          read(ient,1028,end=99)
          read(ient,1028,end=99)
          read(ient,*,end=99) x, y, psi_O
        end if
        if(index(vari,'$paramet').eq.0) go to 5
      endif

   10 CONTINUE

*..Lecture de la chaine de caracteres et association de la valeur a la
*  variable lue.

      READ(ient,2,END=99) vari
    2 FORMAT(A)
      ieg = len(vari)
      do while (vari(1:1).eq.' '.and.ieg.ge.1)
        vari(1:ieg-1)=vari(2:ieg)
        ieg = ieg-1
      end do

      if (index(vari(1:4),'$fin')+index(vari(1:4),'$end').gt.0 .and.
     .                                                ient.ne.5) return

      ieg = INDEX(vari,'=')

      IF (vari(1:6) .EQ. 'repart') THEN
c        READ(vari(ieg+1:80),*,err=98)repart
         call rdfrin(11,vari(ieg+1:80),repart,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'distxo') THEN
c        READ(vari(ieg+1:80),*,err=98)distxo
         call rdfrre(11,vari(ieg+1:80),distxo,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'pntrat') THEN
c        READ(vari(ieg+1:80),*,err=98)pntrat
         call rdfrre(11,vari(ieg+1:80),pntrat,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)tgarde(1)
         call rdfrre(11,vari(ieg+1:80),tgarde(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)tgarde(2)
         call rdfrre(11,vari(ieg+1:80),tgarde(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)tgarde(3)
         call rdfrre(11,vari(ieg+1:80),tgarde(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'tgarde(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)tgarde(4)
         call rdfrre(11,vari(ieg+1:80),tgarde(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)nptseg(1)
         call rdfrin(11,vari(ieg+1:80),nptseg(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)nptseg(2)
         call rdfrin(11,vari(ieg+1:80),nptseg(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)nptseg(3)
         call rdfrin(11,vari(ieg+1:80),nptseg(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)nptseg(4)
         call rdfrin(11,vari(ieg+1:80),nptseg(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(5)') THEN
c        READ(vari(ieg+1:80),*,err=98)nptseg(5)
         call rdfrin(11,vari(ieg+1:80),nptseg(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'nptseg(6)') THEN
c        READ(vari(ieg+1:80),*,err=98)nptseg(6)
         call rdfrin(11,vari(ieg+1:80),nptseg(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)npr(1)
         call rdfrin(11,vari(ieg+1:80),npr(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)npr(2)
         call rdfrin(11,vari(ieg+1:80),npr(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)npr(3)
         call rdfrin(11,vari(ieg+1:80),npr(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)npr(4)
         call rdfrin(11,vari(ieg+1:80),npr(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(5)') THEN
c        READ(vari(ieg+1:80),*,err=98)npr(5)
         call rdfrin(11,vari(ieg+1:80),npr(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'npr(6)') THEN
c        READ(vari(ieg+1:80),*,err=98)npr(6)
         call rdfrin(11,vari(ieg+1:80),npr(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltp1(1)
         call rdfrre(11,vari(ieg+1:80),deltp1(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltp1(2)
         call rdfrre(11,vari(ieg+1:80),deltp1(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltp1(3)
         call rdfrre(11,vari(ieg+1:80),deltp1(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltp1(4)
         call rdfrre(11,vari(ieg+1:80),deltp1(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(5)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltp1(5)
         call rdfrre(11,vari(ieg+1:80),deltp1(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltp1(6)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltp1(6)
         call rdfrre(11,vari(ieg+1:80),deltp1(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltpn(1)
         call rdfrre(11,vari(ieg+1:80),deltpn(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltpn(2)
         call rdfrre(11,vari(ieg+1:80),deltpn(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltpn(3)
         call rdfrre(11,vari(ieg+1:80),deltpn(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltpn(4)
         call rdfrre(11,vari(ieg+1:80),deltpn(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(5)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltpn(5)
         call rdfrre(11,vari(ieg+1:80),deltpn(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltpn(6)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltpn(6)
         call rdfrre(11,vari(ieg+1:80),deltpn(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltr1(1)
         call rdfrre(11,vari(ieg+1:80),deltr1(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltr1(2)
         call rdfrre(11,vari(ieg+1:80),deltr1(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltr1(3)
         call rdfrre(11,vari(ieg+1:80),deltr1(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltr1(4)
         call rdfrre(11,vari(ieg+1:80),deltr1(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(5)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltr1(5)
         call rdfrre(11,vari(ieg+1:80),deltr1(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltr1(6)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltr1(6)
         call rdfrre(11,vari(ieg+1:80),deltr1(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(1)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(1)
         call rdfrre(11,vari(ieg+1:80),deltrn(1),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(2)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(2)
         call rdfrre(11,vari(ieg+1:80),deltrn(2),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(3)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(3)
         call rdfrre(11,vari(ieg+1:80),deltrn(3),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(4)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(4)
         call rdfrre(11,vari(ieg+1:80),deltrn(4),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(5)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(5)
         call rdfrre(11,vari(ieg+1:80),deltrn(5),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:9) .EQ. 'deltrn(6)') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrre(11,vari(ieg+1:80),deltrn(6),ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'nrelax') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrin(11,vari(ieg+1:80),nrelax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:5) .EQ. 'relax') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrre(11,vari(ieg+1:80),relax,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'pasmin') THEN
c        READ(vari(ieg+1:80),*,err=98)deltrn(6)
         call rdfrre(11,vari(ieg+1:80),pasmin,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'rlcept') THEN
c        READ(vari(ieg+1:80),*,err=98) deltrn(6)
         call rdfrre(11,vari(ieg+1:80),rlcept,ierror)
         if(ierror.eq.1) go to 98
         GO TO 10
      ELSE IF (vari(1:6) .EQ. 'sellan') THEN
         call rdfrch(11,vari(ieg+1:80),novlan,ierror)
         if(ierror.eq.1) go to 98
         if(novlan(1:8).eq.'francais') then
           sellan(1:8)=novlan(1:8)
         elseif(novlan(1:7).eq.'english') then
           sellan(1:7)=novlan(1:7)
         else
           if(sellan(1:8).eq.'francais') then
             write(6,*)'Erreur de format. Veuillez recommencer.'
           elseif(sellan(1:7).eq.'english') then
             write(6,*)'Format error. Please try again.'
           endif
         endif
         GO TO 10
      ELSE IF (vari(1:3).EQ.'fin' .or. vari(1:3).eq.'end' .or.
     .       index(vari(1:4),'$fin')+index(vari(1:4),'$end').gt.0) then
         return
      ELSE
         if(sellan(1:8).eq.'francais') then
           write(6,*)'Variable inexistante ou inchangeable.'
         elseif(sellan(1:7).eq.'english') then
           write(6,*)'Invalid assignment. Please try again.'
         endif
         GO TO 10
      ENDIF
98    continue
      pntrat = MIN(pntrat,distxo)
c-langue
      if(sellan(1:8).eq.'francais') then
        write(6,*)'Erreur de format. Veuillez recommencer.'
      elseif(sellan(1:7).eq.'english') then
        write(6,*)'Format error. Please try again.'
      endif
      go to 10

   99 CONTINUE
      ifail = 1

 1028 FORMAT()
      RETURN
      END

      integer function lnblnk(string)
      implicit none
      integer i
      intrinsic len
c
c returns the position of the last non-blank character in "string"
c
      character*(*) string
      do i=len(string),1,-1
        if(string(i:i).ne.' ') then
          lnblnk=i
          return
        endif
      enddo
      lnblnk=1
      return
      end function lnblnk
