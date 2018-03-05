      SUBROUTINE RAPPEL(nptseg,deltp1,deltpn,repart,npr,deltr1,deltrn,
     .             pntrat,tgarde,lg,difpsi,distnv,nreg,nsep,npx,
     .             dpmin,dpmax,drmin,drmax,distxo,iusor,correct)
c
c  version : 07.07.97 18:37
c
c======================================================================
      IMPLICIT NONE

*..  Cette sous-routine imprime a l'ecran un tableau contenant tous les
*  parametres choisis, ainsi que quelques resultats intermediaires.
*  nouvelle valeur desiree.

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

*  arguments
      INTEGER nptseg(*),repart,npr(*),nreg,nsep,npx,iusor
      REAL(kind=R8)
     .       deltp1(*),deltpn(*),deltr1(*),deltrn(*),pntrat,lg(*),
     .       distnv(5,*),difpsi,dpmin(*),dpmax(*),drmin(*),drmax(*),
     .       tgarde(4),distxo
      LOGICAL correct

*  variables en common

#include <COMRLX.F>
#include <COMLAN.F>

*  variables locales
      INTEGER i
c======================================================================
*.. Impression des donnees se rapportant aux separatrices.

      if(sellan(1:8).eq.'francais') then
        write(iusor,105) 'SEGMENTS DE SEPARATRICE:'
  105   FORMAT(//T10,A)
      elseif(sellan(1:7).eq.'english') then
        write(iusor,205) 'SEPARATRIX SEGMENTS:'
 205    FORMAT(//T10,A)
      endif
        write(iusor,110)'#','nptseg(i)','lg(i)','deltp1(i)','deltpn(i)',
     .          'dpmin','dpmax'
  110 FORMAT(/T2,A1,T6,A9,T18,A5,T29,A9,T41,A9,T54,A6,T66,A6/75('='))

      DO 12 i=1, nsep
         write(iusor,114)i,nptseg(i),lg(i),deltp1(i),deltpn(i),dpmin(i),
     .             dpmax(i)
  114    FORMAT(T2,I1,T8,I3,T15,1p5E12.4)
   12 CONTINUE

*.. Impression des donnees se rapportant aux directions radiales de
*   chaque region.

      if(sellan(1:8).eq.'francais') then
        write(iusor,120) 'DIRECTION RADIALE POUR CHAQUE REGION:'
  120   FORMAT(//T10,A)
      elseif(sellan(1:7).eq.'english') then
        write(iusor,120) 'RADIAL DISTRIBUTIONS FOR EACH REGION:'
      endif

      IF (repart .EQ. 1) THEN
        if(sellan(1:8).eq.'francais') then
         write(iusor,121) 'Repartition selon la distance:',
     .     ' repart=',repart
        elseif(sellan(1:7).eq.'english') then
         write(iusor,221) 'Distribution in displacement along plates:',
     .     ' repart=',repart
        endif
  121    FORMAT(T14,A,i5)
  221    FORMAT(T14,2A,i5)

         write(iusor,122)'region','npr(i)','distance','deltr1(i)',
     .     'deltrn(i)','drmin','drmax'
  122    FORMAT(/T2,A6,T9,A6,T17,A8,T29,A9,T41,A9,T54,A6,T66,A6/
     .            75('='))

      ELSE IF (repart .EQ. 2) THEN
        if(sellan(1:8).eq.'francais') then
          write(iusor,121) 'Repartition selon psi: repart=',repart
          write(iusor,124)'region','npr(i)','difference','deltr1(i)',
     .      'deltrn(i)','drmin','drmax'
  124     FORMAT(/T2,A6,T9,A6,T17,A10,T29,A9,T41,A9,T54,A6,T66,A6/
     .            75('='))
        elseif(sellan(1:7).eq.'english') then
          write(iusor,121) 'Distribution in psi: repart=',repart
          write(iusor,124)'region','npr(i)','  width   ','deltr1(i)',
     .      'deltrn(i)','drmin','drmax'
        endif
      ENDIF

      DO 26 i=1, nreg-1
         write(iusor,128)i,npr(i),distnv(repart,i),deltr1(i),deltrn(i),
     .            drmin(i),drmax(i)
  128    FORMAT(T2,I1,T8,I3,T15,1p5E12.4)
   26 CONTINUE

*.. Impression des donnees se rapportant a la region centrale.

      if(sellan(1:8).eq.'francais') then
        write(iusor,134) nreg,distxo
  134   FORMAT(//T10,'REGION CENTRALE: region i=',T37,I1,T43,
     .       'pntrat max.=',F11.8)
      elseif(sellan(1:7).eq.'english') then
        write(iusor,234) nreg,distxo
  234   FORMAT(//T10,'CENTRAL REGION: region i=',T37,I1,T43,
     .       'pntrat max.=',F11.8)
      endif

      IF (repart .EQ. 1) THEN
         write(iusor,138)'pntrat','npr(i)','deltr1(i)','deltrn(i)',
     .            'drmin','drmax'
  138    FORMAT(/T2,A6,T9,A6,T17,A9,T29,A9,T41,A6,T54,A6/75('='))

         write(iusor,140)pntrat,npr(nreg),deltr1(i),deltrn(i),drmin(i),
     .            drmax(i)
  140    FORMAT(/T2,F6.3,T9,I3,T15,1p4E12.4)

      ELSE IF (repart .EQ. 2) THEN

         if(sellan(1:8).eq.'francais') then
           write(iusor,148)'pntrat','npr(i)','difference','deltr1(i)',
     .       'deltrn(i)','drmin','drmax'
         elseif(sellan(1:7).eq.'english') then
           write(iusor,148)'pntrat','npr(i)','  width','deltr1(i)',
     .       'deltrn(i)','drmin','drmax'
         endif
  148    FORMAT(/T2,A6,T9,A6,T17,A,T29,A9,T41,A9,T54,A6,T66,A6/
     .            75('='))

         write(iusor,150)pntrat,npr(nreg),difpsi,deltr1(nreg),
     .      deltrn(nreg),drmin(nreg),drmax(nreg)
  150    FORMAT(T2,F6.3,T9,I3,T15,1p5E12.4)

      ENDIF

*.. Impression des donnees se rapportant a la longueur de garde de
*   chaque plaque de deflecteur.

      if(sellan(1:8).eq.'francais') then
        write(iusor,152)
     .  'LONGUEUR DE GARDE DE CHAQUE PLAQUE DE DEFLECTEUR:'
  152   FORMAT(//T10,A)
      elseif(sellan(1:7).eq.'english') then
        write(iusor,152)'GUARD LENGTH FOR EACH DIVERTOR PLATE:'
      endif

      IF (nsep .EQ. 1 .or. nsep .EQ. 3) THEN

         write(iusor,154) 'tgarde(1)','tgarde(2)'
  154    FORMAT(/T2,A9,T16,A9/75('='))

         write(iusor,156) tgarde(1),tgarde(2)
  156    FORMAT(T2,F8.5,T17,F8.5)

      ELSE

         write(iusor,158)'tgarde(1)','tgarde(2)','tgarde(3)','tgarde(4)'
  158    FORMAT(/T2,A9,T16,A9,T31,A9,T46,A9/75('='))

         write(iusor,160) tgarde(1),tgarde(2),tgarde(3),tgarde(4)
  160    FORMAT(T2,F8.5,T17,F8.5,T32,F8.5,T47,F8.5)
      ENDIF

c  Impression des parametres servant au calcul de la maille orthogonale
c  par relaxation
      if(sellan(1:8).eq.'francais') then
        write(iusor,162)
 162    format(//t10,
     .    'PARAMETRES SERVANT AU CALCUL DE LA MAILLE PAR RELAXATION:'//
     .    75('=')/
     .    'nrelax',t15,'relax',t30,'pasmin',t45,'rlcept')
      elseif(sellan(1:7).eq.'english') then
        write(iusor,262)
 262    format(//t10,
     .    'RELAXATION PARAMETERS USED TO CONSTRUCT THE MESH:'//
     .    75('=')/
     .    'nrelax',t15,'relax',t30,'pasmin',t45,'rlcept')
      endif
      write(iusor,164)nrelax,relax,pasmin,rlcept
 164  format(t2,i5,t15,f6.3,t30,1pe10.3,t45,1pe10.3)

cxpb  Verifier que les valeurs presentees sont correctes

      correct = .true.
      do i = 1, nsep
        if (dpmin(i)*dpmax(i).le.0.0 .or.
     .      dpmin(i)*lg(i).le.0.0 .or.
     .      dpmax(i)*lg(i).le.0.0) then
          correct = .false.
          if(sellan(1:8).eq.'francais') then
            WRITE(6,281) i, dpmin(i), dpmax(i), lg(i)
  281       FORMAT(//T2,'Donnees invalides pour le segment',i2,'!'//
     .   T2,
     .   'Les nombres dpmin, dpmax et lg doivent avoir le meme signe.'//
     .   T2,'Modifiez deltp1 et deltpn en consequence.'//
     .   T2,'dpmin,dpmax,lg = ',1p,3(1e11.4,1x))
          elseif(sellan(1:7).eq.'english') then
            WRITE(6,280) i, dpmin(i), dpmax(i), lg(i)
  280       FORMAT(//T2,'Invalid data for segment',i2,'!'//
     .   T2,'The numbers dpmin, dpmax and lg must have the same sign.'//
     .   T2,'Modify deltp1 and deltpn accordingly.'//
     .   T2,'dpmin,dpmax,lg = ',1p,3(1e11.4,1x))
          endif
        endif 
        if (nptseg(i).le.2) then
          correct=.false.
          if(sellan(1:8).eq.'francais') then
            write(6,2811) i
 2811       format(//T2,'Donnees invalides pour le segment',i2,'!'//
     .        T2,'Il faut plus de deux points sur ce segment!')
          elseif(sellan(1:7).eq.'english') then
            write(6,2810) i
 2810       format(//T2,'Invalid data for segment',i2,'!'//
     .        T2,'You need more than 2 points on this segment!')
          endif
        endif
        if (abs(dpmin(i)).lt.pasmin .or.
     .      abs(dpmax(i)).lt.pasmin) then
          correct = .false.
          if(sellan(1:8).eq.'francais') then
            WRITE(6,2821) i, dpmin(i), dpmax(i), pasmin
 2821       FORMAT(//T2,'Donnees invalides pour le segment',i2,'!'//
     .   T2,
     .   'Les nombres dpmin et dpmax doivent etre superieurs a pasmin'//
     .   ' en valeur absolue.'//
     .   T2,'Modifiez deltp1, deltpn or pasmin en consequence.'//
     .   T2,'dpmin,dpmax,pasmin = ',1p,3(1e11.4,1x))
          elseif(sellan(1:7).eq.'english') then
            WRITE(6,2820) i, dpmin(i), dpmax(i), pasmin
 2820       FORMAT(//T2,'Invalid data for segment',i2,'!'//
     .   T2,'The numbers dpmin and dpmax must be larger than pasmin'//
     .      ' in absolute value.'//
     .   T2,'Modify deltp1, deltpn or pasmin accordingly.'//
     .   T2,'dpmin,dpmax,pasmin = ',1p,3(1e11.4,1x))
          endif
        endif
      enddo
      do i = 1, nreg-1
        if (drmin(i)*drmax(i).le.0.0 .or.
     .      drmin(i)*distnv(repart,i).le.0.0 .or.
     .      drmax(i)*distnv(repart,i).le.0.0) then
          correct = .false.
          if(sellan(1:8).eq.'francais') then
            WRITE(6,291) i, drmin(i), drmax(i), distnv(repart,i)
  291       FORMAT(//T2,'Donnees invalides pour la region',i2,'!'//
     .   T2,'Les nombres drmin, drmax et difference doivent ',
     .   'avoir le meme signe.'//
     .   T2,'Modifiez deltr1 et deltrn en consequence.'//
     .   T2,'drmin,drmax,difference = ',1p,3(1e11.4,1x))
          elseif(sellan(1:7).eq.'english') then
            WRITE(6,290) i, drmin(i), drmax(i), distnv(repart,i)
  290       FORMAT(//T2,'Invalid data for region',i2,'!'//
     .   T2,
     .   'The numbers drmin, drmax and width must have the same sign.'//
     .   T2,'Modify deltr1 and deltrn accordingly.'//
     .   T2,'drmin,drmax,width = ',1p,3(1e11.4,1x))
          endif
        endif
      enddo
      i = nreg
      if (drmin(i)*drmax(i).le.0.0 .or.
     .    drmin(i)*difpsi.le.0.0 .or.
     .    drmax(i)*difpsi.le.0.0) then
        correct = .false.
        if(sellan(1:8).eq.'francais') then
          WRITE(6,2911) i, drmin(i), drmax(i), difpsi
 2911     FORMAT(//T2,'Donnees invalides pour la region',i2,'!'//
     .   T2,'Les nombres drmin, drmax et difference doivent ',
     .   'avoir le meme signe.'//
     .   T2,'Modifiez deltr1 et deltrn en consequence.'//
     .   T2,'drmin,drmax,difference = ',1p,3(1e11.4,1x))
        elseif(sellan(1:7).eq.'english') then
          WRITE(6,2900) i, drmin(i), drmax(i), difpsi
 2900     FORMAT(//'Invalid data for region',i2,'!'//
     .   T2,
     .   'The numbers drmin, drmax and width must have the same sign.'//
     .   T2,'Modify deltr1 and deltrn accordingly.'//
     .   T2,'drmin,drmax,width = ',1p,3(1e11.4,1x))
        endif
      endif
      do i = 1, 2*max(1,npx)
        if (tgarde(i).lt.0.0) then
          correct = .false.
          if(sellan(1:8).eq.'francais') then
            WRITE(6,3011) i, tgarde(i)
 3011       FORMAT(//T2,'Donnees invalides pour la plaque',i2,'!'//
     .     T2,'La longueur tgarde doit etre positive ou nulle.'//
     .     T2,'Modifiez tgarde en consequence.'//
     .     T2,'tgarde = ',1pe11.4)
          elseif(sellan(1:7).eq.'english') then
            WRITE(6,3000) i, tgarde(i)
 3000       FORMAT(//'Invalid data for plate',i2,'!'//
     .     T2,'The length tgarde must be positive or zero.'//
     .     T2,'Modify tgarde accordingly.'//
     .     T2,'tgarde = ',1pe11.4)
          endif
        endif
      enddo
      if (nrelax.lt.1 .or. relax.le.0.0 .or. relax.gt.1.0 .or. 
     &    pasmin.le.0.0 .or. rlcept.le.0.0) then
        correct = .false.
        if(sellan(1:8).eq.'francais') then
          WRITE(6,3111) nrelax, relax, pasmin, rlcept
 3111     FORMAT(//
     .   T2,'Donnees invalides pour l''algorithme de relaxation!'//
     .   T2,'Il faut au moins une iteration.'//
     .   T2,'Les parametres relax, pasmin, et rlcept ',
     .      'doivent etre positifs.'//
     .   T2,'Le parametre relax ne doit pas exceder 1.'//
     .   T2,'Modifiez-les en consequence.'//
     .   T2,'nrelax, relax, pasmin, rlcept = ',i6,1p,3(1e11.4,1x))
        elseif(sellan(1:7).eq.'english') then
          WRITE(6,3100) nrelax, relax, pasmin, rlcept
 3100     FORMAT(//T2,'Invalid data for relaxation algorithm!'//
     .   T2,'The length tgarde must be positive or zero.'//
     .   T2,'At least one iteration is needed.'//
     .   T2,'The relax, pasmin, and rlcept parameters ',
     .      'must be positive.'//
     .   T2,'The relax parameter must not exceed 1.'//
     .   T2,'Modify these accordingly.'//
     .   T2,'nrelax, relax, pasmin, rlcept = ',i6,1p,3(1e11.4,1x))
        endif
      endif

      RETURN
      END
