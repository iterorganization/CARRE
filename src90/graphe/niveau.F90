      SUBROUTINE NIVEAU(XMIN,XMAX,YMIN,YMAX,Z,NXMAX,NX,NY,RWRK,NRW,IWRK, & 
     &  NIW)
!
!  version : 05.04.97 15:59
!
!======================================================================
!  ROUTINE POUR TRACER DES LIGNES DE NIVEAU SUR UNE MAILLE RECTANGULAIRE
!  UNIFORME.
      IMPLICIT NONE
!
!  DONNEES:
!  XMIN, XMAX: VALEURS MINIMUM ET MAXIMUM DE X
!  YMIN, YMAX:    "       "    "     "    "  Y
!  Z:          TABLEAU DES Z
!  NX:         NOMBRE DE COLONNES EN X
!  NY:         NOMBRE DE LIGNES EN Y
!  NXMAX:      PREMIERE DIMENSION DE Z
!  RWRK, IWRK: TABLEAUX DE TRAVAIL
!  NRW, NIW:   DIMENSIONS DES TABLEAUX DE TRAVAIL
!
      INTEGER NX,NY,NXMAX,IWRK(*),NRW,NIW
      REAL XMIN,XMAX,YMIN,YMAX,Z(NXMAX,NY),RWRK(*)
!
!  VARIABLES EN COMMON
#include <PERIM.F>
#include <COMQUA.F>
      real xa,xb,ya,yb
      equivalence (xa,x1a), (xb,x1b), (ya,x2a), (yb,x2b)
!
!  VARIABLES LOCALES
      INTEGER LL,I,J
      INTEGER ICLS,INCL,ICLL,ICLU,ICLD
      REAL XAP,XBP,YAP,YBP,XMINP,XMAXP,YMINP,YMAXP,ECART, & 
     &  ZMAX,ZMIN
      REAL CWM,CLV
!
!  PROCEDURES
      intrinsic log10
      EXTERNAL CPSETI,CPSETR,CPRECT,CPCLDR,CPLBDR
!
!  JEU DE CARACTERES POUR LES MARQUES D'ECHELLE DES LIGNES DE NIVEAU
!    - ROUTINE GKS.
!======================================================================

      if(qualit.eq.1) then
        CALL GSTXFP (-13,2)
      else
        call gstxfp(1,2)
      endif

!  FRACTION DU CADRE CHOISI
      CALL GETSET(XA,XB,YA,YB,XMINP,XMAXP,YMINP,YMAXP,LL)
!  CALCUL DU CADRE
      if(ll.eq.1 .or. ll.eq.2) then
        XAP=XA+(XMIN-XMINP)/(XMAXP-XMINP)*(XB-XA)
        XBP=XA+(XMAX-XMINP)/(XMAXP-XMINP)*(XB-XA)
      else
        XAP=XA+(log10(XMIN)-log10(XMINP))/ & 
     &    (log10(XMAXP)-log10(XMINP))*(XB-XA)
        XBP=XA+(log10(XMAX)-log10(XMINP))/ & 
     &    (log10(XMAXP)-log10(XMINP))*(XB-XA)
      endif
      if(ll.eq.1 .or. ll.eq.3) then
        YAP=YA+(YMIN-YMINP)/(YMAXP-YMINP)*(YB-YA)
        YBP=YA+(YMAX-YMINP)/(YMAXP-YMINP)*(YB-YA)
      else
        YAP=YA+(log10(YMIN)-log10(YMINP))/ & 
     &    (log10(YMAXP)-log10(YMINP))*(YB-YA)
        YBP=YA+(log10(YMAX)-log10(YMINP))/ & 
     &    (log10(YMAXP)-log10(YMINP))*(YB-YA)
      endif
!
      CALL CPSETI('SET',1)
      CALL CPSETR('VPS',0.)
      CALL CPSETR('VPL-VIEWPORT LEFT',XAP)
      CALL CPSETR('VPR-VIEWPORT RIGHT',XBP)
      CALL CPSETR('VPB-VIEWPORT BOTTOM',YAP)
      CALL CPSETR('VPT-VIEWPORT TOP',YBP)
!

!  DEFINITION DU NOMBRE DE LIGNES DE NIVEAUX

!  - CLS: Determine de quelle facon les lignes de niveau seront choisies.
!
!    CLS = 0 :  CONPACK ne calcule pas de lignes de niveau; l'usager doit
!               definir lui-meme le nombre de lignes de niveau a l'aide de
!               l'enonce NCL (voir ci-dessous).
!    CLS = -n : CONPACK calcule n lignes de niveau entre les valeurs minimum
!               et maximum de la fonction, conduisant a n+1 intervalles.
!    CLS = n :  CONPACK calcule les lignes de niveau au choix entre 3 methodes
!               selon les valeurs des parametres suivants: CIS ("Contour
!               interval specifier"), CMN ("Contour minimum"), CMX ("Contour
!               maximum"). [Voir manuel pour plus de details].
!    DEFAUT : CLS = 16.
!
!  - NCL: Indique le nombre de lignes de niveau.
!         Ce parametre doit etre defini uniquement si CLS=0.
!    DEFAUT : Aucun.
!

      ICLS = 0
      INCL = 21

      CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',ICLS)
      CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',INCL)

!  ECRITURE AU MAXIMUM ET AU MININUM.

!  - HIT: Texte ecrit au point maximum.
!
!  - LOT: Texte ecrit au point minimum.
!
!    DEFAUT : 'H:B$ZDV$:E'.
!    Il faut donc specifier HIT et LOT si on ne desire aucun texte.

      CALL CPSETC ('HIT',' ')
      CALL CPSETC ('LOT',' ')


!     ON TROUVE LE MINIMUM DES VALEURS DE Z
      DO I=1,NX
       DO J=1,NY
        IF((I.EQ.1).AND.(J.EQ.1)) THEN
          ZMIN=Z(I,J)
          ZMAX=Z(I,J)
        ELSE
          ZMIN=MIN(ZMIN,Z(I,J))
          ZMAX=MAX(ZMAX,Z(I,J))
        ENDIF
       ENDDO
      ENDDO

!  ON DIVISE L'INTERVALLE ENTRE LE MAX. ET LE MIN. PAR LE NOMBRE DE LIGNES
!  DE NIVEAU.

      ECART=(ZMAX-ZMIN)/REAL(INCL)


!  ON CALCULE LA VALEUR DE CHAQUE LIGNE DE NIVEAU EN SPECIFIANT LES PARA-
!  METRES QUI L'ACCOMPAGNENT

      DO 103 I=1,INCL

!  - PAI: "Parameter array index". Ce parametre indique a quel element de
!         la matrice on refere. Ce parametre doit etre appele pour changer
!         les parametres internes suivants.
!    DEFAUT : 0

        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)

!  FACTEUR MULTIPLICATIF POUR LA GROSSEUR DES CARACTERES

!  - CWM: Facteur mutiplicatif pour les caracteres qui sont utilises dans
!         les marques d'echelle ("labels"!) des lignes de niveaux.
!         CWM multiplie la grosseur par defaut des caracteres.
!    DEFAUT : CWM = 1.

        CWM = 2.
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',CWM)

!  EPAISSEUR DES LIGNES DE NIVEAU

!  - CLL:  Parametre controlant l'epaisseur de la ligne utilisee pour
!          tracer la ligne de niveau.
!    DEFAUT : CLL = 0. (COMPACK prend alors l'epaisseur par defaut qui
!             correspond a environ .1 mm).

        if(qualit.eq.1) then
          ICLL = 6
        else
          icll=1
        endif
        CALL CPSETI ('CLL - CONTOUR LINE LINE WIDTH',ICLL)

!  VALEUR DE LA LIGNE DE NIVEAU

!  - CLV: Ce parametre sert a definir la valeur de la ligne de niveau
!         courante, telle que definie par PAI.

        CLV = ZMIN+REAL(I-1)*ECART
        CALL CPSETR ('CLV - CONTOUR LEVEL',CLV)

!  TYPE DE LA LIGNE DE NIVEAU

!  - CLU: Indique de quelle facon doit etre utilisee la ligne de niveau
!         definie par PAI et CLV.
!    CLU = 0: Aucune ligne de niveau n'est tracee.
!    CLU = 1: La ligne de niveau est tracee sans marque d'echelle ("label"!)
!    CLU = 2: Seule la marque d'echelle est tracee, sans ligne.
!    CLU = 3: Ala fois la ligne de niveau et la marque d'echelle sont tracees.
!
!  - CLD: Specifie le trait utilise pour la ligne de niveau courante.
!
!         Le parametre CLD est compris entre 0 et 2**16 (65536) ou, en base
!         2, chaque 0 correspond a un blanc, et chaque 1 correspond a un
!         point. A vous de creer votre propre ligne!

        IF(MOD(I,5).EQ.1) THEN
          ICLU = 3
          ICLD = 65535
        ELSE
          ICLU = 3
          ICLD = 7710
        ENDIF
        CALL CPSETI ('CLU - CONTOUR LEVEL USE',ICLU)
        CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN',ICLD)


!  - AIA,AIB: Ces deux parametres specifient les niveaux superieur et
!             inferieur au niveau courant; si le niveau AIA et le niveau
!             AIB sont egaux a 0, la ligne courante est ignoree par CON-
!             PACK.

        CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',I)
        CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',I+1)

103   CONTINUE


!
!  CALCUL DES LIGNES DE NIVEAUX
      CALL CPRECT(Z,NXMAX,NX,NY,RWRK,NRW,IWRK,NIW)
!
!  TRACE UN PERIMETRE (PAS NECESSAIRE SI NEWPAG A ETE APPELE).
!     CALL CPBACK(Z,RWRK,IWRK)
!
!  TRACE LES LIGNES DE NIVEAUX
      CALL CPCLDR(Z,RWRK,IWRK)
!  INFORMATION ET HAUT/BAS
      CALL CPLBDR(Z,RWRK,IWRK)
      RETURN
      END
