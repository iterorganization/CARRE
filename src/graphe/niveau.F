      SUBROUTINE NIVEAU(XMIN,XMAX,YMIN,YMAX,Z,NXMAX,NX,NY,RWRK,NRW,IWRK,
     .  NIW)
c
c  version : 05.04.97 15:59
c
c======================================================================
C  ROUTINE POUR TRACER DES LIGNES DE NIVEAU SUR UNE MAILLE RECTANGULAIRE
C  UNIFORME.
      IMPLICIT NONE
C
C  DONNEES:
C  XMIN, XMAX: VALEURS MINIMUM ET MAXIMUM DE X
C  YMIN, YMAX:    "       "    "     "    "  Y
C  Z:          TABLEAU DES Z
C  NX:         NOMBRE DE COLONNES EN X
C  NY:         NOMBRE DE LIGNES EN Y
C  NXMAX:      PREMIERE DIMENSION DE Z
C  RWRK, IWRK: TABLEAUX DE TRAVAIL
C  NRW, NIW:   DIMENSIONS DES TABLEAUX DE TRAVAIL
C
      INTEGER NX,NY,NXMAX,IWRK(*),NRW,NIW
      REAL XMIN,XMAX,YMIN,YMAX,Z(NXMAX,NY),RWRK(*)
C
C  VARIABLES EN COMMON
#include <PERIM.F>
#include <COMQUA.F>
      real xa,xb,ya,yb
      equivalence (xa,x1a), (xb,x1b), (ya,x2a), (yb,x2b)
C
C  VARIABLES LOCALES
      INTEGER LL,I,J
      INTEGER ICLS,INCL,ICLL,ICLU,ICLD
      REAL XAP,XBP,YAP,YBP,XMINP,XMAXP,YMINP,YMAXP,ECART,
     .  ZMAX,ZMIN
      REAL CWM,CLV
C
C  PROCEDURES
      intrinsic log10
      EXTERNAL CPSETI,CPSETR,CPRECT,CPCLDR,CPLBDR
C
C  JEU DE CARACTERES POUR LES MARQUES D'ECHELLE DES LIGNES DE NIVEAU
C    - ROUTINE GKS.
c======================================================================

      if(qualit.eq.1) then
        CALL GSTXFP (-13,2)
      else
        call gstxfp(1,2)
      endif

C  FRACTION DU CADRE CHOISI
      CALL GETSET(XA,XB,YA,YB,XMINP,XMAXP,YMINP,YMAXP,LL)
C  CALCUL DU CADRE
      if(ll.eq.1 .or. ll.eq.2) then
        XAP=XA+(XMIN-XMINP)/(XMAXP-XMINP)*(XB-XA)
        XBP=XA+(XMAX-XMINP)/(XMAXP-XMINP)*(XB-XA)
      else
        XAP=XA+(log10(XMIN)-log10(XMINP))/
     .    (log10(XMAXP)-log10(XMINP))*(XB-XA)
        XBP=XA+(log10(XMAX)-log10(XMINP))/
     .    (log10(XMAXP)-log10(XMINP))*(XB-XA)
      endif
      if(ll.eq.1 .or. ll.eq.3) then
        YAP=YA+(YMIN-YMINP)/(YMAXP-YMINP)*(YB-YA)
        YBP=YA+(YMAX-YMINP)/(YMAXP-YMINP)*(YB-YA)
      else
        YAP=YA+(log10(YMIN)-log10(YMINP))/
     .    (log10(YMAXP)-log10(YMINP))*(YB-YA)
        YBP=YA+(log10(YMAX)-log10(YMINP))/
     .    (log10(YMAXP)-log10(YMINP))*(YB-YA)
      endif
C
      CALL CPSETI('SET',1)
      CALL CPSETR('VPS',0.)
      CALL CPSETR('VPL-VIEWPORT LEFT',XAP)
      CALL CPSETR('VPR-VIEWPORT RIGHT',XBP)
      CALL CPSETR('VPB-VIEWPORT BOTTOM',YAP)
      CALL CPSETR('VPT-VIEWPORT TOP',YBP)
C

C  DEFINITION DU NOMBRE DE LIGNES DE NIVEAUX

C  - CLS: Determine de quelle facon les lignes de niveau seront choisies.
C
C    CLS = 0 :  CONPACK ne calcule pas de lignes de niveau; l'usager doit
C               definir lui-meme le nombre de lignes de niveau a l'aide de
C               l'enonce NCL (voir ci-dessous).
C    CLS = -n : CONPACK calcule n lignes de niveau entre les valeurs minimum
C               et maximum de la fonction, conduisant a n+1 intervalles.
C    CLS = n :  CONPACK calcule les lignes de niveau au choix entre 3 methodes
C               selon les valeurs des parametres suivants: CIS ("Contour
C               interval specifier"), CMN ("Contour minimum"), CMX ("Contour
C               maximum"). [Voir manuel pour plus de details].
C    DEFAUT : CLS = 16.
C
C  - NCL: Indique le nombre de lignes de niveau.
C         Ce parametre doit etre defini uniquement si CLS=0.
C    DEFAUT : Aucun.
C

      ICLS = 0
      INCL = 21

      CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',ICLS)
      CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',INCL)

C  ECRITURE AU MAXIMUM ET AU MININUM.

C  - HIT: Texte ecrit au point maximum.
C
C  - LOT: Texte ecrit au point minimum.
C
C    DEFAUT : 'H:B$ZDV$:E'.
C    Il faut donc specifier HIT et LOT si on ne desire aucun texte.

      CALL CPSETC ('HIT',' ')
      CALL CPSETC ('LOT',' ')


C     ON TROUVE LE MINIMUM DES VALEURS DE Z
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

C  ON DIVISE L'INTERVALLE ENTRE LE MAX. ET LE MIN. PAR LE NOMBRE DE LIGNES
C  DE NIVEAU.

      ECART=(ZMAX-ZMIN)/REAL(INCL)


C  ON CALCULE LA VALEUR DE CHAQUE LIGNE DE NIVEAU EN SPECIFIANT LES PARA-
C  METRES QUI L'ACCOMPAGNENT

      DO 103 I=1,INCL

C  - PAI: "Parameter array index". Ce parametre indique a quel element de
C         la matrice on refere. Ce parametre doit etre appele pour changer
C         les parametres internes suivants.
C    DEFAUT : 0

        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)

C  FACTEUR MULTIPLICATIF POUR LA GROSSEUR DES CARACTERES

C  - CWM: Facteur mutiplicatif pour les caracteres qui sont utilises dans
C         les marques d'echelle ("labels"!) des lignes de niveaux.
C         CWM multiplie la grosseur par defaut des caracteres.
C    DEFAUT : CWM = 1.

        CWM = 2.
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',CWM)

C  EPAISSEUR DES LIGNES DE NIVEAU

C  - CLL: Parametre controlant l'epaisseur de la ligne utilisee pour
C         tracer la ligne de niveau.
C    DEFAUT : CLL = 0. (COMPACK prend alors l'epaisseur par defaut qui
C             correspond a environ .1 mm).

        if(qualit.eq.1) then
          ICLL = 6
        else
          icll=1
        endif
        CALL CPSETI ('CLL - CONTOUR LINE LINE WIDTH',ICLL)

C  VALEUR DE LA LIGNE DE NIVEAU

C  - CLV: Ce parametre sert a definir la valeur de la ligne de niveau
C         courante, telle que definie par PAI.

        CLV = ZMIN+REAL(I-1)*ECART
        CALL CPSETR ('CLV - CONTOUR LEVEL',CLV)

C  TYPE DE LA LIGNE DE NIVEAU

C  - CLU: Indique de quelle facon doit etre utilisee la ligne de niveau
C         definie par PAI et CLV.
C    CLU = 0: Aucune ligne de niveau n'est tracee.
C    CLU = 1: La ligne de niveau est tracee sans marque d'echelle ("label"!)
C    CLU = 2: Seule la marque d'echelle est tracee, sans ligne.
C    CLU = 3: A la fois la ligne de niveau et la marque d'echelle sont tracees.
C
C  - CLD: Specifie le trait utilise pour la ligne de niveau courante.
C
C         Le parametre CLD est compris entre 0 et 2**16 (65536) ou, en base
C         2, chaque 0 correspond a un blanc, et chaque 1 correspond a un
C         point. A vous de creer votre propre ligne!

        IF(MOD(I,5).EQ.1) THEN
          ICLU = 3
          ICLD = 65535
        ELSE
          ICLU = 3
          ICLD = 7710
        ENDIF
        CALL CPSETI ('CLU - CONTOUR LEVEL USE',ICLU)
        CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN',ICLD)


C  - AIA,AIB: Ces deux parametres specifient les niveaux superieur et
C             inferieur au niveau courant; si le niveau AIA et le niveau
C             AIB sont egaux a 0, la ligne courante est ignoree par CON-
C             PACK.

        CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',I)
        CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',I+1)

103   CONTINUE


C
C  CALCUL DES LIGNES DE NIVEAUX
      CALL CPRECT(Z,NXMAX,NX,NY,RWRK,NRW,IWRK,NIW)
C
C  TRACE UN PERIMETRE (PAS NECESSAIRE SI NEWPAG A ETE APPELE).
C     CALL CPBACK(Z,RWRK,IWRK)
C
C  TRACE LES LIGNES DE NIVEAUX
      CALL CPCLDR(Z,RWRK,IWRK)
C  INFORMATION ET HAUT/BAS
      CALL CPLBDR(Z,RWRK,IWRK)
      RETURN
      END
