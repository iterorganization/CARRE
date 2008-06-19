c=======================================================================
      subroutine motifs
      implicit none
c  on definit les motifs de courbes par defaut
c
c  variables locales
      integer nchar(10),ncodm,nic,ic
      parameter(ncodm=10)
      character dshp(ncodm)*30
c
c  procedures
      character agdshn*16
      external agseti,agsetc,agsetr,agdshn
c
c  data
      data dshp/'$$',
     .          '$''',
     .          '$$''',
     .          '$$$''',
     .          '$$$$$''',
     .          '$$$$''$''',
     .          '$$$$$$$$$$$$$$''''',
     .          '$$$$$$$$$$''',
     .          '$$$$$$$$$$$$$''$''',
     .          '$$$$$$$$''$'''/
      data nchar/2,2,3,4,6,7,16,11,16,11/
c
c  definition des motifs de courbes
      NIC = 10 
      CALL AGSETI ('DASH/SELECTOR.',nIC)
      DO 15 ic=1,NIC
      CALL AGSETI ('DASH/LENGTH.',nchar(ic))
      CALL AGSETC (AGDSHN(IC),DSHP(IC))
      CALL AGSETR ('DASH/CHARACTER.',.03)
15    CONTINUE
      return
      end
