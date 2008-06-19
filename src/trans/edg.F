c=======================================================================
      FUNCTION EDG(L,K,E)
      implicit none
C
C     FUNCTION EDG
C
C     PURPOSE:
C     ---------
C
C     FIND EDGE IN TRIANGLE L WHICH IS ADJACENT TO TRIANGLE K
C
C     INPUT:
C     ------
C
C     'L'      - NUMBER OF TRIANGLE
C
C     'K'      - NUMBER OF ADJACENT TRIANGLE
C
C     'E'      - ADJACENCY ARRAY FOR TRIANGULATION
C              - TRIANGLES ADJACENT TO J ARE FOUND IN E(I,J) FOR I=1,2,3
C              - ADJACENT TRIANGLES LISTED IN ANTICLOCKWISE SEQUENCE
C              - ZERO DENOTES NO ADJACENT TRIANGLE
C              - E HAS DIMENSIONS E(3,2*N+1), WHERE N IS THE NUMBER OF
C                POINTS TO BE TRIANGULATED
C
C     'EDG'    - NOT DEFINED
C
C     OUTPUT:
C     -------
C
C     'L'      - UNCHANGED
C
C     'K'      - UNCHANGED
C
C     'E'      - UNCHANGED
C
C     'EDG'    - NUMBER OF EDGE IN TRIANGLE L WHICH IS ADJACENT TO
C                TRIANGLE K
C              - E(EDG,L)=K
C
C     PROGRAMMER:
C     -----------
C     S W SLOAN
C
C     LAST MODIFIED:
C                     
C
C     30 JAN 1986     S W SLOAN
C
      INTEGER L,K,I,E(3,*),EDG
C
      DO 10 I=1,3
        IF(E(I,L).EQ.K)THEN
          EDG=I
          RETURN
        END IF
   10 CONTINUE
      WRITE(6,'(''0***ERROR IN FUNCTION EDG***'')')
      WRITE(6,'(''***ELEMENTS NOT ADJACENT***'')')
      STOP
C
      END
