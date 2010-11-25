!=======================================================================
      FUNCTION EDG(L,K,E)
      implicit none
!
!     FUNCTION EDG
!
!     PURPOSE:
!     ---------
!
!     FIND EDGE IN TRIANGLE L WHICH IS ADJACENT TO TRIANGLE K
!
!     INPUT:
!     ------
!
!     'L'      - NUMBER OF TRIANGLE
!
!     'K'      - NUMBER OF ADJACENT TRIANGLE
!
!     'E'      - ADJACENCY ARRAY FOR TRIANGULATION
!              - TRIANGLES ADJACENT TO J ARE FOUND IN E(I,J) FOR I=1,2,3
!              - ADJACENT TRIANGLES LISTED IN ANTICLOCKWISE SEQUENCE
!              - ZERO DENOTES NO ADJACENT TRIANGLE
!              - E HAS DIMENSIONS E(3,2*N+1), WHERE N IS THE NUMBER OF
!                POINTS TO BE TRIANGULATED
!
!     'EDG'    - NOT DEFINED
!
!     OUTPUT:
!     -------
!
!     'L'      - UNCHANGED
!
!     'K'      - UNCHANGED
!
!     'E'      - UNCHANGED
!
!     'EDG'    - NUMBER OF EDGE IN TRIANGLE L WHICH IS ADJACENT TO
!                TRIANGLE K
!              - E(EDG,L)=K
!
!     PROGRAMMER:
!     -----------
!     S W SLOAN
!
!     LAST MODIFIED:
!
!
!     30 JAN 1986     S W SLOAN
!
      INTEGER L,K,I,E(3,*),EDG
!
      DO 10 I=1,3
        IF(E(I,L).EQ.K)THEN
          EDG=I
          RETURN
        END IF
   10 CONTINUE
      WRITE(6,'(''0***ERROR IN FUNCTION EDG***'')')
      WRITE(6,'(''***ELEMENTS NOT ADJACENT***'')')
      STOP
!
      END
