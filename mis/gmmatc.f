      SUBROUTINE GMMATC( A,ROWA,COLA,MTA, B, ROWB,COLB,NTB, C )
C*****
C     GMMATC - G E N E R A L  M A T R I X  M U L T I P L Y
C                                 A N D
C                           T R A N S P O S E
C            S I N G L E  P R E C I S I O N  V E R S I O N
C     COMPLEX VERSION
C
C     PERFORMS                                     WHEN
C               A            *  B            =  C     MTA=0  NTB= 0
C               A            *  B TRANSPOSE  =  C          0       1
C               A TRANSPOSE  *  B            =  C          1       0
C               A TRANSPOSE  *  B TRANSPOSE  =  C          1       1
C*****
C     A -  IS A MATRIX (ROWA) ROWS BY (COLA) COLUMNS
C     B -  IS A MATRIX (ROWB) ROWS BY (COLB) COLUMNS
C     A,B AND C ARE STORED BY ROWS (EXAMPLE)
C              MATRIX                   STORED
C         A=   1    2              A=   1
C              3    4                   2
C              5    6                   3
C                                       4
C                                       5
C                                       6
C*****
C
C
C     IF MTA .LT. 0, C IS NOT ZEROED OUT.  HENCE THE ROUTINE, IN THIS
C     CASE, COMPUTES  A * B  +  D  =  C  WHERE THE MATRIX  D  HAS BEEN
C     STORED ROW-WISE AT  C  BY THE CALLING PROGRAM.  IF MTA = -1,  A
C     IS TRANSPOSED.  IF MTA = -2,  A  IS NOT TRANSPOSED.  NTB IS
C     DEFINED AS ABOVE AND IS INDEPENDENT OF MTA.
C
C
      INTEGER   ROWA,COLA,  ROWB,COLB
C
C
C
      INTEGER IPARM(2)
      COMPLEX A(1),B(1),C(1)
C
C
C
      NTA = IABS(MTA)
      IF (MTA .EQ. (-2)) NTA = 0
      IF ( NTA .NE. 0 ) GO TO 10
C
C A IS NOT TRANSPOSED
C
      NROWA = ROWA
      NCOLA = COLA
      INCRIK = 1
      IKN = COLA
      INCIK1 = COLA
      GO TO 20
C
C A IS TRANSPOSED
C
   10 NROWA = COLA
      NCOLA = ROWA
      INCRIK = COLA
      IKN = ( ROWA-1 )*COLA + 1
      INCIK1 = 1
   20 IF( NTB .NE. 0 ) GO TO 30
C
C B IS NOT TRANSPOSED
C
      NROWB = ROWB
      NCOLB = COLB
      INCRKJ = COLB
      INCKJ1 = 1
      GO TO 40
C
C B IS TRANSPOSED
C
   30 NROWB = COLB
      NCOLB = ROWB
      INCRKJ = 1
      INCKJ1 = COLB
C
C CHECK CONSISTANT DIMENSIONS AND ZERO C IF NO D MATRIX
C
   40 IF( NCOLA .NE. NROWB ) GO TO 80
      IF( MTA .LT. 0 ) GO TO 50
      NTERMS = NROWA*NCOLB
      DO 42 I=1,NTERMS
      C(I) = 0
   42 CONTINUE
C
C PERFORM MATRIX MULTIPLICATION
C
   50 IJ1 = 1
      IJN = NCOLB
      IK1 = 1
      DO 58 I=1,NROWA
      KJ1 = 1
      DO 56 IJ =IJ1,IJN
      KJ = KJ1
      DO 54 IK=IK1,IKN,INCRIK
      C(IJ) = C(IJ) + A(IK)*B(KJ)
      KJ = KJ + INCRKJ
   54 CONTINUE
      KJ1 = KJ1 + INCKJ1
   56 CONTINUE
      IJ1 = IJN + 1
      IJN = IJN + NCOLB
      IK1 = IK1 + INCIK1
      IKN = IKN + INCIK1
   58 CONTINUE
      RETURN
   80 IPARM(1) = NTA
      IPARM(2) = NTB
      CALL MESAGE (-30,21,IPARM(1))
      RETURN
      END
