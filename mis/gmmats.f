      SUBROUTINE GMMATS (A,IROWA,ICOLA,MTA, B,IROWB,ICOLB,NTB, C)
C*****
C     GMMATS - G E N E R A L  M A T R I X  M U L T I P L Y
C                                 A N D
C                           T R A N S P O S E
C            S I N G L E  P R E C I S I O N  V E R S I O N
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
      DIMENSION   A(1),B(1),C(1),IPARM(2)
C
C
C
      ROWA = IROWA
      COLA = ICOLA
      ROWB = IROWB
      COLB = ICOLB
      NTA = IABS(MTA)
      IF (MTA .EQ. (-2)) NTA = 0
      IF (NTA .EQ. 0  .AND.  NTB .EQ. 0) IF (COLA - ROWB) 80,5,80
      IF (NTA .EQ. 1  .AND.  NTB .EQ. 0) IF (ROWA - ROWB) 80,5,80
      IF (NTA .EQ. 0  .AND.  NTB .EQ. 1) IF (COLA - COLB) 80,5,80
      IF (NTA .EQ. 1  .AND.  NTB .EQ. 1) IF (ROWA - COLB) 80,5,80
    5 IF (NTA .EQ. 1) GO TO 10
      ILIM= ROWA
      KLIM= COLA
      INCI= COLA
      INCKA= 1
      GO TO 20
   10 ILIM= COLA
      KLIM= ROWA
      INCI= 1
      INCKA= COLA
   20 IF(NTB.EQ.1) GO TO 30
      JLIM= COLB
      INCJ= 1
      INCKB= COLB
      GO TO 40
   30 JLIM= ROWB
      INCJ= COLB
      INCKB= 1
   40 IF (MTA .LT. 0) GO TO 47
      LIM = ILIM * JLIM
      DO 45 I = 1,LIM
   45 C(I) = 0.0
   47 IJ = 0
      I = 0
   50 I = I + 1
      IFIX=I*INCI-COLA
      J = 0
   60 J = J + 1
      IJ=IJ+1
      IA=IFIX
      JB=J*INCJ-COLB
      K = 0
   70 K = K + 1
      IA=IA+INCKA
      JB=JB+INCKB
      C(IJ)=C(IJ)+ A(IA) * B(JB)
      IF (K .LT. KLIM) GO TO 70
      IF (J .LT. JLIM) GO TO 60
      IF (I .LT. ILIM) GO TO 50
      RETURN
   80 IPARM(1) = NTA
      IPARM(2) = NTB
      CALL MESAGE (-30,21,IPARM(1))
      RETURN
      END
