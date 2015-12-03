      SUBROUTINE SCRLM (SCURL, XXI, E, H, CONT, RP, ALF1, R1, LAM1,HF)
C
C THIS SUBROUTINE COMPUTES THE STRESS MATRIX IN FIELD COORDINATES
C FOR THE TOROIDAL RING ELEMENT
C
C
C NOTE THE DOUBLE SUBSCRIPTING USED IN THE SCRLM SUBROUTINE IS
C COMPATIBLE WITH THE CALLING PROGRAM. THE SEL ARRAY WILL RETURN WITH
C THE STRESS MATRIX TRANSPOSED (10X15, STORED ROWWISE) BUT IN THE SCRLM
C SUBROUTINE THE STRESS MATRIX IS COMPUTED AS A DOUBLY SUBSCRIPTED
C 15X10 ARRAY (STORED COLUMNWISE).
C
      DIMENSION SCURL (15,10), E (2,2), XXI (3)
      REAL LAM1 , LAM2 , LAM3 ,LAM4
C     ------------------------------------------------------------------
C
      SC1 = H
      SC2 =HF**3 / 12.0
      JJ = 1
      KK = 3
      LL = 5
C
      DO 195 I = 1,3
      XX1 = XXI(I)
      XX2 = XX1 * XX1
      XX3 = XX2 * XX1
      XX4 = XX3 * XX1
      XX5 = XX4 * XX1
      CALL SOLVE1(ALF1,R1,RP,XX1,LAM2,LAM3,LAM4,CONT)
      DO 185 J = 1,2
      SCURL(JJ, 1) = LAM2 * E(J,2)
      SCURL(JJ, 2) = SCURL(JJ,1) * XX1 + E(1,J)
      SCURL(JJ, 3) = SCURL(JJ,1) * XX2 + E(1,J) * 2.0 * XX1
      SCURL(JJ, 4) = SCURL(JJ,1) * XX3 + E(1,J) * 3.0 * XX2
      SCURL(JJ, 5) = LAM1 * E(1,J) + LAM3 * E(J,2)
      SCURL(JJ, 6) = SCURL(JJ,5) * XX1
      SCURL(JJ, 7) = SCURL(JJ,5) * XX2
      SCURL(JJ, 8) = SCURL(JJ,5) * XX3
      SCURL(JJ, 9) = SCURL(JJ,5) * XX4
      SCURL(JJ,10) = SCURL(JJ,5) * XX5
      JJ = JJ + 1
  185 CONTINUE
      JJ = JJ + 3
      DO 190 K = 1,2
      SCURL (KK,1) = 0.0
      SCURL (KK,2) = 0.0
      SCURL (KK,3) = 0.0
      SCURL (KK,4) = 0.0
      SCURL(KK, 5) = 0.0
      SCURL(KK, 6) = -LAM2 * E(K,2)
      SCURL(KK, 7) = SCURL(KK,6) * 2.0 * XX1 - E(1,K) *  2.0
      SCURL(KK, 8) = SCURL(KK,6) * 3.0 * XX2 - E(1,K) *  6.0 * XX1
      SCURL(KK, 9) = SCURL(KK,6) * 4.0 * XX3 - E(1,K) * 12.0 * XX2
      SCURL(KK,10) = SCURL(KK,6) * 5.0 * XX4 - E(1,K) * 20.0 * XX3
      KK = KK + 1
  190 CONTINUE
      KK = KK + 3
      EL = E(1,1) * LAM2
      ELL = EL * LAM1
      EEL = E(1,1) * LAM1
      SCURL (LL,1) = 0.0
      SCURL (LL,2) = 0.0
      SCURL (LL,3) = 0.0
      SCURL (LL,4) = 0.0
      SCURL (LL,5) = 0.0
      SCURL(LL, 6) = LAM2**2 * E(2,2) - LAM4 * E(1,2)
      SCURL(LL, 7) = SCURL(LL,6) * 2.0 * XX1 - 2.0 * EL
      SCURL(LL, 8) = SCURL(LL,6) * 3.0 * XX2 - 6.0 * (EL * XX1 + E(1,1))
      SCURL(LL, 9) = SCURL(LL,6) * 4.0 * XX3 - 12.0 * EL * XX2 - 24.0 *
     1 E(1,1) * XX1
      SCURL(LL,10) = SCURL(LL,6) * 5.0 * XX4 - 20.0 * EL * XX3 - 60.0 *
     1 E(1,1) * XX2
      LL = LL + 5
  195 CONTINUE
C
C     ADJUSTMENT FOR SHELL CAP CASE
      IF ( ALF1 .NE. 0.0 )  GO TO 198
      SCURL (1,2) =  E(1,2) + E(1,1)
      SCURL (2,2) =  E(2,2) + E(1,2)
      SCURL (3,7) = -2. * (E(1,2) + E(1,1) )
      SCURL (4,7) =  2. * (E(2,2) + E(1,2) )
      SCURL (5,8) =  3. * (E(2,2) - 4.*E(1,1) )
  198 DO 200 J = 1,15,5
      DO 200 I = 1,10
      SCURL(J  ,I) = SCURL(J  ,I) * SC1
      SCURL(J+1,I) = SCURL(J+1,I) * SC1
      SCURL(J+2,I) = SCURL(J+2,I) * SC2
      SCURL(J+3,I) = SCURL(J+3,I) * SC2
      SCURL(J+4,I) = SCURL(J+4,I) * SC2
  200 CONTINUE
      RETURN
      END
