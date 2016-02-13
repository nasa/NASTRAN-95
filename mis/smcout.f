      SUBROUTINE SMCOUT ( ZI, ZR, ZD, ZRS, ZRD )
C
C SMCOUT DOES THE FINAL DIVISION OF THE TERMS OF THE PIVOTAL COLUMN
C AND WRITES THE COLUMN DATA TO THE LOWER TRIANGULAR MATRIX.
C
C THE FOLLOWING CALCULATIONS ARE DONE IN SUBROUTINE SMC2-RS,RD,CS,CD
C 
C      do 100 k = 1,n
C         do 10  i = k,n
C         temp = 0.
C         do 5  l = 1,k-1
C            temp = temp + a(i,l)*a(k,l) / a(l,l)
C    5       continue
C         a(i,k) = a(i,k) - temp
C   10    continue
C
C            THE FOLLOWING LAST COMPUTATION TAKES PLACE IN THIS SUBROUTINE.
C            THE RESULTS OF THE DIVISION ARE WRITTEN TO THE OUTPUT FILE BUT
C            THE RESULTS OF THE ABOVE (WITHOUT THE DIVISION BELOW) IS
C            MAINTAINED IN MEMORY FOR REMAINING COLUMN COMPUTATIONS.
C
C         do 11 j = k+1,n
C           a(k,j) = a(j,k) / a( k,k )
C   11      continue
C  100 continue
C
C  THE FINAL COMPUTATIONS ARE WRITTEN TO THE LLL MATRIX USING PUTSTR/ENDPUT.
C
      INTEGER          ZI(10)
      REAL             ZR(10), MINDS, ZRS(10)
      DOUBLE PRECISION ZD(10), DAKK2, DAKKR, DAKKI, DAKK, XND(10)
      DOUBLE PRECISION DR    , ZRD(10)
      INCLUDE  'SMCOMX.COM'
      CHARACTER         UFM*23,UWM*25,UIM*29,SFM*25
      COMMON / XMSSG  / UFM  , UWM  , UIM  , SFM
      COMMON / ZZZZZZ / XNS(10)
      EQUIVALENCE      ( XNS, XND )
      EQUIVALENCE      ( MINDS, MINDD ), (DSR, DDR ), (DSC, DDC )
      DATA               RZERO / 1.0E-10  / 
C      PRINT *,' SMCOUT-ENTER,KCOL=',KCOL
      KDIR  = ( KCOL-1 ) * 4 + 1
      KMIDX = ZI( KDIR )
      KRIDX = KMIDX + 4
      KM2   = ZI( KMIDX+1 )
      KRIDN = KRIDX + KM2   
      NROWS = 0
      DO 10 I = 1, KM2, 2
      NROWS  = NROWS + ZI( KRIDX + I ) 
10    CONTINUE
      KVIDX = KRIDX + KM2  
      GO TO (1000, 2000, 3000, 4000 ), KTYPE
C
C DO DIVISION IN REAL SINGLE PRECISION AND COMPUTE THE  DETERMINANT DDS.
C CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDS")
C
1000  CONTINUE
      AKK          =  ZR( KVIDX )     
CWKBI  7/95 SPR95005
      IF ( AKK .EQ. 0.0 ) GO TO 7002
1010  IF ( ABS( DSR ) .LT. 10. ) GO TO 1020
      DSR = DSR / 10.
      POWER = POWER + 1
      GO TO 1010
1020  IF ( ABS( DSR ) .GT. 0.1 ) GO TO 1030
      DSR = DSR * 10.
      POWER = POWER - 1
      GO TO 1020
1030  DSR    = DSR * AKK
      MINDS  = AMIN1 ( ABS(AKK), MINDS )
      IF ( CHLSKY .EQ. 0 ) GO TO 1040
      IF ( AKK .LE. 0 ) GO TO 7001
      AKK = SQRT( AKK )
1040  IF ( AKK .LT. 0. ) STURM = STURM + 1
CWKBD  7/95 SPR95005
C      IF ( AKK .EQ. 0. ) GO TO 7002
1050  ZRS( 1 ) = AKK
CWKBR 7/95 SPR95005
C      AKK      = 1. / AKK
      AKK      = -1. / AKK
      DO 1150 I = 2, NROWS
CWKBR 7/95 SPR95005
C      ZRS( I ) = -1.0 * ZR( KVIDX + I - 1 ) * AKK 
      ZRS( I ) =  ZR( KVIDX + I - 1 ) * AKK 
1150  CONTINUE
      GO TO 5000
C
C DO DIVISION IN REAL DOUBLE PRECISION AND COMPUTE THE  DETERMINANT DDR.
C CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDD")
C
2000  CONTINUE
      KVIDX = ( KVIDX/2 ) + 1            
      DAKK  =  ZD( KVIDX )  
CWKBI  7/95 SPR95005
      IF ( DAKK .EQ. 0.D0 ) GO TO 7002
2010  IF ( DABS( DDR ) .LT. 10.D0 ) GO TO 2020
      DDR = DDR / 10.D0
      POWER = POWER + 1
      GO TO 2010
2020  IF ( DABS( DDR ) .GT. 0.1 ) GO TO 2030
      DDR   = DDR * 10.D0
      POWER = POWER - 1
      GO TO 2020
2030  DDR   = DDR * DAKK
      MINDD = DMIN1 ( DABS(DAKK), MINDD )
      IF ( CHLSKY .EQ. 0 ) GO TO 2040
      IF ( DAKK .LE. 0 ) GO TO 7001
      DAKK = DSQRT( DAKK )
2040  IF ( DAKK .LT. 0.D0 ) STURM = STURM + 1
CWKBD 7/95 SPR95005
C      IF ( DAKK .EQ. 0.D0 ) GO TO 7002
2050  ZRD( 1 ) = DAKK
CWKBR 7/95 SPR95005
C      DAKK        = 1.D0 / DAKK   
      DAKK        = -1.D0 / DAKK   
      DO 2150 I = 2, NROWS
CWKBR 7/95 SPR95005
C      ZRD( I ) = -1.0D0 * ZD( KVIDX + I - 1 ) * DAKK  
      ZRD( I ) = ZD( KVIDX + I - 1 ) * DAKK  
2150  CONTINUE
      GO TO 5000
C
C DO DIVISION IN COMPLEX SINGLE PRECISION AND COMPUTE THE DETERMINANT 
C DSR AND DSC.
C CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDS")
C
3000  CONTINUE
C   (A+Bi) / (C+Di) = (AC + DB + ( CB-AD )i ) / (C**2 + D**2)
      AKKR  = ZR( KVIDX   )
      AKKI  = ZR( KVIDX+1 )
      AKK2  = AKKR*AKKR + AKKI*AKKI
CWKBI  7/95 SPR95005      
      IF ( AKK2 .EQ. 0. ) GO TO 7002     
3010  IF ( ABS( DSR**2 + DSC**2 ) .LT. 10. ) GO TO 3020
      DSR   = DSR / 10.
      DSC   = DSC / 10.
      POWER = POWER + 1
      GO TO 3010
3020  IF ( ABS( DSR**2 + DSC**2 ) .GT. 0.1 ) GO TO 3030
      DSR   = DSR * 10.
      DSC   = DSC * 10.
      POWER = POWER - 1
      GO TO 3020
3030  RS    = DSR*AKKR - DSC*AKKI
      DSC   = DSR*AKKI + DSC*AKKR
      DSR   = RS
      MINDS = AMIN1 ( ABS(AKK2), MINDS )
      IF ( CHLSKY .EQ. 0 ) GO TO 3040
      IF ( AKK2 .LE. 0 ) GO TO 7001
      AKK2 = SQRT( AKK2 )
3040  IF ( AKKR .LT. 0. ) STURM = STURM + 1
CWKBD  7/95 SPR95005
C      IF ( AKK2 .EQ. 0. ) GO TO 7002     
3050  ZRS( 1 ) = AKKR
      ZRS( 2 ) = AKKI
      NROWM   = NROWS * 2 - 1
CWKBR 7/95 SPR95005
C      AKK2    = 1. / AKK2   
      AKK2    = -1. / AKK2   
      KVIDX   = KVIDX + 1
      DO 3150 I = 2, NROWM, 2
CWKBDB 7/95 SPR95005
C      ZRS( I+1 ) =  -1.0 * ( ZR( KVIDX+I-1   ) * AKKR  +
C     &                         ZR( KVIDX+I   ) * AKKI  ) * AKK2
C      ZRS( I+2 ) =  -1.0 * ( ZR( KVIDX+I     ) * AKKR  -
C     &                         ZR( KVIDX+I-1 ) * AKKI  ) * AKK2
CWKBDE 7/95 SPR95005
CWKBIB 7/95 SPR95005
      ZRS( I+1 ) =  ( ZR( KVIDX+I-1 ) * AKKR  +
     &                ZR( KVIDX+I   ) * AKKI  ) * AKK2
      ZRS( I+2 ) =  ( ZR( KVIDX+I   ) * AKKR  -
     &                ZR( KVIDX+I-1 ) * AKKI  ) * AKK2
CWKBIE 7/95 SPR95005
3150  CONTINUE
      GO TO 5000
C
C DO DIVISION IN COMPLEX DOUBLE PRECISION AND COMPUTE THE DETERMINANT 
C DDR AND DDC.
C CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDD")
C
4000  CONTINUE
      KVIDX = ( KVIDX/2 ) + 1            
      DAKKR = ZD( KVIDX   )
      DAKKI = ZD( KVIDX+1 )
      DAKK2 = DAKKR*DAKKR + DAKKI*DAKKI
CWKBI  7/95 SPR95005      
      IF ( DAKK2 .EQ. 0. ) GO TO 7002 
4010  IF ( DABS( DDR**2 + DDC**2 ) .LT. 10.D0 ) GO TO 4020
      DDR   = DDR / 10.
      DDC   = DDC / 10.
      POWER = POWER + 1
      GO TO 4010
4020  IF ( DABS( DDR**2 + DDC**2 ) .GT. 0.1D0 ) GO TO 4030
      DDR   = DDR * 10.
      DDC   = DDC * 10.
      POWER = POWER - 1
      GO TO 4020
4030  DR    = DDR*DAKKR - DDC*DAKKI
      DDC   = DDR*DAKKI + DDC*DAKKR
      DDR   = DR
      MINDD = DMIN1 ( DABS(DAKK2), MINDD )
      IF ( CHLSKY .EQ. 0 ) GO TO 4040
      IF ( DAKK2 .LE. 0 ) GO TO 7001
      DAKK2 = DSQRT( DAKK2 )
4040  IF ( DAKKR .LT. 0. ) STURM = STURM + 1
CWKBD  7/95 SPR95005
C      IF ( DAKK2 .EQ. 0. ) GO TO 7002 
4050  ZRD( 1 ) = DAKKR
      ZRD( 2 ) = DAKKI
      NROWM1  = NROWS * 2 - 1
CWKBR 7/95 SPR95005
C      DAKK2   = 1.D0 / (DAKK2 )   
      DAKK2   = -1.D0 / (DAKK2 )   
      KVIDX   = KVIDX + 1
      DO 4150 I = 2, NROWM1, 2
CWKBDB 7/95 SPR95005
C      ZRD( I+1 ) = -1.0D0 * ( ZD( KVIDX+I-1 ) * DAKKR  +
C     &                          ZD( KVIDX+I ) * DAKKI  ) * DAKK2
C      ZRD( I+2 ) = -1.0D0 * ( ZD( KVIDX+I )   * DAKKR  -
C     &                        ZD( KVIDX+I-1 ) * DAKKI  ) * DAKK2
CWKBDE 7/95 SPR95005
CWKBIB 7/95 SPR95005
      ZRD( I+1 ) = ( ZD( KVIDX+I-1 ) * DAKKR  +
     &               ZD( KVIDX+I   ) * DAKKI  ) * DAKK2
      ZRD( I+2 ) = ( ZD( KVIDX+I   ) * DAKKR  -
     &               ZD( KVIDX+I-1 ) * DAKKI  ) * DAKK2
CWKBIE 7/95 SPR95005
4150  CONTINUE
      GO TO 5000
C
C NOW WRITE THE COLUMN OUT TO THE OUTPUT MATRIX
C
5000  CONTINUE
      ITWRDS      = 0
      MOBLK( 8 )  = -1
      MOBLK( 12 ) = KCOL
      KROW  = ZI( KRIDX )
      KROWS = ZI( KRIDX+ 1)
      IF ( KTYPE .LE. 2 ) NWDS = 1
      IF ( KTYPE .GT. 2 ) NWDS = 2
      IOL = 1
5050  CALL PUTSTR ( MOBLK )
      MOBLK( 4 ) = KROW
      MOBLK( 7 ) = MIN0 ( KROWS, MOBLK(6) )
      JSTR = MOBLK( 5 )
      NSTR = JSTR + (MOBLK( 7 ) - 1 ) * NWDS 
      IF ( KTYPE .GE. 3 ) NSTR = NSTR + 1
      IF ( KPREC .EQ. 2 ) GO TO 5200
C 
C MOVE REAL SINGLE AND SINGLE COMPLEX VALUES INTO BUFFER
C
5100  DO 5150 JJ = JSTR, NSTR
      XNS( JJ ) = ZRS( IOL  )
      IOL = IOL + 1
5150  CONTINUE
      ITWRDS = NSTR - JSTR + 1
      GO TO 5500
C
C MOVE REAL DOUBLE AND DOUBLE COMPLEX VALUES INTO BUFFER
C
5200  DO 5250 JJ = JSTR, NSTR
      XND( JJ ) = ZRD( IOL )
C      PRINT *,' SMCOUT,ROW,NUM,TERM=',MOBLK(4),MOBLK(7),XND(JJ)
      IOL = IOL + 1
5250  CONTINUE
      ITWRDS = ( NSTR-JSTR+1 ) * 2
5500  CONTINUE
C
C CHECK TO SEE IF ALL CONSECUTIVE ROWS CAN BE STORED IN THE BUFFER
C I.E., ARE THERE ENOUGH WORDS IN THE AVAILABLE STRING
C
      IF ( MOBLK( 7 ) .EQ. KROWS ) GO TO 5600
      ISTORE = MOBLK( 7 )
      KROWS  = KROWS - ISTORE
      KROW   = KROW  + ISTORE
      CALL ENDPUT ( MOBLK )
      GO TO 5050
C
C ALL OF THE CURRENT CONSECUTIVE ROWS WERE STORED IN THE BUFFER.
C GO AND GET THE NEXT SET OF CONSECUTIVE ROWS, IF ANY EXIST.
C
5600  KRIDX = KRIDX + 2
      IF ( KRIDX .GE. KRIDN ) GO TO 7000
      CALL ENDPUT ( MOBLK )
      KROW  = ZI( KRIDX )
      KROWS = ZI( KRIDX+1 )
      GO TO 5050
C
C ALL ROWS OF THIS COLUMN HAVE BEEN STORED, CLOSE OUT THE COLUMN
C
7000  MOBLK( 8 ) = 1
      CALL ENDPUT ( MOBLK )
      GO TO 7777
7001  WRITE ( NOUT, 9001 ) UFM, KCOL
9001  FORMAT(A23,' 3181, ATTEMPT TO PERFORM CHOLESKY DECOMPOSITION'
     &,' ON A NEGATIVE DEFINITE MATRIX IN SUBROUTINE SMCOMP.'
     &,/,' NEGATIVE DIAGONAL TERM FOUND ON COLUMN ',I6)
      IERROR = 4
      CALL MESAGE ( -61, 0, 0 )
7002  WRITE ( NOUT, 9002 ) UWM, KCOL, RZERO
9002  FORMAT(A25,' 2396, SMCOMP COMPUTED A ZERO ON THE DIAGONAL '
     &,'DURING DECOMPOSITION OF ROW NUMBER ',I6,'.',/
     &,' USE OF DIAG 22 OUTPUT SHOULD PERMIT YOU TO CORRELATE THE'
     &,' ROW WITH A MODEL D.O.F.',/,' A VALUE OF ',E13.6
     &,' WILL BE USED IN PLACE OF THE ZERO, HOWEVER',/
     &,' THE ACCURACY OF THE DECOMPOSITION MAY BE IN DOUBT.')
      AKK   = RZERO
      DAKK  = RZERO
      AKKR  = RZERO
      AKKI  = RZERO
      DAKKR = RZERO
      DAKKI = RZERO
      AKK2  = AKKR*AKKR   + AKKI*AKKI
      DAKK2 = DAKKR*DAKKR + DAKKI*DAKKI
CWKBIB 7/95 SPR95005
      GO TO ( 7010, 7020, 7030, 7040 ), KTYPE
7010  CONTINUE
      ZR( KVIDX ) = AKK    
      GO TO 1010
7020  CONTINUE
      ZD( KVIDX ) = DAKK   
      GO TO 2010
7030  CONTINUE
      ZR( KVIDX   ) = AKKR
      ZR( KVIDX+1 ) = AKKI
      GO TO 3010
7040  CONTINUE
      ZD( KVIDX   ) = DAKKR   
      ZD( KVIDX+1 ) = DAKKI   
      GO TO 4010
CWKBIE 7/95 SPR95005
CWKBD  7/95 SPR95005
C      GO TO ( 1050, 2050, 3050, 4050 ), KTYPE
7777  LLL( 6 ) = MAX0( LLL(6), ITWRDS )
      LLL( 7 ) = LLL( 7 ) + ITWRDS
      RETURN
      END
