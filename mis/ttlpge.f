      SUBROUTINE TTLPGE (TOPT)
C
C
      INTEGER         IDATE(3),CARD(20),TOPT,FCHAR
      CHARACTER       MCHTTL*28,VN*15,MCHNAM*11,MACHOS*7
      COMMON /CHMACH/ MCHNAM, MACHOS
      COMMON /MACHIN/ MACHX
      COMMON /SYSTEM/ KSYSTM(100)
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (KSYSTM( 2),NOUT), (KSYSTM(42),IDATE(1)),
     1                (KSYSTM( 9),NLPP), (KSYSTM(11),   IPAGE),
     2                (KSYSTM(91),LPCH)
C
C     ASSEMBLE MCHTTL AND VN LINE
C
      MCHTTL = ' '
      VN = ' ' 
      NCMNAM = INDEX(MCHNAM,' ') - 1
      IF (NCMNAM .LE. -1) NCMNAM = 11
      NCMOS  = INDEX(MACHOS,' ') - 1
      IF (NCMOS .LE. -1) NCMOS = 7
      FCHAR = (11 - NCMNAM)/2 + 1
      MCHTTL(FCHAR:FCHAR+NCMNAM+16) = MCHNAM(1:NCMNAM) // 
     1    ' COMPUTER SYSTEMS'
      FCHAR = (7 - NCMOS)/2 + 1
      VN(FCHAR:FCHAR+NCMOS+7) = MACHOS(1:NCMOS) // ' VERSION'
C
C     SET TOPT DEFAULT TO +2 FOR THE MAIN FRAMES, OR TO -1 FOR UNIX
C     BASE WORKSTATION
C
      IF (TOPT .NE. -9) GO TO 1
      TOPT = +2
      IF (MACHX.GE.6 .AND. MACHX.LE.11) TOPT = -1
   1  CONTINUE
C
C     BRANCH ON OPTION
C
C     TOPT = 1, PRINT ONE NASTRAN LOGO TITLE PAGE
C          = 2, PRINT TWO NASTRAN LOGO TITLE PAGES
C          = 3, PRINT DUMMY MESSAGE AND ONE SHORT TITLE PAGE
C          = 4, READ AND PRINT ONE LINE USER INPUT CARD AND PRINT ONE
C               NASTRAN SHORT TITLE PAGE
C          = 0, OR .GE.5, NO TITLE PAGE PRINTED
C          = NEGATIVE INTEGER, PRINT ONE NASTRAN SHORT TITLE PAGE
C
    5 IF (TOPT.NE.2  .AND. TOPT.NE.1) GO TO 110
C
C     TOPT = 1, OR 2
C
      DO 100 I = 1,TOPT
      IF (IPAGE.LE.0 .OR. I.EQ.2) WRITE (NOUT,10)
      IF (NLPP  .GT. 48) WRITE (NOUT,20)
      WRITE (NOUT,30) MCHTTL,VN
      WRITE (NOUT,50)
      WRITE (NOUT,60) IDATE(2),IDATE(3)
      WRITE (NOUT,70)
      WRITE (NOUT,75)
      WRITE (NOUT,80)
      WRITE (NOUT,85)
      WRITE (NOUT,90)
      WRITE (NOUT,95)
 10   FORMAT (1H1)
 20   FORMAT (///)
 30   FORMAT (34X,17(1HM),
     2       /28X,29(1HM),
     3       /25X,35(1HM),
     4       /22X,20(1HM),1X,20(1HM),22X,1H/,6X,A28,
     5       /20X,45(1HM),18X,2H//,9X,A20)
 40   FORMAT (1H+,93X,A4,10H VERSION -,I5,1HK)
 50   FORMAT (18X,16(1HM),2X,31(1HM),14X,3H///,
     7       /16X,53(1HM),10X,4(1H/),
     8       /14X,13(1HM),9X,35(1HM),6X,5(1H/))
 60   FORMAT (13X,12(1HM),2X, 9(1HM),2X,34(1HM),3X, 6(1H/),9X,
     *       3X,18HSYSTEM RELEASE  - , A3,A2,4H ED.)
 70   FORMAT (12X,12(1HM),1X,13(1HM),3X,15(1HM),2X,15(1HM),6(1H/),
     1       /11X,12(1HM),1X,17(1HM),2X,28(1HM),6(1H/),
     2       /10X,13(1HM),1X,19(1HM),2X,24(1HM),6(1H/),
     3       /9X,5(1HM),2X,7(1HM),1X,13(1HM),1X,7(1HM),2X,19(1HM),8(1H/)
     *,      2HMM,
     4       /9X,14(1HM),1X,23(1HM),2X,14(1HM),8(1H/),1H-,4(1HM),
     *       43X,1H*,1X,1H*,1X,1H*,
     5       /8X,16(1HM),1X,24(1HM),1X,9(1HM),9(1H/),2H--,7(1HM),
     *       41X,1H*,5X,1H*)
 75   FORMAT (8X,16(1HM),1X,25(1HM),2X,4(1HM),10(1H/),2H--,9(1HM),
     *       41X,1H*,2X,1HR,2X,1H*,
     7       /8X,16(1HM),1X,27(1HM),1X,1HM,8(1H/),4HMM--,11(1HM),
     *       41X,1H*,5X,1H*,
     8       /7X,8(1HM),4X,6(1HM),4X,5(1HM),5X,10(1HM),8X,4H//MM,11X,
     *       2HMM,3X,6(1HM),7X,5(1HM),8X,4(1HM),6X,4(1HM),2X,1H*,1X,1H*,
     *       1X,1H*,
     9       /7X,9(1HM),4X,6(1HM),2X,7(1HM),4X,6(1HM),14H///   /// M  M,
     *       25HM- MMM   MMM MMM  M   MMM,7X,4(1HM),9X,4(1HM),6X,2HMM)
 80   FORMAT (7X,9(1HM),5X,5(1HM),2X,6(1HM),3H  M,3X,8(1H/),3X,4(1HM),
     *       5H MM--,5(1HM),3X,7(1HM),21H  M    MMM     MM MMM,8X,
     *       5(1HM),5X,2HMM,
     1       /7X,9(1HM),2X,1HM,4X,5HMMM  ,4(1HM),6H// ///,3X,5(1H/),
     *       13HMMM   MMMM-- ,6(1HM),3X,7(1HM),41H  M    MMM     M   MMM
     *       MM MMMM   MM,
     2       /7X,9(1HM),2X,2HMM,4X,2HMM,3X,4(1H/),2X,3H///,4X,8(1HM),
     *       4X,4H--M ,7(1HM),3X,7(1HM),2X,1HM,3X,3HMMM,5X,2HMM,3X,
     *       4(1HM),6X,2HMM,2X,4(1HM),2X,2HMM)
 85   FORMAT (7X,9(1HM),2X,4(1HM),6X,11H/ /// ///MM,4X,8(1HM),4H---M,
     *       4X,6(1HM),3X,7(1HM),2X,6(1HM),6X,1HM,5X,4(1HM),5X,2HMM,
     *       4X,6(1HM),
     4       /7X,9(1HM),2X,5(1H/),5X,4H// M,11X,6(1HM),3H---,4(1HM),4X,
     *       5(1HM),3X,7(1HM),7H  M MMM,6X,11(1HM),5X,2HMM,5X,5(1HM),
     5       /7X,2HMM,7(1H/),2X,6(1HM),4X,3HMMM,2X,7(1HM),4X,7HMMM----,
     *       4HMMMM,4X,6HM MMMM,3X,7(1HM),8H  M  MMM,4X,2HMM,7X,
     *       4(1HM),4X,2HMM,6X,4(1HM),
     6       /5X,4(1H/),6(1HM),4X,7(1HM),2X,2HMM,4X,5(1HM),5X,5H----M,
     *       9X,6HMM MMM,5X,5(1HM),3X,2HMM,3X,2HMM,2X,4(1HM),5X,
     *       6(1HM),2X,4(1HM),7X,2HMM)
 90   FORMAT (3X,2H//,3X,26(1HM),1X,6(1HM),4(1H-),16(1HM),1X,15(1HM),
     *       6X,3HMMM,
     8       /8X, 27(1HM),7H MM----,19(1HM),1X,15(1HM),
     9       /8X, 27(1HM),3H---,23(1HM),1X,15(1HM),
     O       /9X, 24(1HM),7H---MM  ,22(1HM),1X,13(1HM),
     1       /9X, 22(1HM),2H--,6(1HM),4X,19(1HM),1X,5(1HM),2X,6(1HM),
     2       /10X,19(1HM),3H---,7(1HM),4X,19(1HM),1X,12(1HM),
     3       /11X, 9(1HM),1X,6(1HM),2H--,  33(1HM), 1X, 11(1HM),
     4       /12X,13(1HM),3H---,33(1HM),1X,11(1HM))
 95   FORMAT (13X,11(1HM),2H--, 22(1HM),2X, 9(1HM), 2X, 11(1HM),
     6       /14X, 8(1HM),2H--,26(1HM), 9X,12(1HM),
     7       /16X, 5(1HM),2H--,46(1HM),24X,14HDISTRIBUTED BY,
     8       /18X, 4HMM--,13(1HM),2X,30(1HM),
     9       /19X, 1H-,   45(1HM),5X,
     *       51HCOMPUTER SOFTWARE MANAGEMENT AND INFORMATION CENTER,
     *       9H (COSMIC),
     O       /18X,1H-,3X,41(1HM),26X,22HUNIVERSITY OF  GEORGIA,
     1       /17X,1H-,7X,35(1HM),29X,22HATHENS, GEORGIA  30602,
     2       /28X,29(1HM),
     3       /1X,14X,
     4       19X,17(1HM),28X,40HPHONE: (706)542-3265   FAX: (706)542-480
     5       ,1H7)
 100  CONTINUE
      GO TO 240
C
 110  IF (TOPT  ) 160,240,120
 120  IF (TOPT-4) 130,210,240
C
C     TOPT = 3
C
 130  WRITE  (NOUT, 10)
      WRITE  (NOUT,140)
 140  FORMAT (' THIS COMMENT CAN BE USED TO IDENTIFY LOCAL FIXES - ',
     1        'TO CHANGE, UPDATE DECK TTLPGE.')
      GO TO 160
C
C     TOPT = NEGATIVE (AND 3, AND 4)
C
 160  IF (IPAGE .LE. 0) CALL PAGE1
      WRITE  (NOUT,170) MCHTTL
      WRITE  (NOUT,180) VN,IDATE(2),IDATE(3)
      WRITE  (NOUT,190)
 170  FORMAT (//////34X,4H****, /32X,1H*,6X,1H*, /31X,1H*,8X,1H*,
     1       /31X,16H*  N A S T R A N,
     2       /31X,1H*,8X,1H*, /32X,1H*,6X,1H*, /34X,4H****,
     3       ///25X,A28)
 180  FORMAT(27X,A20,//26X,17HSYSTEM RELEASE - ,A3,A2, 4H ED.)
 190  FORMAT (/32X,'DISTRIBUTED BY', //9X,'COMPUTER SOFTWARE MANAGE',
     1       'MENT AND INFORMATION CENTER (COSMIC)', /17X,'UNIVERSITY ',
     2       'OF GEORGIA, ATHENS, GEORGIA 30602', /17X,
     3       'PHONE: (706)542-3265', 6X, 'FAX: (706)542-4807')
      GO TO 240
C
C     TOPT = 4
C
 210  WRITE  (NOUT,10)
      CALL XREAD (*240,CARD)
      WRITE  (NOUT,220) CARD
 220  FORMAT (1X,20A4)
      GO TO 160
C
C     CALL NSINFO TO PRINTOUT INSTALLATION-CENTER-TO-USER MESSAGES,
C     FROM THE THIRD SECTION OF THE NASINFO FILE
C
 240  CALL NSINFO (3)
C
      RETURN
      END