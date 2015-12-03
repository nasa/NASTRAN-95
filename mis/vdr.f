      SUBROUTINE VDR
C
C     VDR IS THE CONTROL PROGRAM FOR THE VECTOR DATA RECOVERY MODULE
C
C                                                          OPHID
C                                  PHID                    OUDVC1
C                                  UDVF CLAMA              OUDV1
C             CASECC  EQDYN  USETD UDVT PPF          PHLD  OPHIH  OPNL1
C     VDR     CASEXX,HEQDYN,HUSETD,PHIH,TOL   ,XYCBD,PNLH /OUHVC1,HOPNL1
C                                  UHVT HTOL         HPNLD OUHV1
C                                  HUDVT                   HOUVD1
C
C                   TRANRESP     DIRECT
C              /C,N,FREQRESP/C,N,MODAL /V,N,SORT2/V,N,OUTPUT/V,N,SDR2
C                   CEIGN
C
C              /V,N,FMODE  $      PROGRAMMER'S MANUAL PP. 4.60-1 TRHU -7
C
C
      INTEGER         PNL   ,OUTFLE,OPNL1 ,APP   ,TRN   ,VDRREQ,SORT2 ,
     1                OUTPUT,SDR2  ,SSCELL,BUF   ,CASECC
      DIMENSION       NAM(2),BUF(50)      ,MASKS(6)     ,MCB(7),CEI(2),
     1                FRQ(2),TRN(2),MODAL(2)     ,DIRECT(2)
      COMMON /VDRCOM/ VDRCOM,IDISP ,IVEL  ,IACC  ,ISPCF ,ILOADS,ISTR  ,
     1                IELF  ,IADISP,IAVEL ,IAACC ,IPNL  ,ITTL  ,ILSYM ,
     2                IFROUT,IDLOAD,CASECC,EQDYN ,USETD ,INFILE,OEIGS ,
     3                PP    ,XYCDB ,PNL   ,OUTFLE,OPNL1 ,SCR1  ,SCR2  ,
     4                BUF1  ,BUF2  ,BUF3  ,NAM   ,BUF   ,MASKS ,CEI   ,
     5                FRQ   ,TRN   ,DIRECT,XSET0 ,VDRREQ,MODAL
      COMMON /BLANK / APP(2),FORM(2),SORT2,OUTPUT,SDR2  ,IMODE
      COMMON /SYSTEM/ DUMI(68),SSCELL
C
C     EXECUTE THE PHASES OF VDR.
C
      DO 10 I = 1,50
   10 BUF(I) = 0
      CASECC = 101
      OUTPUT = -1
      SORT2  = -1
      CALL VDRA
      IF (SSCELL .NE. 0) SDR2 = 1
      IF (VDRREQ .EQ. 0) RETURN
      MCB(1) = INFILE
      CALL RDTRL (MCB)
      IF (MCB(1) .NE. INFILE) GO TO 20
      CALL VDRB (INFILE,OUTFLE,IADISP)
   20 IF (APP(1) .NE. TRN(1)) RETURN
      MCB(1) = PNL
      CALL RDTRL (MCB)
      IF (MCB(1) .NE. PNL) RETURN
      CALL VDRB (PNL,OPNL1,IPNL)
      RETURN
      END
