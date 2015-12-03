      SUBROUTINE STRSCN (S OR F)        
C        
C     THIS ROUTINE PERFORMS STRESS AND FORCE OUTPUT SCAN.        
C        
C     ACKNOWLEDGEMENT -        
C        
C     THIS ROUTINE WAS WRITTEN ORIGINALLY BY LOCKHEED/GEORGIA FOR USER  
C     DMAP-ALTER APPLICATION. IT WAS GREATLY MODIFIED BY G.CHAN/SPERRY  
C     SO THAT USERS CAN SPECIFY THE OUTPUT SCAN PARAMETERS FROM THE     
C     CASE CONTROL SECTION VIA THE SCAN INPUT CARD(S).  ONLY A VERY     
C     SMALL PORTION OF THE ORIGINAL PROGRAM REMAINS.  THE DMAP-ALTER    
C     APPLICATION IS STILL AVAILABLE TO THE USER        
C        
C     THIS ROUTINE IS CALLED ONLY BY SCAN        
C     IT DOES NOT OPEN NOR CLOSE ANY FILE        
C        
C     SCAN PARAMETER -        
C        
C          S OR F  - STRESS (1) OR FORCE (2) SCAN FLAG        
C          INFILE  - INPUT FILE, EITHER STRESS OR FORCE OUTPUT FILE     
C          OUFILE  - OUTPUT FILE FROM SCAN OPERATION, TO BE PRINTED     
C                    AGAIN BY OFP        
C          IOPT    - OPTION 1, SCAN BY AMAX-AMIN (.GT.AMAX AND .LT.AMIN)
C                    OPTION 2, SCAN BY NTOP-        
C                  . TOP N LARGEST (TENSION) AND SMALLEST (COMPRESSION) 
C                    IN STRESS SCAN.        
C                  . TOP N LARGEST ONLY IF NO COMPRESSION STRESS PRESENT
C                  . TOP N SMALLEST ONLY IF NO TENSION STRESS PRESENT   
C                  . TOP N VALUES SCAN FOR FORCES IF TOP N IS POSITIVE  
C                  . LEAST N VALUES SCAN FOR FORCES OR MARGIN (STRESS)  
C                    IF TOP N IS NEGATIVE        
C                  - IOPT IS INITIALIZED IN SCAN        
C                  - STRSCN WILL SET IOPT TO A NEGATIVE NUMBER IF INPUT 
C                    FILE IS NOT A STRESS OR FORCE FILE        
C          ISET    - A LIST OF ELEMENT IDS TO BE SCANNED        
C          IEL     - ELEMENT TYPE (CODE) TO BE SCANNED        
C          IELT    - ELEMENT NAME IN 2 BCD WORDS        
C          ICASE   - USED LOCALLY FOR SUBCASE NUMBER.        
C          SUBC    - CURRENT SUBCASE NO. USED IN SCAN AND STRSCN        
C          OSUBC   - SUBCASE NO. PROCESSED PREVIOUSLY        
C          ISORT   - SET LOCALLY TO 1 IF INPUT FILE DATA IS IN SORT1    
C                    TYPE FORMAT, TO 2 IF IN SORT2        
C          DEBUG   - LOCAL DEBUG FLAG, SET BY SCAN        
C          OEL     - ELEMENT TYPE PROCESSED PREVIOUSLY        
C        
C     SEE SUBROUTINE SCAN FOR MORE PARAMETER DEFINITIONS        
C        
C *** IF SCAN IS CALLED BY USER VIA DMAP ALTER, WE HAVE        
C        
C          ISET      =-2        
C          LBEG=LEND = 0, NOT USED        
C          LCSE1 AND = BEGINNING AND ENDING POINTERS TO AN ELEM. LIST   
C          LCSE2       (SORT1, ALL SUBCASES), OR A SUBCASE LIST (SORT2, 
C                      ALL ELEMS) IF THEY ARE GIVEN. OTHERWISE, LCSE1=-1
C                      AND LCSE2=0        
C        
C *** IF SCAN IS CALLED BY RIGID FORMAT, WE HAVE        
C        
C          ISET      =-1  IMPLIES THAT ALL ELEMENTS ARE TE BE SCANNED,  
C                         AND LBEG .GT. LEND        
C          ISET      = N  IMPLIES THAT ELEM. ID SET N IS REQUESTED. THE 
C                         SET DATA IS STORED IN IZ(LBEG) THRU IZ(LEND)  
C          ISET      = 0  NOT DEFINED        
C          LCSE1 AND =    ARE COMPONENT DUPLICATION FLAG (IDUPL) AND    
C          LCSE2          INCREMENT FLAG (INC)        
C                    = 0  IMPLIES NO DUPLICATION/INCR SET BY COMPONENT  
C          LCSE1     =-2  SET AND USE LOCALLY IF SORT2 AND ELEM. SET ARE
C                         INVOLVED.        
C          LBEG AND  =    ARE BEGINNING AND ENDING POINTERS TO THE ELEM.
C          LEND           ID SET, ALL ELEMS. (LBEG .GT. LEND IF ISET=-1)
C        
      LOGICAL         IOPEN,    JOPEN,    ANY,      DEBUG        
      LOGICAL         LAYERD,   STRESS,   FORCE
      CHARACTER*100   CHEAD
      CHARACTER*12    FIELD,    SCNFLD(6)
      INTEGER         OUFILE,   HEAD,     IHEAD(25),ID(50),   IZ(2),    
     1                EOR,      OEL,      NAM(2),   SORTX(3), ISCAN(10),
     2                SUBC,     OSUBC,    QUAD4,    TRIA3,    TOPN, 
     3                S OR F,   IBLANK         
      COMMON /OUTPUT/ HEAD(96)        
      COMMON /XSCANX/ INFILE,   OUFILE,   LCORE,    LBEG,     LEND,     
     1                IOPEN,    JOPEN,    IEL,      IOPT,     ISET,     
     2                ISORT,    ITRL3,    SUBC,     OSUBC,    OEL,      
     3                DEBUG,    LLOOP,    QUAD4,    TRIA3,    STRESS,    
     4                FORCE,    LAYERD
      COMMON /BLANK / IELT(2),  ICOMP,    TOPN,     AMAX,     AMIN,     
     1                LCSE1,    LCSE2,    ICOMPX        
      COMMON /SYSTEM/ IBUF,     NOUT,     SPACE(6), NLPP,     DUM,      
     1                NPAGE,    LINE        
      COMMON /NAMES / RD,       RDREW,    WRT,      WRTREW,   REW,      
     1                NOREW,    EOFNRW        
      COMMON /ZZZZZZ/ Z(2)        
      EQUIVALENCE     (IZ(1),Z(1))        
      EQUIVALENCE     (CHEAD, IHEAD(1))
      DATA            NAM,                SORTX                       / 
     1                4HSTRS,   4HCN  ,   4HSORT,   4H1   ,   4H2     / 
      DATA            T24,      EOR,      NOEOR,    IBLANK            /                       / 
     1                1.E+24,   1,        0    ,    1H                / 
C        
C *** SET ISCAN ARRAY FROM COMPONENT SPECIFICATION        
C        
      CHEAD = ' '
      NSCAN=0        
      NTOP =IABS(TOPN)        
C      PRINT *,' ENTERRING STRSCN,NTOP,ICOMP=',NTOP,ICOMP
      DO 20 I=1,30        
      J=2**(I-1)        
      IF (MOD(ICOMP,2*J) .LT. J) GO TO 10        
      NSCAN=NSCAN+1        
      IF (I .EQ. 1) GO TO 670        
      ISCAN(NSCAN)=I        
 10   IF (ICOMPX .EQ. 0) GO TO 20        
      IF (MOD(ICOMPX,2*J) .LT. J) GO TO 20        
      NSCAN=NSCAN+1        
      ISCAN(NSCAN)=I+31        
 20   CONTINUE        
C      PRINT *,' AFTER 20, ICOMP=',ICOMP
      J=2*J        
      IF (ICOMP .LT. J) GO TO 24        
      NSCAN=NSCAN+1        
      ISCAN(NSCAN)=31        
 24   IF (ICOMPX .LT. J) GO TO 26        
      NSCAN=NSCAN+1        
      ISCAN(NSCAN)=62        
 26   IF (ICOMPX .NE. 0) CALL SORT (0,0,1,1,ISCAN,NSCAN)        
C      DEBUG = .TRUE.
C      PRINT *,' AFTER 26,NSCAN=',NSCAN
C      PRINT *,' AFTER 26,ISCAN=',(ISCAN(KB),KB=1,NSCAN)
      IF (.NOT.DEBUG) GO TO 40        
      WRITE (NOUT,30) IOPEN,JOPEN,IELT,IEL,ISET,ICOMP,ICOMPX,LCSE1,     
     1                LCSE2,ISORT,SUBC,ITRL3,LBEG,LEND,NSCAN        
 30   FORMAT (//2X,12HDEBUG/STRSCN,/,2(2X,L1),2X,2A4,13I8)        
      IF (IOPT .EQ. 2) WRITE (NOUT,33) NTOP,(ISCAN(J),J=1,NSCAN)        
      IF (IOPT .EQ. 1) WRITE (NOUT,35) AMAX,AMIN,(ISCAN(J),J=1,NSCAN)   
 33   FORMAT (5X,I9,31I3)        
 35   FORMAT (5X,2E10.3,31I3)        
      IF (LEND .GT. LBEG) WRITE (NOUT,38) ISET,(IZ(J),J=LBEG,LEND)      
 38   FORMAT (/5X,3HSET,I8, (/5X,15I7))        
      IF (NSCAN .GT. 10) GO TO 590        
      IF (NSCAN .EQ.  0) GO TO 670        
C        
C *** INITIALIZATION        
C        
 40   IDUPL=LCSE1        
      INC  =LCSE2        
      JNC  =0        
      IF (ISET+1) 70,50,60        
 50   LCSE1=0        
      LCSE2=0        
      GO TO 70        
 60   LCSE1=IZ(LBEG)        
      LCSE2=IABS(IZ(LEND))        
 70   NS   =-1        
      IF (LCSE1 .GT. LCSE2) GO TO 640        
      IF (.NOT.IOPEN .OR. .NOT.JOPEN) GO TO 600        
C        
      LBUF1=1        
      LBUF3=0        
      LBUF0=LBUF1-1        
      IL2  =0        
      NREW =0        
      ICASE=-1        
      ANY  =.FALSE.        
      IF (OSUBC .EQ. 0) CALL FWDREC (*610,INFILE)        
      IF (ISET.EQ.-2 .OR. ISORT.EQ.2 .OR. SUBC.NE.OSUBC) GO TO 100      
      DO 80 I=1,3        
      CALL BCKREC (INFILE)        
 80   CONTINUE        
      GO TO 90        
C        
C *** READ INPUT FILE ID RECORD AND SET ISORT FLAG FOR SORT1 OR SORT2   
C     DATA TYPE        
C     AT THIS TIME, ISORT MAY BE ALREADY SET BY PREVIOUS SCAN, OR ZERO  
C        
 85   IF (ISORT.EQ.2 .OR. NREW.GE.2) GO TO 490        
      NREW=NREW+1        
      CALL REWIND (INFILE)        
 90   CALL FWDREC (*610,INFILE)        
 100  CALL READ (*85,*110,INFILE,ID,50,0,IWDS)        
      CALL READ (*610,*620,INFILE,HEAD,96,1,IWDS)        
      ISORT=1        
      IF (ID(2) .GE. 2000) ISORT=2        
      IF (ID(2) .GE. 3000) GO TO 500        
C        
C *** SYNCHRONIZE SUBCASE ID (WHICH MAY NOT BE IN ASCENDING ORDER)      
C        
      IF (ISET.EQ.-2 .OR. ISORT.EQ.2 .OR. ID(4).EQ.SUBC) GO TO 130      
 110  CALL REWIND (INFILE)        
      NREW=NREW+1        
      IF (NREW .GT. 2) GO TO 490        
 120  CALL FWDREC (*610,INFILE)        
      CALL READ (*610,*110,INFILE,ID,10,1,IWDS)        
      IF (ID(4) .NE. SUBC) GO TO 120        
      CALL BCKREC (INFILE)        
      GO TO 100        
C        
C *** SYNCHRONIZE ELEMENT TYPE (WHICH MAY NOT BE IN ASCENDING ORDER)    
C        
 130  IF (ID(3)-IEL) 90,140,90        
 140  OEL =IEL        
      NREW=0        
C        
C *** POSITION DATA BLOCK FOR FIRST CASE AND BEGIN SCAN        
C        
      I=140        
      IF (DEBUG) WRITE (NOUT,145) I,ISET,ISORT,ICASE,LCSE1,LCSE2,SUBC   
 145  FORMAT (/9X,12HDEBUG/STRSCN,I4,1H-,/2X,I9,11I7,3X,L1)        
      NWDS =ID(10)        
CWKBNB 1/3/94 SPR93010 & 93011
      LAYERD = .FALSE.
C FOR LAYERED QUAD4 AND TRIA3 IEL WILL BE EITHER 64 OR 83 RESPECTIVELY
C AND ID(10) WILL BE 10 (10 IS THE NUMBER OF WORDS PER LINE TO BE PRINTED). 
      IF ( (IEL .EQ. 64 .OR. IEL .EQ. 83) .AND. ID(10) .EQ. 10 )
     &         LAYERD = .TRUE.
      IF ( .NOT. LAYERD ) GO TO 144
C TO DETERMINE THE NUMBER OF WORDS PER EACH ELEMENT, WILL NEED TO DETERMINE
C HOW MANY LAYERS ARE PRESENT (Z(LBUF1+1)) AND ALLOW 10 WORDS PER LAYER
C PLUS A THREE WORD HEADER AND TWO EXTRA WORDS ON THE END.
      CALL READ (*610,*340,INFILE,IZ(LBUF1),3,0,IWDS)  
      NWDS = 3 + 10*IZ(LBUF1+1) + 2
C      PRINT *,' COMPUTED NWDS=',NWDS
      CALL BCKREC ( INFILE )
CWKBNE 1/3/94 SPR93010 & 93011
 144  NWDS1=NWDS+1        
      LBUF2=LBUF1+NWDS1        
      LBUF3=LBUF2        
      IH1  =LBUF2        
 146  IH2  =LBUF2+NWDS1*NTOP-1        
      IL1  =IH2+1        
      IL2  =IH2+NWDS1*NTOP        
      IF (IL2 .GT. LCORE) GO TO 660        
      II   =0        
      JJ   =0        
      KK   =0        
      MM   =IH1        
      NN   =IL1        
      IDELM=-1        
      ICASE=0        
      ANY  =.FALSE.        
      IF (LCSE1 .EQ. -2) LCSE1=0        
      IF (LCSE1 .GT. -2) NS=LBEG        
 150  NS=NS-1        
      NS=MIN0(NS,LBEG-1)        
      IF (ISORT.EQ.2 .AND. IDELM.NE.-1) GO TO 90        
 160  CALL READ (*610,*340,INFILE,IZ(LBUF1),NWDS,0,IWDS)        
C        
C *** CHECK WHETHER THIS ELEMENT IS NEEDED FOR SCAN        
C     WALK THROUGH SET ARRAY IF IT IS NECESSARY TO DO SO (R.F. ONLY)    
C     CHECK SUBCASE NO. INSTEAD OF ELEM. ID IF THIS IS A USER DAMP ALTER
C     RUN WITH SORT2 TYPE DATA        
C        
      IF (ISET  .NE. -2) GO TO 170        
      IF (LCSE1 .LE. -1) GO TO 200        
      ICASE=IZ(LBUF1)        
      IF (ISORT .EQ. 1) ICASE=ICASE/10        
      IF (ICASE .GE. LCSE1) IF (ICASE-LCSE2) 200,200,330        
      GO TO 160        
 170  IF (ISET.EQ.-1 .OR. LCSE1.EQ.-2) GO TO 200        
      IDELM=IZ(LBUF1)/10        
      IF (ISORT .EQ. 2) IDELM=ID(5)/10        
 180  NS=NS+1        
      IF (NS .GT. LEND) GO TO 330        
      IZN=IZ(NS)        
      IF (IZN .GE. 0) IF (IDELM-IZN) 150,190,180        
      IZN =IABS(IZN)        
      IF (IDELM .EQ. IZN) GO TO 190        
      IZN1=IZ(NS-1)        
      IF (IZN1.LE.0 .OR. IZN1.GT.IZN) GO TO 640        
      IF (IDELM .GT. IZN ) GO TO 180        
      IF (IDELM .LT. IZN1) GO TO 640        
      NS=NS-1        
 190  IF (ISORT .EQ. 2) LCSE1=-2        
C        
C *** MAKE SURE DEVICE CODE IS SET TO PRINT (SORT1 ONLY)        
C     SET UP COMPONENT DUPLICATION/INC LOOP IF THEY ARE VALID        
C        
 200  IF (ISORT .EQ. 1) IZ(LBUF1)=(IZ(LBUF1)/10)*10 + 1        
      I=200        
      IF (DEBUG) WRITE (NOUT,145) I,IZ(LBUF1),ICASE,LCSE1,LCSE2,IDUPL,  
     1                            INC,NS,ISORT,NWDS,ISET,SUBC,IOPT,ANY  
      JDUPL=1        
      JNC  =0        
      IF (ISET.EQ.-2 .OR. IDUPL.LE.0) GO TO 210        
      JDUPL=IDUPL        
      JNC  =INC        
C        
C *** PICKUP MAX AND MIN OF CURRENT ELEMENT DATA        
C     SAVE THESE MAX, MIN AS KEYS FOR SORTING LATER        
C        
 210  BMAX=-T24        
      BMIN= T24        
C QUAD4 (=64) AND TRIA3 (=83) WILL HAVE JDUPL NE 0 FOR LAMINATED
C CASE (I.E., WHEN LAYERD IS TRUE) FOR STRESS CASES
      IF ( ( IEL .EQ. 64 .OR. IEL .EQ. 83 ) .AND. JDUPL .EQ. 49
     &     .AND. .NOT. LAYERD .AND. STRESS ) GO TO 492
      IF ( ( IEL .EQ. 64 .OR. IEL .EQ. 83 ) .AND. JDUPL .NE. 49
     &     .AND.       LAYERD .AND. STRESS ) GO TO 492
CWKBNB 1/3/94 SPR93010 & 93011
C SET QUAD4 OR TRIA3 TO FALSE TO INDICATE TO SUBROUTINE SCAN THAT 
C DATA FOR THESE ELEMENTS HAS BEEN FOUND IN EITHER OES1 OR OES1L FILES.
      IF ( IEL .EQ. 64 ) QUAD4 = 1
      IF ( IEL .EQ. 83 ) TRIA3 = 1
C IF JDUPL IS 49 THAN THIS IS A QUAD4 OR TRIA3 LAYERED ELEMENT, GET
C VALUE AFTER ELEMENT ID IN RECORD TO DETERMINE THE NUMBER OF LAYERS IN
C IN THE ELEMENT.
      IF ( JDUPL .EQ. 49 ) JDUPL = IZ(LBUF0+2)
CWKBNE 1/3/94 SPR93010 & 93011
C      PRINT *,' BEFORE 230,JDUPL,JNC,NSCAN=',JDUPL,JNC,NSCAN
C      PRINT *,' BEFORE 230,ISCAN=',(ISCAN(KB),KB=1,NSCAN)
      DO 230 J=1,NSCAN        
      I=ISCAN(J)        
      IF (I .GT. NWDS) GO TO 230        
      KK=0        
      DO 220 K=1,JDUPL        
C      WRITE(6,77777)Z(LBUF0+I+KK)
C77777 FORMAT(' HEX OF Z=',Z8)
      ZK=Z(LBUF0+I+KK)        
      IF (ZK .GT. BMAX) BMAX=ZK        
      IF (ZK .LT. BMIN) BMIN=ZK        
 220  KK=KK+JNC        
 230  CONTINUE        
C        
      IF (IOPT .EQ. 2) GO TO 250        
C        
C *** OPTION ONE (IOPT=1, BY MAX-MIN)        
C     ===============================        
C        
C     LBUF2 AND LBUF3 ARE BEGINNING AND ENDING POINTERS TO THE SCANNED  
C     DATA ARRAY        
C        
      IF (BMAX.LT.AMAX .AND. BMIN.GT.AMIN) GO TO 160        
      IF (LBUF3+NWDS1 .GT. LCORE) GO TO 630        
      ANY=.TRUE.        
      DO 240 I=1,NWDS        
      Z(LBUF3+I)=Z(LBUF0+I)        
 240  CONTINUE        
      Z(LBUF3)=BMAX        
      IF (BMIN .LE. AMIN) Z(LBUF3)=BMIN        
      LBUF3=LBUF3+NWDS1        
      GO TO 160        
C        
C *** OPTION TWO (IOPT=2)        
C     ===================        
C        
C     TOP AND BOTTOM N VALUES FOR STRESSES        
C     TOP VALUE SCAN FOR FORCES IF TOPN IS POSITIVE        
C     BOTTEM VALUE SCAN FOR FORCES AND MARGIN ETC. IF TOPN IS NEGATIVE  
C        
C     II AND JJ ARE TOP AND BOTTOM ARRAY COUNTERS        
C     MM IS POINTER TO THE SMALLEST OF THE TOP VALUSES        
C     NN IS POINTER TO THE BIGGEST OF THE BOTTOM VALUSES        
C        
C     WHEN TOP AND BOTTOM ARRAYS ARE FILLED UP COMPLETELY WITH SCANNED  
C     DATA (II=JJ=NTOP), IH1 AND IH2 ARE BEGINNING AND ENDING POINTERS  
C     TO THE TOP VALUES, SIMILARY, IL1 AND IL2 ARE FOR THE BOTTOM VALUES
C        
C     REMEMBER, SORF=1 FOR STRESS SCAN, SORF=2 FOR FORCE SCAN        
C               NTOP=IABS(TOPN)        
C        
 250  ANY=.TRUE.        
      IF ( SORF.EQ.2 .AND. TOPN.LE.0) GO TO 290        
      IF ((SORF.EQ.1 .AND. BMAX.LT.0.0) .OR.        
     1   (II.GE.NTOP .AND. BMAX.LT.Z(MM))) GO TO 290        
      DO 260 I=1,NWDS        
 260  Z(MM+I)=Z(LBUF0+I)        
      Z(MM)=BMAX        
      IF (II .GE. NTOP) GO TO 270        
      II=II+1        
      MM=MM+NWDS1        
      IF (II .LT. NTOP) GO TO 290        
 270  MM  =IH1        
      BMAX=+T24        
      DO 280 I=IH1,IH2,NWDS1        
      IF (Z(I) .GT. BMAX) GO TO 280        
      BMAX=Z(I)        
      MM  =I        
 280  CONTINUE        
C        
 290  IF ( SORF.EQ.2 .AND. TOPN.GE.0) GO TO 160        
      IF ((SORF.EQ.1 .AND. BMIN.GT.0 .AND. TOPN.GT.0) .OR.        
     1   (JJ.GE.NTOP .AND. BMIN.GT.Z(NN))) GO TO 160        
      DO 300 I=1,NWDS        
 300  Z(NN+I)=Z(LBUF0+I)        
      Z(NN)=BMIN        
      IF (JJ .GE. NTOP) GO  TO 310        
      JJ=JJ+1        
      NN=NN+NWDS1        
      IF (JJ .LT. NTOP) GO TO 160        
 310  NN  =IL1        
      BMIN=-T24        
      DO 320 I=IL1,IL2,NWDS1        
      IF (Z(I) .LT. BMIN) GO TO 320        
      BMIN=Z(I)        
      NN  =I        
 320  CONTINUE        
      GO TO 160        
C        
C *** ELEM. ID LIST, OR SUBCASE LIST, HAS BEEN EXHAULSTED        
C     (NOTE - SHOULD RETURN WITHOUT FWDREC HERE.  IF STRSCN IS CALLED   
C             AGAIN, FWDREC WILL BE DONE AT 90)        
C        
 330  I=330        
      IF (DEBUG) WRITE (NOUT,145) I,IDELM,ICASE,ISORT,NS,LBEG,LEND,     
     1                            LCSE1,LCSE2        
      IF (ANY) GO TO 350        
      GO TO 510        
C        
C *** EOR READ (FROM 160)        
C        
 340  ID(11)=0        
      IF (ANY) GO TO 350        
      ID(11)=1        
      ID(10)=1        
      NWDS  =1        
      IL2   =IH1+1        
      IZ(IL2)=01        
      IF (ISORT .EQ. 2) IZ(IL2)=0        
      IZ(  2)=1        
C        
C *** THIS ELEMENT TYPE IS DONE.  BEGIN OUTPUT PROCEDURE        
C     MAKE SURE DEVICE CODE IS SET TO PRINT, ALWAYS        
C     ADD SCAN HEADER TO LABEL LINE        
C        
 350  ID(1)=(ID(1)/10)*10 + 1        
      IF (ISORT .EQ. 2) ID(5)=(ID(5)/10)*10 + 1        
      CALL WRITE (OUFILE,ID(1),50,NOEOR)        
      GO TO 530        
 360  IF (IOPT .EQ. 1) GO TO 370        
      WRITE(CHEAD(69:100), 8004 ) NTOP
8004  FORMAT('TOP AND BOTTOM  ',I4,' VALUES')
      GO TO 380        
 370  WRITE(CHEAD(69:100), 8005 ) AMIN, AMAX
8005  FORMAT('EXCLUDING TO ',2(F8.1))
 380  CONTINUE        
      HEAD(73)=IBLANK        
      DO 400 I = 1, 25
400   HEAD(I+64) = IHEAD(I)
      HEAD(95)=SORTX(1)        
      HEAD(96)=SORTX(2)        
      IF (ISORT .EQ. 2) HEAD(96)=SORTX(3)        
      CALL WRITE (OUFILE,HEAD,96,EOR)        
C        
      KK=1        
      J =2        
      IF (.NOT.ANY) GO TO 460        
C        
C *** (IOPT=2 ONLY) IF TOP AND BOTTOM ARRAYS ARE NOT FULL (I.E. II AND/ 
C     OR JJ ARE  .LT. NTOP), WE NEED TO SQUEEZE OUT SOME EMPTY CELLS IN 
C     THE SPACE FROM Z(IH1) THRU Z(IL2) BEFORE SORTING THE SCANNED DATA 
C        
      IF (IOPT .EQ. 2) GO TO 410        
      IL2=LBUF3        
      GO TO 430        
 410  IF (II+JJ .EQ. 2*NTOP) GO TO 430        
      KK =(NTOP-II)*NWDS1        
      IL2=IH2+JJ*NWDS1        
      DO 420 I=IL1,IL2        
 420  Z(I-KK)=Z(I)        
      IL2=IH1+(II+JJ)*NWDS1-1        
C        
C *** MOVE MAX-MIN KEYS BEHIND IL2 SPACE AND BEGIN A 2-COLUMN SORT      
C     THUS AVOID MASSIVE DATA TRANSFER DURING SORTING IF THE ORIGINAL   
C     MULTI-COLUMNS SCANNED DATA WERE USED.        
C        
 430  KK=(IL2-IH1+1)/NWDS1        
      IF (IL2+2*KK .GT. LCORE) IF (IOPT-1) 630,630,660        
      I =IH1        
      J =IL2-2        
      K =0        
 440  J =J+2        
      K =K+1        
      Z (J+1)=Z(I)        
      IZ(J+2)=K        
      I =I+NWDS1        
      IF (I .LT. IL2) GO TO 440        
      K =2*KK        
      CALL SORTF (0,0,2,1,Z(IL2+1),K)        
C        
C *** BEGIN OUTPUT SCANNED DATA        
C        
      J =J+2        
      IF (DEBUG) WRITE (NOUT,450) J,KK,(Z(IL2+I),IZ(IL2+I+1),I=1,K,2)   
 450  FORMAT (/9X,17HDEBUG/STRSCN 450-,2I7,(/15X,E11.3,I5))        
 460  DO 470 K=1,KK        
      I =IH1+(IZ(J)-1)*NWDS1        
      CALL WRITE (OUFILE,IZ(I+1),NWDS,NOEOR)        
 470  J =J-2        
      CALL WRITE (OUFILE,0,0,EOR)        
      ITRL3=ITRL3+2        
      J    =KK*NWDS        
      IF (DEBUG) WRITE (NOUT,750) J,ITRL3,II,JJ        
      IF (.NOT.ANY) GO TO 680        
C        
C*** EOF ON INPUT FILE (FROM 100)        
C     NEXT ACTION WILL BE LOOP-BACK FOR MORE OR RETURN TO SCAN        
C        
C           R.F. (ISET.NE.-2)        I     USER DMAP ALTER (ISET=-2)    
C     -------------------------------+----------------------------------
C     SORT1 - RETURN TO SCAN FOR     I  SORT1 - LOOP BACK FOR NEXT SUB- 
C             NEXT SUBCASE           I          CASE, DISREGARDING THE  
C                                    I          ELEM ID LIST        
C     SORT2 - LOOP BACK FOR NEXT     I  SORT2 - LOOP BACK FOR NEXT ELEM,
C             ELEM. IF NO ELEM. LIST I          DISREGARDING THE SUBCASE
C           - IF ELEM. LIST EXISTS,  I          LIST        
C             LOOP BACK ONLY IF MORE I        
C             ELEM. TO BE PROCESSED  I        
C             OTHERWISE, RETURN      I        
C        
 480  IF (IL2 .LE. 0) GO TO 510        
      IL2 =-1        
      NREW=0        
      IF (ISET  .EQ. -2) GO TO 100        
      IF (ISORT .EQ.  1) GO TO 510        
      IF (LEND .GT. LBEG) IF (NS-LEND) 100,510,510        
      GO TO 100        
C        
C *** COULD NOT FIND ELEMENT OR SUBCASE        
C        
 490  IF (IL2 .NE. 0) GO TO 510        
CWKBNB 1/4/94 SPR93010 & 93011
 492  IF ( IEL .EQ. 64 .AND. QUAD4 .EQ. 0 ) QUAD4 = -1
      IF ( IEL .EQ. 83 .AND. TRIA3 .EQ. 0 ) TRIA3 = -1
      IF ( IEL .EQ. 64 .OR. IEL .EQ. 83 ) GO TO 510
CWKBNE 1/4/94 SPR93010 & 93011
      CALL FNAME (INFILE,Z(1))        
      WRITE (NOUT,710) IELT,Z(1),Z(2),NREW        
      GO TO 510        
C        
C *** JOB DONE        
C        
 500  IOPT=-ID(2)        
 510  DO 520 I=1,16        
 520  HEAD(I+73)=IBLANK        
      HEAD(  95)=IBLANK        
      HEAD(  96)=IBLANK        
      OSUBC=SUBC        
      RETURN        
C        
C *** INTERNAL ROUTINE TO SYNTHESIZE THE COMPONENTS FOR HEADING  
C        
 530  IF (JNC .LE. 0) GO TO 550        
550   NUMFLD = 0
C      PRINT *,' STRSCN,INC,IDUPL,NSCAN=',INC,IDUPL,NSCAN
C      PRINT *,' ISCAN=',(ISCAN(KB),KB=1,NSCAN)
      DO 580 I = 1, NSCAN
C      PRINT *,' STRSCN CALLING STRNAM,I,ISCAN=',I,ISCAN(I)
      IF ( STRESS ) CALL STRNAM ( IEL, ISCAN(I), FIELD )
      IF ( FORCE  ) CALL FORNAM ( IEL, ISCAN(I), FIELD )
      IF ( FIELD  .EQ. ' ' ) GO TO 580
      IF ( NUMFLD .EQ. 0   ) GO TO 570
      DO 560 K = 1, NUMFLD
      IF ( FIELD .EQ. SCNFLD( K ) ) GO TO 580
560   CONTINUE
570   IF ( NUMFLD .GE. 6 ) GO TO 585
      NUMFLD = NUMFLD + 1
      SCNFLD( NUMFLD ) = FIELD
580   CONTINUE
585   IF ( NUMFLD .EQ. 1 ) CHEAD(1:19) = 'SCANNED BY FIELD:  '
      IF ( NUMFLD .NE. 1 ) CHEAD(1:19) = 'SCANNED BY FIELDS: '
      ISTR = 20
      DO 588 I = 1, NUMFLD
      LEN = INDEX( SCNFLD(I), ' ' )
      IEND = ISTR + LEN - 1
      IF ( IEND .GT. 51 ) GO TO 587
      IF ( I .EQ. 1 ) CHEAD( ISTR:IEND ) = SCNFLD(I)(1:LEN) 
      IF ( I .GT. 1 ) CHEAD( ISTR:IEND+2 ) = ', '//SCNFLD(NUMFLD)(1:LEN)
      ISTR = IEND 
      IF ( I .GT. 1 ) ISTR = IEND + 2
      GO TO 588
587   CHEAD( ISTR:51) = ',...'
      GO TO 589
588   CONTINUE
589   CONTINUE
      IF ( ISET .LE. 0 ) GO TO 360
      WRITE ( CHEAD(52:68), 8001 ) ISET
8001  FORMAT(' SET:',I8 )
      GO TO 360        
C        
C *** FILE ERRORS        
C        
 590  WRITE (NOUT,720) IELT        
      GO TO 670        
 600  WRITE (NOUT,700) IOPEN,JOPEN        
      GO TO 510        
 610  IF (.NOT.ANY) GO TO 680        
      J=-2        
      GO TO 650        
 620  J=-3        
      GO TO 650        
 630  WRITE (NOUT,730) IELT        
      GO TO 510        
 640  WRITE (NOUT,740) ISET,LCSE1,LCSE2,LBEG,LEND,NS,(IZ(J),J=LBEG,LEND)
      GO TO 510        
 650  CALL MESAGE (J,INFILE,NAM)        
      GO TO 510        
 660  J=(LCORE-LBUF2+1)/(2*NWDS1)        
      WRITE (NOUT,760) IELT,NTOP,J        
      NTOP=J        
      GO TO 146        
 670  WRITE (NOUT,770) ICOMP,ICOMPX,IELT        
      GO TO 510        
 680  IF (DEBUG) WRITE (NOUT,780) IELT,SUBC        
      CALL MESAGE (30,220,IELT)        
      GO TO 480        
C        
 700  FORMAT (//5X,52HSYSTEM ERROR/STRSCN.  INPUT OR OUTPUT FILE NOT REA
     1DY, 2(2X,L1))        
 710  FORMAT (//5X, 8HELEMENT ,2A4,32H, OR SUBCASE, NOT IN DATA BLOCK , 
     1  2A4,I7,8H REWINDS)        
 720  FORMAT (//5X,34HTOO MANY COMPONENTS SPECIFIED FOR ,2A4)        
 730  FORMAT (//5X,40HINSUFFICIENT CORE TO PROCESS OUTPUT SCAN,        
     1 /5X,56HSMALL VALUES OF AMAX-AMIN REQUIRE LARGE CORE REQUIREMENT) 
 740  FORMAT (//5X,23HSYSTEM ERROR/STRSCN 740,7X,6I7, /,(5X,12I10))     
 750  FORMAT (/,I9,37H WORDS WRITTEN TO OUTPUT FILE, RECORD,I5,9X,2I5)  
 760  FORMAT (//5X,45HINSUFFICIENT CORE TO PROCESS OUTPUT SCAN FOR ,2A4,
     1 /5X,89HLARGE TOPN VALUE REQUIRES EXCESSIVE CORE REQUIREMENT.  TOP
     2N IS AUTOMATICALLY REDUCED FROM,I5,3H TO,I5)        
 770  FORMAT (//5X,40HFIELD COMPONENT ERROR, CASE ABORT/STRSCN,5X,2I9,  
     1  1X,2A4)        
 780  FORMAT (//5X,37HNO APPLICABLE ELEMT OR SUBCASE/STRSCN,3X,2A4,I8)  
      END        
