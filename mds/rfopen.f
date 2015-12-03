      SUBROUTINE RFOPEN (MEMBER,LU)        
C        
C     THIS .MIS ROUTINE OPENS THE RIGID FORMAT FILE, AS AN ORDINARY     
C     FORTRAN FILE. USE REGULAR FORTRAN READ TO READ THE FILE        
C        
C     ENTRY POINT RFCLSE TO CLOSE IT        
C        
C     IF RIGID FORMAT FILE OPENS OK, LU IS THE FORTRAN UNIT NUMBER      
C     OTHERWISE, LU = 0        
C        
C     THIS ROUTINE REPLACES ALL THE MACHINE DEPENDENT DSXOPN, DSXCLS,   
C     DSXREA, AND DSXFRE ROUTINES. PLUS DSXRDS, DSXIO, AND DSXSIO IN    
C     IBM VERSION, AND DSXRET AND DSXZER IN CDC        
C        
C     NOTE - FORTRAN UNIT 'IN' IS USED TO READ THE RIGID FORMAT FILE.   
C            UNIT 'IN' IS SYNCHRONOUS WITH ANY READFILE OR NESTED       
C            READFILE OPERATION.        
C        
C     WRITTEN BY G.CHAN/UNISYS.   10/1990        
C        
      INTEGER         MEMBER(2),FACSF        
      CHARACTER*1     BK,MB1(8)        
      CHARACTER       MB5*5,MB6*6        
      CHARACTER*8     MB8,FREE8,ADD(3)        
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25        
CWKBI 
      CHARACTER*44    RFDIR, DSN
      COMMON /XMSSG / UFM,UWM,UIM,SFM        
      COMMON /MACHIN/ MACH        
      COMMON /XXREAD/ IN        
      COMMON /SYSTEM/ IBUF,NOUT,NOGO        
      EQUIVALENCE     (MB1(1),MB5,MB6,MB8)        
      DATA    BK,     ADD(1),   ADD(3), FREE8     /        
     1        ' '   , '@ADD,E ',' .  ', '@FREE   '/        
C        
      CALL A42K8 (MEMBER(1),MEMBER(2),MB8)        
      IF (MACH .EQ. 3) GO TO 30        
      IN = IN + 1        
      IF (IN .LT. 60) IN = 60        
      J  = 5        
      IF (MB1(6) .NE. BK) J = 6        
C        
C           DUMMY  IBM  UNVC  CDC  VAX  ULTRIX  SUN   AIX   HP        
C             S/G  MAC  CRAY CNVX  NEC  FUJTSU   DG  AMDL PRIME        
C             486 DUMMY ALFA RESV        
C            ---- ----  ---- ---- ----  ------ ----  ---- -----        
      GO TO (  60,  50,   30,  50,  50,     50,  50,   50,  50,        
     1         50,  70,   70,  70,  70,     70,  70,   70,  70,        
     2         50,  60,   50,  70), MACH        
C        
C     UNIVAC ONLY -        
C     ADD FILE TO INPUT STREAM        
C        
 30   ADD(2) = MB8        
      J = FACSF(ADD)        
      LU = 5        
      GO TO 130        
50    CONTINUE
      RFDIR = ' '
      CALL GETENV ( 'RFDIR', RFDIR )
      DO 55 I = 44, 1, -1
      IF ( RFDIR( I:I ) .EQ. ' ' ) GO TO 55
      LENR = I
      GO TO 56
55    CONTINUE
      LENR = 44
56    DSN = ' '
      DSN = RFDIR(1:LENR) // '/' // MB6
CWKBR IF (J .EQ. 6) OPEN (UNIT=IN,FILE=MB6,ACCESS='SEQUENTIAL',ERR=100, 
      OPEN (UNIT=IN,FILE=DSN,ACCESS='SEQUENTIAL',ERR=100, 
     1                    FORM='FORMATTED',STATUS='OLD')        
      GO TO 80        
C        
C     OTHERS -        
C        
 60   GO TO 100        
C        
 70   OPEN (UNIT=IN,FILE=MB8,ACCESS='SEQUENTIAL',ERR=100,STATUS='OLD',  
     1      FORM='FORMATTED')        
C        
C     VERIFY FILE EXISTANCE        
C        
 80   READ (IN,90,ERR=100,END=100) J        
 90   FORMAT (A1)        
      REWIND IN        
      LU = IN        
      GO TO 130        
C        
CWKBR100  WRITE  (NOUT,110) SFM,MB8        
 100  WRITE  (NOUT,110) SFM,DSN        
CWKBR 110  FORMAT (A25,', RFOPEN CAN NOT OPEN ',A8)        
 110  FORMAT (A25,', RFOPEN CAN NOT OPEN ',A44)        
C        
      IF (MACH.GT.7 .AND. MACH.NE.21) WRITE (NOUT,120) MACH        
 120  FORMAT (5X,'MACHINE',I4,' IS NOT AVAILABLE/RFOPEN')        
      LU   = 0        
      NOGO = 1        
C        
 130  RETURN        
C        
C        
      ENTRY RFCLSE (LU)        
C     =================        
C        
      IF (MACH .EQ. 3) GO TO 150        
      IF (LU  .LT. 60) WRITE (NOUT,140) SFM,LU        
 140  FORMAT (A25,'. RFCLSE/RFOPEN ERROR.  LU =',I4)        
      CLOSE (UNIT=LU)        
      IN = IN - 1        
      IF (IN .LT. 60) IN = 0        
      GO TO 160        
C        
 150  ADD(1) = FREE8        
      J = FACSF(ADD)        
 160  LU = 0        
      RETURN        
      END        
