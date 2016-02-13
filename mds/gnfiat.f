        SUBROUTINE GNFIAT
C
C
C    FORMAT OF THE MEMBER DATASET FILE CONTROL BLOCK (MDSFCB)
C          (ONE ENTRY FOR EVERY FILE)
C   0             8            16             24                 31
C   ***************************************************************
C 1 *                      OPEN FLAG                              *
C   ***************************************************************
C 2 *                     CURRENT  DSN                            *
C   ***************************************************************
C 3 *        PREVIOUS DSN       *          NEXT DSN               *
C   ***************************************************************
C
C   FORMAT OF THE FCB
C   ***************************************************************
C 1 *                    OPEN FLAG (0 - READ, 1 - WRITE )         *
C   ***************************************************************
C 2 *                   BUFFER ADDRESS                            *
C   ***************************************************************
C 3 *               CURRENT LOGICAL RECORD (CLR)                  *
C   ***************************************************************
C 4 *                 CURRENT BLOCK NUMBER                        *
C   ***************************************************************
C 5 *              FIRST BLOCK NUMBER ON EXTERNAL FILE            *
C   ***************************************************************
C 6 *               LAST BLOCK NUMBER ON EXTERNAL FILE            *
C   ***************************************************************
C 7 *          NUMBER OF BLOCKS ALLOCATED TO THIS FILE            *
C   ***************************************************************
C 8 *     FLAG FOR WRITING THE FIRST COLUMN ON FILE (0-NO, 1=YES) *
C   ***************************************************************
C 9 *              INDEX TO FIRST IN-MEMORY BLOCK                 *
C   ***************************************************************
C10 *               INDEX TO LAST IN-MEMORY BLOCK                 *
C   ***************************************************************
C11 *             INDEX TO CURRENT IN-MEMORY BLOCK                *
C   ***************************************************************
C12 *            ORIGINAL BUFFER ADDRESS (ON OPEN)                *
C   ***************************************************************
C13 *                    DMAP FILE NAME                           *
C14 *                                                             *
C   ***************************************************************
C15 *               OPEN FLAG FOR EXTERNAL FILE                   *
C   ***************************************************************
C16 *           TOTAL NUMBER OF STRINGS IN THIS MATRIX            *
C   ***************************************************************
C17 *             TOTAL NUMBER OF TERMS IN THIS MATRIX            *
C   ***************************************************************
C
C
C
C
C                          I/O BUFFER FORMAT
C   ***************************************************************
C 1 *                       DMAP FILE NAME                        *
C   ***************************************************************
C 2 *                             CBP                             *
C   ***************************************************************
C 3 *                             CLR                             *
C   ***************************************************************
C 4 *                        BLOCK NUMBER                         *
C   ***************************************************************
C 5 *                             LCW                             *
C   ***************************************************************
C 6 *          I/O BUFFER (4 THRU NBUFF+3 ARE WRITTEN)            *
C   ***************************************************************
C   *                                                             *
C   ***************************************************************
C
C
C
C
C                        I/O BUFFER CONTROL WORDS
C  DEFINITION WORD        0         8        16         24       31
C                         *****************************************
C   RECORD HEADER         *  '11'   *  FLAG   *  NUMBER OF WORDS  *
C                         *****************************************
C   RECORD TRAILER        *  '77'   *  FLAG   *        CLR        *
C                         *****************************************
C   STRING DATA           *  '22'   *  FLAG   *  NUMBER OF WORDS  *
C                         *****************************************
C   EOB STRING            *  '7F'   *  FLAG   *                   *
C                         *****************************************
C   COLUMN HEADER         *  '3B'   *         *  FORMAT  *  TYPE  *
C                         *****************************************
C                         *            COLUMN NUMBER              *
C                         *****************************************
C   COLUMN TRAILER        *  '3F'   *         *  FORMAT  *  TYPE  *
C                         *****************************************
C                         *            COLUMN NUMBER              *
C                         *****************************************
C   STRING HEADER         *  '4B'   *         *  NUMBER OF TERMS  *
C                         *****************************************
C                         *             ROW NUMBER                *
C                         *****************************************
C   STRING TRAILER        *  '4E'   *         *  NUMBER OF TERMS  *
C                         *****************************************
C                         *             ROW NUMBER                *
C                         *****************************************
C   DUMMY STRING          *  'DD'   *                             *
C                         *****************************************
C   END OF BLOCK          *  'EB'   *                             *
C                         *****************************************
C                         *  'EF'   *                             *
C                         *****************************************
C
C          FLAG   =  C-COMPLETE, E-EXTENDED, F-FURTHER EXTENDED
C          TYPE   =  1-RSP, 2-RDP, 3-CSP, 4-CDP
C          FORMAT =  1-TRAILERS, 0-NO TRAILERS
C
*    IPERM OF /SYSTEM/ HAS BITS DESIGNATED FOR THE FOLLOWING FILES
*
*               BIT                     FILE
*               7                       INPT
*               8-16                    INP1-INP9
*
* //////////////////////////////////////////////////////////////////
*
*     PERMANENT FILES IN /XXFIAT/ ARE ALLOCATED ACCORDING TO THE
*     FOLLOWING:
*
*       XFIAT(1) = UNIT FOR POOL = 22
*       XFIAT(2) = UNIT FOR OPTP = 7
*       XFIAT(3) = UNIT FOR NPTP = 8
*       XFIAT(8) = UNIT FOR INPT = 16
*       XFIAT(9) = UNIT FOR INP1 = 17
*       XFIAT(10)= UNIT FOR INP2 = 18
*       XFIAT(11)= UNIT FOR INP3 = 19
*       XFIAT(12)= UNIT FOR INP4 = 20
*       XFIAT(13)= UNIT FOR INP5 = 21
*       XFIAT(18)= UNIT FOR XPTD = 9
*
*              FORTRAN UNITS ARE ASSIGNED AS FOLLOWS:
*
*                 PUNCH = 1
*                 LINK  = 2
*                 LOG   = 3
*                 RDICT = 4
*                 INPUT = 5
*                 OUTPUT= 6
*                 PLOT  = 10
*                 UT1   = 11
*                 UT2   = 12
*                 UT3   = 13
*                 UT4   = 14
*                 UT5   = 15
*                 SOF   = 90
* /////////////////////////////////////////////////////////////////
C
        INCLUDE           'NASNAMES.COM'
        INCLUDE           'DSIOF.COM'
        INCLUDE           'GINOX.COM'
        COMMON / XFIAT  / IFUFA   , IFMXE   , IFCAE   , FIAT(640)
        COMMON / XPFIST / NPFIST
        COMMON / XXFIAT / XFIAT(19)
        COMMON / SYSTEM / ISYSBF  , DUM1(43), IPERM  , DUM2(110),
     *                    INMBLK
        INTEGER*2         IUNIT
        COMMON / DSUNIT / IUNIT(220)
        INTEGER           FIAT    , XFIAT, ANDF
C
        EQUIVALENCE       (DUM1(1), NOUT)
C
        CALL DSIODD
        IFUFA   = 0
        IDSLIM = INMBLK
        NUMBLK = 1
        IF( LENWPB .NE. 0 ) NUMBLK = ISYSBF / LENWPB
        DO 15 I = 1, NUMSOF
        LENSOF( I ) = 0
   15   CONTINUE
        DO 16 I = 1, MAXFCB
        MDSFCB( 1,I ) = 0
        MDSFCB( 2,I ) = 0
        MDSFCB( 3,I ) = 0
   16   CONTINUE
        DO 20 I = 1, MAXFCB
        DO 17 K = 1, 17
        FCB( K, I ) = 0
   17   CONTINUE
        FCB( 7,I ) = 20000000
   20   CONTINUE
        DO 30 I =1, 220
        IUNIT( I ) = 0
   30   CONTINUE
        IF (ANDF(4, IPERM) .EQ. 0) GO TO 40
        MDSNAM( 8 ) = NPTP
   40   MDSNAM( 7 ) = OPTP
        DO 50 I = 1, NPFIST
        XFIAT( I ) = 4095
   50   CONTINUE
        DO 60 I = 7, 22
        IF ( DSNAMES( I ) .EQ. 'none' ) GO TO 60
        IF ( DSNAMES( I ) .EQ. 'NONE' ) GO TO 60
        CALL DSINQR ( DSNAMES( I ), ISTAT, ISIZE )
        IF (ISTAT.EQ.0) GO TO 60
        FCB( 3,I ) = 6
        FCB( 4,I ) = 1
        FCB( 5,I ) = 1
        FCB( 6,I ) = FCB(7,I)
        IF ( I .EQ. 7 ) XFIAT( 2 ) = 7
   60   CONTINUE
        DO 70 I = 23, MAXPRI
        IFUFA = IFUFA + 1
        IND   = IFUFA * 11 - 10
        FIAT( IND ) = I
   70   CONTINUE
        XFIAT( 1 ) = 22
        XFIAT( 3 ) =  8
        XFIAT( 8 ) = 16
        XFIAT( 9 ) = 17
        XFIAT( 10) = 18
        XFIAT( 11) = 19
        XFIAT( 12) = 20
        XFIAT( 13) = 21
        XFIAT( 18) =  9
        IFCAE  = IFUFA
  700   RETURN
        END
