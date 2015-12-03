      BLOCK DATA ITEMBD
CITEMBD
C     ITEMBD BLOCK DATA
C
C     TO ADD NEW ITEMS TO THE SOF THE FOLLOWING CODE CHANGES MUST BE
C     MADE.
C
C        1) INCREASE THE DIMENSION OF ITEM IN THE ITEMDT COMMON BLOCK.
C        2) INCREASE THE DIMENSION OF THE ITEMXX ARRAY AND ADD ANY
C           ADDITIONAL ARRAYS AND EQUIVALENCES IF NECESSARY.
C        3) INCREASE THE VALUE OF NITEM IN THE DATA STATEMENT.
C        4) ADD THE NEW DATA DESCRIBING THE NEW ITEMS.
C        5) SUBROUTINE EXO2 MUST BE CHANGED IF THE NEW ITEM IS A TABLE.
C           THIS ROUTINE PROCESSES THE SOFOUT(EXTERNAL) STATEMENT.
C        6) SUBROUTINE SOFTOC MUST BE CHANGED IF THE NEW ITEMS WILL
C           INCREASE THE NUMBER IF ITEMS TO MORE THEN 27.
C
C     NOTE... IF THE NUMBER OF ITEMS IS DECREASED THE LENGTH OF THE
C             ITEMDT COMMON BLOCK SHOULD NOT BE DECREASED.  SOFS CREATED
C             ON THE OLDER SYSTEM WILL REQUIRE THE EXTRA SPACE WHEN
C             RESTORING THE ITEM STRUCTURE FOR THAT SOF.
C
      INTEGER       ITEM01(7,10)  ,ITEM02(7,10)  ,ITEM03(7,5)
C
      COMMON / ITEMDT /       NITEM    ,ITEM(7,25)
C
      EQUIVALENCE   ( ITEM01(1,1) , ITEM(1, 1) )
     2             ,( ITEM02(1,1) , ITEM(1,11) )
     3             ,( ITEM03(1,1) , ITEM(1,21) )
C
C        NITEM     = NUMBER OF ITEMS
C        ITEM(1,I) = ITEM NAME
C        ITEM(2,I) = ITEM TYPE
C                         LE 0 - TABLE ITEM
C                         GE 1 - MATRIX ITEM
C        ITEM(3,I) = EQUIV DATA FOR GROUP 0 OF ITEMS TO BE COPYIED
C                         X 1000 - WORD WITH NUMBER OF NAMES
C                         X 100  - WORD WITH FIRST NAME
C                         X 1    - NUMBER OF WORDS FOR EACH NAME
C        ITEM(4,I) = IMAGE SUBSTRUCTURE DATA
C                         0 - ITEM IS ONLY A POINTER TO PRIMARY DATA
C                         1 - UNIQUE DATA, RETURN TO FREE BLOCK LIST
C        ITEM(5,I) = SECONDARY SUBSTRUCTURE DATA
C                         0 - ITEM IS ONLY A POINTER TO PRIMARY DATA
C                         1 - UNIQUE DATA, RETURN TO FREE BLOCK LIST
C        ITEM(6,I) = HIGHER LEVEL SUBSTRUCTURE DATA
C                         0 - ITEM DOES NOT PERTAIN TO HIGER LEVEL
C                         1 - ITEM DESCRIBES HIGHER LEVEL
C        ITEM(7,1) = EDIT DATA.  EACH BIT IS SET IF THAT ITEM IS IN
C                    THE COORESPONDING EDIT GROUP.  EXAMPLE - A VALUE
C                    OF 36 WOULD CAUSE THE ITEM TO BE DELETED BY
C                    EDIT(32) OR EDIT(4)
C
C***********************************************************************
C
      DATA NITEM / 25 /
C
C
C          NAME   TYPE     EQUIV     IMAGE    SECONDARY   HIGHER   EDIT
C
      DATA ITEM01 /
     1    4HEQSS   ,0        ,3005002  ,1        ,1        ,0      ,32
     2   ,4HBGSS   ,0        ,0        ,0        ,0        ,0      ,32
     3   ,4HCSTM   ,0        ,0        ,0        ,0        ,0      ,32
     4   ,4HLODS   ,0        ,4005002  ,1        ,1        ,0      ,36
     5   ,4HPLTS   ,0        ,3004014  ,1        ,1        ,0      ,32
     6   ,4HKMTX   ,1        ,0        ,0        ,0        ,0      ,33
     7   ,4HMMTX   ,1        ,0        ,0        ,0        ,0      ,34
     8   ,4HPVEC   ,1        ,0        ,0        ,0        ,0      ,36
     9   ,4HPOVE   ,1        ,0        ,0        ,1        ,1      ,48
     O   ,4HUPRT   ,1        ,0        ,0        ,1        ,1      ,48
     *                                                                 /
      DATA ITEM02 /
     1    4HHORG   ,1        ,0        ,0        ,1        ,1      ,560
     2   ,4HUVEC   ,1        ,0        ,1        ,1        ,0      ,40
     3   ,4HQVEC   ,1        ,0        ,1        ,1        ,0      ,40
     4   ,4HSOLN   ,0        ,0        ,1        ,1        ,0      ,40
     5   ,4HPAPP   ,1        ,0        ,0        ,0        ,0      ,100
     6   ,4HPOAP   ,1        ,0        ,0        ,1        ,1      ,112
     7   ,4HLOAP   ,0        ,4005002  ,1        ,1        ,0      ,100
     8   ,4HLMTX   ,1        ,0        ,0        ,1        ,1      ,48
     9   ,4HGIMS   ,1        ,0        ,0        ,1        ,1      ,48
     O   ,4HPHIS   ,1        ,0        ,0        ,1        ,1      ,288
     *                                                                 /
      DATA ITEM03 /
     1    4HLAMS   ,0        ,0        ,0        ,1        ,1      ,288
     2   ,4HK4MX   ,1        ,0        ,0        ,0        ,0      ,160
     3   ,4HBMTX   ,1        ,0        ,0        ,0        ,0      ,160
     4   ,4HPHIL   ,1        ,0        ,0        ,1        ,1      ,288
     5   ,4HHLFT   ,1        ,0        ,0        ,1        ,1      ,560
     *                                                                 /
      END
