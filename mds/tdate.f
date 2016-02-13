      SUBROUTINE TDATE (DATE)        
C        
C     VAX VERSION        
C     ===========        
C     (ALSO SiliconGraphics, DEC/ultrix, and SUN.        
C      CRAY AND HP DO NOT HAVE IDATE)        
C        
C     THIS ROUTINE OBTAINS THE MONTH, DAY AND YEAR, IN INTEGER FORMAT   
C        
      INTEGER DATE(3), DATE1(3)        
C        
      CALL IDATE (DATE1)        
C                 DAY   MONTH     YEAR        
C     THESE DATES HAD TO BE INTERCHANGED FOR THE SUN
      DATE(1)=DATE1(2)
      DATE(2)=DATE1(1)
CDME  19 JAN 2016
CDME  D. Everhart
CDME  This is basically a Y2K bugfix.
      DATE(3)=MOD(DATE1(3),100)
CDME  DATE(3)=DATE1(3)-1900
      RETURN        
      END        
