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
      DATE(3)=DATE1(3)-1900
      RETURN        
      END        
