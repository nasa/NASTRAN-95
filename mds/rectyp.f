        SUBROUTINE RECTYP ( FILE, ITYPE )                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER           FILE                                                  
        NAME = FILE                                                             
        CALL DSGEFL                                                             
 5      ID   = IAND( IBASE( INDCLR ), MASKQ1 )                                  
        IF ( ID .EQ. IDSSB ) GO TO 10                                           
        IF ( ID .EQ. IDSEB ) GO TO 20                                           
        ITYPE = 0                                                               
        GO TO 7000                                                              
10      ITYPE = 1                                                               
        GO TO 7000                                                              
20      CALL DSRDNB                                                             
        CALL DSSDCB                                                             
        GO TO 5                                                                 
7000    RETURN                                                                  
        END                                                                     
