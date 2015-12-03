        SUBROUTINE BLDPK ( ITYPIN, ITYPOT, FILE, BLOCK, IFLAG )                 
        INCLUDE 'PAKBLK.COM'                                                    
        INCLUDE 'DSIOF.COM'                                                     
        INTEGER  BLOCK(15), FILE                                                
        ITRAIL = IFLAG                                                          
        ITYPI  = ITYPIN                                                         
        ITYPO  = ITYPOT                                                         
        NAME   = FILE                                                           
        IF ( ITYPI .LT. 1 .OR. ITYPI .GT. 4 ) GO TO 40                          
        IF ( ITYPO .LT. 1 .OR. ITYPO .GT. 4 ) GO TO 40                          
        IF ( IFLAG .EQ. 0 ) GO TO 20                                            
        CALL DSBLPK ( BLOCK )                                                   
        GO TO 30                                                                
20      ITRAIL = 0                                                              
        CALL DSBLPK ( IBLKA )                                                   
30      GO TO 700                                                               
40      IF ( IFLAG .EQ. 0 ) CALL DSMSG1 ( IBLKA )                               
        IF ( IFLAG .NE. 0 ) CALL DSMSG1 ( BLOCK )                               
        CALL DSMSG( 118 )                                                       
700     RETURN                                                                  
        END                                                                     
