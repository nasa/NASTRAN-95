        SUBROUTINE INTPK ( *, FILE, BLOCK, ITYPOT, IFLAG )                      
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'PAKBLK.COM'                                                      
        COMMON / ZNTPKX / A(4), IROW, IEOL, IENDRC                              
        INTEGER BLOCK( 15 ), FILE                                               
        NAME = FILE                                                             
        IF ( IFLAG .EQ. 0 ) GO TO 10                                            
        CALL DSIPK1( BLOCK, ITYPOT )                                            
        GO TO 700                                                               
10      IEOL = 0                                                                
        IENDRC = 0                                                              
        CALL DSIPK1( IBLKB, ITYPOT )                                            
700     CONTINUE                                                                
        IF ( IRETRN .NE. 0 ) RETURN 1                                           
        RETURN                                                                  
        END                                                                     
