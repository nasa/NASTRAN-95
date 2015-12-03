        SUBROUTINE PUTSTR ( BLOCK )                                             
********************************************************                        
*                                                                               
*       FORMAT OF THE I/O MATRIX CONTROL TABLE                                  
*                                                                               
*    WORD    QUARTER            DESCRIPTION                                     
*       1       -       GINO FILE NAME                                          
*       2       -       TYPE OF ELEMENTS (1,2,3,4) - REFERS TO TYPE             
*                       BEING WRITTEN (BLDPK--) TO THE BUFFER OR                
*                       TYPE OF ELEMENTS READ (INTPK--) FROM THE BUFFER         
*       3       -       TRAILERS TO BE INCLUDED (0=NO,1=YES) ON WRITE           
*                       TO BUFFER OR ARE INCLUDED ON READ FROM BUFFER           
*       4       -       ROW NUMBER                                              
*       5       -       INDEX TO STRING (RELATIVE TO /XNSTRN/)                  
*       6       -       NUMBER OF ELEMENTS AVAIL. OR  RESIDE IN STRING          
*       7       -       NUMBER OF ELEMENTS WRITTEN TO STRING BY USER            
*       8       -       BEGIN/END FLAG (-1, FIRST CALL FOR COLUMN,              
*                       =0, INTERMEDIATE CALL; =1, LAST CALL)                   
*       9       -       INTERIM FLAG FOR COLUMN ('C','P','X')                   
*       10      -       COUNT OF NON-ZERO WORDS PER COLUMN                      
*       11      -       NUMBER OF WORDS PER ELEMENT (SEE WORD 2)                
*       12      -       COLUMN NUMBER                                           
*       13      -       TYPE OF INPUT (BLDPK) OR OUTPUT (INTPK)                 
*       14      -       DIVISOR FOR COMPUTING BLOCK(5)                          
*       15      -       ROW NUMBER ON INPUT (BLDPK)                             
*                                                                               
**********************************************************************          
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER BLOCK( 15 ), IDIV( 4 )                                          
        DATA    IDIV / 1, 2, 1, 2 /                                             
        NAME = BLOCK( 1 )                                                       
        CALL DSGEFL                                                             
        LIM  = INDBAS + NBUFF + 2                                               
        IF ( BLOCK( 8 ) .EQ. -1 ) GO TO 10                                      
        NWORDS = BLOCK( 11 )                                                    
        IFLG = BLOCK( 9 )                                                       
        GO TO 30                                                                
10      NWORDS = NWRDEL( BLOCK( 2 ) )                                           
        BLOCK( 14 ) = IDIV( BLOCK( 2 ) )                                        
        BLOCK( 11 ) = NWORDS                                                    
        BLOCK(  8 ) = 0                                                         
        BLOCK(  9 ) = IDSC                                                      
        IFLG = IDSC                                                             
        IF ( ( LIM-INDCBP-6-BLOCK(3)*2 ).GE. NWORDS ) GO TO 20                  
        IBASE( INDCBP ) = IDSEB                                                 
        CALL DSWRNB                                                             
        LIM  = INDBAS + NBUFF + 2                                               
20      IBASE( INDCBP+1 ) = IDSCH +  BLOCK( 3 )*MULQ3 + BLOCK( 2 )              
        IBASE( INDCBP+2 ) = BLOCK( 12 )                                         
        INDCBP = INDCBP + 2                                                     
30      NLR = IABS( MOD( INDCBP+2, BLOCK( 14 ) ) )                              
        NELM = ( LIM - INDCBP - NLR - 6 - BLOCK( 3 )*2 ) / NWORDS               
        IF ( NELM .GE. 1 ) GO TO 50                                             
        IFLG = BLOCK( 9 )                                                       
        IF ( IFLG .EQ. IDSX ) GO TO 40                                          
        IFLG = IDSP                                                             
        BLOCK( 9 ) = IDSX                                                       
40      IBASE( INDCLR ) = IDSSB + IFLG + ( INDCBP - INDCLR )                    
        IBASE( INDCBP + 1 ) = IDSRT + IFLG + ( INDCLR-INDBAS+1 )                
        IBASE( INDCBP + 2 ) = IDSEB                                             
        INDCLR = INDCBP + 2                                                     
        CALL DSWRNB                                                             
        LIM  = INDBAS + NBUFF + 2                                               
        GO TO 30                                                                
50      BLOCK( 6 ) = NELM                                                       
        BLOCK( 7 ) = 0                                                          
        BLOCK( 5 ) = ( INDCBP+NLR+2 ) / BLOCK( 14 ) + 1                         
        IF ( NLR .EQ. 0 ) GO TO 70                                              
        IBASE( INDCBP + 1 ) = IDSSD                                             
        INDCBP = INDCBP + 1                                                     
70      CALL DSSDCB                                                             
        RETURN                                                                  
        END                                                                     
