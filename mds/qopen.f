        SUBROUTINE QOPEN ( *, NAMFIL, BUFF, IOP )                               
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        COMMON / SYSTEM / ISYSBF, DUM1(77), IDIAG, DUM2(21)                     
        INTEGER           BUFF(10), DNAME(2), ITRL(7)                          
        DATA     INIT   / 0 /                                                   
        NAME   = NAMFIL                                                         
        IOCODE = IOP                                                            
        IF ( INIT .NE. 0 ) GO TO 10                                             
        IBASBF = LOCFX( IBASE )                                                 
        CALL DSIODD                                                             
        NBUFF  = ISYSBF - 4                                                     
        NBFZ   = 1                                                              
        IF ( LENWPB .NE. 0 ) NBFZ   = NBUFF / LENWPB + .1                       
        INIT   = 1                                                              
   10   IF ( IAND( IDIAG, 2**14 ) .NE. 0 ) CALL DSMSG ( 1 )                     
        LOCBUF = LOCFX( BUFF )                                                  
        INDBAS = LOCBUF - IBASBF + 1                                            
        IF ( MOD( INDBAS,2 ) .EQ. 0 ) INDBAS = INDBAS + 1                       
        IF ( FCB( 2, IFILEX ) .EQ. 0 ) GO TO 20                                 
        CALL DSMSG( 5 )                                                         
   20   DO 30 I =1, MAXPRI                                                      
        IBASTS = FCB( 2, I )                                                    
        IF ( IBASTS .EQ. 0 ) GO TO 30                                           
        IBASHI = IBASTS + ISYSBF - 2                                            
        IBASLO = IBASTS - ISYSBF                                                
        IF( INDBAS .LE. IBASLO .OR. INDBAS .GT. IBASHI ) GO TO 30               
        CALL DSMSG( 3 )                                                         
   30   CONTINUE                                                                
        IBASE( INDBAS )  = NAMFIL                                               
        FCB( 2, IFILEX ) = INDBAS                                               
        FCB(12, IFILEX ) = INDBAS                                               
        CALL DBMNAM ( NAME, DNAME, IFILEX )
        IF( IOCODE .LE. 1 ) GO TO 40                                            
        IF( FCB( 13, IFILEX ) .EQ. DNAME( 1 ) .AND.
     &      FCB( 14, IFILEX ) .EQ. DNAME( 2 ) ) GO TO 35
C        CALL DBMSRF( DNAME, IUNI )
C        IF ( IUNI .EQ. IFILEX ) GO TO 35
        ITRL(1) = NAME
        CALL RDTRL( ITRL )
        DO 32 I = 2, 7
        IF ( ITRL(I) .NE. 0 ) GO TO 35
   32   CONTINUE
        IF ( IOCODE .EQ. 3 ) IOCODE = 1
        IF ( IOCODE .EQ. 2 ) IOCODE = 0
        GO TO 40
   35   CONTINUE
        NBLOCK = FCB( 4,IFILEX )                                                
        IF ( NBLOCK .EQ. 0 ) GO TO 40                                           
        CALL DBMMGR ( 1 )                                                       
        INDCLR = FCB( 3, IFILEX ) + INDBAS - 1                                  
        INDCBP = INDCLR                                                         
        GO TO 60                                                                
   40   NBLOCK = 1                                                              
        FCB( 13, IFILEX ) = DNAME( 1 )
        FCB( 14, IFILEX ) = DNAME( 2 )
        CALL DBMMGR ( 1 )                                                       
        INDCLR = INDBAS + 5                                                     
        INDCBP = INDCLR                                                         
        IF( IOCODE .EQ. 0 ) GO TO 60                                            
        IBASE( INDBAS+3 ) = 1                                                   
        IBASE( INDBAS+4 ) = 0                                                   
        FCB( 8, IFILEX )  = 0                                                   
   60   IF ( NBLOCK .EQ. IBASE( INDBAS+3 ) ) GO TO 70                           
        CALL DSMSG ( 102 )                                                      
   70   CALL DSSDCB                                                             
C        PRINT *,' QOPEN,UN,CLR,BLK,IOP=',IFILEX,FCB(3,IFILEX),                 
C     &     FCB(4,IFILEX),IOP                                                   
        RETURN                                                                  
        END                                                                     
