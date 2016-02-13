       SUBROUTINE ERRTRC ( NAME, IVAL )
       CHARACTER *(*) NAME
       COMMON / SYSTEM / ISYSBF, NOUT
       WRITE ( NOUT, * ) ' ERRTRC CALLED'
       WRITE ( NOUT, * ) ' NAME=',NAME
       WRITE ( NOUT, * ) ' IVAL=',IVAL
       RETURN
       END
