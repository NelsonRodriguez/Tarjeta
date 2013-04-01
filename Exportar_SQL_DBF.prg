CLEAR 
SET DATE BRITISH
SET CENTURY on

wdirdes=GETDIR()
whnd=SQLCONNECT("tarjeta","sa","nestor")
wok=SQLEXEC(whnd,"use tarjeta")
wok=SQLTABLES(whnd,"table","micur")
SELECT micur
GO top
DO WHILE !EOF()
   WAIT WINDOW ALLTRIM(table_name)+"..." NOWAIT 
   wnamedbf=ALLTRIM(table_name)
   
   wexe1=SQLEXEC(whnd,"select * from "+wnamedbf,"micur2")
   IF wexe1>0
      SELECT micur2
      wtarget=wdirdes + wnamedbf
      COPY all to (wtarget)
      IF USED("micur2")
         USE IN micur2
      ENDIF 
   ENDIF 
   
   
   SELECT micur
   SKIP 
ENDDO 
GO top
MESSAGEBOX("FIN")
SQLDISCONNECT(whnd)
CLOSE ALL 
CLEAR 
RETURN 