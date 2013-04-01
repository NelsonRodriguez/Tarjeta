CLEAR
CLOSE ALL
CLEAR ALL
CLOSE DATABASES
set talk off
set exclu off
set safety off
SET CPDIALOG OFF 
set multilocks on
set confirm on
set echo off
set dele on
set date british
set century on
set sysmenu off
set hours to 24
set status bar off
set status off
set sysformats on
SET ESCAPE OFF 
TODOOK=.t.
whnd=0

TRY 
   IF !DIRECTORY("D:\RESPALDOS_BASE")
      MD D:\RESPALDOS_BASE
   ENDIF 

	********** CONEXION A LA BASE DE DATOS -------------------------------------------------------------------------*
	wPC=ALLTRIM(UPPER(LEFT(SYS(0),AT("#",SYS(0))-2)))
	IF "WIN7PC" == wPC  && UPPER(ALLTRIM(SYS(0)))	 && PCs de Casa
	   wserver=wPC
       strconnect="Driver={SQL Server};Server="+wserver+";UID=sa;PWD=nestor;WSID="+wserver+";"+;
                    "DATABASE=MASTER;LANGUAGE=Español;Trusted_Connection=No"
       strconnect2="Driver={SQL Server};Server="+wserver+";UID=sa;PWD=nestor;WSID="+wserver+";"+;
                    "DATABASE=CCAR;LANGUAGE=Español;Trusted_Connection=No"
       sa_connect=strconnect

    ELSE
       IF "RODRIGUEZNPC" == wPC && UPPER(ALLTRIM(SYS(0))) && en CTM
	       strconnect="Driver={SQL Server};Server="+wPC+";UID=sa;PWD=nestor;WSID="+wPC+";"+;
	                    "DATABASE=MASTER;LANGUAGE=Español;Trusted_Connection=No"
	       strconnect2="Driver={SQL Server};Server="+wPC+";UID=sa;PWD=nestor;WSID="+wPC+";"+;
	                    "DATABASE=CCAR;LANGUAGE=Español;Trusted_Connection=No"
	       sa_connect=strconnect
       
       ELSE 
           name_server="SVR"
    	   strconnect="Driver={SQL Server};Server="+name_server+"\SQLEXPRESS;UID=sa;PWD=nestor;WSID="+name_server+"\SQLEXPRESS;"+;
	                    "DATABASE=MASTER;LANGUAGE=Español;Trusted_Connection=No"
    	   strconnect2="Driver={SQL Server};Server="+name_server+"\SQLEXPRESS;UID=sa;PWD=nestor;WSID="+name_server+"\SQLEXPRESS;"+;
	                    "DATABASE=CCAR;LANGUAGE=Español;Trusted_Connection=No"
	       sa_connect=strconnect
       ENDIF 
    ENDIF 
    
    res=SQLSTRINGCONNECT(sa_connect)
    IF res<=0
       AERROR(verr)
       *MESSAGEBOX("NO SE PUDO ESTABLECER CONEXION CON LA BASE DE DATOS",48,"SE CANCELA")
       *lgrabo=grabobitaco("RESPALDO BASE: ERROR!!",TTOC(DATETIME())+" "+STR(verr[1],7)+verr[2])
       
       THROW "NORMAL"
    ENDIF 
    n_handle=res
    whnd=n_handle

    res=SQLSTRINGCONNECT(strconnect2)  && CCAR
    IF res<=0
       AERROR(verr)
       *MESSAGEBOX("NO SE PUDO ESTABLECER CONEXION CON LA BASE DE DATOS",48,"SE CANCELA")
       *lgrabo=grabobitaco("RESPALDO BASE: ERROR!!",TTOC(DATETIME())+" "+STR(verr[1],7)+verr[2])
       
       THROW "NORMAL"
    ENDIF 
    whnd=res
    

wname="D:\RESPALDOS_BASE\CCAR_"+DTOS(DATE())+".BAK"
csql="BACKUP DATABASE [CCAR] TO  DISK = N'"+wname+"' WITH  INIT ,  NOUNLOAD , NAME = N'Copia de seguridad CENTROCAR', "+;
     "NOSKIP ,  STATS = 10,  NOFORMAT "
res=SQLEXEC(n_handle,csql)
IF res<=0
   AERROR(Verr)
   *MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error BACKUP")
   lgrabo=grabobitaco("RESPALDO BASE: ERROR!!",TTOC(DATETIME())+" "+STR(verr[1],7)+verr[2])
   THROW "NORMAL"
ENDIF 

lgrabo=grabobitaco("RESPALDO BASE: OK",TTOC(DATETIME()))

*- copio al PC de Respaldo
wsoloname=JUSTFNAME(wname)
TRY 
wdesfile="\\XXXXX\C\RESPALDOS_BASE\"+wsoloname
IF FILE("&wname")
   COPY FILE (wname) TO (wdesfile)
   IF FILE("wdesfile")
      lgrabo=grabobitaco("RESPALDO BASE - COPIA A PC RESPALDO: OK",TTOC(DATETIME()))
   ELSE
      lgrabo=grabobitaco("RESPALDO BASE - COPIA A PC RESPALDO: ARCHIVO NO SE HALLO EN DESTINO",TTOC(DATETIME())+"  "+wdesfile)
   ENDIF 
ELSE
   lgrabo=grabobitaco("RESPALDO BASE - COPIA A PC RESPALDO: ARCHIVO ORIGEN NO SE HALLO!!!",TTOC(DATETIME())+"  "+wname)
ENDIF    
CATCH
*lgrabo=grabobitaco("RESPALDO BASE - COPIA A PC RESPALDO: ERROR !!!",TTOC(DATETIME()))
ENDTRY 

IF n_handle>0
   SQLDISCONNECT(n_handle)
   n_handle=0
ENDIF    

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL" 
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 

    *MESSAGEBOX(msgerr)
    lgrabo=grabobitaco("RESPALDO BASE: ERROR DESCONOCIDO!!",TTOC(DATETIME())+" "+STR(oErr.ErrorNO,7)+" "+oERR.Message)
    IF n_handle>0
       SQLDISCONNECT(n_handle)
    ENDIF 

CATCH TO oErr WHEN oErr.UserValue="NORMAL" 

    IF n_handle>0
       SQLDISCONNECT(n_handle)
    ENDIF 


ENDTRY 
RETURN 
*-------------------------------------------------------------------------------------------------------------------------*
FUNCTION grabobitaco(p_ope,p_datos)
LOCAL ok_salida,wdatos,wop,c_campos
*------- GRABO BITACORA ---------------*
TRY 

IF whnd<=0
   RETURN .f.
ENDIF 

ok_salida=.t.
WUSUARIO="PROGRAMADO"
WEQUIPO="SVR"

wop=p_ope
wdatos=p_datos

c_campos="'"+wusuario+"','"+wop+"','"+wdatos+"','"+WEQUIPO+"'"

*-- Firma Liquidación ----------------------------------------------------------**
csql0="BEGIN TRANSACTION "+CHR(13)+CHR(10)+;
      "insert into bitacora (usuario, comentario, datos, equipo) VALUES ("+c_campos+") "+CHR(13)+CHR(10)+;
      "COMMIT TRANSACTION"
      
res1=SQLEXEC(whnd,csql0)
IF res1<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],48,"FIRMA 1")
   ok_salida=.f.
   THROW "ERROR"
ELSE
*!*	    MESSAGEBOX("Grabado en Botacora Con Exito",64,"Todo OK")   
   ok_salida=.t.
ENDIF 

CATCH TO  oErr
    ok_salida=.f.
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR")
ENDTRY 
RETURN ok_salida 
*--------------------------------------------------------------------------------*
