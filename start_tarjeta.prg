#DEFINE CRLF CHR(13)+CHR(10)

TRY 

*SET ECHO ON
*SET STEP ON 
set procedure to prgvarios, rutinas_tarjeta
DO seteos
set procedure to prgvarios, rutinas_tarjeta,procesarus_mejorado1
  
*--- SECCION VARIABLES PUBLICAS-CLASES ---------------------------------------------------------------------*
set classlib to sgl,prg,loginfile, registry

PUBLIC oapp, ouser,woldcolor,woldpicture,glotemp,glouni,glonivel,glo_administrador,glo_nom_emp,glomail_cli1,glomail_cli2
PUBLIC gloclave,n_handle,glorespaldo,maxon,maxoff,we0,we1,ncon,ncoff,non,noff,gloserver,glo_mailing, glo_path_files, glo_xroles,glouserpass,glousergmail
PUBLIC wcotar,wcotur,wcotiza, nacion, pais 

Public aApp9,oApp,wnivel,wgcliente,xsaldo,ddatefrom,ddateto,ejecutable,com1,com0,wnacion,;
	glargo,glancho,subon,suboff,maxon,maxoff,pwd,NCON,NCOFF,WCOM1,WCOM0,NON,welocali,wedomici,wetelefo,;
	NOFF,WON,WOFF,W1,WE1,WE0,WDN1,WDN0,L121,L120,L12CN1,L12CN0,L12C1,L12C0,PAGLEN6,paglen12,pag,wtipo,;
	laserie,wexmoneda,wmone,documento,numfac,wtdoc,wcuenta,wbasico,wminimo,wtasamor,wsucu,wvercre,;
	wcaja,wnomcaja,wfechacaja,wcajero,wrubrocj,wcamino,VECFA,l,wcpes,wcdol,wmaxcuota,wcotiza,wnomfanta,;
	wrubroc1,wrubroc2,wcuenta1,wcuenta2,wauxiliar,wsubempresa,wnomempre,wvendedor1,wvendedor2,wdefault,wpath,wnomaux,;
	forms_open, wformset,wcantreg,wvendedor,wauxprefe,wemprefe,whabilmenu,wpais,pais,nacion,wnomusu,wglobaiso,;
	wportfa,wportre,wportredos,	wabierta,wlardoc,mibase0,wdirec,wtele,wfantasia,wcotar,wcotur,wcotdol,wusucodi,webivr,;
	wpresgraba,XCOM1,XCOM0,aryAttach,L121N1,L120N0,wgrancr,wconect,whnd,mywhnd,wbolsatodo,paglen4,wcamico,glo_emp_nom,glo_per_desde,glo_per_hasta,wprestamo



*------- Inicializo Variables Globales -------------------------------------*
glo_nom_emp="Club del Este - "+SYS(0)
glomail_cli1="" && "centroca@montevideo.com.uy"
glomail_cli2="" && "jccm@centrocar.com.uy"
glousergmail="" && "neroig@gmail.com"
glouserpass=""
glonivel=0
glo_graficar=.f.
glo_mailing=.t.
glorespaldo=.f.
glo_administrador=.f.
glo_xroles=.F.
glouni="H:\"
glotemp=""
glo_path_files=""
wip=SYS(0)
SET KEYCOMP TO DOS 
WCAMINO="c:\tempb\"
MYWHND=0
*----saque de entorno------------------------
if parameters()=0
	wtipo=0
	wcamino="c:\tempb\"
else
	wtipo=1
	wcamino="c:\tempn\"
ENDIF
if !directory("c:\temp")
   md c:\temp
endif
if !directory("\temp")
   md \temp
endif
if !directory("c:\tempb")
   md c:\tempb
endif
if !directory("c:\tempn")
   md c:\tempn
endif
if !directory("c:\liquida")
	md c:\liquida
endif

erase wcamino+"*.dbf"
erase wcamino+"*.cdx"
erase wcamino+"*.idx"
erase wcamino+"*.tmp"


glo_administrador=es_administrador()
DO controles
*-------------------------------------------------------------------------*

*- defino Unidad de Trabajo y Carpetas
*DO defino_unidades
glouni="D:\"
IF glouni="H:\"
   glo_path_files = "H:\"
ENDIF 
IF DIRECTORY("H:\")
   glo_path_files = "H:\"
ELSE
   glo_path_files = "D:\"
ENDIF 

*-- Control Winsock ---------*
DO controles_winsck

*---------- instancio objetos globales ++++++++++++++++++++---------------------------------------------------------*
oapp=createobject("app")
if type("oapp")<>"O"
   messagebox("No Se Pudo Instanciar la Aplicaci�n",16,"Error General")
   set classlib to
   THROW "NORMAL"
ENDIF

DO CASE 
   CASE "WIN7PC" $ UPPER(SYS(0)) OR "RODRIGUEZNPC" $ UPPER(SYS(0))
       *oapp.dbms="MSSQL"
       oapp.dbms="MYSQL"
   OTHERWISE 
       oapp.dbms="MYSQL"
ENDCASE    

oapp.miseteos()	&& Comandos SETs ...
oapp.onemplaza=3
oapp.zona="SLT"
oapp.ousuario=""
oapp.oclave=""  
oapp.strconnect="" 
oapp.oldcolor=_screen.backcolor
oapp.oldpicture=_screen.picture
oapp.salida=.t.
oapp.otitulo=glo_nom_emp && t�tulo de la Aplicaci�n
*--- SECCION SCREEN -------------------------------------------------------------------------------------------*
_screen.windowstate=2
_screen.Icon='visa1.gif'
_screen.picture=SYS(5)+CURDIR()+"offisupp.bmp"
_screen.backcolor=rgb(201,189,165)
_screen.caption=oapp.otitulo
*--------------------------------------------------------------------------------------------------------------*
	
*--- biblioteca de funciones varias +++++++++++++++
oprg=createobject("prg")
if type("oprg")<>"O"
   messagebox("No Se Pudo Instanciar Biblioteca de Funciones",16,"Error General")
   set classlib to
   THROW "NORMAL"
ENDIF
oapp.ousuario=0
oapp.hnd=0
oapp.emplazamiento=""  && guarda el Literal Completo del Emplazamiento
oapp.oclave=""
oapp.reguser=""
oapp.tarjeta=alltrim(oprg.tarjetared())	   && Tarjeta de Red
oapp.pedirlogin=.t.	&& si pide formulario de LOGIN o no
oapp.remota=.f.
oapp.winuser=upper(substr(sys(0),at("#",sys(0))+1))
oapp.ip=wip
oapp.dirtmp="C:\TEMP"
oapp.discopc=""
wPC=ALLTRIM(UPPER(LEFT(SYS(0),AT("#",SYS(0))-2)))
wus=ALLTRIM(UPPER(SUBSTR(SYS(0),AT("#",SYS(0))+2)))

	
********** CONEXION A LA BASE DE DATOS -------------------------------------------------------------------------*
SET ECHO ON 
SET STEP ON 
*oapp.sa_connect="DATABASE=mytarjeta;DSN=TARJETA_MYSQL;OPTION=0;PWD=rubengh;PORT=3306;SERVER="+'localhost'+";UID=root"
wok_conecto=ges_conecto()
IF !wok_conecto
   *MESSAGEBOX("NO SE PUDO ESTABLECER CONEXION CON LA BASE DE DATOS: "+oapp.dbms,48,"SE CANCELA")
   THROW "NORMAL"
ENDIF 


*------------ VARIABLES GLOBALES DE PARAMETROS --------------------------------------------------------------------*
res=SQLEXEC(oapp.hnd,"select * from parametros","curini")
IF res<=0
   THROW "NORMAL"
ENDIF 
SELECT curini 
BROWSE 
wbolsatodo="" &&indica para procesos con autorizaciones, si procesa archivos para usuario
wdirec=direccion
wlocal=localidad
wtele=telefono
wnomempre=razonsocial
wfantasia=nombre
wmail=mail
wpais=pais
nacion=pais
wrazon=razonsocial
mpais=iif(pais=1,"A","U")
wcodiso="999999"
wnacion=nacion
wusucodi=space(5)
wglobaiso=iso
wpresgraba=presgraba
wcajero=.f.
wcotiza=cot_dol
if nacion<>2
	wtope_dift=tope_dift
	wtbcn=int_fin
	wtbcd=int_fid
	wpbcn=int_punc
	wpbcd=int_pudc
	* llamado fdift(calculo,tasa,wtbcn,wtope_dift)  controlar moneda de tasa, wtbcn)
ENDIF
USE IN curini
*-----------------------------------------------------------------------------------------------------------------------*


*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
do form loginsis2 with "Ingreso al Sistema" to wresul
if empty(oapp.ousuario) 
   THROW "NORMAL"
ENDIF

*---- Correo Por Ingreso al Sistema ---------------------------------------------------------------------------*
*DO correo_ingreso_sistema
*--------------------------------------------------------------------------------------------------------------*

*-------VERIFICO BACKUPS --------------------------------------------------------------------------------------*
DO verifico_backups
*--------------------------------------------------------------------------------------------------------------*
*-------COTIZACIONES --------------------------------------------------------------------------------------*
*!*	wok=tomo_cotizacion(oapp.hnd)
*!*	IF !wok
*!*	   MESSAGEBOX("DEBE INGRESARSE COTIZACION PARA INGRESAR AL SISTEMA",48,"SE CANCELA")
*!*	   THROW "NORMAL"
*!*	ENDIF    
*--------------------------------------------------------------------------------------------------------------*
WAIT CLEAR 

TRY 
on shutdown do misalida
do menuceste.mpr
read events
CATCH TO pErr

       MsgErr="[  Error: ] " + STR(pErr.ErrorNo) + CRLF + ;
    	   "[  L�nea: ] " + STR(pErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + pErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + pErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + pErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(pErr.StackLevel) + CRLF + ; 
    	   "[  Instrucci�n: ] " + pErr.LineContents 

       MESSAGEBOX(msgerr)

ENDTRY 


*------------- CIERRO EVENTUALES CONEXIONES ABIERTAS A LA BASE DE DATOS ---------------------------------------*
wvhn=ASQLHANDLES(vec_hnd)
FOR r=1 TO wvhn
    SQLDISCONNECT(vec_hnd[r])
NEXT 

*---- FINAL DEL PROGRAMA DE INICIO +++++++++++++++++++++++++++++++++++++++++++++++++
wait clear
set classlib to
set procedure to
_screen.backcolor=oapp.oldcolor
_screen.picture=oapp.oldpicture
_screen.closable=.t.
_screen.Caption = glo_nom_emp
set sysmenu to default
set sysmenu on
on shutdown
ON ERROR 
CLOSE ALL
RELEASE ALL EXTENDED
CLEAR
CLEAR ALL
RELEASE ALL

CATCH TO oErr WHEN oErr.Uservalue<>"NORMAL"

**       werr=IIF(TYPE("oapp")="O",oapp.trato_error(oErr),"13")
       MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  L�nea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucci�n: ] " + oErr.LineContents 

       MESSAGEBOX(msgerr)
       
CATCH TO oErr WHEN oErr.Uservalue="NORMAL"
    *-nothing 

ENDTRY 
_screen.closable=.t.
set sysmenu to default
set sysmenu on
ON SHUTDOWN 
ON ERROR 
IF TYPE("oapp.oldcolor")<>"U"
   _screen.backcolor=oapp.oldcolor
   _screen.picture=oapp.oldpicture
ENDIF 
CLEAR ALL 
release all
RETURN
*-----------------------------------------------------------------------------------------------------------------*
