FUNCTION misletras
*********************************
maxon=chr(27)+chr(33)+chr(5)
maxoff=chr(27)+chr(33)+chr(0)
**** COMPRIMIDA Y NEGRITA *******
NCON=CHR(15)+CHR(27)+"G"
NCOFF=CHR(18)+CHR(27)+"H"
**** COMPRIMIDA COMUN ***********
COM1=CHR(15)
COM0=CHR(18)
**** COMUN Y NEGRITA ************
NON=CHR(27)+"G"
NOFF=CHR(27)+"H"
**** WIDE Y NEGRITA *************
WON=CHR(14)+CHR(27)+"G"
WOFF=CHR(27)+"H"
**** WIDE ***********************
W1=CHR(14)
**** WIDE ENFATIZADO ************
WE1=CHR(14)+CHR(27)+"E"
WE0=chr(20)+CHR(27)+"F"
**** WIDE DOBLE ANCHO NEGRITA ***********
WDN1=CHR(14)+CHR(27)+"G"+CHR(27)+"W"
WDN0=CHR(27)+"H"+CHR(27)+"W"
**** 12 CARACTERES PULGADA ******
L121=CHR(27)+"M"
L120=CHR(27)+"P"
**** 12 CARACTERES PULGADA - CONDENSADO - NEGRITA ******
L12CN1=CHR(27)+"M"+CHR(15)+CHR(27)+"G"
L12CN0=CHR(27)+"P"+CHR(18)+CHR(27)+"H"
**** 12 CARACTERES - CONDENSADO ************************
L12C1=CHR(27)+"M"+CHR(15)
L12C0=CHR(27)+"P"+CHR(18)
**---------------------------------------------------------------------
DECLARE INTEGER ShellExecute IN shell32.dll ; 
 INTEGER hndWin, ;
 STRING cAction, ;
 STRING cFileName, ;
 STRING cParams, ;
 STRING cDir, ;
 INTEGER nShowWin
 
declare Integer GetFileAttributes in win32api string @
 
RETURN .t.
********************************************************************************
Function SureFile(tcFileName)
return (GetFileAttributes(@m.tcFileName)  <> -1)
*-------------------------------------------------------------------------------*
function strzero(p1,p2,p3)
private uno
if parameters()=2
   uno=alltrim(str(p1,p2))
else
   uno=alltrim(str(p1,p2,p3))
endif
uno=padl(uno,p2,"0")
return(uno)
*-------------------------------------------------------------------------------*
FUNCTION DISCOLOCAL
*- Localiza la unidad asignada al disco LOCAL de la PC. 
*- Retorna C:\ ó D:\ ó Q:\ etc.
*Funciona en cualquier versión de Windows.

Dimension aKnownDrives[1]
lnDrives = GetDriveStrings(@aKnownDrives)
unidad=""
If lnDrives > 0
    Dimension aDriveInfo[3]
    For ix = 1 to lnDrives
        =GetVolInfo( aKnownDrives[ix], @aDriveInfo )
        if "CLIENT C:" $ adriveinfo[1]
            unidad=alltrim(aknowndrives[ix])
            exit
        endif
    Endfor
Endif
return unidad
*-------------------------------------------------------------------------------*
Function cDriveType
    Lparameters tcRoot && Root of drive ie: "c:\"
    Local array aDrvTypes[7]
    aDrvTypes[1]="CANNOT_DETERMINE"
    aDrvTypes[2]="INVALID_DRIVE"
    aDrvTypes[3]="DRIVE_REMOVABLE"
    aDrvTypes[4]="DRIVE_FIXED"
    aDrvTypes[5]="DRIVE_REMOTE"
    aDrvTypes[6]="DRIVE_CDROM"
    aDrvTypes[7]="DRIVE_RAMDISK"
    Return aDrvTypes[nDriveType(tcRoot)+1]
*******************************************************************************
Function nDriveType
    Lparameters tcRoot && Root of drive ie: "c:\"
    Declare integer GetDriveType in WIN32API string @cDrvLetter
    Return GetDriveType(@tcRoot)
*******************************************************************************
Function GetVolInfo
    Lparameters lcRoot, taInfo && Root of drive ie: "c:\", InfoArray passed by ref
    Declare SHORT GetVolumeInformation IN Win32API;
        STRING @lpRootPathName, STRING @lpVolumeNameBuffer,;
        INTEGER nVolumeNameSize, INTEGER @lpVolumeSerialNumber,;
        INTEGER @lpMaximumComponentLength, INTEGER @lpFileSystemFlags,;
        STRING @lpFileSystemNameBuffer, INTEGER nFileSystemNameSize
    Store 0 TO lnserialno, lncomplen, lnsysflags
    Store SPACE(260) TO lcvolname, lcsysname
    Store LEN(lcvolname) TO lnvolsize, lnnamesize
    If (GetVolumeInformation(@lcRoot, @lcvolname,;
            lnvolsize, @lnserialno, @lncomplen, @lnsysflags,;
            @lcsysname, lnnamesize) # 0)
        Dimension taInfo[3]
        taInfo[1] = substr(lcvolname, 1, at(chr(0),lcvolname)-1) && Volume name
        taInfo[2] = substr(lcsysname, 1, at(chr(0),lcsysname)-1) && System
        taInfo[3] = dec2hex(lnserialno) && Serial in hex format
    Else
        taInfo = ""
    Endif
    Return
*******************************************************************************
Function GetDriveStrings
    Parameters aDriveStrings && Array passed by ref
    Local lpBuffer, nBufferLength, lnBuflen, lnDriveCount, ix
    Declare integer GetLogicalDriveStrings in Win32API ;
        integer  nBufferLength,    string @ lpBuffer
    lpBuffer = space(26*128)
    nBufferLength = 26*128
    lnBuflen = GetLogicalDriveStrings(nBufferLength, @lpBuffer)
    If lnBuflen # 0 && Succeeded
        lpBuffer = left(lpBuffer,lnBuflen)
        lnDriveCount = occurs(chr(0),lpBuffer)
        Dimension aDriveStrings[lnDriveCount]
        For ix = 1 to lnDriveCount
            aDriveStrings[ix] = ;
                substr(lpBuffer, ;
                iif( ix=1,1,at(chr(0),lpBuffer,ix-1)+1 ), ;
                at(chr(0),lpBuffer,ix) ;
                - iif( ix=1,0,at(chr(0),lpBuffer,ix-1)) - 1)
        Endfor
    Else
        lnDriveCount = 0
    Endif
    Return lnDriveCount
*******************************************************************************
Function dec2hex
    Parameter nDecimal, nDigits
    **  Converts from base 10 to base 16.  Returns Hex notation in a string whose length
    **  is always a multiple of 2, unless the nDigits parameter is specified to pad the
    **  string with zeroes.
    cHex = ""
    Do WHILE nDecimal >= 16
        cHex = hexdigit(nDecimal % 16) + cHex
        nDecimal = int(nDecimal/16)
    Enddo
    cHex = hexdigit(nDecimal) + cHex
    Return PADL(cHex, ;
        iif( PARAMETERS() < 2, ;
        ceiling(len(cHex)/2)*2, nDigits ), "0")
*******************************************************************************
Function hexdigit
    Parameters nDecimal
    Return iif(nDecimal>9,chr(asc("A")+nDecimal%10),str(nDecimal,1))

*-----------------------------------------------------------------------------------------------*
FUNCTION LDOM(nYear, nMonth)
local dDate,xsale
if parameters()=1 .and. type("nyear")="D"
   dDate=nYear
   xsale=GOMONTH(DATE(YEAR(dDate), MONTH(dDate), 1),1)-1
else
   xsale=GOMONTH(DATE(nYear, nMonth,1),+1)-1
endif   
RETURN xsale
*******************************************************************************
*- Cuando estamos trabajando en C/S y queremos enviar una 
*- sentencia SQL al servidor debemos disponer de una 
*- conexión. Esta rutina intenta obtenerla desde las áreas 
*- ya abiertas.
function GetConnHandle
local lnConn
lnConn = -1
for i = 1 to aused( laOpenView )
   if cursorgetprop('SourceType', laOpenView[i,1]) = 2		&& vista remota
      lnConn = cursorgetprop('ConnectHandle', laOpenView[i,1])
      exit
   endif
endfor	
return m.lnConn
*----------------------------------------------------------------------------------*
FUNCTION MiMacAddress
Local pGUID,rGUID,lnSize
Declare integer CoCreateGuid in 'OLE32.dll' ;
  string @pguid
Declare integer StringFromGUID2 in 'OLE32.dll' ;
  string rguid, string @lpsz, integer cchMax
pGUID=replicate(chr(0),16)
rGUID=replicate(chr(0),80)

If "5." $ OS() && 2000/XP
  Declare integer UuidCreateSequential in 'RPCRT4.dll'  string @ Uuid
  Return substr( iif( UuidCreateSequential(@pGUID) = 0 ;
    and StringFromGUID2(pGUID,@rGUID,40) # 0, ;
    StrConv(left(rGUID,76),6), "" ), 26,12)
Else
  Return substr( iif( CoCreateGuid(@pGUID) = 0 ;
    and StringFromGUID2(pGUID,@rGUID,40) # 0, ;
    StrConv(left(rGUID,76),6), "" ), 26,12)
Endif
RETURN
*----------------------------------------------------------------------------------*
Function iniciomes(ldfecha As Date)
  *!* Program: iniciomes
  *!* Author: José G. Samper
  *!* Date: 07/04/04 05:59:24 PM
  *!* Copyright: NetBuzos
  *!* Description: Devuelve el primer dia del mes
  *!* Parametros: ldfecha as date, si no es una fecha devuelve Null
  *!* Ejemplo:?iniciomes(date())
  *!* Revision Information:
  If Vartype(ldfecha)#'D'
    Return Null
  Endif
Return ((ldfecha-Day(ldfecha))+1)
*----------------------------------------------------------------------------------*
Function finmes(ldfecha As Date)
  *!* Program: finmes
  *!* Author: José G. Samper
  *!* Date: 07/04/04 05:59:24 PM
  *!* Copyright: NetBuzos
  *!* Description: Devuelve el ultimo día del mes
  *!* Parametros: ldfecha as date, si no es una fecha devuelve Null
  *!* Ejemplo:?finmes(date())
  *!* Revision Information:
  If Vartype(ldfecha)#'D'
    Return Null
  Endif
Return (Gomonth(ldfecha,1)-Day(ldfecha))
*----------------------------------------------------------------------------------*
Function textdate(ldfecha As Date)
  *!* Program: textdate
  *!* Author: José G. Samper
  *!* Date: 07/04/04 05:59:24 PM
  *!* Copyright: NetBuzos
  *!* Description: Devuelve la fecha en texto
  *!* Parametros: ldfecha as date, si no es una fecha devuelve Null
  *!* ?textdate(date())
  *!* Revision Information:
  If Vartype(ldfecha)#'D'
    Return Null
  Endif
Return (Alltrim(Str(Day(ldfecha)))+' de '+Cmonth(ldfecha)+' de '+Alltrim(Str(Year(ldfecha))))
*----------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------*
*******************************************************************************
FUNCTION emptynull(ofecha)
LOCAL sale 
sale=.f.
DO case
   CASE TYPE("ofecha")="D"
        IF EMPTY(ofecha) OR ISNULL(ofecha) 
           sale=.t.
        ELSE
           IF ofecha=CTOD("01/01/1900")
              sale=.t.
           ENDIF 
        ENDIF 
   CASE TYPE("ofecha")="T"
        IF EMPTY(ofecha) OR ISNULL(ofecha) 
           sale=.t.
        ELSE
           IF TTOD(ofecha)=CTOD("01/01/1900")
              sale=.t.
           ENDIF 
        ENDIF 
   CASE TYPE("ofecha")="N"
        IF EMPTY(ofecha) OR ISNULL(ofecha)
           sale=.t.
        ENDIF 
   CASE TYPE("ofecha")="C"
        IF EMPTY(ofecha) OR ISNULL(ofecha)
           sale=.t.
        ENDIF 
ENDCASE    
RETURN sale
********************************************************************************
FUNCTION ntoc(unro)
RETURN (ALLTRIM(STR(unro,10)))
*-------------------------------------------------------------------------------------*
FUNCTION getletra(xnro)
LOCAL cba
IF xnro>26 .or. xnro<1
   MESSAGEBOX("ERROR en GETLETRA: Nro DEBE Estar entre 1 y 26",16,"Error")
   RETURN "A"
endif
cba=64
wascii=chr(cba+xnro)
RETURN wascii
*--------------------------------------------------------------------------------------*
FUNCTION graba_log(u_hnd,N_operacion,C_NOTA,n_liqnro)
LOCAL wfch,wliq,qleg,wnop,c_campos,csql,wq,sale
sale=.t.
WFCH="'"+TTOC(DATETIME())+"'"
WLIQ=ALLTRIM(STR(n_liqnro,6))
WLEG=ALLTRIM(STR(OAPP.OUSUARIO,6))
WNOP=ALLTRIM(STR(N_OPERACION))

c_campos=wliq+","+wfch+","+wleg+","+wnop+",'"+c_nota+"'"
csql="insert into logoper VALUES ("+c_campos+")"

wq=SQLEXEC(u_hnd,csql)
sale=(wq>0)
RETURN sale
*---------------------------------------------------------------------------------*
*******************************************************************************
*----------------------------------------------------------------
* FUNCTION Num2Let(tnNumero)
*----------------------------------------------------------------
* Devuelve un número en letras con centavos
* USO: ? Num2Let(15.11) -> QUINCE CON ONCE CENTAVOS
* RETORNA: Caracter
*----------------------------------------------------------------
FUNCTION Num2Let(tnNumero)
  LOCAL lnEntero, lnFraccion
  tnNumero = ROUND(tnNumero, 2)
  lnEntero = INT(tnNumero)
  lnFraccion = INT((tnNumero - lnEntero) * 100)
RETURN N2L(lnEntero, 0) + 'CON ' + N2L(lnFraccion, 1) + 'CENTAVOS.'
*----------------------------------------------------------------
* FUNCTION N2L(tnNro, tnFlag)
*----------------------------------------------------------------
* Devuelve un número entero en letras
* Usada por Let2Num (deben estar ambas)
* USO: ? N2L(32) -> TREINTA Y DOS
* RETORNA: Caracter
*----------------------------------------------------------------
FUNCTION N2L(tnNro, tnFlag)
  IF EMPTY(tnFlag)
    tnFlag = 0
  ENDIF
  LOCAL lnEntero, lcRetorno, lnTerna, lcMiles, ;
    lcCadena, lnUnidades, lnDecenas, lnCentenas
  lnEntero = INT(tnNro)
  lcRetorno = ''
  lnTerna = 1
  DO WHILE lnEntero > 0
    lcCadena = ''
    lnUnidades = MOD(lnEntero, 10)
    lnEntero = INT(lnEntero / 10)
    lnDecenas = MOD(lnEntero, 10)
    lnEntero = INT(lnEntero / 10)
    lnCentenas = MOD(lnEntero, 10)
    lnEntero = INT(lnEntero / 10)

    *--- Analizo la terna
    DO CASE
      CASE lnTerna = 1
        lcMiles = ''
      CASE lnTerna = 2 AND (lnUnidades + lnDecenas + lnCentenas # 0)
        lcMiles = 'MIL '
      CASE lnTerna = 3 AND (lnUnidades + lnDecenas + lnCentenas # 0)
        lcMiles = IIF(lnUnidades = 1 AND lnDecenas = 0 AND ;
          lnCentenas = 0, 'MILLON ', 'MILLONES ')
      CASE lnTerna = 4 AND (lnUnidades + lnDecenas + lnCentenas # 0)
        lcMiles = 'MIL MILLONES '
      CASE lnTerna = 5 AND (lnUnidades + lnDecenas + lnCentenas # 0)
        lcMiles = IIF(lnUnidades = 1 AND lnDecenas = 0 AND ;
          lnCentenas = 0, 'BILLON ', 'BILLONES ')
      CASE lnTerna > 5
        lcRetorno = ' ERROR: NUMERO DEMASIADO GRANDE '
        EXIT
    ENDCASE

    *--- Analizo las unidades
    DO CASE
      CASE lnUnidades = 1
        lcCadena = IIF(lnTerna = 1 AND tnFlag = 0, 'UNO ', 'UN ')
      CASE lnUnidades = 2
        lcCadena = 'DOS '
      CASE lnUnidades = 3
        lcCadena = 'TRES '
      CASE lnUnidades = 4
        lcCadena = 'CUATRO '
      CASE lnUnidades = 5
        lcCadena = 'CINCO '
      CASE lnUnidades = 6
        lcCadena = 'SEIS '
      CASE lnUnidades = 7
        lcCadena = 'SIETE '
      CASE lnUnidades = 8
        lcCadena = 'OCHO '
      CASE lnUnidades = 9
        lcCadena = 'NUEVE '
    ENDCASE

    *--- Analizo las decenas
    DO CASE
      CASE lnDecenas = 1
        DO CASE
          CASE lnUnidades = 0
            lcCadena = 'DIEZ '
          CASE lnUnidades = 1
            lcCadena = 'ONCE '
          CASE lnUnidades = 2
            lcCadena = 'DOCE '
          CASE lnUnidades = 3
            lcCadena = 'TRECE '
          CASE lnUnidades = 4
            lcCadena = 'CATORCE '
          CASE lnUnidades = 5
            lcCadena = 'QUINCE '
          OTHER
            lcCadena = 'DIECI' + lcCadena
        ENDCASE
      CASE lnDecenas = 2
        lcCadena = IIF(lnUnidades = 0, 'VEINTE ', 'VEINTI') + lcCadena
      CASE lnDecenas = 3
        lcCadena = 'TREINTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
      CASE lnDecenas = 4
        lcCadena = 'CUARENTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
      CASE lnDecenas = 5
        lcCadena = 'CINCUENTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
      CASE lnDecenas = 6
        lcCadena = 'SESENTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
      CASE lnDecenas = 7
        lcCadena = 'SETENTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
      CASE lnDecenas = 8
        lcCadena = 'OCHENTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
      CASE lnDecenas = 9
        lcCadena = 'NOVENTA ' + IIF(lnUnidades = 0, '', 'Y ') + lcCadena
    ENDCASE

    *--- Analizo las centenas
    DO CASE
      CASE lnCentenas = 1
        lcCadena = IIF(lnUnidades = 0 AND lnDecenas = 0, ;
          'CIEN ', 'CIENTO ') + lcCadena
      CASE lnCentenas = 2
        lcCadena = 'DOSCIENTOS ' + lcCadena
      CASE lnCentenas = 3
        lcCadena = 'TRESCIENTOS ' + lcCadena
      CASE lnCentenas = 4
        lcCadena = 'CUATROCIENTOS ' + lcCadena
      CASE lnCentenas = 5
        lcCadena = 'QUINIENTOS ' + lcCadena
      CASE lnCentenas = 6
        lcCadena = 'SEISCIENTOS ' + lcCadena
      CASE lnCentenas = 7
        lcCadena = 'SETECIENTOS ' + lcCadena
      CASE lnCentenas = 8
        lcCadena = 'OCHOCIENTOS ' + lcCadena
      CASE lnCentenas = 9
        lcCadena = 'NOVECIENTOS ' + lcCadena
    ENDCASE

    *--- Armo el retorno terna a terna
    lcRetorno = lcCadena + lcMiles + lcRetorno
    lnTerna = lnTerna + 1
  ENDDO
  IF lnTerna = 1
    lcRetorno = 'CERO '
  ENDIF
RETURN lcRetorno
*----------------------------------------------------------------------------------------*
function resolucion_actual
local nval1,nval2
#define SM_CXSCREEN 0           && Ancho de Screen en Píxeles 
#define SM_CYSCREEN 1           && Alto de Screen en Píxeles  
DECLARE INTEGER GetSystemMetrics IN WIN32API INTEGER nIndex
nval1=GetSystemMetrics(SM_CXSCREEN)
nval2=GetSystemMetrics(SM_CYSCREEN)
RETURN allt(str(nval1,4))+","+allt(str(nval2,4))
*-----------------------------------------------------------------*
Function Changeres(tnWidth, tnHeight)
LOCAL lnWidth, lnHeight, lnModeNum, lcDevMode
* Valores
LnModeNum 	=0
lcDevMode 	=replicate(chr(0),156)
lnWidth		=iif(empty(tnWidth),800,tnWidth)
lnHeight	=iif(empty(tnHeight),600,tnHeight)
* Instrucciones decalre dll para cambiar resolucion
DECLARE INTEGER EnumDisplaySettings IN Win32API string @lpszDevicename,;
		INTEGER iModeNum, STRING @lpDevMode
		
DECLARE INTEGER ChangeDisplaySettings IN Win32API string @lpDevMode ,;
		INTEGER dwFlags
			
*- Bucle para obtener todos los modos disponibles
Do While EnumDisplaySettings(NULL, lnModeNum, @lcDevMode)<>0
	lnModeNum =lnModeNum+1
enddo
*--Configurar la structura DevMode
lcDevMode =stuff(lcDevMode, 41, 4, Longtostr(1572864))
lcDevMode =stuff(lcDevMode, 109, 4, Longtostr(lnWidth)) && ancho
lcDevMode =stuff(lcDevMode, 41, 4, Longtostr(lnHeight)) && Alto
*-Cambio resolucion
ChangeDisplaySettings(@lcdevmode,1)
RETURN .t.
***********************************************************************************************
Function LongTostr(lnLongval)
*- Utilizada por CHANGERES
Local lncnt, lcRetstr
lcRetstr=' '
for lncnt=24 to 0 step -8
	lcRetstr= chr(int(lnLongval/(2^lncnt))) + lcretstr
	lnLongval= Mod(lnLongval, (2^lncnt))
next
return lcretstr
***********************************************************************************************
FUNCTION grabaerr_log(ctira,clog_file,lcrea)
LOCAL  gnErrFile,sale,ctira_aux
sale=.t.
clog_file=IIF(PARAMETERS()=1,"sgl_errors.log",clog_file)
lcrea=IIF(PARAMETERS()<=2,.t.,lcrea)
ctira_aux=REPLICATE("*",80)+CHR(13)+CHR(10)+;
      CDOW(DATE())+" - "+DTOC(DATE())+" - "+TIME()
IF TYPE("oapp.username")="C"
   ctira_aux=ctira_aux+CHR(13)+CHR(10)+;
             "[  Usuario: ] "+ALLTRIM(UPPER(oapp.username))+CHR(13)+CHR(10)+;
             "[ Equipo-Usuario: ] "+UPPER(SYS(0))
ENDIF 
ctira=ctira_aux+CHR(13)+CHR(10)+ctira
IF FILE('&clog_file')  && Does file exist? 
   gnErrFile = FOPEN('&clog_file',12)     && If so, open read/write
ELSE
   gnErrFile = FCREATE('&clog_file')  && If not create it
ENDIF
IF gnErrFile < 0     && Check for error opening file
   sale= .f. 
ELSE  && If no error, write to file
   nsize=FSEEK(gnErrFile,0,2)
   nwritten=FWRITE(gnErrFile,ctira+CHR(13)+CHR(10))
ENDIF 
IF gnErrFile > 0
   FCLOSE(gnErrFile)
ENDIF    
RETURN sale
***********************************************************************************************
FUNCTION _EOM(dFecha)  
*------------------------------------------------
* Retorna el último día del mes (EndOfMonth)
* USO: _EOM(DATE())
* RETORNA: Fecha
*------------------------------------------------
  LOCAL ld 
  ld = GOMONTH(dFecha,1)
RETURN ld - day(ld)
*------------------------------------------------
function exrembarra(obarra)
local area
area=ALIAS()
if len(alltrim(obarra))<7
   wcodbarra=padl(alltrim(obarra),13,"0")
   obarra=wcodbarra
endif
wvalor=padl(obarra,13,"0")
res=REQUERY("crem_barra")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"ERROR")
   RETURN .f. 
ENDIF 
SELECT crem_barra
GO TOP 
IF EOF()
   MESSAGEBOX("Código NO HALLADO",16,"Medicamento")
   SELECT &area
   RETURN .f. 
ENDIF 
wremedio=producto
wcodbarra=codbarra
SELECT &area
RETURN .t.
********************************************************************************
PROCEDURE preparoup(ucam)
*ucam="D:\AJBASES\"

CLOSE DATABASES 
*----- SALIDAS ---------------------------------
WAIT WINDOW "Salidas..." NOWAIT 
SELECT 0 
USE (ucam+"salidas") EXCLUSIVE 
ZAP
APPEND FROM \usr\medicam\salidas FOR nromov>0 AND !DELETED()
replace ALL orirece WITH 9 FOR orirece=0
replace ALL medico WITH 999 FOR medico=0
replace ALL enfermero WITH 99 FOR enfermero=0
replace ALL usuario WITH "SILVIA",fchmodif WITH CTOD("01/01/1900"),;
            fchingreso WITH fecha,usumodif WITH ""
use
SELECT 0 
USE (ucam+"salilin") EXCLUSIVE 
ZAP
APPEND FROM \usr\medicam\salilin FOR nromov>0 AND !DELETED()
USE 

*-------- FACTURAS  ----------------------------
WAIT WINDOW "Facturas..." NOWAIT 
SELECT 0 
USE (ucam+"facturas") EXCLUSIVE
zap
APPEND FROM \usr\medicam\facturas FOR tipodoc $ "NCR/FAC/NDB/REC"
replace ALL nromov WITH RECNO(),usuario WITH "SILVIA",fchmodif WITH CTOD("01/01/1900"),;
            fchingreso WITH fecha,usumodif WITH "",moneda WITH '1'
replace ALL parastock WITH "N" FOR EMPTY(parastock)

*------- FACLINE ---------------------------------
WAIT WINDOW "Líneas de Facturas..." NOWAIT 
SELECT 0 
USE (ucam+"ofacline") EXCLUSIVE
ZAP
INDEX on tipodoc+PADR(serie,2)+STR(factura,10)+STR(proveedor,4) TAG unico
SET ORDER TO tag unico 
APPEND FROM \usr\medicam\facline

SELECT facturas
GO top
werr=0
DO WHILE !EOF()
   wtp=tipodoc
   wser=serie
   wprov=proveedor
   wfac=factura
   wnromov=nromov
   wmuevestock=parastock
   SELECT ofacline
   SEEK wtp+PADR(wser,2)+STR(wfac,10)+STR(wprov,4)
   IF EOF()
      werr=werr+1
   ELSE
      li=1
      DO WHILE !EOF() AND wtp=tipodoc AND ALLTRIM(wser)=ALLTRIM(serie) AND wprov=proveedor AND wfac=factura
         replace nromov WITH wnromov,linea WITH li
         li=li+1
         SKIP
      ENDDO 
   ENDIF 
   SELECT facturas
   SKIP 
ENDDO
USE IN facturas
USE IN ofacline
USE (ucam+"facline") EXCLUSIVE
ZAP
APPEND FROM (ucam+"ofacline.dbf")
replace ALL vence_medi WITH CTOD("01/01/1900")
USE 

MESSAGEBOX("REGISTROS DE FACTURAS SIN LINEAS="+STR(werr,5),16,"FACLINE")

*------------ MEDICAMENTOS --------------------------------------------------------*
SELECT 0
USE (ucam+"remedios") EXCLUSIVE
zap
COPY STRUCTURE TO (ucam+"reme_noe")
APPEND FROM \usr\medicam\remedios
replace ALL activo WITH "S",stk_cierre WITH saldo
USE
CLOSE DATABASES 
*- Compras con REMEDIOS que no están ------*
SELECT distinct producto ;
FROM (ucam+"facline") ;
WHERE producto NOT in (SELECT producto FROM (ucam+"remedios")) ;
ORDER BY producto ;
into DBF(ucam+"noexisten")

USE IN noexisten 
USE (ucam+"reme_noe") EXCLUSIVE
APPEND FROM (ucam+"noexisten.DBF")
REPLACE ALL nombre WITH "FUE ELIMINADO",detalle WITH "FUE ELIMINADO",pventa WITH 0.00,;
            sicofar WITH "N",receta WITH "N",saldo WITH 0.00,proveedor WITH 99999,;
            factor WITH 0,codbarra WITH "XXUU",fecsaldo WITH CTOD("01/01/1900"),;
            activo WITH "N",stk_cierre WITH 0.00
USE
USE IN remedios
USE IN facline 
CLOSE DATABASES
USE (ucam+"remedios") EXCLUSIVE
APPEND FROM (ucam+"reme_noe.DBF")
USE IN remedios 

*- SALIDAS con REMEDIOS que no están -----------------------------------------------*
SELECT distinct producto ;
FROM (ucam+"salilin") ;
WHERE producto NOT in (SELECT producto FROM (ucam+"remedios")) ;
ORDER BY producto ;
into DBF(ucam+"noexisten")

USE IN noexisten 
USE (ucam+"reme_noe") EXCLUSIVE
zap
APPEND FROM noexisten
REPLACE ALL nombre WITH "FUE ELIMINADO",detalle WITH "FUE ELIMINADO",pventa WITH 0.00,;
			sicofar WITH "N",receta WITH "N",saldo WITH 0.00,proveedor WITH 99999,;
			factor WITH 0,codbarra WITH "XXUU",fecsaldo WITH CTOD("01/01/1900"),;
			activo WITH "N",stk_cierre WITH 0.00
USE
USE IN remedios
USE IN salilin
CLOSE DATABASES
USE (ucam+"remedios") EXCLUSIVE
APPEND FROM (ucam+"reme_noe.DBF")
USE IN remedios 

*---------------------------- SALIDAS CON MEDICAMENTOS QUE NO EXISTEN -------------
WAIT WINDOW "Salidas Con Medicamentos que no EXISTEN..." NOWAIT 
SELECT distinct nromov,producto ;
FROM (ucam+"salilin") ;
WHERE producto NOT in (SELECT producto FROM (ucam+"remedios"));
order by nromov;
into cursor moveli

USE IN salilin
USE IN remedios 

SELECT 0
USE (ucam+"salidas") EXCLUSIVE 
INDEX on nromov TAG pormov
SET ORDER TO pormov

SELECT 0
USE (ucam+"salilin") EXCLUSIVE 
INDEX on nromov TAG pormov 
SET ORDER TO pormov 

SELECT moveli
GO top
DO WHILE !EOF()
    wmov=nromov
    SELECT salidas
    SEEK wmov
    IF !EOF()
       DELETE
    ELSE
       MESSAGEBOX("SALIDAS NO HALLADA")
    ENDIF 
    SELECT salilin
    SEEK wmov
    IF !EOF()
       DO WHILE !EOF() AND wmov=nromov
          DELETE
          skip
       ENDDO 
    ELSE
       MESSAGEBOX("SALILI NO HALLADA")
    ENDIF 
    SELECT moveli
    skip
ENDDO
SELECT salidas
PACK
SELECT salilin
PACK

SELECT salidas
GO top
DO WHILE !EOF()
   wmov=nromov
   SELECT salilin
   SEEK wmov
   IF EOF()
      SELECT SALIDAS
      DELETE
      SKIP
      LOOP 
   ENDIF 
   wli=1
   DO WHILE !EOF() AND wmov=nromov
      replace linea WITH wli
      wli=wli+1
      IF wli>2
         DELETE
      ENDIF 
      SKIP 
   ENDDO
   SELECT salidas
   IF enfermero=0
      replace enfermero WITH 99
   ENDIF 
   SKIP 
ENDDO
SELECT salilin
PACK
USE 
CLOSE ALL 
*---------------------------- MEDICOS QUE NO EXISTEN EN SALIDAS -------------
WAIT WINDOW "MEDICOS QUE NO EXISTEN EN SALIDAS..." NOWAIT 
SELECT 0
USE (ucam+"salidas") EXCLUSIVE 

SELECT 0
USE (ucam+"medicos") EXCLUSIVE 
ZAP
APPEND FROM \usr\cajas\medicos FOR medico>0
INDEX on medico TAG pormed 
SET ORDER TO tag pormed

SELECT salidas
GO top
DO WHILE !EOF()
   wmed=medico
   IF wmed=999
      SKIP
      LOOP
   ENDIF  
   SELECT medicos
   SEEK wmed
   IF EOF()
      SELECT salidas
      replace medico WITH 999
   ELSE
      SELECT salidas 
   ENDIF 
   SKIP 
ENDDO
CLOSE DATABASES 
WAIT CLEAR 
MESSAGEBOX("FIN")
RETURN 
*------------------------------------------------------------------------------*
FUNCTION cdow_es(u_fecha)
LOCAL sale
DO case
   CASE DOW(u_fecha)=1
        sale="Domingo"
   CASE DOW(u_fecha)=2
        sale="Lunes"
   CASE DOW(u_fecha)=3
        sale="Martes"
   CASE DOW(u_fecha)=4
        sale="Miércoles"
   CASE DOW(u_fecha)=5
        sale="Jueves"
   CASE DOW(u_fecha)=6
        sale="Viernes"
   CASE DOW(u_fecha)=7
        sale="Sábado"
   OTHERWISE 
        sale="??????"
ENDCASE 
RETURN sale
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
PROCEDURE update_local
*------------------------------------------------------------------------------*
whnd=oapp.hnd
wsdia="C:\BASELOCAL\"+DTOS(DATE())+".TXT"

WAIT WINDOW "Un Momento Por Favor: Actualizando Archivos Locales..." NOWAIT 

*------- SOCIOS -------------------------------------------*
csql="select * from socios order by socnom"
csql_activos="select * from socios where soctip='A'"  && si quiero solo los Activos
res=SQLEXEC(whnd,csql,"cursocios")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 
res=SQLEXEC(whnd,csql_activos,"cur_activos")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 

SELECT cursocios
COPY all to C:\BASELOCAL\SOCIOS
SELECT cur_activos
COPY all to C:\BASELOCAL\SOCIOS_A

SELECT 0
USE c:\baselocal\socios EXCLUSIVE 
INDEX on soccod TAG isoccod
INDEX on socnom TAG isocnom

SELECT 0
USE c:\baselocal\socios_a EXCLUSIVE 
INDEX on soccod TAG isoccod
INDEX on socnom TAG isocnom

USE IN socios
USE IN cursocios
USE IN cur_activos
USE IN socios_a

*------- MEDICAMENTOS -------------------------------------------*
csql="select * from remedios order by nombre"
res=SQLEXEC(whnd,csql,"curemedios")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 

SELECT curemedios
COPY all to C:\BASELOCAL\remedios

SELECT 0
USE c:\baselocal\remedios EXCLUSIVE 
INDEX on producto TAG iprodu
INDEX on nombre TAG inombre

USE IN remedios
USE IN curemedios

*------- MEDICOS -------------------------------------------*
csql="select * from medicos order by nombre"
res=SQLEXEC(whnd,csql,"curmedicos")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 

SELECT curmedicos
COPY all to C:\BASELOCAL\medicos

SELECT 0
USE c:\baselocal\medicos EXCLUSIVE 
INDEX on medico TAG imedico
INDEX on nombre TAG inombre

USE IN medicos
USE IN curmedicos

*------- PROVEEDORES -------------------------------------------*
csql="select * from proveedo order by nombre"
res=SQLEXEC(whnd,csql,"curprov")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 

SELECT curprov
COPY all to C:\BASELOCAL\proveedo

SELECT 0
USE c:\baselocal\proveedo EXCLUSIVE 
INDEX on proveedor TAG iprov
INDEX on nombre TAG inombre

USE IN proveedo
USE IN curprov

*------- CIUDAD -------------------------------------------*
csql="select * from ciudad order by detalle"
res=SQLEXEC(whnd,csql,"curciudad")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 

SELECT curciudad
COPY all to C:\BASELOCAL\ciudad

SELECT 0
USE c:\baselocal\ciudad EXCLUSIVE 
INDEX on ciudad TAG iciudad
INDEX on detalle TAG inombre

USE IN ciudad
USE IN curciudad

*------- DEFING -------------------------------------------*
csql="select * from defing order by detalle"
res=SQLEXEC(whnd,csql,"curdefing")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN 
ENDIF 

SELECT curdefing
COPY all to C:\BASELOCAL\defing

SELECT 0
USE c:\baselocal\defing EXCLUSIVE 
INDEX on tipo TAG itipo
INDEX on detalle TAG inombre

COPY all to (wsdia) for 2=3

USE IN defing
USE IN curdefing

*------------------------------------------------------------------------------------------*
WAIT CLEAR 
RETURN
*------------------------------------------------------------------------------------------*
FUNCTION add_logueos(u_hnd,u_caja,u_user,u_tar,u_ip)
LOCAL lerror,xsql,xcampos,oErr1,xres 
lerror=.f.

TRY 
x_campos=ALLTRIM(STR(u_caja))+",'"+u_user+"','"+u_tar+"','"+u_ip+"','"+;
         ALLTRIM(UPPER(SUBSTR(SYS(0),1,AT("#",SYS(0))-1)))+"')"
xsql="insert into a_logueos (caja,usuario,tarjeta,ip, pcname) values ("+x_campos
xres=SQLEXEC(u_hnd,xsql)
IF res<=0
   THROW
ENDIF 
CATCH TO oErr1
    lerror=.t.
ENDTRY 
RETURN lerror
*----------------------------------------------------------------------------------------*   
FUNCTION del_logueos(u_hnd,u_caja,u_user,u_tar,u_ip)
**-- Al Salir del Sistema Correctamente Elimino Registro/s Con La Tarjeta de Red Actual
LOCAL lerror,xsql,oErr1,xres 
lerror=.f.

TRY 
*!*	xsql="delete from a_logueos where usuario='"+u_user+"' and tarjeta='"+u_tar+"' "+;
*!*	     "and caja="+ALLTRIM(STR(u_caja))
xsql="delete from a_logueos where tarjeta='"+u_tar+"' "
xres=SQLEXEC(u_hnd,xsql)
IF res<=0
    AERROR(verr)
    MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"ERROR")
   THROW
ENDIF 
CATCH TO oErr1
    lerror=.t.
ENDTRY 
RETURN lerror
*----------------------------------------------------------------------------------------*   
FUNCTION ok_logueos(u_hnd,u_caja)
*--- Verifica que no Haya Otro Cajero con U_CAJA en Otra Tarjeta de Red
*-- SI Hubo Apagón Se Puede Entrar En la Misma PC que EStaba antes del Apagón
LOCAL cestaba,xsql,oErr1,xres 
cestaba=""

TRY 
xsql="select * from a_logueos where tarjeta<>'"+oapp.tarjeta+"' "+;
     "and caja="+ALLTRIM(STR(u_caja))
xres=SQLEXEC(u_hnd,xsql,"u_esta")
IF res<=0
   THROW
ENDIF 
SELECT u_esta
GO TOP 
IF !EOF() AND u_caja<>0 && BLOQUEAR ENTRADA
   cestaba="Caja "+ALLTRIM(STR(u_caja))+" Poseída Por el Usuario "+ALLTRIM(usuario)+;
           " En la Computadora: "+ALLTRIM(pcname)+CHR(13)+CHR(10)+;
           "Fecha: "+TTOC(fecha)+"  Tarjeta: "+ALLTRIM(tarjeta)
ELSE
   cestaba=""
ENDIF    
USE IN u_esta 

CATCH TO oErr1
    cestaba=""
ENDTRY 
RETURN cestaba
*----------------------------------------------------------------------------------------*   
function puntos(cifra,forma)
local t,numero
pasaje="999,999,999.99"
if pcount()<1 .or. pcount()>2
   numero=transform(cifra,'&pasaje.')
   RETURN numero
endif
if pcount()=1
   numero=cifra
else
   numero=transform(cifra,'&forma.')
endif
t=at(".",numero)
if t<>0
   numero=stuff(numero,t,1,";")
endif
t=at(",",numero)
do while t<>0
   numero=stuff(numero,t,1,".")
   t=at(",",numero)
enddo
t=at(";",numero)
if t<>0
   numero=stuff(numero,t,1,",")
endif
RETURN numero
*******************************************************************************
FUNCTION f_ulbackup(phnd)
LOCAL sale
sale=CTOD("")
csql="Select a.name,Backup_Date from master.dbo.sysdatabases a "+;
	 "left join "+;
     "(select database_name,max(backup_finish_date) backup_date "+;
     "from msdb.dbo.backupset where backup_finish_date <= getdate() "+;
     "group by database_name)  B on  a.name = b.database_name"

res=SQLEXEC(phnd,csql,"ubackups")
IF res<>1
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error")
   RETURN CTOD("")
ENDIF

SELECT ubackups
LOCATE FOR ALLTRIM(UPPER(name))="AJP"
IF !eof()
   sale=backup_date
ENDIF 
USE IN ubackups
RETURN sale
*------------------------------------------------------------------------------------------*
FUNCTION f_renamebkp(fname)
LOCAL sale
TRY 
EXTERNAL ARRAY afil
sale=0
IF DIRECTORY("G:\USR\")
	wson=ADIR(afil,"G:\usr\&fname")
	IF wson=1
	   wname="AJP"+ALLTRIM(STR(YEAR(afil[1,3]),4))+ALLTRIM(STRZERO(month(afil[1,3]),2))+;
	         ALLTRIM(STRZERO(day(afil[1,3]),2))+".BKP"
	   wdes="G:\USR\"+wname
	   wori="G:\USR\"+fname+"."
	   IF !FILE("&wdes")
   	      RENAME (wori) TO (wdes)
   	   ENDIF 
	ENDIF 
ENDIF 
CATCH TO oErr

ENDTRY 
RETURN sale   
*------------------------------------------------------------------------------------------*
function nomes(man)
declare m[12]
if man<=0 .or. man>12
   return(replicate("-",9))
endif
m[1]="Enero    "
m[2]="Febrero  "
m[3]="Marzo    "
m[4]="Abril    "
m[5]="Mayo     "
m[6]="Junio    "
m[7]="Julio    "
m[8]="Agosto   "
m[9]="Setiembre"
m[10]="Octubre  "
m[11]="Noviembre"
m[12]="Diciembre"
return(m[man])
**********************************************************************************************************
*--------------------------------------------------------------------------------------------------------*
Function InternetOnLine 
Declare long InetIsOffline IN url.dll long dwFlags 
Return (InetIsOffline(0)=0) 
*--------------------------------------------------------------------------------------------------------*
***************************************** 
*-- comprobación de conexion a internet 
***************************************** 
Function IsInterNetActive (tcURL) 
*********************************** 
* PARAMETERS: URL, no olvidar pasar la URL completa, con http:// al inicio 
* Retorna .T. si hay una conexion a internet activa 
*********************************** 
tcURL = IIF(TYPE("tcURL")="C" AND !EMPTY(tcURL),tcURL,"http://www.yahoo.com") 

DECLARE INTEGER InternetCheckConnection in wininet; 
STRING lpszUrl,; 
INTEGER dwFlags,; 
INTEGER dwReserved 

RETURN ( InternetCheckConnection(tcURL, 1, 0) == 1) 
***********************************************************************************************************
FUNCTION correo_smtp_google(txtnamesis,txtbody,pasunto,panexo)
LOCAL lcSchema, loConfig, loMsg, loError, lcErr
TRY
  panexo=IIF(PARAMETERS()=3,"",panexo)
  lcErr = ""
  lcSchema = "http://schemas.microsoft.com/cdo/configuration/"
  loConfig = CREATEOBJECT("CDO.Configuration")
  WITH loConfig.FIELDS
    .ITEM(lcSchema + "smtpserver") = "smtp.gmail.com"
    .ITEM(lcSchema + "smtpserverport") = 465
    .ITEM(lcSchema + "sendusing") = 2
    .ITEM(lcSchema + "smtpauthenticate") = .T. 
    .ITEM(lcSchema + "smtpusessl") = .T.
    .ITEM(lcSchema + "sendusername") = "neroig@gmail.com"
    .ITEM(lcSchema + "sendpassword") = "cangrejo"
    .UPDATE
  ENDWITH
  
  loMsg = CREATEOBJECT ("CDO.Message")
  WITH loMsg
    .Configuration = loConfig
    .FROM = "NRI Sistemas <rodnel@hotmail.com>"
    .TO = glomail_cli1  && 
    
     * Acuses 
*!*	    .Fields("urn:schemas:mailheader:disposition-notification-to") = .From
*!*	    .Fields("urn:schemas:mailheader:return-receipt-to") = .From    
*!*	    .Fields("urn:schemas:mailheader:disposition-notification-to") = "rodnel@hotmail.com"
*!*	    .Fields("urn:schemas:mailheader:return-receipt-to") = "rodnel@hotmail.com"
    
    .Cc = glomail_cli2 && "jcc@centrocar.com.uy"
    .Bcc = "rodnel@hotmail.com"
    
    .Subject = pasunto+txtnamesis
    .TextBody = "Fecha y Hora: "+TTOC(datetime())+CHR(13)+CHR(10)+txtbody+CHR(13)+CHR(10)

    IF !EMPTY(panexo)      
       .AddAttachment(panexo)
    ENDIF 
    *.AddAttachment("D:\CTM\control1.xls")
    
  *-- Prioridad
  && -1=Low, 0=Normal, 1=High
  .Fields("urn:schemas:httpmail:priority") = 1
  .Fields("urn:schemas:mailheader:X-Priority") = 1
  *-- Importancia
  && 0=Low, 1=Normal, 2=High
  .Fields("urn:schemas:httpmail:importance") = 2
  .Fields.Update
    
      
    .Send()
  ENDWITH
  IF !EMPTY(panexo)
     ERASE (panexo)
  ENDIF 
CATCH TO loError

	    crlf=CHR(13)+CHR(10)
	    WAIT CLEAR 
        MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	 *MESSAGEBOX(msgerr,16,"Rutina Correo")	

FINALLY
  RELEASE loConfig, loMsg
  STORE .NULL. TO loConfig, loMsg
  IF EMPTY(lcErr)
    *MESSAGEBOX("El mensaje se envió con éxito", 64, "Aviso")
  ELSE
    *MESSAGEBOX(lcErr, 16 , "Error")
  ENDIF
ENDTRY
RETURN EMPTY(lcErr)
*-------------------------------------------------------------------------------------------------------------------------*
*------------------------------------------------------
* FUNCTION WaitWinCen(tcTexto, tcOpc, tnTimeout)
* - - - - - - - - - - - - - - - - - - - - - - - - - - -
* Muestra la ventana de WAIT WINDOWS centrada
* USO: WaitWindowsCentrado(tcTexto, tcOpc, tnTimeOut)
* EJEMPLO: WaitWindowsCentrado("Espere un momento...", "NOWAIT", 0)
*    ? WaitWinCen()
*    ? WaitWinCen(lcTexto,6,,,5)
*    ? WaitWinCen(lcTexto,6,"NOWAIT")
* RETORNA: Caracter
* AUTOR: LMG
*------------------------------------------------------
FUNCTION WaitWinCen(tcTexto, mifila, tcOpc, tnTimeOut)

  LOCAL lnMaxLen, lnNroLin, lnRelFil, lnRelCol, ;
    lnRows_VFP, lnFil, lnCol, lcRet, lcCmd, ln, la(1)

  *-- Texto del mensaje
  IF EMPTY(tcTexto)
    tcTexto = "Presione una tecla para continuar..."
  ENDIF
  IF EMPTY(mifila)
     mifila=0
  ENDIF 

  *-- Linea mas larga de tcTexto (si es multilinea)
  lnMaxLen = 0
  lnNroLin = ALINES(la, tcTexto)
  FOR ln = 1 TO lnNroLin
    lnMaxLen = MAX(lnMaxLen,LEN(la(ln)))
  ENDFOR

  *-- Filas de ventana completa _VFP, distinto a WROWS(_SCREEN.NAME)
  lnRows_VFP = _VFP.HEIGHT / FONTMETRIC(1,_SCREEN.FONTNAME,_SCREEN.FONTSIZE)

  *-- Relación entre el tamaño de las
  *-- fuentes de WAIT WINDOWS y _SCREEN
  lnRelFil = FONTMETRIC(1,'Arial',9) / FONTMETRIC(1,_SCREEN.FONTNAME,_SCREEN.FONTSIZE)
  lnRelCol = FONTMETRIC(6,'Arial',9) / FONTMETRIC(6,_SCREEN.FONTNAME,_SCREEN.FONTSIZE)
  lnFil = WLROW(_SCREEN.NAME) + (lnRows_VFP - lnNroLin * lnRelFil) / 2
  lnCol = WLCOL(_SCREEN.NAME) + (WCOLS(_SCREEN.NAME) - lnMaxLen * lnRelCol) / 2
  IF mifila=0
     mifila=lnFil
  ELSE 
     lnFil=mifila
  ENDIF 

  *-- Armo el Comando
  lcCmd = [WAIT WINDOWS tcTexto TO lcRet AT lnFil,lnCol]  && Lo Coloca en CEntro CEntro

  *-- Cláusulas NOWAIT y NOCLEAR
  IF NOT EMPTY(tcOpc) AND VARTYPE(tcOpc) = "C"
    IF "NOWA" $ UPPER(tcOpc)
      lcCmd = lcCmd + [ NOWAIT]
    ENDIF
    IF "NOCL" $ UPPER(tcOpc)
      lcCmd = lcCmd + [ NOCLEAR]
    ENDIF
  ENDIF

  *-- Cláusula TIMEOUT
  IF NOT EMPTY(tnTimeOut) AND VARTYPE(tnTimeOut) = "N"
    lcCmd = lcCmd + [ TIMEOUT tnTimeOut]
  ENDIF

  *-- Ejecuto el comando
  &lcCmd

RETURN lcRet
*-----------------------------------------------------------------------------------------------------------------*
FUNCTION registrar_graficador
LOCAL wdirsys,nerror,wfile,wdestino
wdirsys=_systemdir()
nerror=.t.
wfile=wdirsys+"\"+"flpgrf.ocx"
wdestino=wdirsys+"\"

IF !FILE("&wfile") 
   IF glo_administrador
      COPY FILE H:\BITMAPS\Activex\flpgrf.* to (wdestino+"flpgrf.*")
      IF !FILE("&wfile")
         *- error al copiar 
         nerror=.f.
         *MESSAGEBOX("Error Al Copiar Archivos Necesarios al Directorio de Windows",48,"ERROR")
      ENDIF 
   ELSE && !glo_administrador
      nerror=.f.
      *MESSAGEBOX("Debe Ser Administrador del Equipo Para Instalar Graficador",48,"Consulte con DOSA: Int. 3366")
   ENDIF 
ENDIF 
IF !nerror
   RETURN nerror
ENDIF 
   
IF auto_Registro(wdirsys,"flpgrf.ocx")
   *MESSAGEBOX("GRAFICADOR INSTALADO",64,"OK")
   lok=.t.
ELSE
   *MESSAGEBOX("GRAFICADOR NO FUE INSTALADO",64,"ERROR")
   IF !glo_administrador
      *MESSAGEBOX("Debe Ser Administrador del Equipo Para Instalar Graficador",48,"Consulte con DOSA: Int. 366")
   ENDIF 
   lok=.f.
ENDIF 
RETURN lok 
*-------------------------------------------------------------------------------------------------------------------------------*
FUNCTION auto_registro(p_dirsystem,p_file)
LOCAL sale
sale=.f.
DECLARE LONG DllRegisterServer IN (p_dirsystem+"\"+p_file)
IF DllRegisterServer() = 0
    sale=.t.
ELSE
    sale=.f.
ENDIF
RETURN sale 
*--------------------------------------------------------------------------------------------------------------------------------------*
FUNCTION registrar_mswinsck
LOCAL wdirsys,nerror,wfile,wdestino
wdirsys=_systemdir()
nerror=.t.
wfile=wdirsys+"\"+"mswinsck.ocx"
wdestino=wdirsys+"\"

IF !FILE("&wfile") 
   IF glo_administrador
      COPY FILE H:\BITMAPS\Activex\mswinsck.* to (wdestino+"mswinsck.*")
      IF !FILE("&wfile")
          nerror=.f.
          MESSAGEBOX("Error Al Copiar Archivos Necesarios al Directorio de Windows",48,"ERROR")
      ENDIF 
   ELSE && !glo_administrador
       MESSAGEBOX("Debe Ser Administrador del Equipo Para Instalar Software Necesario del Sistema",48,"Consulte con DOSA: Int. 366")
       nerror=.f.
   ENDIF 
ENDIF 
IF !nerror
   RETURN nerror
ENDIF 
   
IF auto_Registro(wdirsys,"mswinsck.ocx")
   *MESSAGEBOX("MSWINSCK INSTALADO",64,"OK")
   lok=.t.
ELSE
   *MESSAGEBOX("MSWINSCK NO FUE INSTALADO",64,"ERROR")
   IF !glo_administrador
      MESSAGEBOX("Debe Ser Administrador del Equipo Para Instalar Software Necesario del Sistema",48,"Consulte con DOSA: Int. 366")
   ENDIF 
   lok=.f.
ENDIF 
RETURN lok 
*------------------------------------------------------------------------------------------------------------------------------------------*
FUNCTION WinDir()
LOCAL lcPath, lnSize
lcPath = SPACE(255)
lnsize = 255
DECLARE INTEGER GetWindowsDirectory IN Win32API ;
         STRING @pszSysPath,;
         INTEGER cchSysPath
lnSize = GetWindowsDirectory(@lcPath, lnSize)
IF lnSize <= 0
   lcPath = ""
ELSE
   lcPath = ADDBS(SUBSTR(lcPath, 1, lnSize))
ENDIF
RETURN lcPath
*******************************************************************************
FUNCTION _SystemDir
*--------------------------------------------
* Retorna el directorio SYSTEM de Windows
* sin "\" al final ("C:\WINNT\SYSTEM32")
*--------------------------------------------
LOCAL lcPath, lnSize
lcPath = SPACE(255)
lnsize = 255
DECLARE INTEGER GetSystemDirectory IN Win32API ;
   STRING  @pszSysPath,;
   INTEGER cchSysPath
lnSize = GetSystemDirectory(@lcPath, lnSize)
IF lnSize <= 0
   lcPath = ""
ELSE
   lcPath =  SUBSTR(lcPath, 1, lnSize)
ENDIF
RETURN lcPath
*******************************************************************************
*- Tambien se puede hacer sin el WSH, funciona con archivos y carpetas ocultas, 
*- de solo lectura, de sistema ...
*
* Función: DELTREE(path)
*
* Igual al DELTREE de MS-DOS, pero no pregunta
* borra un directorio completo.
*
* Parametros:
*
*		path		- path absoluto a borrar
*
* Ejemplos:
*		 llret = DELTREE("C:\TEMP\009")
*
* Retorno: 
*
*	.F. / .T. si se há conseguido borrar o no
*
clear
ret=deltree("D:\TEMP\PRUEBA")
?ret
return
*------------------------------------------------------------------------------------------*
FUNCTION deltree(tcdirectorio)
DECLARE SHORT SetFileAttributes IN KERNEL32 ;
	STRING @ lpFileName, INTEGER dwAttributes

LOCAL lcdirectorio,lamatriz, llret
DIMENSION lamatriz(1)
IF DIRECTORY(tcdirectorio) && ,1)
	lcdirectorio="'"+tcdirectorio+"'"
	DO WHILE ADIR(lamatriz,tcdirectorio+"\*.*","HD")>2
		IF 'R'$lamatriz(3,5) OR 'H'$lamatriz(3,5) OR 'A'$lamatriz(3,5)
			SetFileAttributes(tcdirectorio+"\"+lamatriz(3,1), 0)
			DELETE FILE (tcdirectorio+"\*.*")
		ELSE
			deltree(tcdirectorio+"\"+lamatriz(3,1))
		ENDIF 
	ENDDO
	DELETE FILE(tcdirectorio+"\*.*")
	IF ADIR(lamatriz,tcdirectorio+"\*.*","HD")=2
   		   RMDIR &lcdirectorio
	ENDIF
	IF DIRECTORY(tcdirectorio) && ,1)
		llret = .F.
	ELSE
		llret = .T.
	ENDIF
ELSE
	llret = .t.
ENDIF
RETURN llret
*---------------------------------------------------------------------------------*
FUNCTION grabo_bitacora_pc(p_ope,p_datos)
LOCAL ok_salida,wdatos,wop,c_campos
*------- GRABO BITACORA ---------------*
TRY 

ok_salida=.t.
WUSUARIO=ALLTRIM(OAPP.username)
WEQUIPO=ALLTRIM(oapp.winuser)+" / "+ALLTRIM(oapp.ip)+" / "+ALLTRIM(oapp.tarjeta)

wop=p_ope
wdatos=p_datos

c_campos="'"+wusuario+"','"+wop+"','"+wdatos+"','"+WEQUIPO+"','"+oapp.pc+"'"

whnd=oapp.hnd
*-- Firma Liquidación ----------------------------------------------------------**
csql0="BEGIN TRANSACTION "+CHR(13)+CHR(10)+;
      "insert into bitacora (usuario, comentario, datos, equipo,pc) VALUES ("+c_campos+") "+CHR(13)+CHR(10)+;
      "COMMIT TRANSACTION"
      
res1=SQLEXEC(whnd,csql0)
IF res1<=0
   AERROR(verr)
   *MESSAGEBOX(STR(verr[1],7)+" "+verr[2],48,"FIRMA 1")
   ok_salida=.f.
   *THROW "ERROR"
ELSE
   *MESSAGEBOX("Grabado en Botacora Con Exito",64,"Todo OK")   
   ok_salida=.t.
ENDIF 

CATCH TO  oErr
    ok_salida=.f.
ENDTRY 
RETURN ok_salida 
*--------------------------------------------------------------------------------*
FUNCTION correo_smtp_presupus(txtnamesis,txtbody,pasunto,panexo,emailccar)
LOCAL lcSchema, loConfig, loMsg, loError, lcErr
TRY
*  panexo=IIF(PARAMETERS()=3,"",panexo)
  lcErr = ""
  lcSchema = "http://schemas.microsoft.com/cdo/configuration/"
  loConfig = CREATEOBJECT("CDO.Configuration")
  WITH loConfig.FIELDS
    .ITEM(lcSchema + "smtpserver") = "smtp.gmail.com"
    .ITEM(lcSchema + "smtpserverport") = 465
    .ITEM(lcSchema + "sendusing") = 2
    .ITEM(lcSchema + "smtpauthenticate") = .T. 
    .ITEM(lcSchema + "smtpusessl") = .T.
    .ITEM(lcSchema + "sendusername") = glousergmail && "neroig@gmail.com"
    .ITEM(lcSchema + "sendpassword") = glouserpass && "cangrejo"
    .UPDATE
  ENDWITH
  
  loMsg = CREATEOBJECT ("CDO.Message")
  WITH loMsg
    .Configuration = loConfig
    .FROM = "Centro CAR <"+glousergmail+">"
    .TO = emailccar  && 
    
     * Acuses 
*!*	    .Fields("urn:schemas:mailheader:disposition-notification-to") = .From
*!*	    .Fields("urn:schemas:mailheader:return-receipt-to") = .From    
*!*	    .Fields("urn:schemas:mailheader:disposition-notification-to") = "rodnel@hotmail.com"
*!*	    .Fields("urn:schemas:mailheader:return-receipt-to") = "rodnel@hotmail.com"
    
    .Cc =  "" && "rodnel@hotmail.com" && "jcc@centrocar.com.uy"
    .Bcc = "" && "neroig@gmail.com"
    
    .Subject = pasunto
    .TextBody = "Fecha y Hora: "+TTOC(datetime())+CHR(13)+CHR(10)+txtbody+CHR(13)+CHR(10)

    EXTERNAL ARRAY panexo
    IF !EMPTY(panexo)
       FOR yi=1 TO ALEN(panexo,1)
            wcfile=panexo[yi]
           .AddAttachment(wcfile)
       NEXT 
    ENDIF 
    *.AddAttachment("D:\CTM\control1.xls")
    
  *-- Prioridad
  && -1=Low, 0=Normal, 1=High
  .Fields("urn:schemas:httpmail:priority") = 1
  .Fields("urn:schemas:mailheader:X-Priority") = 1
  *-- Importancia
  && 0=Low, 1=Normal, 2=High
  .Fields("urn:schemas:httpmail:importance") = 2
  .Fields.Update
  
  WAIT WINDOW "Enviando..." NOWAIT 
      
    .Send()
  ENDWITH
  WAIT CLEAR 
*!*	  IF !EMPTY(panexo)
*!*	     ERASE (panexo)
*!*	  ENDIF 
CATCH TO loError

	    crlf=CHR(13)+CHR(10)
	    WAIT CLEAR 
        MsgErr="[  Error: ] " + STR(loError.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(loError.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + loError.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + loError.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + loError.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(loError.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + loError.LineContents 
    	 MESSAGEBOX(msgerr,16,"Rutina Correo")	

FINALLY
  RELEASE loConfig, loMsg
  STORE .NULL. TO loConfig, loMsg
  IF EMPTY(lcErr)
    *MESSAGEBOX("El mensaje se envió con éxito", 64, "Aviso")
  ELSE
    *MESSAGEBOX(lcErr, 16 , "Error")
  ENDIF
ENDTRY
RETURN EMPTY(lcErr)
*-------------------------------------------------------------------------------------------------------------------------*
FUNCTION GetItem( tcList, tnItem, tcSepBy )
LOCAL lcRetVal, lcSepBy
lcRetVal = ""
*** Default to Comma Separator if none specified
lcSep = IIF( VARTYPE(tcSepBy) # 'C' OR EMPTY( tcSepBy ), ',', tcSepBy )
*** Default to First Item if nothing specified
tnItem = IIF( TYPE( 'tnItem' ) # "N" OR EMPTY( tnItem ), 1, tnItem)
*** If we have exceeded the length of the string, return NULL
IF tnItem > GETWORDCOUNT( tcList, lcSep )
    lcRetVal = NULL
ELSE
    *** Get the specified item
    lcRetVal = GETWORDNUM( tcList, tnItem, lcSep )
ENDIF
*** Return result
RETURN ALLTRIM(lcRetVal)
*--------------------------------------------------------------------------------------*
function getwordcount(clista,csep)
local cwork,nwords
cwork=alltrim(clista)
nwords=0
if at(csep,cwork)<>0
   do while at(csep,cwork)<>0
      nwords=nwords+1
      xwork=substr(cwork,at(csep,cwork)+1)
      cwork=xwork
   enddo
   nwords=nwords+1
else
   if len(cwork)>0
      nwords=1
   endif
endif   
return nwords
*---------------------------------------------------------------------------------------*
function getwordnum(clista,nitem,csep)
local cwork,nwords,wpala
cwork=alltrim(clista)
wpalas=getwordcount(cwork,csep)
if nitem>wpalas
   return ""
endif   
nwords=0
wpala=""
if at(csep,cwork)<>0
	do while at(csep,cwork)<>0
	   wpala=substr(cwork,1,at(csep,cwork)-1)
	   nwords=nwords+1
	   if nitem=nwords
	      if wpalas=nitem && pidio el tope
      	     cwork=substr(cwork,at(csep,cwork)+1)
      	  else
      	     cwork=substr(cwork,1,at(csep,cwork)-1)
	      endif
	      exit
	   endif
	   cwork=substr(cwork,at(csep,cwork)+1)
	enddo
	wpala=cwork
else	
   if len(cwork)<>0
      wpala=cwork
   endif   
endif   
return wpala
*----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------*
**********************************************************************************
* FUNCTION GetFileX(tcRuta, tcExtension, tcLeyenda, tcBoton, tnBoton, tcTitulo)
********************************************************************************
* Función GetFile eXtendida. Al igual que la función GetFile muestra un cuadro
* de diálogo Abrir y retorna el nombre del archivo seleccionado. Si no se
* selecciona ningun archivo retorna una cadena vacia. La diferencia con
* GetFile() es que se puede especificar con el primer parámetro la carpeta
* donde se abre el cuadro de dialogo.
*
* RETORNA:
*   Caracter
* PARAMETROS:
*   tcRuta: Ruta inicial
*   tcExtension: Extension de los archivos que se muestran
*   tcLeyenda: Título del cuadro de texto "Nombre de archivo"
*   tcBoton: Título del botón "Aceptar"
*   tnBoton: Tipo y número de botones [0, 1 ó 2]
*   tcTitulo: Título de la ventana Abrir
* USO:
*   ? GetFileX("C:\Dat\","DBF","Nombre tabla:", "Abrir tabla", 0, "Busca tabla")
********************************************************************************
FUNCTION GetFileX(tcRuta, tcExtension, tcLeyenda, tcBoton, tnBoton, tcTitulo)
  LOCAL lcDirAnt, lcGetPict
  tcRuta = IIF(NOT EMPTY(tcRuta) AND DIRECTORY(tcRuta,1),tcRuta,"")
  tcExtension = IIF(EMPTY(tcExtension), "", tcExtension)
  tcLeyenda = IIF(EMPTY(tcLeyenda), "", tcLeyenda)
  tcBoton = IIF(EMPTY(tcBoton), "", tcBoton)
  tnBoton = IIF(EMPTY(tnBoton), 0, tnBoton)
  tcTitulo = IIF(EMPTY(tcTitulo), "", tcTitulo)
  lcDirAnt = FULLPATH("")
  SET DEFAULT TO (tcRuta)
  lcGetPict = GETFILE(tcExtension, tcLeyenda, tcBoton, tnBoton, tcTitulo)
  SET DEFAULT TO (lcDirAnt)
RETURN lcGetPict
********************************************************************************
* FUNCTION GetPictX(tcRuta, tcExtension, tcLeyenda, tcBoton)
********************************************************************************
* Función GetPict eXtendida. Al igual que la función GetPict muestra un cuadro
* de diálogo Abrir imagen y retorna el nombre del archivo de imagen
* seleccionado. Si no se selecciona ningun archivo retorna una cadena vacia.
* La diferencia con GetPict() es que se puede especificar con el primer
* parámetro la carpeta donde se abre el cuadro de dialogo.
*
* RETORNA:
*   Caracter
* PARAMETROS:
*   tcRuta: Ruta inicial
*   tcExtension: Extension de los archivos de imagen que se muestra
*   tcLeyenda: Título del cuadro de texto "Nombre de archivo"
*   tcBoton:  Título del botón "Aceptar"
* USO:
*   ? GetPictX("C:\Imagenes\","JPG","Foto:", "Abrir foto")
********************************************************************************
FUNCTION GetPictX(tcRuta, tcExtension, tcLeyenda, tcBoton)
  LOCAL lcDirAnt, lcGetPict
  tcRuta = IIF(NOT EMPTY(tcRuta) AND DIRECTORY(tcRuta,1),tcRuta,"")
  tcExtension = IIF(EMPTY(tcExtension), "", tcExtension)
  tcLeyenda = IIF(EMPTY(tcLeyenda), "", tcLeyenda)
  tcBoton = IIF(EMPTY(tcBoton), "", tcBoton)
  lcDirAnt = FULLPATH("")
  SET DEFAULT TO (tcRuta)
  lcGetPict = GETPICT(tcExtension, tcLeyenda, tcBoton)
  SET DEFAULT TO (lcDirAnt)
RETURN lcGetPict
********************************************************************************
function xnuevoitem(tcalias,cereo)
Local Lcalias, lnid, lcoldreprocees, lnoldarea,wuids

TRY 

lnoldarea=select()
lnid=0
wuids=.f.
	
if empty(alltrim(tcalias))
	lcalias=upper(alias())
else
	lcalias=upper(tcalias)
endif
lcoldreprocess=set("reprocess")
*bloquea hasta que usuario presiona esc
*set reproces to automatic
if !used("ids")
	use tarjeta!ids in 0
	wuids=.t.
endif
sele ids
wfb=CURSORGETPROP("Buffering","ids")
if wfb>1
	if !cursorsetprop("buffering",1,"ids")
		messagebox(" ERROR: Archivo de IDS  Con buffer: "+str(Wfb,2),48,"Avisar a Ruben")
		if wuids
			use in ids
		endif
		select (lnoldarea)
		lnid=0
		THROW "NORMAL"
	ENDIF 	 
ENDIF 
select ids
wexac=set("exact")
set exact on
=seek(alltrim(lcalias),"ids","tabla")
if eof("ids")
	sele ids
	insert into ids (tabla,nextid) values (lcalias,0)
	*append blank
	*replace tabla with lcalias, nextid with 0
	flush
endif
=seek(alltrim(lcalias),"ids","tabla")
if !eof("ids")
	if rlock()
	    if  isrlocked()
			lnid=iif(empty(cereo),ids.nextid+1,0)
			replace ids.nextid with lnid
			flush
			unlock
		endif	
	endif	
else
	wgraba=.f.
	messagebox("No se encuentra "+tcalias+" en IDS",48)	
endif
if wuids
	use in ids
endif
select (lnoldarea)
*set exact &wexac
set reproces to lcoldreprocess

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lnid=0

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
     lnid=0

ENDTRY 

return lnid
*******************************************************************************
function cotiza(wmoneda,wpres,wmpais)
local area,dn
store 0.00 to wcotar,wcotur,wcotdol
area=select()
dn=.t.

TRY 

wfechacot=wpres
res=SQLEXEC(n_handle,"select * from cotiza where fecha=?wpres","ccotiza")
*res=REQUERY("cotiza")
IF res<=0
   THROW "NORMAL"
ENDIF 
if !EOF("ccotiza")
    if nacion=1
        wcotiza=iif(wmoneda=wmpais,1.00,iif(wmoneda="U",;
        ccotiza.pesosuru,ccotiza.dolares))
    else
        wcotiza=iif(wmoneda=wmpais,1.00,iif(wmoneda="A",;
         ccotiza.pesosarg,ccotiza.dolares))
    endif
    wcotar=ccotiza.pesosarg
    wcotur=ccotiza.pesosuru
    wcotdol=ccotiza.dolares
else
    wcotiza=1
    dn=.f.
ENDIF
wfechacot=CTOD("")
USE IN ccotiza
*REQUERY("cotiza")
sele (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
*    r_cancel=oapp.cancel_trans(thisform.hnd)
     dn=.f.


CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
*    r_cancel=oapp.cancel_trans(thisform.hnd)
   dn=.f.
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    *MESSAGEBOX(msgerr,16,"ERROR")	   


ENDTRY 

return (dn)
**********************************************************************************
function getautov9(wxarchia,wnacion,wtarjau,wcotarge,wxpai)
local wtodos,area,wfileaug,lsaleok

TRY 

lsaleok=.t.

if !ejecutable
	set echo on
	set step on
endif
*!*	set exclu off
*!*	set dele on
area=select()
wtodos=.f.
if pcount()=1   &&paso el archivo temporario
	if !used('parametros')
		*use tarjeta!parametros in 0
		MESSAGEBOX("PARAMETROS NO ESTA ABIERTO",48,"CORRIJA")
		THROW "NORMAL"
	ENDIF 
	wtodos=.t.
	wnacion=nacion
	wxpai=nacion
	wcotarge=1
	wcotdol=parametros.cot_dol
endif
wfileaug=alltrim(wcamino+wxarchia)

if !file(wfileaug+".dbf")
	create table (wfileaug) (paisco n(2,0),lote n(4,0), tarjeta c(16,0),comercio c(15,0),;
		fecha d(8), impo n(10,2),ncupon n(8,0), nfina c(1,0), cuotas n(3,0),terminal n(10,0) ,moneda n(3,0),;
		codauto c(10,0),hora c(8,0),saldodp n(12,2),plus_a n(5,2),i_cotiz n(12,2),fepago d(8),cambio n(6,2))
endif		
*---inicio con parametro cargado para la direccion ws, que los otros cargan vacio
*webivr="\\192.168.20.10\"+"discoe\tempivr\"  && se carga en WS. 
wdir=webivr
if directory("h:\tempivr") and empty(wdir)
	wdir="h:\tempivr\"
endif
if directory("y:\tempivr") and empty(wdir)
	wdir="y:\tempivr\"
endif

if directory("k:\tempivr") and empty(wdir)
	wdir="k:\tempivr\"
endif
if directory("d:\tempivr") and empty(wdir)
	wdir="d:\tempivr\"
endif
if directory("c:\tempivr") and empty(wdir)
	wdir="c:\tempivr\"
endif
if EMPTY(WDIR)
	sele (area)
	THROW "NORMAL"
ENDIF 

if !used(wxarchia)
	use (wfileaug) in 0 exclu 
endif

wflec=date()
wlen=adir(mfivr,wdir+"*.dbf")
sele (wxarchia)
zap
for i=1 to wlen
	wfj=alltrim(mfivr[i,1])
	*wn1=wdir+wfj
	wnombre=STRTRAN(wfj,'.DBF','')
	wn1=wdir+wNOMBRE
	if wtodos
		append from (wn1) for (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0")
	else
		*append from (wn1) for tarjeta=wtarjau
		append from (wn1) for substr(tarjeta,7,7)=right(wtarjau,7) and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0")
	endif
	if used(wnombre)
		use in (wnombre)
	endif
endfor
for i=1 to wlen
	wfj=alltrim(mfivr[i,1])
	wnombre=STRTRAN(wfj,'.DBF','')
	if used(wnombre)
		use in (wnombre)
	endif
endfor

sele (wxarchia)
dele all for impo<0 and fecha<date()-2
if wnacion=1
	if wxpai=2  &&usuario uruguayo

		*replace all impo with impo*iif(paisco=1,wcotarge,1) ,cambio with iif(paisco=1,wcotarge,1) for moneda=032
		replace all impo with impo*iif(paisco=1,parametros.cot_dolpx,parametros.cot_dolpx*wcotarge) ,cambio with iif(paisco=1,parametros.cot_dolpx,parametros.cot_dolpx*wcotarge) for moneda=840
		sum all impo to wpauto for moneda=032 and (alltrim(codauto)="I0" and alltrim(codauto)="M0")
		sum all impo to wdauto for moneda=840 and (alltrim(codauto)="I0" and alltrim(codauto)="M0")
	endif
	if wxpai<>2 	 &&usuario argentino
		replace all impo with impo*iif(paisco=2,wcotarge,1) ,cambio with iif(paisco=2,parametros.cot_peso,1) for moneda=032
		*replace all impo with impo*wcotdol  ,cambio with wcotdol for moneda=840	
		sum all impo to wpauto for moneda=032 and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0")
		sum all impo to wdauto for moneda=840 and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0")
	endif
endif

if wnacion=2
	if wxpai=1  &&usuario argentino
		replace all impo with impo*iif(paisco=1,wcotarge,1) ,cambio with iif(paisco=1,wcotarge,1) for moneda=032
		replace all impo with impo*iif(paisco=1,parametros.cot_dolpx,parametros.cot_dolpx*wcotarge) ,cambio with iif(paisco=1,parametros.cot_dolpx,parametros.cot_dolpx*wcotarge) for moneda=840
		sum all impo to wpauto for moneda=032 and (alltrim(codauto)="I0" and alltrim(codauto)="M0")
		sum all impo to wdauto for moneda=840 and (alltrim(codauto)="I0" and alltrim(codauto)="M0")
	endif
	if wxpai=2  &&usuario uruguayo
		replace all impo with impo*iif(paisco=1,wcotarge,1) ,cambio with iif(paisco=1,parametros.cot_peso,1) for moneda=032
		replace all impo with impo  ,cambio with wcotdol for moneda=840	
		
		sum all impo to wpauto for moneda=032 and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0")
		sum all impo to wdauto for moneda=840 and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0")
	endif
endif

go top in (wxarchia)
if !ejecutable
	browse title "Autorizaciones"
endif
SELECT (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lsaleok=.f.

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    lsaleok=.f.
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR")	   

ENDTRY 
RETURN lsaleok
*browse title "Pesos "+str(wpesos,8,2)+"  Dolar "+str(wdolar,8,2)
*******************************************************************************************
******************************************************************************
function wintuser(wtmov,wpago,wpresenta,wcuotas,wcm)
local area,wcmt
if parameters()<5
	wcmt=0
else
	wcmt=wcm	
endif
if !ejecutable
*!*		set echo on
*!*		set step on
endif
area=select()
*--verifica el plan.----

res=SQLEXEC(oapp.hnd,"select * from planes where  tipo_doc=?wtmov","wcplan")
IF res<=0
   RETURN .t.
ENDIF 
store 0.00 to wincuop,wincuod,wcomision
if EOF("wcplan")
	messagebox("No existe el Plan ",48,"Wintuser")
	RETURN .t.
else
	if wcplan.intparam
		*---interes y comision por parametros----
		reS=SQLEXEC(oapp.hnd,"select * from parametros","cparam")
		IF res<=0
		   RETURN .t.
		ENDIF 
		if !EOF("cparam")
			wincuop=cparam.int_fin
			wincuod=cparam.int_fid
			wcomision=cparam.comision
		ENDIF
		use in cparam
		
	else
		*---interes y comision por lplanes----
        res=SQLEXEC(oapp.hnd,"select * from lplanes where tipo_doc=?wtmov","wlplan")
        IF res<=0
           RETURN .t.
        ENDIF 
		if !EOF("wlplan")
			wincuop=wlplan.i_pesos
			wincuod=wlplan.i_dola
			wcomision=wcplan.comision
			if wcmt<>0
				wincuop=wlplan.i_pesos_n
				wincuod=wlplan.i_dola_n
			endif
		ENDIF
		USE IN wlplan
	endif
ENDIF
USE IN wcplan
sele (area)
return 
*******************************************************************************
function reajucuo(wtmov,wcuotas,wmoneda,wcm)
local area,wsion
area=select()
if wtmov="U" and nacion=1
	select * from tarjeta!comcuota where comercio=wcm and moneda=wmoneda into cursor ccomc
	if _tally>0
		sele ccomc
		scan all
			do case
            case wcuotas=2
                wsion=descu2
            case wcuotas=3
                wsion=descu3
            case wcuotas=4
                wsion=descu4
            case wcuotas=5
                wsion=descu5
            case wcuotas=6
                wsion=descu6
            case wcuotas=7
                wsion=descu7
            case wcuotas=8
                wsion=descu8
            case wcuotas=9
                wsion=descu9
            case wcuotas=10
	            wsion=descu10
            case wcuotas=11
                wsion=descu11
            case wcuotas=12
                wsion=descu12
            endcase
		endscan
		if wmoneda="D"
			wincuod=wsion
		else
			wincuop=wsion
		endif	
		use in ccomc
	endif
endif
sele (area)
return 
*******************************************************************************
*******************************************************************************
function plasticok(ogru,ousu,otitu,oplas,overif)
local sale
sale=.t.
osgru=strzero(ogru,2)
osusu=strzero(ousu,5)
ostitu=strzero(otitu,1)
osplas=strzero(oplas,1)
oiso=globaiso  && definida global en el menu CUPONES.PRG
unotar=oiso+osgru+osusu+ostitu+osplas
dimension  rvec(15)
for i=1 to 15
	rvec(i)=0
next	
*rvec={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
for t=1 to len(unotar)
    if mod(t,2)<>0
       rvec[t]=int(val(substr(unotar,t,1)))*2
    else
       rvec[t]=int(val(substr(unotar,t,1)))
    endif
next
pbolsa=0
for t=1 to 15
    if rvec[t]<10
       pbolsa=pbolsa+rvec[t]
    else
       pbolsa=pbolsa+int(val(substr(str(rvec[t],2),1,1)))+int(val(substr(str(rvec[t],2),2,1)))
    endif
next
eldigito=mod(pbolsa,10)
if eldigito<>0
   eldigito=10-eldigito
endif
sale=(eldigito=overif)
if !sale
   messagebox("ERROR DIGITO VERIFICADOR!!",48)
endif
return(sale)
******************************************************************************
function proxfech(wmas,l)
if empty(wmas) or l=0
    return(wmas)
endif
if !ejecutable
	*set echo on
	*set step on
endif
wdia=day(wmas)
wmes=month(wmas)+l
wano=year(wmas)
if wmes>12
    wano=wano+int(wmes/12)
    wmes=wmes-int(wmes/12)*12
    if wmes=0
    	wmes=1
    endif
endif
if wdia>28 and wmes=2
     wdia=28
endif
if wdia>30 and (wmes=4 or wmes=6 or wmes=9 or wmes=11)
    wdia=30
endif
return ctod(str(wdia,2)+"/"+str(wmes,2)+"/"+str(wano,4))
***********************************************************************
function cronogra(gru,ini,feca,j)
local area,dife,fere,cc1,wucro

TRY 
area=select()
wucro=.f.
dife=0
fere=feca
if j=0
	THROW 'NORMAL'
*	return (fere)
ENDIF 
*!*	if !used("cronograma")	
*!*		use cronograma in 0
*!*		wucro=.t.
*!*	endif
cc1=ctod("")
wini=ini
wgrupo=gru

res=SQLEXEC(n_handle,"select * from cronograma where grupo=?wgrupo order by cierre","curcrono")
IF res<=0
   THROW "NORMAL"
ENDIF 
if eof("curcrono")
    messagebox("Error :Sin Fecha Cierre GR : "+str(gru,2)+" Inicio "+dtoc(ini) ,48,"Control")
    *return(fere)
    THROW "NORMAL"
ENDIF 
SELECT curcrono
SKIP j
if eof() 
    messagebox("Error :Sin Fecha Cierre GR : "+str(gru,2)+" Inicio "+dtoc(ini) ,48,"Control")
    return(fere+5)
ENDIF 
if feca>cierre
    dife=feca-cierre
    fere=feca-dife-1
else
    skip -1
    if feca<=cierre
        dife=cierre-feca
        fere=feca+dife+5
    endif
ENDIF
USE IN curcrono
SELECT (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR COMCUOTA")	   

ENDTRY 
return(fere)
******************************************************************
function quitaextension
lparameters lcficheroext
*-recorta extension del fichero ----
*------------------------------------------------------------------------------
*	Variable		Tipo				Uso
*.............................................................
* lcsinextension    (c)             valor de retorno
* lcficheroext      (c) 			nombre del fichero
*-----------------------------------------------------------------------------
if "." $ lcficheroext
	lcsinextension =substr(alltrim(lcficheroext),1,rat(".",lcficheroext)-1)
else
	lcsinextension =lcficheroext
endif
return lcsinextension
************************************************************************************
function quitaprefijodir
lparameters lcnombrefichero
*-recorta prefijodir del fichero*****
*------------------------------------------------------------------------------
*	Variable		Tipo				Uso
*.............................................................
* lcsindirectorio   (c)             valor de retorno
* lcnombrefichero   (c) 			nombre del fichero
*-----------------------------------------------------------------------------
do case
case "\" $ lcnombrefichero
	lcnsindirectorio=substr(alltrim(lcnombrefichero),rat("\",lcnombrefichero)+1)
case ":" $ lcnombrefichero
	lcnsindirectorio=substr(alltrim(lcnombrefichero),rat(":",lcnombrefichero)+1)
otherwise
	lcnsindirectorio=lcnombrefichero
endcase
return 	lcnsindirectorio
*****************************************************************************************
function prefijodir
lparameters lcnombrefichero
*-recorta extension del fichero ----
*------------------------------------------------------------------------------
*	Variable		Tipo				Uso
*.............................................................
* lcsindirectorio   (c)             valor de retorno
* lcnombrefichero   (c) 			nombre del fichero
*-----------------------------------------------------------------------------
do case
case "\" $ lcnombrefichero
	lcdirectorio=left(alltrim(lcnombrefichero),rat("\",lcnombrefichero))
case ":" $ lcnombrefichero
	lcdirectorio=left(alltrim(lcnombrefichero),rat(":",lcnombrefichero))
otherwise
	lcdirectorio="??"
endcase
return 	lcdirectorio
*****************************************************************************************
function comcuota(wtipodo,wcomer,wsucu,wpago,wprese,wcuotas,wmone)
local area,wmonex,wcomision,wanco
TRY 

area=select()
wcomision=0
if wtipodo="H"
	THROW "NORMAL"
ENDIF
res=SQLEXEC(oapp.hnd,"select * from comercio where paisco=?nacion and comercio=?wcomer and sucursal=?wsucu","welco")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error WELCO")
   THROW "NORMAL"
ENDIF 

IF !EOF("welco")
	wcomision=iif(alltrim(wmone) $ "N_U_A",welco.descuento,welco.dctodol)
ENDIF 
if used("welco")
	use in welco
endif
if nacion=1 or wpago-wprese<=38
    THROW "NORMAL"
ENDIF 
wpais=nacion
wcomercio=wcomer
wsucursal=wsucu

wmonex=iif(wmone<>"D","N",wmone)
wanco=wcomision
res=REQUERY("comcuota")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error COMCUOTA")
   THROW "NORMAL"
ENDIF

sele comcuota      && comercio N,5,0- descuento N,5,2 - cuotas N,4
GO TOP 
do while !eof()
    if wcomer=comercio and moneda=wmonex and sucursal=wsucu
        if wtipodo="U"
            wcampo="descu"+ALLTRIM(STR(wcuotas))
            wcomision=&wcampo
*!*	            do case
*!*	            case wcuotas=2
*!*	                wcomision=descu2
*!*	            case wcuotas=3
*!*	                wcomision=descu3
*!*	            case wcuotas=4
*!*	                wcomision=descu4
*!*	            case wcuotas=5
*!*	                wcomision=descu5
*!*	            case wcuotas=6
*!*	                wcomision=descu6
*!*	            case wcuotas=7
*!*	                wcomision=descu7
*!*	            case wcuotas=8
*!*	                wcomision=descu8
*!*	            case wcuotas=9
*!*	                wcomision=descu9
*!*	            case wcuotas=10
*!*	                wcomision=descu10
*!*	            endcase
        else
            wcomision=diferido
        endif
        go BOTTOM 
    ENDIF 
    SKIP 
ENDDO 
wcomision=iif(wcomision<>0,wcomision,wanco)
sele (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR COMCUOTA")	   

ENDTRY 
return (wcomision/100)
****************************************************************************************************
function comcuzona(wtipodo,wcuotas,wmone,wpress,wpresm)
local area,wmonex,wret,wupl,wupa
TRY 

store .f. to wret,wupl,wupa
area=select()
store 0.00 to wcomizona
wdiasp_zona=ctod("")
wint_zona=""
if !used("parametros")
	wupa=.t.
	res=SQLEXEC(n_handle,"select * from parametros","micuram")
	IF res<=0
	   AERROR(verr)
	   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error COMCUZONA")
	   THROW "NORMAL"
	ENDIF 
ENDIF 
res=SQLEXEC(n_handle,"select * from lplanes where tipo_doc=?wtipodo AND cuota=?wcuotas","welco")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error COMCUZONA")
   THROW "NORMAL"
ENDIF 

if !EOF("welco")
	wret=.t.	
	wcomizona=welco.dcto_com
	wdiasp_zona=iif(wtipodo=micuram.plan_mes,wpresm,wpress)+welco.dias_pago
	wint_zona=welco.interes
endif
USE IN welco
USE IN micuram

sele (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"
    wret=.f.

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    wret=.f.
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR COMCUOTA")	   

ENDTRY 
return (wret)
*************************************************************************************************************************************
*PROCEDURE dbusar (lNconex AS INTEGER,lCdb AS STRING)
PROCEDURE dbusar (lNconex,lCdb)
	**lNconex = Conexion
	**lCdb = NOMBRE DE LA BASE DE DATOS A USAR
	LOCAL llreturn AS Boolean
	llreturn=.F.

	IF TYPE('lNconex')='N' OR lNconex>0
		=SQLCANCEL(lNconex)
		llreturn =(SQLEXEC(lNconex,'use '+lCdb)>0)
	ENDIF
	RETURN llreturn
ENDPROC
*************************************************************************************************************************************
function _mysqlfecha(wff)
local wfcv,wxx
if empty(wff)
	wff=date()
endif
wxx=dtos(wff)
wfcv="'"+left(wxx,4)+"-"+substr(wxx,5,2)+"-"+right(wxx,2)+" 00:00:00'"
return (wfcv)
**************************************************************************************************************************************
function explastico(p1,p2,p3,p4)
local area,dz,ctitu
area=select()
dz=.f.
ctitu=0 && necesito verificar titular
*---primero verifica el usuario titular y luego el adicional- OJO PARTE DE CLAVE indice U4--------------*
TRY 

csql="SELECT * FROM usuarios u WHERE u.cod1_cli=?p1 AND u.cod2_cli=?p2 AND u.cod3_cli=?ctitu"
res=SQLEXEC(n_handle,csql,"ctitu")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error Usuarios-EXPLASTICO")
   THROW "NORMAL"
ENDIF 
dz=!EOF("ctitu")

if p3<>0 and !EOF("ctitu")
    csql="select * from adicional a where a.cod1_cli=?p1 AND a.cod2_cli=?p2 AND a.cod3_cli=?p3 AND a.cod4_cli=?p4"
    res=SQLEXEC(n_handle,csql,"cadic")
    IF res<=0
       AERROR(verr)
       MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error Adicionales-EXPLASTICO")
       THROW "NORMAL"
    ENDIF 
	dz=!EOF("cadic")
ENDIF 
*--cargo variables del usuario------------------*
if used("ctitu")
	wjuri=ctitu.juridica
	wnacionu=ctitu.ppais
	whabili=(ctitu.habilitado<1)
	use in ctitu
endif	
if wnacionu=0
	wnacionu=elpais(p1)		
endif	

if used("cadic")
	use in cadic
endif	
SELECT (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR EXPLASTICO")	   


ENDTRY 
IF USED("ctitu")
   USE IN ctitu
ENDIF 
IF USED("cadic")
   USE IN cadic 
ENDIF    
SELECT (area)
return(dz)
******************************************************************************************************************************************
function elpais(wgrp)
local area,wpp,wugru
TRY 

res=SQLEXEC(oapp.hnd,"select * from grupos where grupo=?wgrp","curgru")
IF res<=0
   THROW "NORMAL"
ENDIF 
IF EOF("curgru")
   wpp=0
ELSE
   wpp=curgru.ppais
ENDIF 
USE IN curgru

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
   wpp=0
CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
   wpp=0
ENDTRY 
return  wpp
************************************************************************
function excomer(wp,wcom,wcuot,wmon)
local area,dz,wcuotx
area=select()

TRY 

dz=.f.
*wcpa=pcount()  && par 4 parametros devuelve plazo de pago de cuotas uruguay

csql="select * from comercio c where c.paisco=?wp and c.comercio=?wcom and c.sucursal=0"
res=SQLEXEC(n_handle,csql,"ccome")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error Comercios-EXCOMER")
   THROW "NORMAL"
ENDIF 
dz=!EOF("ccome")
*--cargo variables del usuario------------------*
if !EOF("ccome")
	wtopecta=ccome.topecta
	wdiapago=ccome.condpago &&indica dias de pago para compras C en rou, lecutar pos
	wnacionc=ccome.paisco
	wsucu=ccome.sucursal
	wramores=ccome.ramo
	use in ccome
	if wp=nacion
		wcuotx=wcuot
		if type("wcuotx")<>"N"
			wcuotx=val(wcuot)
		endif
		if  wcuotx>1 and nacion=2
		    csql="select * from comcuota c where c.comercio=?wcom  and c.sucursal=0 and c.moneda=?wmon"
			res=SQLEXEC(n_handle,csql,"ccta")
			IF res<=0
			   AERROR(verr)
			   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error ComCuota-EXCOMER")
			   THROW "NORMAL"
			ENDIF 
			if !EOF("ccta")
				wcampo="ccta.dip"+alltrim(str(wcuotx,3))
				wdiapago=&wcampo
			ENDIF 
		ENDIF 
	ENDIF 
ENDIF 
sele (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"

ENDTRY 
return(dz)
*******************************************************************************************************************************************
function nuevoitem(tcalias)
Local Lcalias, lnid, lcoldreprocees, lnoldarea,wuids,wincre

TRY 

lnoldarea=select()
lnid=0
CRLF=CHR(13)
wuids=.f.
wincre=0
whnd=n_handle
if empty(alltrim(tcalias))
	THROW "NORMAL"
	lcalias=upper(alias())
ENDIF

tcalias=UPPER(tcalias)
res=SQLEXEC(n_handle,"select count(tabla) as veces from ids where tabla=?tcalias","curalias") 
IF res<=0
    AERROR(verr)
   	MESSAGEBOX(STR(verr[1],7)+" "+verr[2],48,"Error")
   THROW "NORMAL"
ENDIF 
sqltxt=""
IF EOF("curalias") 
	*sqltxt="insert into ids (tabla,nextid,SERIE) values (?tcalias,0,'')"
	sqltxt="insert into ids (tabla,nextid) values (?tcalias,0)"
	wincre=1
ELSE
	x=TYPE("curalias.veces")
	IF VAL(curalias.veces)=0	
		sqltxt="insert into ids (tabla,nextid) values (?tcalias,0)"
	ENDIF
ENDIF
IF !EMPTY(sqltxt)
	res=SQLEXEC(n_handle,sqltxt)
	IF res<=0
	    AERROR(verr)
	   	MESSAGEBOX(STR(verr[1],7)+" "+verr[2],48,"Error")
	  	THROW "NORMAL"
	ENDIF 
ENDIF

*res=sqlexec(whnd,"call ndores(?tcalias,@nrodoc,@ws);")
res=sqlexec(whnd,"call ndores(?tcalias)","curprox")
IF res<=0
    AERROR(verr)
	MESSAGEBOX(STR(verr[1],7)+" "+verr[2],48,"Error")
    THROW "NORMAL"
ENDIF 
IF !EOF("curprox")
   lnid=curprox.nextid
ENDIF    

*--tomo la serie
*!*	res=sqlexec(whnd,"select @nrodoc,@ws;","NUMERO")
*!*	IF res<=0
*!*	   AERROR(verr)
*!*	   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],48,"Error")
*!*	   THROW "NORMAL"
*!*	ENDIF 
*!*	SELECT numero
*!*	IF !EOF()
*!*	   IF !ISNULL(numero._nrodoc)
*!*	  	      lnid=VAL(numero._nrodoc)+wincre
*!*	  	      WSERIEM=numero._ws
*!*	   ENDIF
*!*	ENDIF 
*!*	USE IN numero
CATCH TO oErr WHEN oErr.UserValue<>"NORMAL" 

    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 

    MESSAGEBOX(msgerr)

CATCH TO oErr WHEN oErr.UserValue="NORMAL" && ATRAPO ERROR ------------------------------*
 	* nothing   
FINALLY 
ENDTRY 
return INT(lnid)
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************
***************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************
function Pprocesarus(wtabin,wtabcu,wtabac,wnaci,wgrp,wusu,wciean,wcieac,wvtoan,wvthoy,wnmr,wlt,wing,wlcr,wsdod,wxlc,wxlcc,winvn,wcdeta,wcer,hnd)
local area,wsaldon,wminn,wsaldod,wmind,sminima,ssdoa,sminimd,ssdod ,wcier_act,wvto_act,wcier_an,;
	  wvto_an,wnmru,wlet,wingr,wrg,wuss,wulpan,wulpad,ipun,ipud,upa,codd
	  
TRY 
	  
*-- Parametros: wtabin,wtabcu,wtabc -tablas para datos. wnaci,wgrp,wusu -nacion grupo y usuario
*		wciean,wcieac,wvtoan,wvthoy -cierres y vencimientos. wnmr -numerado usuario.  wlt -letratar. wing -Ingreso.		
If !ejecutable
	set echo on
	set step on
ENDIF   

wtcu=wcamino+wtabcu
wcereo=.f. &&indico para no procesar los intereses atrasados. que se borran en cereo
if parameters()<19	
*!*		select *,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,.f. as dfuturo,000 as ramo,0000.00 as extrapunto;
*!*			from tarjeta!cupusu c;
*!*			where cod1_cli=99999;
*!*			order by presenta;  
*!*			into table (wtcu) && (c.cod2_cli,c.cod3_cli) ->no sirve este orden, por la cronologia 
	res=SQLEXEC(hnd,"select *,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,false as dfuturo,000 as ramo,0000.00 as extrapunto from cupusu c where cod1_cli=99999 and false order by presenta",wtcu)
	IF res<=0
       AERROR(verr)
       MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - PARAMETROS")
       upa=CTOD("")
       THROW "NORMAL"
	ENDIF 
	messagebox("Indico menos de 20 parametros ",48,"Control")
	upa=CTOD("")
	THROW "NORMAL"
ENDIF 
if parameters()=20
	wcereo=wcer
ENDIF 
store 0.00 to wsaldon,wminn,wsaldod,wmind,ifin,ifid,pfin,ws,sminima,ssdoa,sminimd,ssdod,wcupan,wcupad,wprestamo  && saldos necesarios para un proceso inventario.
store ctod("") to upa,upd  
*---cargo fecha supuesta de ultimo pagos $,U$----
if !empty(wtabac) and empty(webivr)
	*wait window "Procesando: "+str(wgrp,2)+str(wusu,5)+" "+dtoc(wciean)+" a "+dtoc(wcieac)  nowait
	wxu=str(wgrp,2)+str(wusu,5)
	wait window "Procesando: "+wxu  nowait
ENDIF 
area=select()
*---------variables parametricas------------------------
res=SQLEXEC(hnd,"select * from parametros","cparametros")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - PARAMETROS")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF 
welpa=cparametros.pais
wmonservi=cparametros.monpais    &&determino moneda del servicio para darlo de alta en cupones
wugest=.f.  && si es o no usuario de gestion.-----
limite=wlcr
wlc=wxlc
wlcc=wxlcc
wsaldodis=wsdod
wlet=wlt
wingr=wing
wuss=wusu
wrg=wgrp
wcier_an=wciean
wcier_act=wcieac
wvto_an=wvtoan
wvto_act=wvthoy
wnmru=wnmr
wultpago=wvtoan   && supone ultimo pago igual al vencimiento cierre anterior a proceso de hoy. 
wcomnfin=cparametros.comernofin  && se carga lista de comercios que la tarjera no financia en cuotas.
wgracia=quegracia(wgrp)
wgrapag=cparametros.grapag
ipun=cparametros.int_pun
ipud=cparametros.int_pud
ifin=cparametros.int_fin  &&se pisan con costoser y son excluyentes
ifid=cparametros.int_fid
pfin=cparametros.por_fin
codd=cparametros.cot_dol
wsellcomp=iif(nacion<>2,cparametros.sellcomp/1000,0)
wpormin=cparametros.por_min
wcdajup=cparametros.ajupago
piva=cparametros.iva
*--variable que filtra planes de movimientos.----
tiraplan=""
SELECT planes
SCAN ALL FOR origen="PL"
	tiraplan=tiraplan+tipo_doc+"_"
ENDSCAN 
*---variables parametricas-----------------------------------------------------------------------
store wvtoan to wpaga,wpagd && cargo fechas de calculo intereses como dia del vencimiento anterior
*--Paso cparametros tablas, nacion, grupo,usuario-- el costo de la tarjeta del usuario------------
ws=costoser(wnaci,wlet,wingr,wgrp) && carga: costoser.dbf ifin=.int_fin ,ifid=.int_fid, pfin=.por_fin
*--reparo intereses si no estan cargados en costoser
if wgrp<>10
	if ifin=0
		ifin=cparametros.int_fin  &&se pisan con costoser y son excluyentes
	ENDIF 
	if ifid=0
		ifid=cparametros.int_fid
	ENDIF 
ENDIF 
*---debo armar saldo anterior usuario desde el inventario (saldusu) y dar alta registro en wtabin y sumo por nombre campo.
*!*	select * from tarjeta!inventario i;
*!*		where i.cierre=wciean and i.cod1_cli=wgrp and i.cod2_cli=wusu;
*!*		order by i.cierre desc top 1;
*!*		into cursor cinven
	
res=SQLEXEC(hnd,"select * from inventario i where i.cierre=?wciean and i.cod1_cli=?wgrp and i.cod2_cli=?wusu order by i.cierre desc top 1","cinven")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - INVENTARIO")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF 
	
if !EOF("cinven")
	*messagebox("con saldo",48,"procesarus")
	*-pesos para cargar a nuevo inventario
	wsaldon=cinven.saldo_acn
	wminn=cinven.pago_minn
	
	*-dolar para cargar a nuevo inventario
	wsaldod=cinven.saldo_acd
	wmind=cinven.pago_mind
ELSE 
	* sin saldo de cierre anterios para cargar al inventario =>0.00
ENDIF 
if used("cinven")
	USE IN cinven
ENDIF 
append blank in (wtabin)   && agrego registro inventario ultimo cierre---
replace cod1_cli with wgrp, cod2_cli with wusu, ppais with wnaci, cierre with wcier_act,;
	numerado with wnmru, saldo_a_n with wsaldon, saldo_a_d with wsaldod, paga_minn with wminn,;
	paga_mind with wmind, int_fin with ifin, int_fid with ifid, por_fin with iif(pfin=0,cparametros.por_fin,pfin),servicio with ws,;
	financian with 0.00, nfinancian with 0.00, financiad with 0.00, nfinanciad with 0.00, nproceso with 0 in (wtabin)
SELECT (wtabin)
if !ejecutable
	*browse	title "wtabin"
ENDIF 
*---------------------------------------------------------------------
*----SALDO DE OTRO PAIS: ARCHIVO PROCESADO DE SISCARD, RETRASMITIDO
*---------------------------------------------------------------------
wppai=elpais(wgrp) && pais del usuario que compra
if wppai<>nacion and wglobaiso="504736"
	wtcu=wcamino+wtabcu
	
*!*		select *,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,.f. as dfuturo,000 as ramo,0000.00 as extrapunto;
*!*			from tarjeta!cupusu c;
*!*			where cod1_cli=99999 and .f.;
*!*			order by presenta;  
*!*		into table (wtcu) && (c.cod2_cli,c.cod3_cli) ->no sirve este orden, por la cronologia 

	res=SQLEXEC(hnd,"select *,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,false as dfuturo,000 as ramo,0000.00 as extrapunto from cupusu c where cod1_cli=99999 and false order by presenta",wtcu)
	IF res<=0
       AERROR(verr)
       MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - PARAMETROS")
       upa=CTOD("")
       THROW "NORMAL"
	ENDIF 
	
	wdirre=alltrim(cparametros.dirsisca)+iif(right(alltrim(cparametros.dirsisca),1)="\","","\")
	if !file(wdirre+"cuentas.dbf") or !file(wdirre+"positivo.dbf")
		replace sdo_cpra with  0,	sdo_cuota with 0,sdo_dispo with 0 in (wtabin)  &&saldo disponible de la cuenta.
		sele (area)
        upa=CTOD("")
        THROW "NORMAL"
	ENDIF 
	store 0.00 to wpauto,wdauto  &&voy a sumar importe autorizado en getuato (hoy ivr)
	*if nacion=2
		sele (wtabin)
		wtrau=wglobaiso+strtran(str(wgr,2)," ","0")+strtran(str(wus,5)," ","0")
		*wxarchi="x"+quitaprefijodir(quitaextension(wtabin))
		wxarchi="x"+sys(2015)
		*--se refiere a los archivos locales de autorizacion en este caso IVR.---
		if empty(wbolsatodo)
			wctpeso=cparametros.cot_peso
			getautov9(wxarchi,nacion,wtrau,wctpeso,wppai)  && fncion que trae datos de autorizaciones
		ELSE 
			sumauto(wbolsatodo,wtrau)
		ENDIF 
		
	*endif	
	*---cuentas
	wcnta=wglobaiso+strtran(str(wgrp,2)," ","0")+strtran(str(wusu,5)," ","0")
	use (wdirre+"cuentas") in 0 alias wcuentas exclu
	sele wcuentas
	loca for np=wcnta
	if !eof()
		*wimptrasmi=val(wcuentas.co)/100*iif(nacion=1<>wppai,cparametros.cot_peso,1)
		if nacion=1
			wimptrasmi=val(wcuentas.co)/100  && ya esta convertido 
		endif
		if nacion=2
			wimptrasmi=val(wcuentas.co)/100*cparametros.cot_peso
		endif
		vr2=wpauto
		vr3=wdauto*cparametros.cot_dolpx
		replace sdo_dispo with wimptrasmi-wpauto-wdauto in (wtabin)  &&saldo disponible de la cuenta.	
	endif
	use in wcuentas
*!*		*---positivo
*!*		use (wdirre+"positivo")  in 0 alias wpositiv exclu
*!*		loca for
*!*		use in wpositiv
	select(area)
    upa=CTOD("")
    THROW "NORMAL"
ENDIF 
*---------------------------------------------------------------------
*----?? grabo para el proceso el reintegro del iva
*---cargo intereses en archivo
*.........................PROCESAR CARGOS , DEVOLUCIONES,ETC........................
*--   debo procesar cargos, devoluciones,etc en archivo altas cupones antes de procesar.
*---  tabla wtabac
*.........................FIN PROCESAR CARGOS, DEVOLUCINES,ETC.
*---debo armar wtabcu o sea cupones a procesar, del periodo y cuotas futuras para poder grabarle altas.
*--metodo 2 con indice movius
wtcu=wcamino+wtabcu
*wcl1=str(welpa,2)+str(wrg,2)+str(wuss,5)+dtos(wcier_an+1)
*wcl2=str(welpa,2)+str(wrg,2)+str(wuss,5)+dtos(wcier_act+5000)

wcl1=str(iif(wglobaiso="504736",welpa,wppai),2)+str(wrg,2)+str(wuss,5)+dtos(wcier_an+1)
wcl2=str(iif(wglobaiso="504736",welpa,wppai),2)+str(wrg,2)+str(wuss,5)+dtos(wcier_act+5000)

*!*	select *,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,presenta>wcier_act as dfuturo,000 as ramo,0000.00 as extrapunto;
*!*		from tarjeta!cupusu c;
*!*		where between(str(ppais,2)+str(cod1_cli,2)+str(cod2_cli,5)+dtos(presenta),wcl1,wcl2) and tipopago<>"G" ;
*!*		order by presenta;  
*!*		into table (wtcu) && (c.cod2_cli,c.cod3_cli) ->no sirve este orden, por la cronologia 

wcier=wcier_an+1	
TEXT TO csql NOSHOW 	
select c.*,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,presenta>wcier_act as dfuturo,000 as ramo,0000.00 as extrapunto
	from cupusu c 
	where ppais=?welpa and cod1_cli=?wrg and cod2_cli=?wuss and presenta>?wcier  and tipopago<>'G' 
	order by presenta
ENDTEXT 

res=SQLEXEC(hnd,csql,wtcu)
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - CUPUSU - Sacando CUPONES")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF
 
*---cargo intereses que hay para cobrar al pago, en morosos y evitar iva ($=wintiva,U$=wintivd) 
store 0.00 to wintiva,wintivd,wseguro,wmovivn,wmovivd,wcomin,wcomid,iva_m,iva_do,wppes,wpdol,servi,servd,;
	winten,winted,wmovivd,wmovivp
*--winten y winted son intereses de planes del mes
*--win y wpn son intereses de financiacion
*-------------------------------------------------------------------------------------------------
*--cargo intereses de mora en tabcu, si es cierre de 
*Inventario no iria la carga para no calcular saldo que no figura en proceso.
if !wcereo
	wintemor=cargainter(wrg,wuss)
ENDIF 

*---cargo pagos del pos
if nacion=2 and wglobaiso="504736" and mywhnd>0
	sqlcancel(mywhnd)
	wtxsql="SELECT date(fecobro) as fecha, substr(trim(tarjeta),1,2) as cod1_cli,substr(trim(tarjeta),3,5) as cod2_cli, if(moneda=858,monto/100,0000000.00) as importe, "+;
			" 'P' as tipo_doc, if(moneda=840,monto/100,0000000.00) as impmone, if(moneda='858','U','D') as moneda, date(fecobro) as presenta, autorizacion as autoriza,'POSREDPA' as codoper "+;
	       "FROM `tarjetapos`.`pagoec` where anulado<>'S' and registrado<>'S' and tarjeta like ?wtj"
	wtj=strtran(str(wrg,2)," ","0")+strtran(str(wuss,5)," ","0")+"%"
	wrs=sqlexec(mywhnd,wtxsql,"sqlpagos")
	IF wrs<=0
       AERROR(verr)
       MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - Recuperando PAGOS")
	   upa=CTOD("")
       THROW "NORMAL"
	ENDIF 
		SELECT sqlpagos
		SCAN ALL 
			scatter memvar
			m.cod1_cli=val(sqlpagos.cod1_cli)
			m.cod2_cli=val(sqlpagos.cod2_cli)
			if type("sqlpagos.autoriza")="N"
				m.autoriza=str(autoriza,10)
			endif
			m.ppais=nacion
			m.ingresado="E"
			m.haber=iif(moneda="U",importe,impmone)
			m.campo=iif(moneda="U","pagon","pagod")
	   	    m.financia="N"
	    	m.signo="-"	
	    	*---trato intereses Mora---------------------------------------

		    wsaldo=round((importe+impmone)/(1+piva/100),2)
		    store 0.00 to wpagoin,wpagoid,wintepes,wintedol
			*--INTERESES MORA:
			if wintiva+wintivd>0
			    if moneda<>"D"
			    	wintepes=iif(wsaldo>wintiva,wintiva,wsaldo)
			    	wintiva=wintiva-wintepes
			    endif
			    if moneda="D"
	   		    	wintedol=iif(wsaldo>wintivd,wintivd,wsaldo)
	   		    	wintivd=wintivd-wintedol
			    endif
				*---IVA DEL INTERES: saco por diferencia antes y ahora y agrego el iva que se cobro y no almaceno.
				wpagoin=round(wintepes*(1+(piva/100)),2)
				wpagoid=round(wintedol*(1+(piva/100)),2)
			endif
			m.interes=wpagoin+wpagoid
			m.importe=iif(moneda<>"D",m.importe-(wpagoin+wpagoid),0)
			m.impmone=iif(moneda="D",m.impmone-(wpagoin+wpagoid),0)
			m.tasaiva=pIva
			m.cuotas="1/1"
			insert into (wtabcu) from memvar
		ENDSCAN 
		sele (wtabcu)
		if !ejecutable
			*browse
		endif
		use in sqlpagos

ENDIF 
if !winvn
	wpiva=cparametros.iva/100
	wbc=(nacion<>2)   &&banco central
	weldeta=iif(nacion=2,"Intereses Acumulados","Compras en Rou")		
	if wintiva>0
		insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta);
			values(nacion,'0',0,"P",wgrp,wusu,0,iif(wbc,"*","G"),iif(wbc,-99,-8),wintiva,wintiva/codd,wmonservi,wcier_act,wcier_act,"inpunn","+","N",weldeta,"-")
	ENDIF 
	*,impiva,tasaiva,iva
	*,wintiva*wpiva,wpiva*100,.t.
	if wintivd>0
		insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta);
			values(nacion,'0',0,"P",wgrp,wusu,0,iif(wbc,"*","G"),iif(wbc,-99,-8),wintivd*codd,wintivd,"D",wcier_act,wcier_act,"inpund","+","N",weldeta,"-")
	ENDIF 
*---Fin cargo:
ENDIF 
*---quito desde aqui el 18/2/10



*---DEVOLUCION PARA URUGUAY DEL IMESI COMBUSTIBLE
*--PROCESO LOS ITEMS QUE SE GRABAN EN PROCESO / CIERRE EN VCUPUSUNP-----
*-----------------------------------------------------------------------
*---- DGI DEVOLUCION IMESI COMBUSTIBLE  ------  URUGUAY -----* Y RRRRRREESSSSTTTTTAAAAUUUURRRRAAAANTESSSSS
sele (wtabcu)	
if !ejecutable
	*BROWSE 
ENDIF 
DELETE all for "nada" $ campo
weldes=sys(2015)
*copy to (wcamino+weldes)   &&creo archivo temporario para tomar los descuentos
SELECT * FROM (wtabcu) INTO CURSOR weldestmp
*use (wcamino+weldes) in 0
weldes="weldestmp"
SELECT (weldes)
*---agrego registros desde wtabac para luego procesar datos totales del cierre
walicuor=iif(nacion=2,cparametros.alicuor,0.0)  	&& alicuota restorant
walicuoe=cparametros.alicuoe  	&& alicuota estaciones
wramocomb=cparametros.ramocomb 	&& codigo ramo combustibles
wiva_r=1+cparametros.iva/100
wramorest=iif(nacion=2,cparametros.ramo_res,-100.00)
wcod_ade="????"
if nacion=2 and wglobaiso="504736"
	wcod_ade=alltrim(cparametros.codigo_ad)
ENDIF 
*store 0.00 to wreincomp,wreinvarp,wreincomd,wreinvard
store 0.00 to wtcupones,wtreinte,wtcuponesn,wtreinten,wtcuponesnN
store 0.00 to wtcuponed,wtreinted,wtcuponedn,wtreintedn,wtcuponednN   &&n es de nafta
scan all for between(presenta,wcier_an+1,wcier_act)	and nacion=2 and wglobaiso="504736"
	wali=walicuoe
	wnafta=.f.
	wresto=.f.
	wprs=&weldes..presenta
	wfps=&weldes..fecha
	wcomer=comercio
	wpais=paisco  &&para cupones de comercios argentinos.
	wcta=iif(empty(&weldes..cuotas),"1/",substr(alltrim(&weldes..cuotas),1,2))  && cuota del plan
	wcondiva=""
	wbole=&weldes..boleta
	wnrcu=&weldes..nrocupon
	wmnop=&weldes..moneda
	wcmop=&weldes..comercio
	if &weldes..tipo_doc $ tiraplan and (&weldes..paisco=2 and wglobaiso="504736") and wcta="1/" and left(&weldes..boleta,1) $ "1_2" and len(alltrim(&weldes..boleta))=10 and;
		val(right(&weldes..boleta,9))>0
		
		csql1="select ramo,condiva from comercio where comercio.comercio=?wcomer and comercio.paisco=?wppai" 
		res=SQLEXEC(hnd,csql1,"cramo")
		IF res<=0
           AERROR(verr)
           MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - Verificando DESCUENTOS POR RAMOS")
	       upa=CTOD("")
           THROW "NORMAL"
		ENDIF 
		if !EOF("cramo")
			replace ramo with cramo.ramo in (weldes)
			wcondiva=cramo.condiva
		endif	
		if used("cramo")
			use in cramo
		ENDIF 	
		wmt=0
		wdet=""
		wdiv=1     && el divisor para el calculo que en restorant sera 1.22 o sea el iva_r
		wali=0.00  &&si no corresponde a combustible no va 
		if &weldes..ramo=wramocomb 
			wmt=-13
			wnafta=.t.
			wdet="Red IMESI Dto 398/7"
			wali=walicuoe	
		endif	
		if &weldes..ramo=wramorest
			wdiv=wiva_r
			wmt=-31
			wresto=.t.
			wdet="Red_Alíc IVA L.17.934"
			wali=walicuor	
		endif	
		*--calcular total de cupon para casos cuotas, contado.
		wcut=val(iif(empty(&weldes..cuotas),"1",substr(alltrim(&weldes..cuotas),3,2)))  && cuotas totales
		wtotcup=round(&weldes..importe*wcut,2)
		wvtot=alltrim(str(wtotcup,10,2))
		*ver $,U$
		if &weldes..moneda="D"
			if !wnafta and !wresto
				wtcuponed=wtcuponed+wtotcup/codd
				wtreinted=wtotcup/wdiv*wali/100/codd
			else
				wtcuponednN=wtcuponednN+iif(wresto,wtotcup/codd,0.00)   && acumulo descuento restorant para el pos
				wtcuponedn=wtcuponedn+wtotcup/codd
				wtreintedn=wtotcup/wdiv*wali/100/codd
			endif
		else
			if !wnafta and !wresto
				wtcupones=wtcupones+wtotcup
				wtreinte=wtotcup/wdiv*wali/100
			else
				wtcuponesnN=wtcuponesnN+iif(wresto,wtotcup,0.00)   && acumulo descuento restorant para el pos
				wtcuponesn=wtcuponesn+wtotcup
				wtreinten=wtotcup/wdiv*wali/100
			endif	
		endif
		replace valret with (wtotcup/wdiv*wali/100)/iif(&weldes..moneda="D",codd,1), tcupones with wtotcup/iif(&weldes..moneda="D",codd,1) in (weldes) 
		
		*----?? grabo para el proceso el reintegro del iva/ consulta periodo activo/inventario
		if wtreinte>0
				insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
					values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinte,wtreinte/codd,wmnop,wfps,wprs,"ajusten","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
				if winvn   && si esta procesando cierre va a cupusu con nproceso
						insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nocupon,autoriza,comercio,codoper);
						values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinte,wtreinte/codd,wmnop,wfps,wprs,"ajusten","-","N",wtreinte,wtcupones,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)	
				ENDIF 	
		ENDIF 
		if wtreinten>0
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
				values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinten,wtreinten/codd,wmnop,wfps,wprs,"ajusten","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
			if winvn   && si esta procesando cierre va a cupusu con nproceso
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nrocupon,autoriza,comercio,codoper);
					values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinten,wtreinten/codd,wmnop,wfps,wprs,"ajusten","-","N",wtreinten,wtcuponesn,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)		
			ENDIF 	 
		ENDIF 
		*--para dolares
		if wtreinted>0
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
				values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinted*codd,wtreinted,wmnop,wfps,wprs,"ajusted","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
			if winvn   && si esta procesando cierre va a cupusu con nproceso		
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nrocupon,autoriza,comercio,codoper);
					values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinted*codd,wtreinted,wmnop,wfps,wprs,"ajusted","-","N",wtreinted,wtcuponed,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
		
			endif
		endif	
		if wtreintedn>0
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
				values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreintedn*codd,wtreintedn,wmnop,wfps,wprs,"ajusted","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
			if winvn   && si esta procesando cierre va a cupusu con nproceso	
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nrocupon,autoriza,comercio,codoper);
					values(iif(wglobaiso="504736",nacion,wppai),wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreintedn*codd,wtreintedn,wmnop,wfps,wprs,"ajusted","-","N",wtreintedn,wtcuponedn,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)		
			ENDIF 	
		ENDIF 	
	ENDIF 	
	SELECT (weldes)	
ENDSCAN 
use in (weldes)
wtcuponedn=wtcuponedn-wtcuponednN
wtcuponesn=wtcuponesn-wtcuponesnN


*--- "Red IMESI Dto 398/7"
*--FIN REINTEGRO IVA DECRETO imesi 398/7 --
************************************************************************
************************************************************************
SELECT (wtabcu)
*!*	if !ejecutable
*!*		browse title "Al salir de devolucion"
*!*		set echo on
*!*		set step on
*!*	endif
*----quito aca lo de desucento en uruguay---------------
if alltrim(wglobaiso)="504736" 
	*---promos armado.
	wlprom=promosa(wtabcu,iif(winvn,"vcupusunp",""),wcier_an,wcier_act)
	if wvthoy >wvtoan+wgrapag
		*----decido si quito la promo por inhabilitado o falta de pago.
		wvaprom=.t.
		wminan=&wtabin..paga_minn+&wtabin..paga_mind*codd
		SELECT (wtabcu)	
		sum all iif(moneda="D",(impmone+interes)*codd,importe+interes) to wsupamini for (tipo_doc="P" or tipo_doc="J" and motivo=142 and nacion=2) and presenta<=wcier_act
		wvaprom= wsupamini >= (wminan *wpormin/100) && si cubre el porcentaje del minimo
		csql2="select * from usuarios where ppais=nacion and wrg=cod1_cli and wuss=cod2_cli"
		res=SQLEXEC(hnd,csql2,"cprom")
		IF res<=0
           AERROR(verr)
           MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - Datos del Usuario")
	       upa=CTOD("")
           THROW "NORMAL"
        ENDIF 
		
		if EOF("cprom")
		   MESSAGEBOX("Error IMPOSIBLE - Usuario NO HALLADO",16,"PROCESARUS - Datos USUARIO")
	       upa=CTOD("")
           THROW "NORMAL"
		ENDIF 
		wvaprom=wvaprom and (cprom.habilitado<2 )
		if !wvaprom 
			*--debo quitar de archivos las promos (wtabcu,vcupusunp)
			quitapromo(wtabcu,iif(winvn,"vcupusunp",""),wcier_an,wcier_act,wgrp,wusu)
		ENDIF 
	ENDIF 
ENDIF 
************************************************************************
*----------------FIN REINTEGRO EL Shell/LAGO   combustibles-------------------------
*-----------------------------------------------------------------------
*--FIN PROCESO LOS ITEMS QUE SE GRABAN EN PROCESO / CIERRE EN VCUPUSUNP-----
*-----------------------------------------------------------------------


******************************************************************
******************************************************************
*--- :PROCESO  DE CALCULO -between(presenta,wciean,wcier_act) -cierre actual
******************************************************************
******************************************************************
sele (wtabcu)
replace all debe with iif(moneda="D",impmone,importe) for signo="+"  in (wtabcu)
replace all haber with iif(moneda="D",impmone,importe)+iif(tipo_doc="P",interes,0.00) for signo="-" in (wtabcu)
if !ejecutable
	*browse title "Antes de select comercios"
ENDIF

*------- Prepara para Listas --------------------------------------------*
SELECT distinct paisco,comercio FROM (wtcu) INTO CURSOR preplista
STORE "" TO wlstpais,wlstn
SCAN 
   IF ppais=nacion
       IF EMPTY(wlstpais)
          wlstpais=wlstpais+ALLTRIM(STR(comercio))
       ELSE
          wlstpais=wlstpais+","+ALLTRIM(STR(comercio))
       ENDIF 
    ENDIF 
   IF ppais<>nacion
       IF EMPTY(wlstn)
          wlstn=wlstn+ALLTRIM(STR(comercio))
       ELSE
          wlstn=wlstn+","+ALLTRIM(STR(comercio))
       ENDIF 
    ENDIF 
ENDSCAN 
*-----------------------------------------------------------------------*

if wcdeta or winvn		&& si es para mostrar debo cargar los detalles y scoring
	repla all item with recno() for item=0
	*--carga nombre de comercios en cupones correspondientes ---
	repla all detalle with iif(tipo_doc="P","Su Pago ",iif(tipo_doc="G" and motivo=0,"Costo del Resumen",""))  for empty(detalle)
	
*!*		select alltrim(c.nombre)+" "+iif(alltrim(u.cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(u.cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre,u.item,c.extrapunto;
*!*			from tarjeta!comercio c,(wtcu)u;
*!*			where str(u.paisco,2)+str(u.comercio,5)+str(u.sucursal,1)=str(c.paisco,2)+str(c.comercio,5)+str(c.sucursal,1);
*!*			into cursor ucomer

    csql4="select nombre, extrapunto from comercio where (paisco=?nacion and inSTR(?wlstpais,comercio)) or (paisco<>?nacion inSTR(?wlstn,comercio))"
    res=sqlexec(hnd,csql4,"ucomer")
    IF res<=0
       AERROR(verr)
       MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - Datos del Usuario")
	   upa=CTOD("")
       THROW "NORMAL"
    ENDIF 
    
	csql3="select alltrim(c.nombre)+' '+iif(alltrim(u.cuotas)='1/ 1' and len(alltrim(cuotas))=4 or alltrim(u.cuotas)='1/1' and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre,u.item,c.extrapunto;
		from comercio c,(wtcu)u;
		where str(u.paisco,2)+str(u.comercio,5)+str(u.sucursal,1)=str(c.paisco,2)+str(c.comercio,5)+str(c.sucursal,1);
		into cursor ucomer
		
		
	if _tally<>0
		sele ucomer
		scan all
			uitem=item
			sele (wtabcu)
			scan all for item=uitem
				replace detalle with ucomer.nombre, extrapunto with ucomer.extrapunto
			endscan
			sele ucomer
		endscan
	endif	
	if used("ucomer")
		use in ucomer
	endif
	if wcdeta
		*--carga tipos de documento sin motivo---
		select alltrim(p.detalle)+" "+iif(alltrim(u.cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(u.cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre ,u.item from tarjeta!planes p,(wtcu)u;
			where u.tipo_doc=p.tipo_doc and p.pais=welpa;
				and u.motivo=0 and u.comercio=0;
			into cursor pmoti
		if _tally<>0
			sele pmoti
			scan all
				uitem=item
				sele (wtabcu)
				scan all for item=uitem
					replace detalle with pmoti.nombre
				endscan
				sele pmoti
			endscan
		endif	
		if used("pmoti")
			use in pmoti
		endif
		*----carga motivos ------------------
		select alltrim(m.nota)+" "+iif(alltrim(u.cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(u.cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre,u.item from tarjeta!motivos m,(wtcu)u;
			where u.motivo=m.motivo and m.pais=iif(wglobaiso="504736",nacion,wppai);
			into cursor mmoti
		if _tally<>0
			sele mmoti
			scan all
				uitem=item
				sele (wtabcu)
				scan all for item=uitem
					replace detalle with alltrim((mmoti.nombre))+iif(motivo=94 and nacion=1 or motivo=293 and nacion=2," "+autoriza,"")
				endscan
				sele mmoti
			endscan
		endif	
		if used("mmoti")
			use in mmoti
		endif
		sele (wtabcu)
		*repla all detalle with "Seguro Sobre Saldo " for motivo=-1
	endif
endif
*---fin carga de motivos----------------
sele (wtabcu)	
if !ejecutable
	*browse	title "cupones del proceso wtabcu"
	copy to wcamino+"wtabcu"
	*set echo on
	*set step on
endif
wcereo=.f.
store 0.00 to winpagp,winpagd,wajupn,wajupd
store 0.00 to wincen,wpncen,widcen,wpdcen          && para controlar en caso mora u otro la dif int.
scan all for between(presenta,wcier_an+1,wcier_act)
	*--ver signo,financia,campo, proceso movimiento asignando campo copia inventario
	wtcd=&wtabcu..tipo_doc
	store 0.00 to win,wpn
	cp=alltrim(&wtabcu..campo)
	cval=(iif(moneda="D",&wtabcu..impmone,&wtabcu..importe)+iif(&wtabcu..tipo_doc="P",&wtabcu..interes,0.00))*iif(&wtabcu..signo="-",-1,1)
	replace &cp with &cp+cval in (wtabin)
	*-- identifico movimiento seguro de vida MOTIVO=-1, asi como los que tienen iva.
	if &wtabcu..iva      && iva a calcular sobre el importe
		*wmovivp=wmovivp+iif(moneda<>"D",&wtabcu..importe,0.00)
		*wmovivd=wmovivd+iif(moneda= "D",&wtabcu..impmone,0.00)
	endif
	if &wtabcu..iva   and &wtabcu..tipo_doc="Q"   and nacion=1  && iva a calcular sobre el importe
		wmovivp=wmovivp+iif(moneda<>"D",&wtabcu..importe,0.00)
		*wmovivd=wmovivd+iif(moneda= "D",&wtabcu..impmone,0.00)
	endif	
	if motivo=-1
		wseguro=wseguro+importe && debo separar para calcularle iva
	endif
	if &wtabcu..tipo_doc="J"
		if  motivo=wcdajup && para poder salvar ajustes a cuotas del mes
			replace ajupagon with ajupagon+iif(&wtabcu..moneda="D",0.00,&wtabcu..importe+iif(alltrim(&wtabcu..autoriza)<>"CEREO",&wtabcu..interes,0.00)),;
				 ajupagod with ajupagod+iif(&wtabcu..moneda="D",&wtabcu..impmone+iif(alltrim(&wtabcu..autoriza)<>"CEREO",&wtabcu..interes,0.0),0.00) in &wtabin
			wj=&wtabin..ajupagon
		else
			*--los ajustes del periodo solo afectan el saldo final o Total.
			*wajupn=wajupn+iif(&wtabcu..moneda="D",0.00,&wtabcu..importe+iif(alltrim(&wtabcu..autoriza)<>"CEREO",&wtabcu..interes,0.00))
			*wajupd=wajupd+iif(&wtabcu..moneda="D",&wtabcu..impmone+iif(alltrim(&wtabcu..autoriza)<>"CEREO",&wtabcu..interes,0.00),0.00)
			wajupn=wajupn+iif(&wtabcu..moneda="D",0.00,&wtabcu..importe)
			wajupd=wajupd+iif(&wtabcu..moneda="D",&wtabcu..impmone,0.00)
		endif
	endif
	*--acumulo interes y comision del movimiento y la financiacion o no para el minimo.
	*--considero lista de comerios que no financian. podria cargarla en lectura y aqui lo olvido.
	*wfinan=(&wtabcu..financia="S" and &wtabcu..paisco=nacion)
	if &wtabcu..tipo_doc $ wcod_ade   && para adelantos en efectivo
		wcupan=wcupan+iif(moneda="D",importe*codd,importe)
	endif
	wfinan=(&wtabcu..financia="S")
	wmon=&wtabcu..moneda
	wimpn=&wtabcu..importe*iif(&wtabcu..signo="-",-1,1)
	wimpd=&wtabcu..impmone*iif(&wtabcu..signo="-",-1,1)
	wcomision=&wtabcu..comision*iif(&wtabcu..signo="-",-1,1)*iif(&wtabcu..tipo_doc="P",0,1)
	*--no descartar filtro wglobaiso=504736 ojo el interes esta inventariado
	winteres=&wtabcu..interes*iif(&wtabcu..signo="-",-1,1)*iif(&wtabcu..tipo_doc="P",0,1)
	
	if ! alltrim(&wtabcu..tipo_doc) $ "P_J" or &wtabcu..tipo_doc="J" and &wtabcu..signo="+"
		*wcomnfin=cparametros.comernofin && comercios que no financian nada.
		*--INADEN,INADED: acumulador intereses de cuotas,etc, comisiones,etc si no es pago...?? 
		*wfinan=financia="S" and ! ("_"+alltrim(str(comercio,5))+"_" $ wcomnfin ) && puedo sacarlo esta en lecturas
		*inaden with inaden+abs(iif(wmon="D",0,winteres)), inaded with inaded+abs(iif(wmon="D",winteres,0))
		replace financian with financian+iif(wmon="D" or !wfinan,0.00,wimpn),;
			nfinancian with nfinancian+iif(wmon="D" or wfinan,0.00,wimpn),;
			financiad with financiad+iif(wmon<>"D" or !wfinan,0.00,wimpd),; 
			nfinanciad with nfinanciad+iif(wmon<>"D" or wfinan,0.00,wimpd),;
			inaden with inaden+iif(wmon="D",0,winteres), inaded with inaded+iif(wmon="D",winteres,0) ,;
			gaston with gaston+abs(iif(wmon="D",0,wcomision)),gastod with gastod+abs(iif(wmon="D",wcomision,0)) in (wtabin)
			
			*---acumulo comisiones para saber iva.---
			wcomin=wcomin+abs(iif(wmon="D",0,wcomision))
			wcomid=wcomid+abs(iif(wmon="D",wcomision,0))
			winten=winten+abs(iif(wmon="D",0,winteres))
			winted=winted+abs(iif(wmon="D",winteres,0))
			iva_m=iva_m+&wtabcu..impiva*iif(wmon="D"   or wtcd="D" and resumen=-11,0,1)      &&acumulador de iva
			iva_do=iva_do+&wtabcu..impiva*iif(wmon="D" or wtcd="D" and resumen=-11,1,0)    &&acumulador de iva 
	endif
	
	*calcular interes financiacion y punitorio en variables INTN,inpunn,INTD,inpund si hay Mora van a interes.dbf--------
	if alltrim(&wtabcu..tipo_doc) $ "P_J" and &wtabcu..signo="-"   
		*--debo calcular intereses corridos por cada pago y saldo.
		wss=&wtabin..saldo_a_n
		wstd=&wtabcu..signo
		wsti=&wtabcu..importe
		wmintiva=0.00
		do case
		case wmon<>"D"
			*=================PESOS
			*winreng=interes  		&& interes actual del pago
			winpagp=winpagp+iif(tipo_doc="P",interes,0) &&para calcular interes sobre interes del pago 
			*---acumulo comisiones para saber iva.---
			wcomin=wcomin+iif(wmon="D",0,wcomision)
			winten=winten+iif(wmon="D",0,winteres)

			*CALCULO: Incluye (importe) pues esta descontado y a esta fecha se debe en saldos.
			** upa=&wtabcu..presenta     		&& ultimo pago el de abajo

			iva_m=iva_m+&wtabcu..impiva*iif(&wtabcu..tipo_doc="P",1,-1) &&*------19/12/2012 0, cambio por -1
			if &wtabcu..impiva=0.00 and alltrim(&wtabcu..autoriza)<>"CEREO"
				iva_m=iva_m+&wtabcu..interes-round(&wtabcu..interes/(1+&wtabcu..tasaiva/100),2)  && iva del interes, esta incluido al cobrar 
			endif
			if alltrim(&wtabcu..autoriza)="CEREO"
				wcereo=.t.
				wmintiva=&wtabcu..impiva+&wtabcu..interes
				iva_m=iva_m+&wtabcu..impiva*-1
			ENDIF
			wppes=wppes+iif(&wtabcu..tipo_doc="P",round(&wtabcu..interes/(1+&wtabcu..tasaiva/100),2),0.00)	&& intereses acumulados en pago
			*--en ssdoa esta descontado el pago importe del actual movimiento.
			ssdoa=&wtabin..saldo_a_n-abs(&wtabin..pagon)-abs(&wtabin..ajupagon)+&wtabcu..importe-wmintiva
			sminima=&wtabin..paga_minn-abs(&wtabin..pagon)-abs(&wtabin..ajupagon)+&wtabcu..importe-wintiva
			ffg=wvtoan+wgrapag
			if presenta>ffg and ssdoa>0.00 and (alltrim(&wtabcu..autoriza)="CEREO" or &wtabcu..tipo_doc="P")
				win=intdias(wpaga,presenta,ssdoa+winpagp+wintiva,ifin)
				wpn=intdias(wpaga,presenta,sminima+winpagp+wintiva,ipun)
				wincen=wincen+intdias(wpaga,presenta,winpagp+wintiva,ifin)*iif(nacion=1 and wglobaiso="504736",1,0)
				wpncen=wpncen+intdias(wpaga,presenta,winpagp+wintiva,ipun)*iif(nacion=1 and wglobaiso="504736",1,0)
				replace intn with intn+win, inpunn with inpunn+wpn in (wtabin)
					*nfinancian with nfinancian+win+wpn in (wtabin)
			endif
			if &wtabcu..presenta<=ffg and ssdoa>0.00
				*ssdoa+abs(&wtabcu..importe)>=0.00	&& se esta pagando anticipado al vto
				*  26/05 replace intn with 0.00, inpunn with 0.00 in (wtabin)
			endif
			upa=iif(&wtabcu..tipo_doc="P" or alltrim(&wtabcu..autoriza)="CEREO",&wtabcu..presenta,upa) && avanza fecha de pago si es "P"
			wpaga=iif(&wtabcu..presenta>ffg and &wtabcu..tipo_doc="P" or alltrim(&wtabcu..autoriza)="CEREO",&wtabcu..presenta,wpaga)
		case wmon="D"
			*================DOLARES
			*store 0.00 to wintivd,wmovivd,wcomid,iva_do,wpdol,servd,winted,wmovivd
			*winreng=interes  		&& interes actual del pago
			winpagd=winpagd+iif(tipo_doc="P",interes,0)
			*---acumulo comisiones para saber iva.---
			wcomid=wcomid+iif(wmon="D",wcomision,0)
			winted=winted+iif(wmon="D",winteres,0)

			*CALCULO: Incluye (importe) pues esta descontado y a esta fecha se debe en saldos.
			iva_do=iva_do+&wtabcu..impiva*iif(&wtabcu..tipo_doc="P",1,-1) &&*------19/12/2012 0, cambio por -1
			if &wtabcu..impiva=0.00
				iva_do=iva_do+&wtabcu..interes-round(&wtabcu..interes/(1+&wtabcu..tasaiva/100),2)  && iva del interes, esta incluido al cobrar 
			endif
			if alltrim(&wtabcu..autoriza)="CEREO"
				wmintiva=&wtabcu..impiva+&wtabcu..interes
				iva_do=iva_do+&wtabcu..impiva*-1
			endif

			wpdol=wpdol+iif(&wtabcu..tipo_doc="P",round(&wtabcu..interes/(1+&wtabcu..tasaiva/100),2),0.00)	&& intereses acumulados en pago
			
			*--ssdod ya tiene descontado impmone de actual movimiento
			ssdod=&wtabin..saldo_a_d-abs(&wtabin..pagod)-abs(&wtabin..ajupagod)+&wtabcu..impmone-wmintiva
			*ssdod=iif(ssdod>0,ssdod,0.00)
			sminimd=&wtabin..paga_mind-abs(&wtabin..pagod)-abs(&wtabin..ajupagod)+&wtabcu..impmone-wmintiva
			ffg=wvtoan+wgrapag
			if presenta>ffg and ssdod>0.00 and (alltrim(&wtabcu..autoriza)="CEREO" or &wtabcu..tipo_doc="P")
				win=intdias(wpagd,presenta,ssdod+winpagd+wintivd,ifid)
				wpn=intdias(wpagd,presenta,sminimd+winpagd+wintivd,ipud)
				widcen=widcen+intdias(wpagd,presenta,winpagd+wintivd,ifid)*iif(nacion=1 and wglobaiso="504736",1,0)
				wpdcen=wpdcen+intdias(wpagd,presenta,winpagd+wintivd,ipud)*iif(nacion=1 and wglobaiso="504736",1,0)
				replace intd with intd+win, inpund with inpund+wpn in (wtabin)
			endif

			if &wtabcu..presenta<=ffg and ssdod>0.00
				*ssdod+abs(&wtabcu..impmone)>=0.00	&& se esta pagando anticipado al vto
				*  26/05  replace intd with 0.00, inpund with 0.00 in (wtabin)
			endif
			upd=iif(&wtabcu..tipo_doc="P" or alltrim(&wtabcu..autoriza)="CEREO",&wtabcu..presenta,upd) && avanza fecha de pago si es "P"
			wpagd=iif(&wtabcu..presenta>ffg and &wtabcu..tipo_doc="P" or alltrim(&wtabcu..autoriza)="CEREO",&wtabcu..presenta,wpagd)
		endcase	
		*--para reflejar interes y comision como gasto y ajustes no financiable
		*--se suma inte y comi tiene signo, lo otro esta sin sgino
		
		if &wtabcu..tipo_doc<>"P"
			*--para reflejar interes y comision como gasto y ajustes no financiable
			replace inaden with inaden+iif(wmon="D",0,winteres), inaded with inaded+iif(wmon="D",winteres,0) ,;
				gaston with gaston+iif(wmon="D",0,wcomision),gastod with gastod+iif(wmon="D",wcomision,0) in (wtabin)
		endif	
	endif	
	wnade=&wtabin..inaden
	jjjj=2
endscan
*FINALIZADO PROCESO: hace calculo de intereses y ve que pasa con la MORA.
store 0.00 to wimpsell
if nacion<>2 and !wugest    &&nacion=1 para ccdia

	wimpsell=round(wsellcomp*(codd*(&wtabin..comprad+&wtabin..cuotad+&wtabin..ppagod+&wtabin..adelantod+&wtabin..gastod-wajupd)+&wtabin..compran+&wtabin..cuotan+&wtabin..ppagon+&wtabin..adelanton+&wtabin..gaston-wajupn),2)
	if wimpsell>0  and nacion<>2 and !wugest
		insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe);
			values(iif(wglobaiso="504736",nacion,wppai),-5,-5,"P",wgrp,wusu,0,"G",-5,wimpsell,wimpsell/codd,cparametros.monpais,wcier_act,wcier_act,"selladon","+","N","Sellado Provincial ",wimpsell)
		if winvn   && si esta procesando cierre va a cupusu con nproceso
			insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia);
				values(iif(wglobaiso="504736",nacion,wppai),-5,-5,"P",wgrp,wusu,0,"G",-5,wimpsell,wimpsell/codd,cparametros.monpais,wcier_act,wcier_act,"selladon","+","N")
		endif
	else
		store 0 to wimpsell			
	endif
endif
sele (wtabin)
if !ejecutable
	*if winvn and cod2_cli=534
		*browse title "wtabin "+str(wimpsell,8,2)
	*endif
endif
if wugest    
  store 0.00 to infi,inpun,wimpsell,servi,servd
  replace &wtabin..servicio with 0.00 in (wtabin)
endif
********************************************************************
* se determina si graba o no intereses en archivo de mora
* segun se cubre minimo o no
****************PESOS ************* 
ssdoa=&wtabin..saldo_a_n-abs(&wtabin..pagon)-abs(&wtabin..ajupagon)
*ssdoa=iif(ssdoa>0,ssdoa,0.00)
sminima=&wtabin..paga_minn-abs(&wtabin..pagon)-abs(&wtabin..ajupagon)
*sminima=iif(sminima>0,sminima,0.00)
store 0.00 to win,wpn  && acumula interes
if ssdoa+wintiva+winpagp >0 
    if sminima+winpagp>0 .and. wvtoan+wgracia<=date()
   *if sminima+winpagp>0 .and. wvtoan+wgracia<wpaga
        wpn=intdias(Wpaga,wvto_act,Sminima+wintiva+winpagp,ipun)
   		wpncen=wpncen+intdias(wpaga,wvto_act,wintiva+winpagp,ipun)*iif(nacion=1 and wglobaiso="504736",1,0)
    endif
    win=intdias(Wpaga,wvto_act,Ssdoa+wintiva+winpagp,ifin)
    wincen=wincen+intdias(wpaga,wvto_act,wintiva+winpagp,ifin)*iif(nacion=1 and wglobaiso="504736",1,0)
    if wpn<0 or win<0 &&fecha de vto menor al pago no interes
       store 0 to wpn,win,wincen,wpncen
    endif

endif ssdoa >0
if !ejecutable
	*browse title "Ver que tengo cargado en nfinancia "
endif
*---debo reemplazar campos calculados en (wtabin) no se suma
*			 sellado, intn,inpunn  en el total esta en nfinancian caso wintiva--------
replace intn with intn+win, inpunn with inpunn+wpn, selladon with wimpsell in (wtabin)
replace	nfinancian with nfinancian+&wtabin..intn+&wtabin..inpunn+wimpsell+wcomin+winten-iif(!winvn,wintiva,0.00) in (wtabin)
* ************DOLARES***************
            ********** 
ssdod=&wtabin..saldo_a_d-abs(&wtabin..pagod)-abs(&wtabin..ajupagod)
*ssdod=iif(ssdod>0,ssdod,0.00)
sminimd=&wtabin..paga_mind-abs(&wtabin..pagod)-abs(&wtabin..ajupagod)
*sminimd=iif(sminimd>0,sminimd,0.00)
store 0.00 to win,wpn && acumula interes
if ssdod+wintivd+winpagd >0 
    if sminimd+winpagp>0 .and. wvtoan+wgracia<=date()
    *if sminimd+winpagp>0 .and. wvtoan+wgracia<wpaga
        wpn =intdias(Wpagd,wvto_act,Sminimd+wintivd+winpagd,ipud)
		wpdcen=wpdcen+intdias(wpagd,wvto_act,wintivd+winpagd,ipud)*iif(nacion=1 and wglobaiso="504736",1,0)
    endif
    win =intdias(Wpagd,wvto_act,Ssdod+wintivd+winpagd,ifid)
    widcen=widcen+intdias(wpagd,wvto_act,wintivd+winpagd,ifid)*iif(nacion=1 and wglobaiso="504736",1,0)
    if wpn<0 or win<0   &&fecha de vto menor al pago no interes
       store 0 to wpn,win,widcen,wpdcen
    endif

endif 
*---debo reemplazar campos calculados en (wtabin) no se suma
*			 sellado, intn,inpunn  en el total esta en nfinancian caso -wintivd--------
replace intd with intd+win, inpund with inpund+wpn in (wtabin)
*replace nfinanciad with nfinanciad+win+wpn+wcomid+winted in (wtabin)
replace	nfinanciad with nfinanciad+&wtabin..intd+&wtabin..inpund+wcomid+winted-iif(!winvn,wintivd,0.00) in (wtabin)


************* ANALISIS DE MORA E INTERESESE Y TASAS *************
       ***********************************************************
*======>Analizo si el usuario va a generar mora en archivo interes
     *seteo acumuladores para el inventario y se graba interes.
     * si estoy en consulta solo cargo punitorio como suma
     * lo hago por separado y tambien al grabar.
     * wproxcie indica el proximo cierre para iva de mora afuera de sistema.
       ***********************************************************
*..11/10/07...inicio.......................      ANALINTER
*-investigo exeso de tasa y redondeo *--usuario va a tener interes directo al resumen_*
*function fdift(ical,tsis,tbc,itope,iva)
*---cparametros ical=Calculo, tsis=tasa aplicabel, tbc=asa ficta, itoper =minimo imponible, iva del mes
*wtaexcif=fdift(&wtabin..intn,ifin,cparametros.int_fin,cparametros.tope_dift,piva)  	&& interes financiero
store 0.00 to inpuniva, inpundiva       
pames=abs(&wtabin..pagon)+abs(&wtabin..ajupagon)
wnmra=.f.
if pames<(wminn * wpormin/100) and  wminn>0  and wvthoy>wvto_an  &&controlo hasta el vto del grupo si esta en mora
	wnmra=.t.
    inpuniva=&wtabin..inpunn+&wtabin..intn
    *if nacion=2
	    replace inpunn with 0.00, intn with 0.00, inmorap with inpuniva,;
   			nfinancian with nfinancian-inpuniva in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
   	*endif
endif	
*---todo en nacion 2 dara 0 porque devuelvo esto en cero
*store 0.00 to wincen,wpncen,widcen,wpdcen          && para controlar en caso mora u otro la dif int.
store 0.00 to wdifif,wdifip
if !wnmra
	winf=&wtabin..intn
	wipn=&wtabin..inpunn
	wdifif=fdift(winf-wincen,ifin,cparametros.int_fin,cparametros.tope_dift,piva)+wincen  	&& interes financiero
	wdifip=fdift(wipn-iif(!winvn,wintiva,0.00)-wpncen,ipun,cparametros.int_punc,cparametros.tope_dift,piva)+wpncen  && interes punitorio
	if wdifif<0  &&corresponde rebajar por no poder desagregar y nada mas
		replace intn with intn-wdifif,nfinancian with nfinancian-wdifif in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
	endif
	if wdifip<0  &&corresponde rebajar por no poder desagregar y nada mas
		replace inpunn with inpunn-wdifip,nfinancian with nfinancian-wdifip in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 		
	endif
	*--caso para pesos-----------
	wlbcmora=0
	if wdifif>0
		*--corresponde grabar movimiento de ajustes con correspondiente iva para restar
		*iif(!wcereo,0,wdifip)
		if wnmra   &&esta en mora dejo solo los intereses financieros correctos y cargo mora para grabar si cierra
		    replace intn with winf-wdifif, inmorap with iif(!wcereo,wdifif,0),;
	   			nfinancian with nfinancian-wdifif in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
		endif
		if !wnmra or (wnmra and !wcereo)
			wcpp=iif(wdifif<cparametros.tope_serv,"gaston","compran")   &&que campo
			wtxt=iif(wdifif<cparametros.tope_serv,"Llamadas Telefonicas ","Compras Rou") && que texto
			
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe,impiva,tasaiva,boleta);
				values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifif,wdifif/codd,cparametros.monpais,wcier_act,wcier_act,wcpp,"+","N",wtxt,wdifif,wdifif*(piva/100),piva,iif(!wnmra,"*","I"))
		endif	
		if winvn  and !wnmra && si esta procesando cierre va a cupusu con nproceso
			insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,tasaiva,boleta,comercio,paisco);
				values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifif,wdifif/codd,cparametros.monpais,wcier_act,wcier_act,wcpp,"+","N",0,0,wdifif*(piva/100),piva,"*",iif(wcpp="compran",wgrancr,0),2)	
			wgrancr=wgrancr+1	
		endif	
	endif
	if wdifip>0  
		if wnmra  &&esta en mora dejo solo los intereses financieros correctos y cargo mora para grabar si cierra
		    replace inpunn with wipn-wdifip, inmorap with inmorap+iif(!wcereo,wdifip,0),;
	   			nfinancian with nfinancian-wdifip in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
	   			wlbcmora=wlbcmora+wdifip
		endif
		if !wnmra or (wnmra and !wcereo)
			wcpp=iif(wdifip<cparametros.tope_serv,"gaston","compran")
			wtxt=iif(wdifip<cparametros.tope_serv,"Llamadas Telefonicas ","Compras Rou") && que texto
			*--corresponde grabar movimiento de ajustes con correspondiente iva para restar
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe,impiva,tasaiva,boleta);
				values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifip,wdifip/codd,cparametros.monpais,wcier_act,wcier_act,"nada","+","N",wtxt,wdifip,wdifip*(piva/100),piva,iif(!wnmra,"-","I"))
		endif		
		if winvn and !wnmra  && si esta procesando cierre va a cupusu con nproceso
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,tasaiva,boleta,comercio,paisco);
					values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifip,wdifip/codd,cparametros.monpais,wcier_act,wcier_act,"nada","+","N",0,0,wdifip*(piva/100),piva,"-",iif(wcpp="compran",wgrancr,0),2)	
				wgrancr=wgrancr+1		
		endif	
	endif
endif
*--fin caso pesos

*---caso de dolares---------U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,U$,-------------
wlbcmorad=0
wnmrad=.f.
pamed=abs(&wtabin..pagod)+abs(&wtabin..ajupagod)
if pamed<(wmind * wpormin/100) and wmind>0 and wvthoy>wvto_an &&controlo hasta el vto del grupo
	wnmrad=.t.
    inpundiva=&wtabin..inpund+&wtabin..intd
    *if nacion=2
	    replace inpund with 0.00, intd with 0.00,inmorad with inpundiva,;
    	    nfinanciad with nfinanciad-inpundiva in (wtabin) && los replace de interes normal y punitorio=store 0.00 to infd,inpud 
    *endif    
endif
*-investigo exeso de tasa y redondeo *--usuario va a tener interes directo al resumen_*
*function fdift(ical,tsis,tbc,itope)
*---cparametros ical=Calculo, tsis=tasa aplicabel, tbc=asa ficta, itoper =minimo imponible
*store 0.00 to wincen,wpncen,widcen,wpdcen          && para controlar en caso mora u otro la dif int.
store 0.00 to wdifif,wdifip
if !wnmrad
	winfd=&wtabin..intd
	wipnd=&wtabin..inpund
	wdifif=fdift(winfd-widcen,ifid,cparametros.int_fid,cparametros.tope_dift/codd,piva)+widcen  && interes financiero
	wdifip=fdift(wipnd-iif(!winvn,wintivd,0.00)-wpdcen,ipud,cparametros.int_pudc,cparametros.tope_dift/codd,piva)+wpdcen  && interes punitorio
	if wdifif<0  &&corresponde rebajar por no poder desagregar y nada mas
		replace intd with intd-wdifif,nfinanciad with nfinanciad-wdifif in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
	endif
	if wdifip<0  &&corresponde rebajar por no poder desagregar y nada mas
		replace inpund with inpund-wdifip,nfinanciad with nfinanciad-wdifip in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 		
	endif
	if wdifif>0
		*--corresponde grabar movimiento de ajustes con correspondiente iva para restar
		if wnmrad  &&esta en mora dejo solo los intereses financieros correctos y cargo mora para grabar si cierra
		    replace intd with winfd-wdifif, inmorad with iif(!wcereo,wdifif,0),;
	   			nfinanciad with nfinanciad-wdifif in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
			wlbcmora=wdifif
		endif
		if !wnmra or (wnmra and !wcereo)
			wcpp=iif(wdifif<cparametros.tope_serv,"gastod","comprad")   &&que campo
			wtxt=iif(wdifif*codd<cparametros.tope_serv,"Llamadas Telefonicas ","Compras Rou") && que texto
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe,impiva,tasaiva,boleta);
				values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifif*codd,wdifif,"D",wcier_act,wcier_act,wcpp,"+","N",wtxt,wdifif,wdifif*(piva/100),piva,iif(!wnmra,"*","I"))
		endif		
		if winvn  and !wnmra && si esta procesando cierre va a cupusu con nproceso
			insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,tasaiva,boleta,comercio,paisco);
				values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifif*codd,wdifif,"D",wcier_act,wcier_act,wcpp,"+","N",0,0,wdifif*(piva/100),piva,"*",iif(wcpp="compran",wgrancr,0),2)	
			wgrancr=wgrancr+1		
		endif	
	endif
	if wdifip>0  
		if wnmra  &&esta en mora dejo solo los intereses financieros correctos y cargo mora para grabar si cierra

		    replace inpund with wipnd-wdifip, inmorad with inmorad+iif(!wcereo,wdifip,0),;
	   			nfinanciad with nfinanciad-wdifip in (wtabin) && son los replace de interes normal y punitorio=store 0.00 to infi,inpun 
			wlbcmora=wlbcmora+wdifip	
		endif
		if !wnmra or (wnmra and !wcereo)
			wcpp=iif(wdifip*codd<cparametros.tope_serv,"gastod","comprad")
			wtxt=iif(wdifip*codd<cparametros.tope_serv,"Llamadas Telefonicas ","Compras Rou") && que texto
			*--corresponde grabar movimiento de ajustes con correspondiente iva para restar
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta);
			values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifip*codd,wdifip,"D",wcier_act,wcier_act,wcpp,"+","N",wtxt,iif(!wnmra,"","I"))
		endif
		if winvn and !wnmrad  && si esta procesando cierre va a cupusu con nproceso
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,boleta,comercio,paisco);
					values(iif(wglobaiso="504736",nacion,wppai),-99,-99,"P",wgrp,wusu,0,"*",-99,wdifip*codd,wdifip,"D",wcier_act,wcier_act,"nada","+","N",0,0,wdifip*(piva/100),"-",iif(wcpp="compran",wgrancr,0),2)	
		endif	
	endif
endif
*--fin caso pdolares
*..11/10/07...FIN......      ANALINTER
********************************************************************
* FIN ZONA DE DETERMINACION DE INTERESES Y TASAS MORA--------------------
*
*******************************************************************
*  :CALCULO DE IVA DEL MES  -------------------------
if nacion<>2    && nacion=1 para ccdia
	*OJO ver intereses como van en el saldo total.
   * seria aconsejable saber si es cierre o no para sumar los punitorios---
   *  :PESOS
   if !ejecutable
   		*browse title "Que iva tiene"
   endif
   Iva_m =Iva_m+round((&wtabin..intn+&wtabin..inpunn+&wtabin..inaden+wcomin+wmovivp+iif(!winvn,inmorap,00))*(pIva/100),2) &&iva de mora si no inventario redondeo a 2 22/10/07
   Parfi  = &wtabin..saldo_a_n+&wtabin..financian &&financian tiene todo lo que se puede financiar
   pames=abs(&wtabin..pagon)+abs(&wtabin..ajupagon)
   Sdo_to =wppes+ Parfi +&wtabin..nfinancian+Iva_m - Pames-wajupn
   pami   =wppes+ Parfi +&wtabin..nfinancian+Iva_m - Pames-wajupn
   *  :Gasto resumen ya esa cargado en wtabin al ppio-----------
   *if sdo_to<>0.00
   if sdo_to>0.00
        sdo_to=sdo_to+&wtabin..servicio*(1+piva/100)
        pami=pami+&wtabin..servicio*(1+piva/100)
        iva_m =Iva_m+&wtabin..servicio*piva/100
   endif
*----lo hago para dolares --cobro (servicio+iva) si saldo $ es <> de cero 
   *  :DOLARES
   Iva_do =Iva_do+round((&wtabin..intd+&wtabin..inpund+&wtabin..inaded+wcomid+wmovivd+iif(!winvn,inmorad,00))*(pIva/100),2)
   Parfd  = &wtabin..saldo_a_d+&wtabin..financiad &&financian tiene todo lo que se puede financiar
   pamed=abs(&wtabin..pagod)+abs(&wtabin..ajupagod)
   *Sdo_to =wimpsell+wppes+ Parfi +cuomes+Plames+Iva_m+Comis+incuo+Ajumes+Admes+Gasto+Inadel+Insuna+insupe+infi+inpun+iif(invtario,0,wintivd+inpunivd) - (Pames+cancea)
   Sdo_td =wpdol+ Parfd +&wtabin..nfinanciad+Iva_do - Pamed-wajupd
   pamd   =wpdol+ Parfd +&wtabin..nfinanciad+Iva_do - Pamed-wajupd
   *  :Gasto resumen-----------
   if sdo_td>0.00.and.sdo_to=0.00
   	    wmonservi="D"                      &&cambio moneda del servicio
   		servd=round(&wtabin..servicio/codd,2)
        sdo_td =sdo_td+servd*(1+piva/100)
        pamd =pamd+servd*(1+piva/100)
        iva_do=iva_do+servd*piva/100
        replace &wtabin..servicio with servd 
   endif
   if sdo_td+sdo_to<=0.00
   		replace servicio with 0.00, selladon with 0.00 in (wtabin)      && si no hay saldo cerea costo resumen
		sele (wtabcu)
		dele all for motivo=-5 and cod2_cli=wusu
		if winvn
			sele vcupusunp
			loca for motivo=-5 and cod2_cli=wusu
			if !eof()
				if recno()>0
					delete
				else
					tablerevert(.f.,"vcupusunp")
				endif
			endif
		endif   
		sele (wtabin)		
   endif
   pami=iif(sdo_to=pami,pami,round(PAMI,0))
   pamd=iif(sdo_td=pamd,pamd,round(PAMD,0))
endif

*   :FIN NACION 1 ***************************************************************************
if nacion=2
   *  :PESOS
   Iva_m =Iva_m+(&wtabin..intn+&wtabin..inpunn+&wtabin..inaden+wcomin+wmovivp+iif(!winvn,inmorap,00))*(pIva/100)
   Parfi  = &wtabin..saldo_a_n+&wtabin..financian &&financian tiene todo lo que se puede financiar
   pames=abs(&wtabin..pagon)+abs(&wtabin..ajupagon)
   *Sdo_to =wimpsell+wppes+ Parfi +cuomes+Plames+Iva_m+Comis+incuo+Ajumes+Admes+Gasto+Inadel+Insuna+insupe+infi+inpun+iif(invtario,0,wintiva+inpuniva) - (Pames+cancea)
   *saldo_a_n+intn+inpunn+gaston+compran+cuotan+ajusten+adelanton+(innafn+insupn+inaden)+iva_n+selladon+ (pagon)
   Sdo_to =wppes+ Parfi +&wtabin..nfinancian+Iva_m - Pames-wajupn
   pami   =wppes+ Parfi +&wtabin..nfinancian+Iva_m - Pames-wajupn
   *  :Gasto resumen ya esa cargado en wtabin al ppio-----------
   if sdo_to<>0.00
        sdo_to=sdo_to+&wtabin..servicio*(1+piva/100)
        pami=pami+&wtabin..servicio*(1+piva/100)
        iva_m =Iva_m+&wtabin..servicio*piva/100
   endif
*!*	*-----------------------------lo hago para dolares-----------------------    
   *  :DOLARES
   Iva_do =Iva_do+(&wtabin..intd+&wtabin..inpund+&wtabin..inaded+wcomid+wmovivd+iif(!winvn,inmorad,00))*(pIva/100)
   Parfd  = &wtabin..saldo_a_d+&wtabin..financiad &&financian tiene todo lo que se puede financiar
   pamed=abs(&wtabin..pagod)+abs(&wtabin..ajupagod)
   *Sdo_to =wimpsell+wppes+ Parfi +cuomes+Plames+Iva_m+Comis+incuo+Ajumes+Admes+Gasto+Inadel+Insuna+insupe+infi+inpun+iif(invtario,0,wintiva+inpuniva) - (Pames+cancea)
   Sdo_td =wpdol+ Parfd +&wtabin..nfinanciad+Iva_do - Pamed-wajupd
   pamd   =wpdol+ Parfd +&wtabin..nfinanciad+Iva_do - Pamed-wajupd
   *  :Gasto resumen-----------
   if sdo_td<>0.00.and.sdo_to=0.00
   		servd=round(&wtabin..servicio/codd,2)
        sdo_td =sdo_td+servd*(1+piva/100)
        pamd =pamd+servd*(1+piva/100)
        iva_do=iva_do+servd*piva/100
        replace &wtabin..servicio with servd 
   endif
   if sdo_td+sdo_to<=0.00 and !wcereo
   		replace &wtabin..servicio with 0.00     && si no hay saldo cerea costo resumen
   endif
   if sdo_td+sdo_to>0.00
	   pami=iif(sdo_to=pami,pami,round(PAMI,0))
	   pamd=iif(sdo_td=pamd,pamd,round(PAMD,0))
   endif

endif
*---------------calculo de minimo ---lo hago para pesos-------------------------
*   :CALCULO DE MINIMOS POR FINANCIAR .............-----------------------------
*--no se afectan los calculos totales ??
*   :PESOS
pames=abs(&wtabin..pagon)+abs(&wtabin..ajupagon)
ctrol= Parfi-pames-wajupn
pfin=100-&wtabin..por_fin      && se financia la diferencia entre por_fin(le dije ej 60) y 100=40
if wglobaiso<>"504736"
	pami=sdo_to
endif
*  :02/09/04 analizado      -------
if pames >= (wminn * wpormin/100)-0.001 and pami>0  && si cubrio minimo, aplica financiacion.
    if ctrol+wintiva >0
        *pami =round((ctrol*pfin/100)+nfinancian+Iva_m+&wtabin..inaden+&wtabin..servicio+&wtabin..intn+&wtabin..inpunn,2) 
		ws=&wtabin..inaden &&NO NECESITO CARGAR VIENE EN NFINANCIAN winten/winted
		wa=&wtabin..servicio
        pami =round((ctrol*pfin/100)+nfinancian+Iva_m+&wtabin..servicio,2) 
        if wglobaiso<>"504736" and pami>0.00
        	pami=round(sdo_to*pfin/100,0)
        endif
    endif ctrol >0
    wcon_min=0
endif
if ( (Sdo_to-Pami) > LIMITE and (Sdo_to-abs(&wtabin..ajupagon)) > LIMITE )
    exeso = Sdo_to - Pami - LIMITE
    Pami =pami+exeso
endif

* AGREGO PARA CONTROL DE PANTALLA E INVENTARIOS-----------------
*-------------calculo de minimo -----lo hago para dolares-----------------------------
*   :DOLARES
pamed=abs(&wtabin..pagod)+abs(&wtabin..ajupagod)
ctrold= Parfd-pamed-wajupd
if wglobaiso<>"504736"
	pamd=sdo_td
endif
if pamed>=(wmind * wpormin/100)-0.001 
    if ctrold+wintivd >0
    	wsrv=iif(sdo_to>0.00,0.00,&wtabin..servicio)
       	*-Inaded esta cargado en nfinanciad	/winted pues el interes esta globalizado
        pamd =round((ctrold*pfin/100) +nfinanciad+Iva_do+wsrv+&wtabin..intd+&wtabin..inpund ,2)
        if wglobaiso<>"504736"
        	pamd=round(sdo_td*pfin/100,0)
        endif
    endif ctrol >0
    wcon_mid=0
endif
LIMITD=round(limite/codd,2)
IF ( (Sdo_td-Pamd) > LIMITD and (Sdo_td-abs(&wtabin..ajupagod)) > LIMITD )
     	exesd = Sdo_td - Pamd - LIMITD 
*---- para dolares tambien lo hago.........
     Pamd =pamd+exesd
ENDIF
*_-----------------------------------------------------------
*--------------------tratamiento de puntos en estado y cierre---
*   :FIN PROCESO DEL MES..........................
*-	 procesowcier_act<presenta				   -diferidos y fuera de periodo
*--inserto el SERVICIO Y EL IVA servicio y el iva.
if &wtabin..servicio>0
	wimpiva=&wtabin..servicio*piva/100
	insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe,impiva,tasaiva);
			values(iif(wglobaiso="504736",nacion,wppai),-6,-6,"P",wgrp,wusu,0,"G",-6,&wtabin..servicio,&wtabin..servicio/codd,wmonservi,wcier_act,wcier_act,iif(wmonservi="D","gastod","gaston"),"+","N","Costo del Resumen",&wtabin..servicio,wimpiva,piva)
			
	if winvn   && si esta procesando cierre va a cupusu con nproceso
		insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,iva,impiva,tasaiva);
			values(iif(wglobaiso="504736",nacion,wppai),-6,-6,"P",wgrp,wusu,0,"G",-6,&wtabin..servicio,&wtabin..servicio/codd,cparametros.monpais,wcier_act,wcier_act,"gaston","+","N",.t.,wimpiva,piva)
	endif	
	replace &wtabin..gaston with &wtabin..gaston+iif(wmonservi="D",0.00,&wtabin..servicio), ;
		&wtabin..gastod with &wtabin..gastod+iif(wmonservi="D",&wtabin..servicio,0.00) in (wtabin)

endif
*--fin ingreso de servicio
sele (wtabin)
if !ejecutable
	browse title "Mira el pago en Pagonf o Pagodf antes de procesarlo"
endif

sele (wtabcu)
scan all for presenta > wcier_act
	if &wtabcu..tipo_doc<>"P"
		cp=miacumu(alltrim(campo))
		if empty(cp)
			cp=alltrim(campo)
			if messagebox("Error en campo futuro no asignado "+cp+" Cancela",36,"Control")=6
				cancel
			endif	
		endif
	else
		*--trato los pagos futuros como dinero a favor en acumulador/ con pago se tomaria tambien--..	
		*cp=alltrim(campo)+"f"
		upa=iif(&wtabcu..tipo_doc="P" or alltrim(&wtabcu..autoriza)="CEREO",&wtabcu..presenta,upa) && avanza fecha de pago si es "P"
		cp="ajusten"+"f"
	endif
	replace impiva with iif(impiva>0,impiva,round(interes*piva/100,2)) in (wtabcu)	
	wmon=&wtabcu..moneda
	wcomision=&wtabcu..comision*iif(signo="-",-1,1)
	winteres=&wtabcu..interes*iif(signo="-",-1,1)
	*cval=(iif(moneda="D",impmone,importe)+iif(tipo_doc="P",interes,0.00))*iif(signo="-",-1,1)
	cval=(iif(&wtabcu..moneda="D",&wtabcu..impmone,&wtabcu..importe)+&wtabcu..interes+&wtabcu..comision+&wtabcu..impiva)*iif(&wtabcu..signo="-",-1,1)    
	replace &cp with &cp+cval in (wtabin)
	*acumulo interes y comision en la variable de inventario para simplificar.
	if ! &wtabcu..tipo_doc $ "P_J" or tipo_doc="J" and signo="+"
	*	replace inadenf with inadenf+iif(wmon="D",0,winteres), inadedf with inadedf+iif(wmon="D",winteres,0),;
	*		gastonf with gastonf+iif(wmon="D",0,wcomision),gastodf with gastodf+iif(wmon="D",wcomision,0) in (wtabin)
	endif
	if &wtabcu..tipo_doc $ "P_J" and signo="-"   
		*--para reflejar interes y comision como gasto			
	*	replace inaden with inaden+iif(wmon="D",0,winteres), inaded with inaded+iif(wmon="D",winteres,0),;
	*		gaston with gaston+iif(wmon="D",0,wcomision),gastod with gastod+iif(wmon="D",wcomision,0) in (wtabin)
	endif
endscan
sele (wtabcu)
*sum all iif(moneda<>"D",importe+interes,(impmone+interes)*wcotiza) for tipo_doc="H" to wprestamo  &&para sacar los prestamos del saldo disponible
*wprestamo=wprestamo*iif(nacion=2,0,1) && solo para Argentina
*--fin procesos
sele (wtabin)
if !ejecutable
	browse title "Mira el pago en Pagonf o Pagodf"
endif
*: reemplazos MONEDA NACIONAL con impuestos y acumulados no financiables
* saldo_a_n+intn+inpunn+gaston+compran+cuotan+ajusten+adelanton+(innafn+insupn+inaden)+iva_n+selladon+ (pagon)
*--si no es inventario se suman los ivad de intereses en mora.
replace iva_n with iva_m,nfinancian with nfinancian+iva_m, pago_minn with round(pami,iif(pami>0,0,2)),saldo_acn with iif(sdo_to>0.00,iif(round(pami,0)>sdo_to,round(sdo_to,0),sdo_to),sdo_to) + iif(!winvn,inmorap,00) in (wtabin) 
replace iva_d with iva_do,nfinanciad with nfinanciad+iva_do, pago_mind with pamd,saldo_acd with sdo_td+iif(!winvn,inmorad,00) in (wtabin) 
if cod1_cli=11 and wglobaiso="504736"
	replace pago_minn with saldo_acn
endif
*   : reemplazos MONEDA EXTRANJERA
*-----------------------------------------------------------------------------
*CONFORMA TOTALES EN INVENTARIO-- SITUACION FINAL DEL USUARIO, SCORING DEL USUARIO Y PLUS DE LIMITE-
*SCORING:
wsuple=0
wenmora=wnmra and wnmrad   && resultado de saber si esta o no en mora al cerrar para 
wescoring=0
*if !winvn 
wescoring=scoring(wrg,wuss,wingr,wxlc,iif(!winvn ,4,3),wcier_an+1,wcier_act,wminn*wpormin/100,wvtoan+wgrapag,winvn)
*endif
*--------------------------------------------------------------------------------------
*wplus=iif(wescoring>=cparametros.scori_plus,1+cparametros.por_plus/100,1)

wplus=1
*--nuevo codigo para el plus segun scoring---------------------------------------------
select * from \tarjeta\pluscor ;
	where wescoring=pluscor.scoring;
	into cursor cpluscor
if _tally>0
	wplus=1+(cpluscor.pluscor/100)
endif	
if used("cpluscor")
	use in cpluscor
endif
*---------------------------------------------------------------------------------------
*--inmorap esta incluido en saldo_acn si no es inventario

store 0.00 to wpauto,wdauto  &&voy a sumar importe autorizado/ call center  (hoy ivr)
sele (wtabin)
wtrau=wglobaiso+strtran(str(wgr,2)," ","0")+strtran(str(wus,5)," ","0")
*wxarchi="x"+sys(2015)
*----&&indica estando en este pais, que leo de autorizaciones
if nacion<>2  && nacion=1 para ccdia
	*--se refiere a los archivos locales de autorizacion.---
	if cparametros.sumautoriza
		wtrau=str(wgr,2)+str(wus,5)
		wctpeso=cparametros.cot_peso
		getautocall(wtrau,wctpeso)  && fncion que trae datos de autorizaciones
	endif
endif
if nacion=2 or nacion=1 and wglobaiso="504736"
	if empty(wbolsatodo)
		wxarchi="x"+quitaprefijodir(quitaextension(wtabin))
		wppai=elpais(wgrp) && pais del usuario que compra
		*--se refiere a los archivos locales de autorizacion en este caso IVR.---
		wctpeso=cparametros.cot_peso
		getautov9(wxarchi,nacion,wtrau,wctpeso,wppai)  && fncion que trae datos de autorizaciones
		wss=saldopos(wtrau,wxarchi)
		wpauto=wpauto+wss
	else
		sumauto(wbolsatodo,wtrau)
	endif
		
endif	

*(abs(wajupn)+abs(wajupd)*codd)   &&monto total de ajustes en el periodo

replace sdo_p_dol with saldo_acn+cuotanf+compranf+adelantonf+gastonf+ajustenf+pagonf+wpauto+;
	(saldo_acd+inmorad+cuotadf+compradf+adelantodf+gastodf+ajustedf+pagodf+wdauto)*codd,;
	inaden with inaden+wppes, inaded with inaded+wpdol,;
	compras with (adelanton+compran+cuotan+ppagon+(adelantod+comprad+cuotad+ppagod)*codd),;
	cuotas with (cuotanf+adelantonf+(adelantodf+cuotadf)*codd), scoring with wescoring, plus with wplus  in (wtabin)
*--saldos que pueden no existir en la vista	
replace sdo_cpra with  wlc*wplus-compras,	sdo_cuota with wlcc-cuotas,;
	sdo_dispo with wlcr*plus-sdo_p_dol+wprestamo in (wtabin)  &&saldo disponible de la cuenta.
if wglobaiso="504736" and winvn
	replace iplus with wplus , isdo_dispo with wlcr*plus-sdo_p_dol+wprestamo-wimpsell-&wtabin..servicio*(1+piva/100) in (wtabin)  &&saldo disponible de la cuenta.
endif	
	
*browse
if !ejecutable
	sele (wtabin)
	browse title "wtabin  "+str(wimpsell,8,2)+" sdo Dispo "+str(sdo_dispo,8,2)
endif
*--cargao tarjeta correcta
if wcdeta
	sele (wtabcu)
	GO TOP 
	SCAN ALL 
		wd=digiver(cod1_cli,cod2_cli,cod3_cli,cod4_cli,wglobaiso)
		wt=wglobaiso+strtran(str(cod1_cli,2)," ","0")+strtran(str(cod2_cli,5)," ","0")+;
			+str(cod3_cli,1)+str(cod4_cli,1)+wd
		REPLACE utarjeta with wt in (wtabcu)
	ENDSCAN 
		
ENDIF 
SELECT (wtabcu)
if empty(wbolsatodo)
	set order to
	index on utarjeta+dtos(presenta)+dtos(fecha) to (wcamino+wtabcu)+".idx"
ENDIF 
SELECT (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    upa=CTOD("")
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR COMMAND9")	   
ENDTRY 
RETURN (upa) &&retorna ultimo pago realizado por el usuario
***************************************************************************************************************
function sumauto(wxarchia,wtarjau)
local area
TRY 
area=select()
sele (wxarchia)
sum all impo to wpauto for moneda=032 and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0") and left(wtarjau,13)=left(tarjeta,13)
sum all impo to wdauto for moneda=840 and (alltrim(codauto)<>"I0" and alltrim(codauto)<>"M0") and left(wtarjau,13)=left(tarjeta,13)
go top in (wxarchia)
SELECT (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"
      wpauto=0
      wdauto=0
CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    upa=CTOD("")
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR SUMAUTO")	   
      wpauto=0
      wdauto=0
ENDTRY 

RETURN 
*browse title "Pesos "+str(wpesos,8,2)+"  Dolar "+str(wdolar,8,2)
*******************************************************************************************
function cargainter(wrg,wuss)
local area,wsumin

TRY 

area=select()
wsumin=0.00
*---formo intereses del usuario, en el pago se pide sin sumatoria para obtener registros afectados
TEXT TO csql NOSHOW 
select cod1_cli,cod2_cli,sum(interes_p) as interes_p, sum(interes_d) as interes_d
	from interes i
	where i.cod1_cli=?wgr AND i.cod2_cli=?wuss 
	group by i.cod1_cli,i.cod2_cli
ENDTEXT
res=SQLEXEC(n_handle,csql,"cinter")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error CARGAINTER")
   THROW "NORMAL"
ENDIF 

if !EOF("cinter")
	wintiva=cinter.interes_p
	wintivd=cinter.interes_d
	wsumin=wintiva+wintivd
ENDIF 
USE IN cinter 	
sele (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     wsumin=0.00
CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    wsumin=0.00
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR CARGAINTER")	   
ENDTRY 
return wsumin>0
************************************************************************************
function quitapromo(warchigra1,warchigra2,wf_ciean,wf_cieact,wgru,wtar)
*--warchigra1=archivo donde grabo para mostrar 
*--warchigra2=archivo de inventario si graba 
*--wfepro=fecha que le pongo al registro de la promo1 
*----------codigo
local pwupr,parea,pwgrp,pwusu,lsalida

lsalida=.t.

TRY 

parea=select()
*---para quitar las promo que no van a devolverse
if used("tquita")
	use in tquita
endif
wcm=wcamino+"tquita"
if !file(wcm+".dbf")
	wcm=wcamino+"tquita"
	sele (warchigra1)
	copy stru to (wcm)
	*create table (wcm) free (item n(10,0))
endif
if !file("\tarjeta\quitapromo.dbf")
	sele (warchigra1)
	copy stru to "\tarjeta\quitapromo.dbf"
endif
if !used("tquita")
	use (wcm) in 0 exclu
endif
if !used("quitapromo")
	*use \tarjeta\quitapromo.dbf in 0
	res=SQLEXEC(n_handle, "select * from quitapromo","quitapromo")
	IF res<=0
	ENDIF 
ENDIF 	

sele (warchigra1)

if !used("promos")	
	pwupr=.t.
	*use \tarjeta\promos in 0 
	res=SQLEXEC(n_handle, "select * from promos","promos")
	IF res<=0
	   THROW "NORMAL"
	ENDIF 
endif
sele promos
go top
do while !eof() 
	wmt=promos.motivo
	sele (warchigra1)
	scan all
		if motivo=wmt and between(presenta,wf_ciean,wf_cieact) and cod1_cli=wgru and cod2_cli=wtar
			scatter memvar
			insert into tquita from memvar
			delete
		endif
	endscan
	if !empty(warchigra2)
		*--quito de vcupusunp
		sele (warchigra2)
		scan all
			if motivo=wmt and between(presenta,wf_ciean,wf_cieact) and cod1_cli=wgru and cod2_cli=wtar
				if recno()<0
					tablerevert(.f.,"vcupusunp")
					* marcamos registro
					* replace
				else
					*delete in vcupusunp
					*-DELETE 
					*- queda para pensar 08/03/2013
					
				ENDIF 
			ENDIF 	
		ENDSCAN 
	ENDIF 
	sele promos
	skip
enddo
sele (parea)
if pwupr
	use in promos
endif
sele (parea)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
    lsalida=.f.
    
CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"

    lsalida=.f.
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR QUITAPROMO")	   
ENDTRY 
RETURN lsalida
**************************************************************************************
function promosa(warchigra1,warchigra2,wf_ciean,wf_cieact)
*--warchigra1=archivo donde grabo para mostrar 
*--warchigra2=archivo de inventario si graba 
*--wcod1,wusu= grupo y usuario 
*--warchidat=archivo de datos a procesar son solo del cliente 
*--wfepro=fecha que le pongo al registro de la promo1 
*----------codigo
local pwupr,parea,pwgrp,pwusu, lsalida

lsalida=.t.

TRY 

set date british
parea=select()
warchri=sys(2015)
warchidat=wcamino+warchri
sele (warchigra1)
copy to (warchidat)
use (warchidat) in 0 exclu
pwupr=.f.
*!*	If !ejecutable
*!*		set echo on
*!*		set step on
*!*	endif
*!*	if !ejecutable
*!*		sele (warchri)
*!*		browse
*!*	endif
if !used("promos")	
	pwupr=.t.
*	use \tarjeta\promos in 0 
	res=SQLEXEC(n_handle, "select * from promos","promos")
	IF res<=0
	   THROW "NORMAL"
	ENDIF 
ENDIF 
*"_"+alltrim(str(comercio,5))+"_" $ p.comercios and c.presenta between p.fechade and p.fechaa and
*laform=
*select p.motivo, summ(importe*iif(moneda<>"D",1,0)) as importe,summ(impmone*iif(moneda="D",1,0)) as impmone;
*---testeo si hay cupones con compras anteriores q pueden pertenecer a la promo
whayviejo=.f.
select * from (warchidat) ;
		where fecha <wf_ciean;
		into cursor ptest
if !EOF("ptest")
	whayviejo=.t.
ENDIF 
*-----------fin testeo
sele promos
go top
do while !eof() 
	if  between(wf_ciean,promos.fechade,promos.fechaa) or between(wf_cieact,promos.fechade,promos.fechaa) or whayviejo
		welfilp=alltrim(promos.laform)   &&Para poner una condicion entre los datos del periodo de la promo
		wlisc=alltrim(promos.comercios)
    	wlisdb=alltrim(promos.codmoti)
		wfechade=promos.fechade
		wfechaa=promos.fechaa
		wpctn=promos.porcen/100
		wtopep=promos.tope
		wmotip=promos.motivo
		wdetpr=promos.detalle
		wpaisp=promos.paisp
		wcmini=promos.comini
		wcmfin=promos.comfin
		store 0 to wtvarp
		
	    if "descuento" $ wlisc
	    	select * from (warchidat) ;
	         where "_"+alltrim(str(motivo,3))+"_" $ wlisdb  and nacion=2 and tipo_doc="D"  and presenta<=wf_cieact and fecha between wfechade and wfechaa ;
			into cursor wcprm
			
	    	wtopep=iif(empty(promos.laform),promos.tope,eval(promos.laform))
	    	welfilp=""
	    	*deberia tener un campo, tope formula para no trancar este campo.
			*select * from (warchidat) ;
	        *where  motivo=815 and nacion=2 and tipo_doc="D"  and presenta<=wf_cieact and fecha between wfechade and wfechaa ;
			*into cursor wcprm
			*wtopep=_tally*250
		
	    else
	    	*---where "_"+alltrim(str(comercio,5))+"_" $ wlisc  and fecha<=wf_cieact and fecha between wfechade and wfechaa and cuota<=1 and iif(nacion<>wpaisp, "1/" $ cuotas and len(strtran(cuotas," ",""))=4 or tipo_doc="C" ,.t.) and paisco=wpaisp 
			select w.*,upper(cdow(fecha)) as diapr from (warchidat) w;
	         where ! tipo_doc $ "A_D_G_H_J_M_P_Q_S_T_Z_K_" and ("_"+alltrim(str(comercio,5))+"_" $ wlisc or between(comercio,wcmini,wcmfin)) and presenta<=wf_cieact and between(fecha,wfechade,wfechaa) and cuota<=1 and iif(nacion<>wpaisp, "1/" $ cuotas and len(strtran(cuotas," ",""))<=4 or tipo_doc="C" ,.t.) and paisco=wpaisp;
			into cursor wcprm
		endif
		
		*--sacar el proximo
		if _tally>0	
			wrestiva=0.00
			if wpaisp=2  and !("descuento" $ wlisc)   && debo descontar de los resto el iva devuelto
				select w.* from (warchidat) w;
				where tipo_doc<>"J" and "_"+alltrim(str(comercio,5))+"_" $ wlisc and fecha between wfechade and wfechaa and motivo=-31 into cursor wcdiva
				if _tally>0
					sele wcdiva
					sum all importe to wrestiva 
				endif
				use in wcdiva
			endif
			*filtra la condicion de la promo para hacer el registro resultado va en campo de la promo
			wtvarp=0.00
			sele wcprm
			go top
			scan all
				if !ejecutable
					*browse title "Datos de la promo antes"
				endif
				pwgrp=cod1_cli
				pwusu=cod2_cli
				wpasapromo=.t.
				if !empty(welfilp)	
					wpasapromo=eval(welfilp)	
				endif
				if !wpasapromo
					sele wcprm
					loop
				endif
				if !ejecutable
					*browse title "Datos de la promo "
				endif
				varp=importe*iif(cuota<1,1,val(righ(cuotas,len(cuotas)-at("/",cuotas))))*iif(moneda<>"D",1,0)
				vard=impmone*iif(cuota<1,1,val(righ(cuotas,len(cuotas)-at("/",cuotas)))) *iif(moneda="D",1,0)
				
				wtvarp=wtvarp+round(varp*wpctn,2)+round(vard*wcotiza*wpctn,2)  	&&acumula total devuelto
				wsaldo=round(varp*wpctn,2)+round(vard*wcotiza*wpctn,2)			&&acumula parcial devuelto
				if wtvarp+wrestiva>wtopep
					wtvaran=wtvarp
					*wtvarp=wtopep-wrestiva
					wsaldo=wsaldo-(wtvaran-wtopep-wrestiva)
				endif
				*----consigo cantidad de cuotas
*				scan all for wtvarp>0.00 
				if wsaldo>0
					wcanc=iif(empty(cuotas),1,val(substr(cuotas,at("/",cuotas)+1,2)))
					wprinic=presenta		&& debo ajustar presentacion cta 2->
					wpori=presenta
					for i=1 to wcanc
						scatter  memvar
						m.tipo_doc="J"
						m.campo="ajusten"					
						m.signo="-"
						m.nrocupon=wcprm.nrocupon
						m.financia="N"
						m.motivo=wmotip
						m.cuotas=str(i,2)+"/"+str(wcanc,2)
						m.cuotas=""
						m.cuota=i
						m.cuota=0
						wmaspre=i-1
						fechacu=proxfech(wpori,wmaspre)
	    				fechacu=cronogra(cod1_cli,wpori,fechacu,wmaspre)
	    				*m.presenta=presenta+30*(i-1)
						m.presenta=fechacu
						m.importe=round(importe*wpctn,2)
						m.impmone=round(impmone*wpctn,2)
						*m.detalle=promos.detalle
						m.detalle=wdetpr
						m.item=''
						m.comercio=0
						m.paisco=wpaisp
						m.interes=0.00
						m.impiva=0.00
						if wsaldo<m.importe	
							wimpmone=m.impmone*wsaldo/m.importe	&& relacion proporcional			
							m.importe=wsaldo
							m.impmone=wimpmone
						endif
						if m.importe>0
							insert into (warchigra1) from memvar
							if !empty(warchigra2)   && si esta procesando cierre va a cupusu con nproceso
								insert into vcupusunp from memvar
							endif
						endif
						wsaldo=wsaldo-m.importe
					endfor
				*endscan
				endif
				sele wcprm
			endscan	
		endif			
	endif
	sele promos	
	skip
enddo
if !ejecutable
	sele (warchigra1)
	*browse title "resultado promos"
endif
if pwupr
	use in promos
endif
sele (warchri)
use 
erase (warchidat+".dbf")
sele (parea)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lsalida=.f.

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR PROMOSA")	   
    lsalida = .f.
ENDTRY 
RETURN lsalida
**************************************************************************************
FUNCTION intdias(fe1,fe2,monto,intr)
local calcu
****  la funcion es llamada con el importe y calcula intereses en dias
calcu =0.00
if monto<0 or fe2<=fe1
	RETURN calcu
ENDIF 

di=fe2-fe1
calcu=ROUND((MONTO*DI*INTR*12/36000),2)
* wait window "Paga",fe1,"Vto",fe2,"monto ",monto," dias ",di," tasa ",int,"total ",calcu

return calcu
*******************************************************************************************
function fdift(ical,tsis,tbc,itope,tpiv)
*---parametros ical=Calculo, tsis=tasa aplicabel, tbc=asa ficta, itoper =minimo imponible
local iok,idif
store 0.00 to iok,idif,tdif
if ical=0 or tsis<=tbc or nacion=2 or wglobaiso<>"504736"
	return (idif)
endif
iok=round(ical*tbc/tsis,2)
idif=ical-iok
*--se controla que el importe no sea menor igual al tope
*--de ser asi manda redondear interes-idif y ya.
*(1+(pIva/100)) debo agregar el iva  en la funcion
if idif*(1+(tpiv/100))<=itope
	idif=-1*idif
endif
return (idif)
*********************************************************************************
function arreglo_fdift(wtabin,wtabcu,wf_ciean,wf_cieact)
*--tiene como cometido arreglar los datos que se ven en cabezales de inventario
local area,winfnc,wivfnc,winpnc,wivpnc,winfdc,wivfdc,winpdc,wivpdc,pivaar,wupla,tiraplan,wivaplan,wivaplad,wintecuon,wintecuod,wlatar,lsalida

TRY 

area=select()
if nacion=2 or wglobaiso<>"504736"
	THROW "NORMAL"
ENDIF 
store 0.00 to winfnc,wivfnc,winpnc,wivpnc,winfdc,wivfdc,winpdc,wivpdc,wivpan,wivpad,pivaar,wivaplan,wivaplad
if !ejecutable
	set echo on
	set step on
endif
*--variable que filtra planes de movimientos.----
wupla=.f.
if !used("planes")
    res=SQLEXEC(n_handle,"select * from planes","planes")
    IF res<=0
       THROW "NORMAL"
    ENDIF 
	*use tarjeta!planes in 0
	wupla=.t.
ENDIF
tiraplan=""
sele planes
scan all for origen="PL"
	tiraplan=tiraplan+tipo_doc+"_"
endscan

*--la fecha debo manejarla para los casos en que tengo datos superpuestos, como ser historia de inventarios----
if !ejecutable
	sele (wtabin)
	browse title "Cabezal antes"
	sele (wtabcu)
	browse title "reglones antes"
endif
sele (wtabin)
*go top
wnpwtb=nproceso
*--repreparo variables para agregar registros de pagos si lo necesito
wgrp=cod1_cli
wusu=cod2_cli
wcier_act=cierre
wcier_act=cierre
pivaar=parametros.iva
*-------------------------------------------------------------------------
sele (wtabcu)
wlatar=utarjeta
sum all debe, impiva to winfnc,wivfnc for tipo_doc="*" and "*" $ boleta and moneda<>"D" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all debe, impiva to winpnc,wivpnc for tipo_doc $ "*_G" and "-" $ boleta and moneda<>"D" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all debe, impiva to winmon,wivmon for tipo_doc="*" and "I" $ boleta and moneda<>"D" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all interes to wdifif for tipo_doc="P" and moneda<>"P" and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all interes to wintecuon  for (tipo_doc+"_" $ tiraplan)  and moneda<>"D" and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
*-------------------------
sum all debe, impiva to winfdc,wivfdc for tipo_doc="*" and "*" $ boleta and moneda="D" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all debe, impiva to winpdc,wivpdc for tipo_doc $ "*_G" and "-" $ boleta and moneda="D" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all debe, impiva to winmod,wivmod for tipo_doc="*" and "I" $ boleta and moneda="D" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all interes to wdifid for tipo_doc="P" and moneda="D" and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
sum all interes to wintecuod  for (tipo_doc+"_" $ tiraplan)  and moneda="D" and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)

replace all debe with debe+ impiva for tipo_doc="*" and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso) in (wtabcu) 
replace all impiva with 0 for (tipo_doc="*" or tipo_doc="G" and "-" $ boleta) and (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso) in (wtabcu)
replace all interes with 0  for tipo_doc="P" and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
*--interes en planes-
wivaplan=round(wintecuon*pivaar/100,2)
wivaplad=round(wintecuod*pivaar/100,2)
replace all debe with importe+interes+round(interes*pivaar/100,2) for (tipo_doc+"_" $ tiraplan)  and moneda<>"D" and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
replace all debe with importe+interes+round(interes*pivaar/100,2) for (tipo_doc+"_" $ tiraplan)  and moneda="D"  and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso) &&interes esta incluido en la cuota
replac all interes with 0.00 for (tipo_doc+"_" $ tiraplan)    and  (between(presenta,wf_ciean,wf_cieact) or wnpwtb=nproceso)
*--intereses en pagos.

if wdifif>0
	wivpan=wdifif-round(wdifif/(1+pivaar/100),2)
	wcpp=iif(wdifif<parametros.tope_serv,"gaston","compran")   &&que campo
	wtxt=iif(wdifif<parametros.tope_serv,"Llamadas Telefonicas ","Compras Rou") && que texto
	insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe,impiva,tasaiva, nproceso,utarjeta);
		values(nacion,-99,-99,"P",wgrp,wusu,0,"*",-99,wdifif,wdifif/parametros.cot_peso,"A",wcier_act,wcier_act,wcpp,"+","N",wtxt,wdifif,0,0,wnpwtb,wlatar)
endif
*atar=str(cod1_cli,2)+str(cod2_cli,5)+str(cod3_cli,1)
if wdifid>0
	wivpad=wdifid-round(wdifid/(1+pivaar/100),2)
	wcpp=iif(wdifid<parametros.tope_serv,"gaston","compran")   &&que campo
	wtxt=iif(wdifid*codd<parametros.tope_serv,"Llamadas Telefonicas ","Compras Rou") && que texto
	insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,debe,impiva,tasaiva,nproceso,utarjeta);
		values(nacion,-99,-99,"P",wgrp,wusu,0,"*",-99,wdifid*parametros.cot_peso,wdifid,"D",wcier_act,wcier_act,wcpp,"+","N",wtxt,wdifid,0,0,wnpwtb,wlatar)
endif
*--quito de intereses
replace intn with intn-winfnc, iva_n with iva_n - wivfnc-wivpnc-wivmon-wivpan-wivaplan, inpunn  with inpunn-winpnc,;
	intd with intd-winfdc, iva_d with iva_d-wivfdc-wivpdc-wivmod-wivpad-wivaplad, inpund  with inpund-winpdc,;
	inmorap with inmorap-winmon, inmorad with inmorad-winmod, inaden with inaden-(wdifif-wivpan)-wintecuon , inaded with inaded-(wdifid-wivpad)-wintecuod  in (wtabin)
*sumo a compras	
replace compran with compran +	winfnc+wivfnc+wivpnc+wivmon+wivaplan + winpnc+winmon + wdifif +wintecuon;
		comprad with comprad +  winfdc+wivfdc+wivfdc+wivpdc+wivmod+wivpad+wivaplad + winpdc+winmod + wdifid +wintecuod in (wtabin)
if wupla
	use in planes
endif	
if !ejecutable
	sele (wtabin)
	browse title "Cabezal despues"
	sele (wtabcu)
	browse title "reglones despues"
endif
sele (area)	

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lsalida=.f.

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR PROMOSA")	   
    lsalida = .f.
ENDTRY 
RETURN lsalida
*******************************************************************************************************************************
function scoring(wgrupo,wcod2,winnx,wll,wcper,wfc1,wfc2,wmtpm,wvtact,winvn)
local area,pmt,lc,perf,anti,ref,wgracia,wesco,wcanp,wccompra,wlimcomp
TRY 

*wfc1=fecha del cierre para el inventario actual
*wfc2=fehca del cierre para elinvenario actual
*wmtpm=pago minimo del cierre actual inventario
if !ejecutable
	set echo on
	set step on
endif
if nacion=1 and wgrupo=11
	THROW "NORMAL"
endif
area=select()
store 0 to pmt,lc,ref,perf,anti,wgracia,wcanp
wcanpmy=StR(wcper)
wcanp=wcper
wogrup=wgrupo
wgracia=graciascoring(wgrupo)
*--------------------averiguo si tiene - de 1 anio --------
if winnx+365<=date()
	anti=1  &&  punto de antiguedad  gana por ser mas o igual al año
endif
wlimcomp=wll
res=SQLEXEC(n_handle,"select * from cierres where grupo=?wogrup order by cierre_act desc limit "+WCANPMY,"cc")
IF res<=0
   THROW "NORMAL"
ENDIF 
*wcanp=4
sele cc
scan all
	store 0 to pmp
	wfechai=cc.cierre_an
	wfechaf=cc.cierre_act
	pmp=0
	wfechaiy=_mysqldate(cc.cierre_an)
	wfechafy=_mysqldate(cc.cierre_act)
	sqltxt="select cod1_cli,cod2_cli,cierre,paga_minn,paga_mind from inventario where cierre=?wfechaf and cod1_cli=?wgrupo and cod2_cli=?wcod2"
	res=SQLEXEC(n_handle,sqltxt,"weli")
	IF res<=0
	   THROW "NORMAL"
	ENDIF 
	if !EOF("weli")
	    pmp=(weli.paga_minn+weli.paga_mind)*0.8
    ELSE     
	    LOOP 
	ENDIF 	
	supa=1
	***********************************************************************************
	wcl1=str(nacion,2)+str(wgrupo,2)+str(wcod2,5)+dtos(wfechai+1)
	wcl2=str(nacion,2)+str(wgrupo,2)+str(wcod2,5)+dtos(wfechaf)
	wrd="presenta"
	wfechamy=wfechai+1

	TEXT TO csql NOSHOW 	
	    select c.*,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,false as dfuturo,0 as extrapunto 
		from cupusu c 
		where ppais=?nacion and cod1_cli=?wgrupo and cod2_cli=?wcod2 and (presenta between ?wfechamy and ?wfechaf) and tipo_doc='P'	
		order by presenta
	ENDTEXT 
	res=SQLEXEC(n_handle,csql,"wcupu")
	IF res<=0
	   AERROR(verr)
	   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en SCORING")
	   THROW "NORMAL"
	ENDIF
	***********************************************************************************
	sele wcupu
	scan all for between(presenta,wfechai,wfechaf)
		pmp =pmp-iif(tipo_doc="P" and moneda<>"D",importe,impmone)	
		if supa=1 and tipo_doc="P" and pmp<=0 and presenta<=cc.vto_an+wgracia and wcanp<=wcper
			pmt=pmt+1
		    supa=0
		endif
	endscan	
	sele cc
endscan	 
if winvn	
	***********************************************************************************
	wcl1=str(nacion,2)+str(wgrupo,2)+str(wcod2,5)+dtos(wfc1)
	wcl2=str(nacion,2)+str(wgrupo,2)+str(wcod2,5)+dtos(wfc2)
	wrd="presenta"
	wcier_act=wfc2
	TEXT TO csql NOSHOW 	
	select c.*,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,presenta>?wcier_act as dfuturo,0 as extrapunto
		from cupusu c
		where ppais=?welpa and cod1_cli=?wgrupo and cod2_cli=?wcod2 and (presenta between ?wfc1 and ?wfc2)
		order by presenta
	ENDTEXT 
	res=SQLEXEC(n_handle,csql,"wcupua")
	IF res<=0
	   AERROR(verr)
	   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en SCORING")
	   THROW "NORMAL"
	ENDIF

	***********************************************************************************
	if !EOF("wcupua")
		supa=1
		pmp=wmtpm
		sele wcupua
		scan all for between(presenta,wfc1,wfc2)
			pmp =pmp-iif(tipo_doc="P" and moneda<>"D",importe,impmone)	
			if supa=1 and tipo_doc="P" and pmp<=0 and presenta<=wvtact
				pmt=pmt+1
			    supa=0
			endif
		endscan	
	endif
endif
sele(area)
*wesco=anti+iif(pmt>4,4,pmt)
wesco=anti+pmt

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lsalida=.f.
     wesco=0

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    wesco=0
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR SCORING")	   
    lsalida = .f.
    wesco=0
ENDTRY 
return wesco
**************************************************************************************
function getautocall(wtarjau,wcota)
local wtodos,area,wfileaug

TRY 

MESSAGEBOX("ATENCION: Entro a GETAUTOCALL - TERMINA RUTINA",48,"ADVERTENCIA")
THROW "NORMAL"

res=SQLEXEC(n_handle,"select * from parametros","curparametros")
IF res<=0
   THROW "NORMAL"
ENDIF 

if !curparametros.sumautoriza
	THROW "NORMAL"
ENDIF 

area=select()
sele vautusu
wfechau1=ctod("01/01/2000")
wfechau2=date()

res=SQLEXEC(n_handle,"select * from autoriza where cod1_cli=?  and cod2_cli=? and between(fecha,?wfechau1,?wfechau2) and nproceso=0","wcauto")
IF res<=0
   THROW "NORMAL"
ENDIF 

if !EOF("wcauto")
	sele wcauto
	sum all importe*iif(moneda=mpais,1,wcota) to wpauto for moneda=mpais &&si es moneda uruguaya
	sum all importe to wdauto for moneda="D" 
endif	 
if !ejecutable
	browse title "Autorizaciones"
ENDIF 
sele (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lsalida=.f.

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR GETAUTOCALL")	   
    lsalida = .f.
ENDTRY 
RETURN  lsalida
*browse title "Pesos "+str(wpesos,8,2)+"  Dolar "+str(wdolar,8,2)
*******************************************************************************************
function saldopos(wtar,wxarch)
local wft,wtft,wmontot,area,wlamo
TRY 

area=select()
store 0.00 to wmontop,wmontod
wlamo=""
wmontot=0
if !ejecutable
	set echo on
	set step on
endif
if mywhnd<=0
	wautorizar=91   && salida de autorizacion error de comunicacion
	THROW "NORMAL"
ENDIF
IF USED("numtar")
   USE IN numtar
ENDIF 
if used("ctran_sd")
	use in ctran_sd 
endif
wlsucc=dbusar(mywhnd,"tarjetapos")
if !wlsucc
	wtexto_expli="Error al conectar bd"
	wautorizar=91   && salida de autorizacion error de comunicacion
	sele (area)
	THROW "NORMAL"
ENDIF 
wres2=-1
if nacion=1
	wlamo="032"
	if empty(wtar)
		wsqlca=' select rrn_indice,rrn37,cod_proceso3 as cd_pc3, comercio42,tipo_mensaje as tip_m, terminal41,num_lote as lotep, monto4,moneda49,num_audit11 as audit, num_tarj2, track2_35, facturada, '+;
			'proceso, fecha_hora7 as fh, inf_ticket62 as tick,factura46 from transacciones where facturada="N" and num_lote="   "  and proceso=0  '+;
			' order by fecha_hora7 '
	else		
	    wsqlca=' select rrn_indice,rrn37,cod_proceso3 as cd_pc3, comercio42,tipo_mensaje as tip_m, terminal41,num_lote as lotep, monto4,moneda49,num_audit11 as audit, num_tarj2, track2_35, facturada, '+;
			'proceso, fecha_hora7 as fh, inf_ticket62 as tick,factura46 from transacciones where facturada="N" and num_lote="   "  and proceso=0 and ( &wtar=substr(track2_35,1,13) or &wtar=substr(num_tarj2,1,13) ) '+;
			' order by fecha_hora7 '
	endif	
	wres1=sqlexec(mywhnd,wsqlca,"numtar")
	IF wres1<0
	   AERROR(verr)
	   MESSAGEBOX("ATENCION ERROR Al EJECUTAR SENTENCIA SQL EN TARJETAPOS",16,"TOME NOTA")
	   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"ERROR SQL")
       THROW "NORMAL"
	ENDIF 
ENDIF 
if nacion=2
	wlamo="858"
	if empty(wtar)
		wsqlca=' select rrn_indice,rrn37,cod_proceso3 as cd_pc3, comercio42,tipo_mensaje as tip_m,terminal41, num_lote as lotep, monto4,moneda49,autorizacion as audit, num_tarj2, track2_35, facturada, '+;
		'proceso, fecha_hora7 as fh, inf_ticket62 as tick,factura46 from transacciones where  facturada="N"  '+;
		' order by fecha_hora7 '
	else
		wsqlca=' select rrn_indice,rrn37,cod_proceso3 as cd_pc3, comercio42,tipo_mensaje as tip_m,terminal41, num_lote as lotep, monto4,moneda49,autorizacion as audit, num_tarj2, track2_35, facturada, '+;
		'proceso, fecha_hora7 as fh, inf_ticket62 as tick,factura46 from transacciones where  facturada="N" and  ( &wtar=substr(track2_35,1,13) or &wtar=substr(num_tarj2,1,13))  '+;
		' order by fecha_hora7 '
	endif
	wres1=sqlexec(mywhnd,wsqlca,"numtar")
	IF wres1<0
	   AERROR(verr)
	   MESSAGEBOX("ATENCION ERROR Al EJECUTAR SENTENCIA SQL EN TARJETAPOS",16,"TOME NOTA")
	   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"ERROR SQL")
       THROW "NORMAL"
	ENDIF 
ENDIF 
if wres1<=0 and wres2<=0
	wmontot=0
	sele (area)	
	THROW "NORMAL"
else	
	if wres1>0
		sele numtar 
		copy to wcamino+"ctran_sd"
		if !used("ctran_sd")
			use wcamino+"ctran_sd" in 0
		endif
		use in numtar
	endif
	if wres2>0
		sele track2
		if !used("ctran_sd")
			copy to wcamino+"ctran_sd"
			if !used("ctran_sd")
				use wcamino+"ctran_sd" in 0
			endif
		else
			copy to wcamino+"track2"
			sele ctran_sd
			append from wcamino+"track2"
		endif
		use in track2
	endif
endif	

if !ejecutable
	sele ctran_sd
	*browse title "saldo del cliente antes de Enlazar "+wtar
endif


wft=sys(2015)
wtft=wcamino+wft
sele ctran_sd
copy to (wtft) for cd_pc3="020000" or tip_m="0400"
if !used(wft)	
	use (wtft) in 0
endif
if nacion=1
	sele (wft)
	scan all
		*--voy por los codigos q estan involucrados en la anulacion.
		wrrn37=rrn37
		*wtk=tick
		wnau=audit
		wtarj=iif(!empty(num_tarj2),num_tarj2,left(track2_35,16))
		wmnt=alltrim(monto4)
		wcomer=comercio42
		sele ctran_sd
		scan all
			if empty(num_tarj2)
				replace num_tarj2 with left(track2_35,16) in ctran_sd
			endif
			if alltrim(wtarj)=alltrim(num_tarj2) and (wnau=audit or wrrn37=rrn37) and wmnt=alltrim(monto4) and wcomer=comercio42 and facturada<>"S"
					replace facturada with "S" in ctran_sd
			endif
		endscan
		sele (wft)
	endscan

endif
if nacion=2
	sele (wft)
	scan all
		*--voy por los codigos q estan involucrados en la anulacion.
		*wrrn37=rrn37
		wtk=tick
		wnau=audit
		wtarj=iif(!empty(num_tarj2),num_tarj2,left(track2_35,16))
		wmnt=alltrim(monto4)
		wcomer=comercio42
		sele ctran_sd
		scan all
			if empty(num_tarj2)
				replace num_tarj2 with left(track2_35,16) in ctran_sd
			endif
			if alltrim(wtarj)=alltrim(num_tarj2) and (wtk=tick or wnau=audit) and wmnt=alltrim(monto4) and wcomer=comercio42
					replace facturada with "S" in ctran_sd
			endif
		endscan
		sele (wft)
	endscan
endif
*---reviso si fue un cierre sin exito.La transaccion no va mas. SOLO ROU
sele ctran_sd
scan all for proceso<>0 and nacion=2
	wnpr=alltrim(str(proceso))
	wsqlca='select * from trancierre where &wnpr=proceso '
	wresf=sqlexec(mywhnd,wsqlca,"trafac")
	if wresf>0
		sele trafac
		loca for proceso=val(wnpr)
		if ok_cierre="N"
			replace facturada with "S" in ctran_sd
			*--la bajo de la db
			wnin=alltrim(str(ctran_sd.rrn_indice))
			wsqlcup='update transacciones set facturada="S"  where &wnin=rrn_indice'
			wrp=sqlexec(mywhnd,wsqlcup)
		endif
	endif
	sele ctran_sd
endscan
if used("trafac")
	use in trafac
endif
sele ctran_sd
go top
if !ejecutable
	*browse title "sin facturar"
endif
*----combustible  en variables globales wmontop,wmontod
sum all val(monto4)/100 to wmontop for moneda49="858" and facturada="N" and val(factura46)>0 and left(factura46,1)="2"
sum all val(monto4)/100 to wmontod for moneda49<>"858" and facturada="N" and val(factura46)>0 and left(factura46,1)="2"

*---------------
sum all val(monto4)/100 * iif(moneda49=wlamo,1,wcotiza) to wmontot for facturada="N"
if reccoun("ctran_sd")>0
	wfileaug=alltrim(wcamino+wxarch)
	if !file(wfileaug+".dbf")
		create table (wfileaug) free (paisco n(2,0),lote n(4,0), tarjeta c(16,0),comercio c(15,0),;
			fecha d(8), impo n(10,2),ncupon n(8,0), nfina c(1,0), cuotas n(3,0),terminal n(10,0) ,moneda n(3,0), codauto c(10,0),hora c(8,0),fepago d(8),cambio n(6,2))
	endif		
	if !used(wxarch)
		use (wfileaug) in 0 exclu
	endif
	sele ctran_sd
	scan all for facturada="N"
		c1=nacion
		c2=val(lotep)
		c3=num_tarj2
		c4=left(comercio42,15)
		c5=ctod(left(ttoc(fh),8))
		c6=val(monto4)/100
		c7=val(moneda49)
		c8=audit
		c9=val(tick)
		cr=rrn_indice
		if type("cr")="C"
			cr=val(cr)
		endif
		insert into (wxarch) (paisco,lote,tarjeta,comercio,fecha , impo, moneda ,codauto,ncupon,nfina,terminal) ;
			values(nacion,c2,c3,c4,c5,c6,c7,c8,c9,"P",cr)
			
			*ncupon , nfina, cuotas,terminal
	endscan
endif
use in ctran_sd
sele (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     lsalida=.f.
     wmontot=0

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR SALDOPOS")	   
    lsalida = .f.
    wmontot=0
ENDTRY 
return wmontot
******************************************************************************************************
function cerrar(wgr)
local area,wucie,wucro, curci
curci=""

TRY 

if parameters()<>1
	messagebox("Cerrar Necesita Parametros <Grupo> -> <Cursor>",48,"Control")
	THROW "NORMAL"
endif
if !ejecutable
*	set echo on
*	set step on
endif
area=select()
store .f. to wucie,wucro
curci=sys(2015)
if used(curci)
	use in (curci)
endif
wf=wcamino+curci
create cursor (curci) (cierre_an d(8,0),vto_an d(8,0),cierre_cr d(8,0),vto_cr d(8,0))
*!*	if !used(curci)
*!*		use (wf) in 0
*!*	endif

sele (curci)
append blank
if !used("cierres")
	wucie=.t.
	res=SQLEXEC(n_handle,"select * from cierres","cierres")
	IF res<=0
	   THROW "NORMAL" 
	ENDIF 
ENDIF 
if !used("cronograma")
	wucro=.t.
	res=SQLEXEC(n_handle,"select * from cronograma","cronograma")
	IF res<=0
	   THROW "NORMAL" 
	ENDIF 
endif
*--ingreso un grupo y tengo un cursor con variables de cierre y cronograma del proximo----
wci=date()
wdia=33
res=SQLEXEC(n_handle,"select * from cierres where grupo=?wgr order by cierre_act desc limit 1","curcerrar")
IF res<=0
   THROW "NORMAL" 
ENDIF 
if !EOF("curcerrar")
	replace cierre_an with curcerrar.cierre_act, vto_an with curcerrar.vto_act in (curci)
	wci=curcerrar.cierre_act+1
	if !ejecutable
		sele curcerrar 
		*browse title "curcerrar cierres del grupo"
	endif
else
	wdia=30	
ENDIF
wcifin=wci+wdia
res=SQLEXEC(n_handle,"select * from cronograma where grupo=?wgr and (cierre between ?wci and ?wcifin) order by cierre desc limit 1","curcronog")
IF res<=0
   THROW "NORMAL" 
ENDIF 
if !EOF("curcronog")
	replace cierre_cr with iif(curcronog.cierre>=date(),curcronog.cierre,date()), vto_cr with iif(curcronog.vto>=date(),curcronog.vto,date()) in (curci)
	if !ejecutable
		sele curcronog
		*browse title "curcronog cronograma del grupo"
	endif
*	replace cierre_cr with curcronog.cierre, vto_cr with curcronog.vto in (curci)
endif
if wucie
	use in cierres
endif
if wucro
	use in cronograma
endif
use in curcerrar
use in curcronog
*--- protejo fecha de cierre anterior en caso de un grupo nuevo.
sele (curci)
if empty(cierre_an)
	replace cierre_an with cierre_cr-29, vto_an with vto_cr-29 in (curci)
endif	
sele (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"
     curci=""

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR CERRAR")	   
    curci=""
    
ENDTRY 
return curci
******************************************************************************
function proxcie(gru,ini,j)
local area,dife,fere,cc1,wucro

TRY 

set dele on
set date british
area=select()
wucro=.f.
dife=0
fere=ini
if j=0
	return (fere)
endif
if !used("cronograma")
	res=SQLEXEC(n_handle,"select * from cronograma where grupo=?gru","cronograma")
	IF res<=0
	   THROW "NORMAL" 
	ENDIF 
	wucro=.t.
ELSE
   wgrupo=gru
   res=REQUERY("cronograma")
   IF res<=0
      THROW "NORMAL"
   ENDIF  	
ENDIF
select top j * from  cronograma;
	where cierre>ini;
	order by cierre;
	into cursor ccrr
if wucro
	use in cronograma 
ELSE
*!*	    wgrupo=-1
*!*	    REQUERY("cronograma")	
endif
select(area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR PROXCIE")	   
    
ENDTRY 
return("ccrr")
******************************************************************
function detestado(westa,whabil)
local area,weldeta
TRY 

area=select()
weldeta=""

res=SQLEXEC(n_handle,"select * from estadusu where estado=?westa","cesta")
IF res<=0
   THROW "NORMAL" 
ENDIF 
if !EOF("cesta")
	weldeta="CUENTA:  "+alltrim(cesta.detalle)
else
	weldeta=replica("?",15)	
endif
use in cesta
res=SQLEXEC(n_handle,"select * from motibol where motivo=?whabil","cmoti")
IF res<=0
   THROW "NORMAL" 
ENDIF 
if !EOF("cmoti")
	weldeta=weldeta+" ,   BOLETIN: "+alltrim(cmoti.detalle)+" "+ALLTRIM(STR(WHABIL))
else
	weldeta=weldeta+", "+replica("?",15)		
endif
use in cmoti
sele (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR DETESTADO")	   
    
ENDTRY 
return weldeta
******************************************************************************
function ultpago(wppais,wogru,wousu,wotitu,woplas)
local wfep
TRY 
wfep=ctod("")
wclavu=str(wppais,2)+str(wogru,2)+str(wousu,5)+str(woplas,1)
res=SQLEXEC(n_handle,"select * from usuarios where ppais=?wpais and cod1_cli=?wogru and cod2_cli=?wousu and cod3_cli=?wotitu and tipo_doc='P'","celus")
IF res<=0
   THROW "NORMAL"
ENDIF 
if !EOF("celus")
	wfep=celus.ultpa
endif	
if used("celus")
	use in celus
ENDIF

CATCH 
   wfep=ctod("")
ENDTRY 
return (wfep)
*************************************************************************************
function fultpago(wppais,wogru,wousu,wfecha,wcorrer)
local wfep,area
if !ejecutable
	set echo on
	set step on
ENDIF
wfcorrer=.t.
IF PCOUNT()=5
	wfcorrer=wcorrer
ENDIF
area=select()
wfep=ctod("")
wclavu=str(wppais,2)+str(wogru,2)+str(wousu,5)
*wclavi=str(wppais,2)+str(wogru,2)+str(wousu,5)+dtos(wfecha-999)
wclavi=str(wppais,2)+str(wogru,2)+str(wousu,5)+'19000101'
wclavf=str(wppais,2)+str(wogru,2)+str(wousu,5)+dtos(wfecha)
res=SQLEXEC(n_handle,"select * from cupusu where ppais=?wpais and cod1_cli=?wogru and cod2_cli=?wousu and presenta<=?wfecha and tipo_doc='P' order by presenta limit 1","celus");
IF res<=0
   THROW "NORMAL" 
ENDIF 

*!*	select * from tarjeta!cupusu where presenta<= wfecha and ppais=wppais and cod1_cli=wogru and cod2_cli=wousu  and tipo_doc="P";
*!*		order by presenta desc top 1;
*!*		into cursor celus
if !EOF("celus")
	wfep=celus.presenta
	weli=celus.importe
	*--al haber hecho pago debo correr fecha hacia el vto  posterior paraque no cuente como atraso esos dias, banco central y RA
	if wfcorrer
		*wclavei=str(wogru,2)+dtos(wfep)	
		wclavef=wfep
		res=SQLEXEC(n_handle,"select * from cierres where grupo=?wogru and cierre_an<?wclavef order by cierre_an desc limit 1","cxcierres")
		IF res<=0
		   THROW "NORMAL" 
		ENDIF 
		
		if !EOF("cxcierres")
			if between(wfep,cxcierres.cierre_an,cxcierres.cierre_act)
				wfep=cxcierres.vto_act
			endif
		endif	
	endif
endif	
if used("celus")
	use in celus
endif
sele (area)
CATCH 
   wfep=ctod("")
ENDTRY 
return (wfep)
*************************************************************************************
function _mysqlfecha(wff)
local wfcv,wxx
if empty(wff)
	wff=date()
endif
wxx=dtos(wff)
wfcv="'"+left(wxx,4)+"-"+substr(wxx,5,2)+"-"+right(wxx,2)+" 00:00:00'"
return (wfcv)
*************************************************************************
function _mysqldate(wff)
local wfcv,wxx
if empty(wff)
	wff=date()
endif
wxx=dtos(wff)
wfcv="'"+left(wxx,4)+"-"+substr(wxx,5,2)+"-"+right(wxx,2)+"'"
return (wfcv)
****************************************************************************
function  miacumu(mcampo)
local wcampo,area
TRY
*----se pasa campo para saber que nombre campo le corresponde como acumulador futuro del inventario.-
wcampo=""
area=select()
sele parametros
for i=1 to fcount()
	wc=field(i)
	if type(wc)='C'
		if upper(mcampo) $ &wc
				wcampo=field(i)
		endif	
	endif
endfor
select (area)
CATCH 
	wcampo=''
ENDTRY 
*messagebox(" campo "+wcampo)
return wcampo

**********************************************************************************
function graciascoring(wgr)
local wgra,wgrup,area,wupa
TRY 
area=select()
wupa=.f.
wgrup=wgr
wgra=0
res=SQLEXEC(n_handle,"select * from grupos where grupo=?wgrup",'cgracia')
IF res<=0
	   THROW "NORMAL" 
ENDIF 
if !EOF('cgracia')
	wgra=cgracia.graciascoring
endif
use in cgracia
CATCH 
	WGRA=0
ENDTRY 
sele (area)
return (wgra)
*************************************************************************
function estadocuenta(warchi,wproce,wcieac,wciehoy)
*---------------------------------------------
*---warchi archivo inventario del proceso
*---wproce es el proceso que llamo.MBOLETI ES BOLETIN SISCARD
*---wcieac es el cierre del grupo
*---wciehoy es fecha que preoceso, para control de mora
*---------------------------------------------
local area,wfgrave,wgrave,wcubmin
store 0 to wfgrave,wgrave   && WFGRAVE se usa para SISCARD
area=select()
sele (warchi)
if parameters()<4
	messagebox("Son 4 parametros Estadocuenta",48,"Control")
	*return
endif
wcubmin=.t.
*---SISCARD----
*vgrv={"0-Habilitada                 ","2-Autorizaci¢n               ",
*      "3-Inhabilitada               ","7-Inv lida                   ",
*      "8-Excedida                   ","9-Capturar                   "}
** mora por no pago del minimo anterior o del mes
*if cierhoy>vtoan+wgracia and abs(pagon)+abs(ajupagon)+(abs(pagod)+abs(ajupagod))*codd < wpm*(paga_minn+paga_mind*codd)  and paga_minn+paga_mind>0
if wciehoy>=wcieac and abs(pagon)+abs(ajupagon)+(abs(pagod)+abs(ajupagod))*codd < wpm*(paga_minn+paga_mind*codd)  and paga_minn+paga_mind>0
	wgrave=1 &&  3 siscard
    wfgrave=2  &&forzada
    
    *store 0 to wgrave,wfgrave  &&por pedido hasta el cierre
    
	wcubmin=.f.
endif	

** caso 2 PASA LIMITE DE COMPRA
*!*	if  wcompras>(wxlc*plus)  and wgrave=0
*!*	    wgrave=2   && 2 siscard
*!*	    wfgrave=2  &&forzada
*!*	endif
      
** caso 3 PASA LIMITE DE COMPRA CON MORA (1)
*----pasa el limite de compras y no ha pago el minimo al 80%----
*!*	if  wcompras >wxlc .and. !wcubmin and wgrave=0
*!*	     wgrave=3 && 3 siscard
*!*	     wfgrave=3  &&forzada
*!*	endif

** caso 4 PASA LIMITE DE COMPRA +CREDITO 
*-------compro limite de compras y credito y cubrio minimo al -------------------
*!*	if  wcompras >wxlc + wlcr .and. wcubmin and (wgrave<>1 and wgrave<>3)
*!*	     wgrave=4  && 3 siscard
*!*	     wfgrave=3  &&forzada
*!*	endif
    
** caso 5 PASA LIMITE DE COMPRA +CREDITO CON MORA (1)
*-----------compro limite de compras y credito y no cubrio minimo al 80%-------
*!*	if  wcompras >wxlc + wlcr .and. !wcubmin and (wgrave<>1 and wgrave<>3)
*!*	     wgrave=5   && 3 siscard
*!*	     wfgrave=3  &&forzada
*!*	endif

** caso 6 PASA LIMITE DE COMPRA 
*-------tiene compras diferidas que pasan el 60% del limite de compras
*!*	if mcompranf+mcompradf*codd >WXLC * 0.60 and wgrave=0
*!*	*if  wcompras > wxlc+wxlcc  and wgrave=0
*!*	    wgrave=6  && 2 siscard
*!*	    wfgrave=2  &&forzada
*!*	endif
** caso 7 EL SALDO TOTAL DE LA CUENTA SUPERA EL LC
	
*--------tiene saldo mayor que el limite de credito-------------
if sdo_dispo<0 and wgrave=0
    wgrave=7  && 2 siscard
    wfgrave=8 &&forzada  antes 2
endif
sele (area)
return iif(wproce="mboleti",wfgrave,wgrave)
endfun
**************************************************************************************
function migrave(wwgrave)
local area,wdet
TRY
wdet=padr("Cuenta en Orden",32)
area=select()
IF !USED('MOTIGR')
	res=SQLEXEC(oapp.hnd,"select * from motigr",'motigr')
	IF res<=0
		   THROW "NORMAL" 
	ENDIF 
	if EOF('motigr')
	   THROW "NORMAL" 
	endif
ENDIF
sele motigr
scan for motivo=wwgrave
	wdet=detalle
ENDSCAN
CATCH 
ENDTRY 
sele (area)
return (wdet)
****************************************************************************
function fcuponus(wcupant,wp,wgrp,wcod2,wfini,wfefi,wcdeta) 
* carga en archivo los cupones de 1 usuario en formato impresion/pantalla.
* wcupant=archiv, wp=pais, wgrp=grupo,wcod2=usuario,wfini=desde, wfefi=hasta.
*.............................................................................
local area,welpa

TRY 

area=select()
set exact on
if parameters()<7	
	messagebox("Indico menos de 18 parametros ",48,"Control")
	THROW "NORMAL"
endif
if !ejecutable
	set echo on
	set step on
endif
*---8/7/06
if used(wcupant)
    USE IN (wcupant)
ENDIF 
*sele (wcupant)
*zap
welpa=wp
*---debo armar wtabcu o sea cupones a procesar, del periodo y cuotas futuras para poder grabarle altas.
*movius=str(ppais,2)+str(cod1_cli,2)+str(cod2_cli,5)+dtos(presenta)
*where between(c.presenta,wfini,wfefi) and c.cod1_cli=wgrp and c.cod2_cli=wcod2; **condicion cambiada
* order by presenta;  este es el orden quitado
wcl1=str(welpa,2)+str(wgrp,2)+str(wcod2,5)+dtos(wfini)
wcl2=str(welpa,2)+str(wgrp,2)+str(wcod2,5)+dtos(wfefi)
wtcu=wcamino+wcupant
*wtcu=fullpath(wcupant)
wrd="presenta,cod2_cli"
if wcdeta
	wrd="cod1_cli,cod2_cli,cod3_cli"
ENDIF
*!*	select *,space(44) as detalle, 0000000.00 as debe,0000000.00 as haber,0000000.00 as dolar,0000000.00 as peso, space(16) as utarjeta,space(1) as bitcon, .f. as dfuturo,0000.00 as extrapunto;
*!*		from tarjeta!cupusu c;
*!*		where between(str(ppais,2)+str(cod1_cli,2)+str(cod2_cli,5)+dtos(presenta),wcl1,wcl2) ;
*!*		order by &wrd;
*!*		into table (wtcu) && (c.cod2_cli,c.cod3_cli) ->no sirve este orden, por la cronologia 

TEXT TO csql NOSHOW 	
select c.*,space(44) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,false as dfuturo,
	if(c.comercio=0,0,o.ramo) as ramo,
    if(c.comercio=0,'', nombre) as detacomer, if(c.comercio=0,0,o.extrapunto) as extrapunto, 000000000.00 as dolar, 000000000.00 as peso,
    p.detalle as detatipdoc, if(c.motivo=0,'',m.nota) as detamotivo, if(c.comercio=0,"",o.condiva) as condiva, space(1) as bitcon
	from cupusu c
	left join comercio o on c.paisco=o.paisco and c.comercio=o.comercio and c.sucursal=o.sucursal
	left join planes p on c.tipo_doc=c.tipo_doc
	left join ramos r on o.ramo=r.ramo
	left join motivos m on c.motivo=m.motivo
	where ppais=?welpa and cod1_cli=?wgrp and cod2_cli=?wcod2 and (presenta between ?wfini and ?wfefi)
	order by c.cod1_cli,c.cod2_cli,c.cod3_cli
ENDTEXT 

res=SQLEXEC(n_handle,csql,wcupant)
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - FCUPONUS")
   THROW "NORMAL"
ENDIF
	
if EOF(wcupant)
	THROW "NORMAL"
ENDIF 	 
*---debo conseguir datos de los comercios, movimientos , motivos----
if !used(wcupant)
	use (wtcu) in 0 exclu
endif
SELECT (wcupant)
*repla all detalle with iif(tipo_doc="P","Su Pago ",iif(tipo_doc="G" and (motivo=0 or motivo=-5),"Costo del Resumen",""))  in (wcupant)
*dele all for tipo_doc="*"
sum all impmone*iif(signo="+",1,-1)  to tarjear for moneda="A"  &&pesos argentinos 
repla all paisco with iif(nacion=1,2,1), sucursal with 0 for tipo_doc="*"
replace all debe with iif(moneda="D",impmone,importe)+interes+comision for signo="+" in (wcupant)
replace all	haber with iif(moneda="D",impmone,importe)+interes+comision for signo="-" in (wcupant)
*--para manejar la misma tabla con la moneda
replace all dolar with iif(signo="+",debe,-1*haber) for moneda="D"  in (wcupant)
replace all	peso  with iif(signo="+",debe,-1*haber) for moneda<>"D" in (wcupant)
*--------------------------
sum all importe to wseguro for motivo=-1 &&valor del seguro para restar en resumenes
sum all interes+comision to wincpn for tipo_doc<>"A" and tipo_doc<>"P" and moneda<>"D" &&valor interes restar en resumenes
sum all interes+comision to wincpd for tipo_doc<>"A" and tipo_doc<>"P" and moneda="D"
sum all interes+comision to winapn for tipo_doc="A" and moneda<>"D" && interes adelanto en resumen $
sum all interes+comision to winapd for tipo_doc="A" and moneda="D"  && idem en dolares

*--carga nombre de comercios en cupones correspondientes debo hacerlo para los puntos---
repla all detalle with iif(tipo_doc="P","Su Pago ",iif(tipo_doc="G" and motivo=0,"Costo del Resumen",""))  for empty(detalle)
*!*	select alltrim(c.nombre)+" "+iif(alltrim(u.cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(u.cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre,u.item,c.extrapunto;
*!*		from tarjeta!comercio c,(wtcu)u;
*!*		where str(u.paisco,2)+str(u.comercio,5)+str(u.sucursal,1)=str(c.paisco,2)+str(c.comercio,5)+str(c.sucursal,1);
*!*		into cursor ucomer
*!*	if _tally<>0
*!*		sele ucomer
*!*		scan all
*!*			uitem=item
*!*			sele (wcupant)
*!*			scan all for item=uitem
*!*				replace detalle with ucomer.nombre, extrapunto with ucomer.extrapunto
*!*			endscan
*!*			sele ucomer
*!*		endscan
*!*	endif	
*!*	if used("ucomer")
*!*		use in ucomer
*!*	endif
if wcdeta	 && si es para mostrar debo cargar los detalles
*!*		*--carga tipos de documento sin motivo---
*!*		select alltrim(p.detalle)+" "+iif(alltrim(u.cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(u.cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre ,u.item;
*!*			from tarjeta!planes p,(wtcu)u;
*!*			where u.tipo_doc=p.tipo_doc and p.pais=welpa;
*!*				and u.motivo=0 and u.comercio=0;
*!*			into cursor pmoti
*!*		if _tally<>0
*!*			sele pmoti
*!*			scan all
*!*				uitem=item
*!*				sele (wcupant)
*!*				scan all for item=uitem
*!*					replace detalle with pmoti.nombre 
*!*				endscan
*!*				sele pmoti
*!*			endscan
*!*		endif	
*!*		if used("pmoti")
*!*			use in pmoti
*!*		endif
*!*		*----carga motivos ------------------
*!*		select alltrim(m.nota)+" "+iif(alltrim(u.cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(u.cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),u.cuotas) as nombre,u.item;
*!*			from tarjeta!motivos m,(wtcu)u;
*!*			where u.motivo=m.motivo and m.pais=nacion;
*!*			into cursor mmoti
*!*		if _tally<>0
*!*			sele mmoti
*!*			scan all
*!*				uitem=item
*!*				sele (wcupant)
*!*				scan all for item=uitem
*!*					replace detalle with alltrim((mmoti.nombre))+iif(motivo=94 and nacion=1 or motivo=293 and nacion=2," "+autoriza,"")
*!*				endscan
*!*				sele mmoti
*!*			endscan
*!*		endif	
*!*		if used("mmoti")
*!*			use in mmoti
*!*		endif
*--carga nombre de comercios en cupones correspondientes ---
	repla all detalle with iif(tipo_doc="P","Su Pago ",iif(tipo_doc="G" and motivo=0,"Costo del Resumen",""))  for empty(detalle) AND ALLTRIM(item)<>''
	
	replace ALL detalle WITH alltrim(detacomer)+" "+iif(alltrim(cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),cuotas) for comercio<>0 AND ALLTRIM(item)<>''
	        
	replace detalle WITH alltrim(detatipdoc)+" "+iif(alltrim(cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),cuotas) FOR comercio=0 AND motivo=0 
	
	replace ALL detalle with alltrim(detamotivo)+" "+iif(alltrim(cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),cuotas) FOR comercio=0 AND motivo<>0

	repla all item with STR(recno()) for ALLTRIM(item)=''
	sele (wcupant)
	*repla all detalle with "Seguro Sobre Saldo " for motivo=-1
endif
if wcdeta
	sele (wcupant)
	SCAN ALL 
	
		wd=digiver(cod1_cli,cod2_cli,cod3_cli,cod4_cli)
		wt=wglobaiso+strtran(str(cod1_cli,2)," ","0")+strtran(str(cod2_cli,5)," ","0")+;
			str(cod3_cli,1)+str(cod4_cli,1)+wd
		REPLACE utarjeta WITH wt IN (wcupant)
	ENDSCAN 
	repla all detalle with iif(nacion=1,"Compras ROU","Compras RA ") for tipo_doc="*" and empty(detalle)
endif
if wcdeta
	sele (wcupant)
	repla all detalle with left(detalle,35)+" $A"+trans(round(impmone,1),"####.#") for nacion=2 and moneda="A"
	repla all detalle with left(detalle,35)+" $U"+trans(impmone,"####.#") for nacion=1 and moneda="U"
endif

if !ejecutable
	*browse title "carga ucuponant"
endif
*!*	*FIN CARGA VER DATOS
SELECT (area)

CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR FCUPONUS")	   
    
ENDTRY 
return .t.
************************************************************************************
function cuentapunto(warchi)
*--parametros
	*Warchi=Archivo de puntos
	*Wvalpunto=Valor de puntos variables globales
	*Whabilitado=Estado de usuario/boletin variables globales
*----------------------------------------	
local area, wpuntos,wvalpunto,wfacpun
TRY 

area=select()
wpuntos=0
wvalpunto=parametros.valpunto
if nacion=1 
	*or whabilitado>1
	SELECT (area)
	THROW "NORMAL"
ENDIF 
sele (warchi)
scan all 
	*--si es cuota y primera paga todo el importe, sino 0. wfi
     *---cotizacion para multimplicar por importe dolares-----
     wnc=val(substr(cuotas,1,at("/",cuotas)-1))
     wfi=iif(tipo_doc $ "F_U" and comercio>0, iif(wnc=1, val(substr(cuotas,at("/",cuotas)+1,2)),0),1)
     wmt=iif(nacion=1,57,36)
     wtd="PJG"+iif(nacion=2,"HA","")
     wfacpun=iif(tipo_doc $ wtd or (tipo_doc="U" and motivo=wmt) ,0,1)
     if wfacpun>0
           wfacpun=1+extrapunto/100
     endif
	 wpuntos=wpuntos+int(importe*wfi*iif(moneda="D",wcotiza,1)/wvalpunto)*wfacpun
ENDSCAN 
go top in (warchi)
sele (area)
CATCH TO oErr WHEN oErr.UserValue="NORMAL"

CATCH TO oErr WHEN oErr.UserValue<>"NORMAL"
    wpuntos=0
    crlf=CHR(13)+CHR(10)
    MsgErr="[  Error: ] " + STR(oErr.ErrorNo) + CRLF + ;
    	   "[  Línea: ] " + STR(oErr.LineNo) + CRLF + ; 
    	   "[  Mensaje: ] " + oErr.Message + CRLF + ; 
    	   "[  Procedimiento: ] " + oErr.Procedure + CRLF + ; 
    	   "[  Detalles: ] " + oErr.Details + CRLF + ; 
    	   "[  StackLevel: ] " + STR(oErr.StackLevel) + CRLF + ; 
    	   "[  Instrucción: ] " + oErr.LineContents 
    	   
    MESSAGEBOX(msgerr,16,"ERROR CUENTAPUNTOS")	   
    wpuntos=0
    
ENDTRY 
return (wpuntos)
**************************************************************************
