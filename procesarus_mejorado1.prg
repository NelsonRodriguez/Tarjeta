*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************
function procesarus(wtabin,wtabcu,wtabac,wnaci,wgrp,wusu,wciean,wcieac,wvtoan,wvthoy,wnmr,wlt,wing,wlcr,wsdod,wxlc,wxlcc,winvn,wcdeta,wcer)
local area,wsaldon,wminn,wsaldod,wmind,sminima,ssdoa,sminimd,ssdod ,wcier_act,wvto_act,wcier_an,;
	  wvto_an,wnmru,wlet,wingr,wrg,wuss,wulpan,wulpad,ipun,ipud,upa,codd
	  
TRY 
	  
*-- Parametros: wtabin,wtabcu,wtabc -tablas para datos. wnaci,wgrp,wusu -nacion grupo y usuario
*		wciean,wcieac,wvtoan,wvthoy -cierres y vencimientos. wnmr -numerado usuario.  wlt -letratar. wing -Ingreso.		
HND=OAPP.HND
wtcu=wtabcu
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
IF  parameters()=20
	wcereo=wcer
ENDIF 
IF USED("xplanes")
   USE IN xplanes
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
wgracia=quegracia(wgrp) && convertido
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
res=SQLEXEC(n_handle,"select * from planes order by tipo_doc","xplanes")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error Al Recuperar PLANES - PROCESARUS")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF 
*!*	SELECT planes
SELECT xplanes
SCAN ALL FOR origen="PL"
	tiraplan=tiraplan+tipo_doc+"_"
ENDSCAN 
*---variables parametricas-----------------------------------------------------------------------
store wvtoan to wpaga,wpagd && cargo fechas de calculo intereses como dia del vencimiento anterior
*--Paso cparametros tablas, nacion, grupo,usuario-- el costo de la tarjeta del usuario------------
ws=costoser(wnaci,wlet,wingr,wgrp) && carga: costoser.dbf ifin=.int_fin ,ifid=.int_fid, pfin=.por_fin  && CONVERTIDO 
*--reparo intereses si no estan cargados en costoser
IF  wgrp<>10
	IF  ifin=0
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
	
res=SQLEXEC(hnd,"select saldo_acn,pago_minn,saldo_acd,pago_mind from inventario i where i.cierre=?wciean and i.cod1_cli=?wgrp and i.cod2_cli=?wusu order by i.cierre desc LIMIT 1","cinven")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - INVENTARIO")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF 
	
IF  !EOF("cinven")
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
IF  used("cinven")
	USE IN cinven
ENDIF
 
append blank in (wtabin)   && agrego registro inventario ultimo cierre---
replace cod1_cli with wgrp, cod2_cli with wusu, ppais with wnaci, cierre with wcier_act,;
	numerado with wnmru, saldo_a_n with wsaldon, saldo_a_d with wsaldod, paga_minn with wminn,;
	paga_mind with wmind, int_fin with ifin, int_fid with ifid, por_fin with iif(pfin=0,cparametros.por_fin,pfin),servicio with ws,;
	financian with 0.00, nfinancian with 0.00, financiad with 0.00, nfinanciad with 0.00, nproceso with 0 in (wtabin)
	
SELECT (wtabin)

IF  !ejecutable
	*browse	title "wtabin"
ENDIF 
*---------------------------------------------------------------------
*----SALDO DE OTRO PAIS: ARCHIVO PROCESADO DE SISCARD, RETRASMITIDO
*---------------------------------------------------------------------
wppai=elpais(wgrp) && pais del usuario que compra  CONVERTIDO
IF  wppai<>nacion and wglobaiso="504736"
	wtcu=wtabcu
	
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
	IF   !file(wdirre+"cuentas.dbf") or !file(wdirre+"positivo.dbf")
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
		IF   empty(wbolsatodo)
			wctpeso=cparametros.cot_peso
			getautov9(wxarchi,nacion,wtrau,wctpeso,wppai)  && fncion que trae datos de autorizaciones CONVERTIDA
		ELSE 
			sumauto(wbolsatodo,wtrau) && CONVERTIDO
		ENDIF 
		
	*endif	
	*---cuentas
	wcnta=wglobaiso+strtran(str(wgrp,2)," ","0")+strtran(str(wusu,5)," ","0")
	use (wdirre+"cuentas") in 0 alias wcuentas exclu
	sele wcuentas
	loca for np=wcnta
	IF  !eof()
		*wimptrasmi=val(wcuentas.co)/100*iif(nacion=1<>wppai,cparametros.cot_peso,1)
		IF  nacion=1
			wimptrasmi=val(wcuentas.co)/100  && ya esta convertido 
		ENDIF 
		IF  nacion=2
			wimptrasmi=val(wcuentas.co)/100*cparametros.cot_peso
		ENDIF 
		vr2=wpauto
		vr3=wdauto*cparametros.cot_dolpx
		replace sdo_dispo with wimptrasmi-wpauto-wdauto in (wtabin)  &&saldo disponible de la cuenta.	
	ENDIF 
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
wtcu=wtabcu
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

select c.*,space(35) as detalle, 0000000.00 as debe,0000000.00 as haber, space(16) as utarjeta,presenta>?wcier_act as dfuturo,
	if(c.comercio=0,0,o.ramo) as ramo,
    if(c.comercio=0,'', nombre) as detacomer, if(c.comercio=0,0,o.extrapunto) as extrapunto,
    p.detalle as detatipdoc, if(c.motivo=0,'',m.nota) as detamotivo, if(c.comercio=0,"",o.condiva) as condiva
	from cupusu c
	left join comercio o on c.paisco=o.paisco and c.comercio=o.comercio and c.sucursal=o.sucursal
	left join planes p on c.tipo_doc=c.tipo_doc
	left join ramos r on o.ramo=r.ramo
	left join motivos m on c.motivo=m.motivo
	where ppais=?welpa and cod1_cli=?wrg and cod2_cli=?wuss and presenta>?wcier  
	order by presenta
	
ENDTEXT 

res=SQLEXEC(hnd,csql,wtcu)
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - CUPUSU - Sacando CUPONES")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF

*--- elimino los Pagos de Gestión. Antes estaban en el Select anterior. ---------------------------*
SELECT (wtcu)
COPY all to d:\bases\pepe
DELETE ALL FOR tipopago='G'
GO TOP 

MESSAGEBOX("HAGO BROW DE CAMPOS PARA CONSTATAR REPETICION ")
BROWSE  TITLE "WTCU"
 
*---cargo intereses que hay para cobrar al pago, en morosos y evitar iva ($=wintiva,U$=wintivd) 
store 0.00 to wintiva,wintivd,wseguro,wmovivn,wmovivd,wcomin,wcomid,iva_m,iva_do,wppes,wpdol,servi,servd,;
	winten,winted,wmovivd,wmovivp
*--winten y winted son intereses de planes del mes
*--win y wpn son intereses de financiacion
*-------------------------------------------------------------------------------------------------
*--cargo intereses de mora en tabcu, si es cierre de 
*Inventario no iria la carga para no calcular saldo que no figura en proceso.
IF  !wcereo
	wintemor=cargainter(wrg,wuss) && CONVERTIDO
ENDIF 

*---cargo pagos del pos
IF  nacion=2 and wglobaiso="504736" and mywhnd>0
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
		IF  type("sqlpagos.autoriza")="N"
			m.autoriza=str(autoriza,10)
		ENDIF 
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
		IF  wintiva+wintivd>0
		    IF  moneda<>"D"
		    	wintepes=iif(wsaldo>wintiva,wintiva,wsaldo)
		    	wintiva=wintiva-wintepes
		    ENDIF 
		    IF  moneda="D"
   		    	wintedol=iif(wsaldo>wintivd,wintivd,wsaldo)
   		    	wintivd=wintivd-wintedol
		    ENDIF 
			*---IVA DEL INTERES: saco por diferencia antes y ahora y agrego el iva que se cobro y no almaceno.
			wpagoin=round(wintepes*(1+(piva/100)),2)
			wpagoid=round(wintedol*(1+(piva/100)),2)
		ENDIF 
		m.interes=wpagoin+wpagoid
		m.importe=iif(moneda<>"D",m.importe-(wpagoin+wpagoid),0)
		m.impmone=iif(moneda="D",m.impmone-(wpagoin+wpagoid),0)
		m.tasaiva=pIva
		m.cuotas="1/1"
		insert into (wtabcu) from memvar
	ENDSCAN 
	sele (wtabcu)
	use in sqlpagos
ENDIF 
IF  !winvn
	wpiva=cparametros.iva/100
	wbc=(nacion<>2)   &&banco central
	weldeta=iif(nacion=2,"Intereses Acumulados","Compras en Rou")		
	IF  wintiva>0
		insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta);
			values(nacion,'',0,"P",wgrp,wusu,0,iif(wbc,"*","G"),iif(wbc,-99,-8),wintiva,wintiva/codd,wmonservi,wcier_act,wcier_act,"inpunn","+","N",weldeta,"-")
	ENDIF 
	*,impiva,tasaiva,iva
	*,wintiva*wpiva,wpiva*100,.t.
	IF  wintivd>0
		insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta);
			values(nacion,' ',0,"P",wgrp,wusu,0,iif(wbc,"*","G"),iif(wbc,-99,-8),wintivd*codd,wintivd,"D",wcier_act,wcier_act,"inpund","+","N",weldeta,"-")
	ENDIF 
*---Fin cargo:
ENDIF 
*---quito desde aqui el 18/2/10



*---DEVOLUCION PARA URUGUAY DEL IMESI COMBUSTIBLE
*--PROCESO LOS ITEMS QUE SE GRABAN EN PROCESO / CIERRE EN VCUPUSUNP-----
*-----------------------------------------------------------------------
*---- DGI DEVOLUCION IMESI COMBUSTIBLE  ------  URUGUAY -----* Y RRRRRREESSSSTTTTTAAAAUUUURRRRAAAANTESSSSS
sele (wtabcu)	
IF  !ejecutable
	*BROWSE 
ENDIF 
DELETE all for "nada" $ campo
weldes=sys(2015)
*copy to (wcamino+weldes)   &&creo archivo temporario para tomar los descuentos
SELECT * FROM (wtabcu) INTO CURSOR weldestmp readwrite
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
IF  nacion=2 and wglobaiso="504736"
	wcod_ade=alltrim(cparametros.codigo_ad)
ENDIF 
*store 0.00 to wreincomp,wreinvarp,wreincomd,wreinvard
store 0.00 to wtcupones,wtreinte,wtcuponesn,wtreinten,wtcuponesnN
store 0.00 to wtcuponed,wtreinted,wtcuponedn,wtreintedn,wtcuponednN   &&n es de nafta
SCAN  all for between(presenta,wcier_an+1,wcier_act)	and nacion=2 and wglobaiso="504736"
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
	wcondiva=&weldes..condiva
	IF  &weldes..tipo_doc $ tiraplan and (&weldes..paisco=2 and wglobaiso="504736") and wcta="1/" and left(&weldes..boleta,1) $ "1_2" and len(alltrim(&weldes..boleta))=10 and;
		val(right(&weldes..boleta,9))>0
		
		*--atrapo condiva en el join
		
*!*			csql1="select ramo,condiva from comercio where comercio.comercio=?wcomer and comercio.paisco=?wppai" 
*!*			res=SQLEXEC(hnd,csql1,"cramo")
*!*			IF res<=0
*!*	           AERROR(verr)
*!*	           MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - Verificando DESCUENTOS POR RAMOS")
*!*		       upa=CTOD("")
*!*	           THROW "NORMAL"
*!*			ENDIF 
*!*			IF   !EOF("cramo")
*!*				replace ramo with cramo.ramo in (weldes)
*!*				wcondiva=cramo.condiva
*!*			ENDIF 	
*!*			IF  used("cramo")
*!*				use in cramo
*!*			ENDIF 	
        *----------------------------------------
		wmt=0
		wdet=""
		wdiv=1     && el divisor para el calculo que en restorant sera 1.22 o sea el iva_r
		wali=0.00  &&si no corresponde a combustible no va 
		IF  &weldes..ramo=wramocomb 
			wmt=-13
			wnafta=.t.
			wdet="Red IMESI Dto 398/7"
			wali=walicuoe	
		ENDIF 	
		IF  &weldes..ramo=wramorest
			wdiv=wiva_r
			wmt=-31
			wresto=.t.
			wdet="Red_Alíc IVA L.17.934"
			wali=walicuor	
		ENDIF 	
		*--calcular total de cupon para casos cuotas, contado.
		wcut=val(iif(empty(&weldes..cuotas),"1",substr(alltrim(&weldes..cuotas),3,2)))  && cuotas totales
		wtotcup=round(&weldes..importe*wcut,2)
		wvtot=alltrim(str(wtotcup,10,2))
		*ver $,U$
		IF  &weldes..moneda="D"
			IF  !wnafta and !wresto
				wtcuponed=wtcuponed+wtotcup/codd
				wtreinted=wtotcup/wdiv*wali/100/codd
			ELSE 
				wtcuponednN=wtcuponednN+iif(wresto,wtotcup/codd,0.00)   && acumulo descuento restorant para el pos
				wtcuponedn=wtcuponedn+wtotcup/codd
				wtreintedn=wtotcup/wdiv*wali/100/codd
			ENDIF 
		ELSE 
			IF  !wnafta and !wresto
				wtcupones=wtcupones+wtotcup
				wtreinte=wtotcup/wdiv*wali/100
			ELSE 
				wtcuponesnN=wtcuponesnN+iif(wresto,wtotcup,0.00)   && acumulo descuento restorant para el pos
				wtcuponesn=wtcuponesn+wtotcup
				wtreinten=wtotcup/wdiv*wali/100
			ENDIF 	
		ENDIF 
		replace valret with (wtotcup/wdiv*wali/100)/iif(&weldes..moneda="D",codd,1), tcupones with wtotcup/iif(&weldes..moneda="D",codd,1) in (weldes) 
		
		*----?? grabo para el proceso el reintegro del iva/ consulta periodo activo/inventario
		IF  wtreinte>0
				insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
					values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreinte,wtreinte/codd,wmnop,wfps,wprs,"ajusten","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
				IF  winvn   && si esta procesando cierre va a cupusu con nproceso
						insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nocupon,autoriza,comercio,codoper);
						values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreinte,wtreinte/codd,wmnop,wfps,wprs,"ajusten","-","N",wtreinte,wtcupones,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)	
				ENDIF 	
		ENDIF 
		IF  wtreinten>0

				insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
					values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreinten,wtreinten/codd,wmnop,wfps,wprs,"ajusten","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
			IF  winvn   && si esta procesando cierre va a cupusu con nproceso
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nrocupon,autoriza,comercio,codoper);
					values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreinten,wtreinten/codd,wmnop,wfps,wprs,"ajusten","-","N",wtreinten,wtcuponesn,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)		
			ENDIF 	 
		ENDIF 
		*--para dolares
		IF  wtreinted>0
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
				values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreinted*codd,wtreinted,wmnop,wfps,wprs,"ajusted","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
			IF  winvn   && si esta procesando cierre va a cupusu con nproceso		
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nrocupon,autoriza,comercio,codoper);
					values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreinted*codd,wtreinted,wmnop,wfps,wprs,"ajusted","-","N",wtreinted,wtcuponed,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
		
			ENDIF 
		ENDIF 	
		IF  wtreintedn>0
			insert into (wtabcu) (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,detalle,boleta,nrocupon,autoriza,comercio,codoper);
				values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreintedn*codd,wtreintedn,wmnop,wfps,wprs,"ajusted","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
			IF  winvn   && si esta procesando cierre va a cupusu con nproceso	
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,boleta,nrocupon,autoriza,comercio,codoper);
					values(nacion,STR(wmt),wmt,"P",wgrp,wusu,0,"J",wmt,wtreintedn*codd,wtreintedn,wmnop,wfps,wprs,"ajusted","-","N",wtreintedn,wtcuponedn,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)		
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
		csql2="select * from usuarios where ppais=?nacion and cod1_cli=?wrg and cod2_cli=?wuss"
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


if wcdeta or winvn		&& si es para mostrar debo cargar los detalles y scoring
	*--carga nombre de comercios en cupones correspondientes ---
	repla all detalle with iif(tipo_doc="P","Su Pago ",iif(tipo_doc="G" and motivo=0,"Costo del Resumen",""))  for empty(detalle) AND ALLTRIM(item)<>''
	
	replace ALL detalle WITH alltrim(detacomer)+" "+iif(alltrim(cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),cuotas) for comercio<>0 AND ALLTRIM(item)<>''
	        
	replace detalle WITH alltrim(detatipdoc)+" "+iif(alltrim(cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),cuotas) FOR comercio=0 AND motivo=0 
	
	replace ALL detalle with alltrim(detamotivo)+" "+iif(alltrim(cuotas)="1/ 1" and len(alltrim(cuotas))=4 or alltrim(cuotas)="1/1" and len(alltrim(cuotas))=3,space(5),cuotas) FOR comercio=0 AND motivo<>0

	repla all item with STR(recno()) for ALLTRIM(item)=''
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
			values(nacion,'-5',-5,"P",wgrp,wusu,0,"G",-5,wimpsell,wimpsell/codd,cparametros.monpais,wcier_act,wcier_act,"selladon","+","N","Sellado Provincial ",wimpsell)
		if winvn   && si esta procesando cierre va a cupusu con nproceso
			insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia);
				values(nacion,'-5',-5,"P",wgrp,wusu,0,"G",-5,wimpsell,wimpsell/codd,cparametros.monpais,wcier_act,wcier_act,"selladon","+","N")
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
				values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifif,wdifif/codd,cparametros.monpais,wcier_act,wcier_act,wcpp,"+","N",wtxt,wdifif,wdifif*(piva/100),piva,iif(!wnmra,"*","I"))
		endif	
		if winvn  and !wnmra && si esta procesando cierre va a cupusu con nproceso
			insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,tasaiva,boleta,comercio,paisco);
				values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifif,wdifif/codd,cparametros.monpais,wcier_act,wcier_act,wcpp,"+","N",0,0,wdifif*(piva/100),piva,"*",iif(wcpp="compran",wgrancr,0),2)	
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
				values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifip,wdifip/codd,cparametros.monpais,wcier_act,wcier_act,"nada","+","N",wtxt,wdifip,wdifip*(piva/100),piva,iif(!wnmra,"-","I"))
		endif		
		if winvn and !wnmra  && si esta procesando cierre va a cupusu con nproceso
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,tasaiva,boleta,comercio,paisco);
					values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifip,wdifip/codd,cparametros.monpais,wcier_act,wcier_act,"nada","+","N",0,0,wdifip*(piva/100),piva,"-",iif(wcpp="compran",wgrancr,0),2)	
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
				values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifif*codd,wdifif,"D",wcier_act,wcier_act,wcpp,"+","N",wtxt,wdifif,wdifif*(piva/100),piva,iif(!wnmra,"*","I"))
		endif		
		if winvn  and !wnmra && si esta procesando cierre va a cupusu con nproceso
			insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,tasaiva,boleta,comercio,paisco);
				values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifif*codd,wdifif,"D",wcier_act,wcier_act,wcpp,"+","N",0,0,wdifif*(piva/100),piva,"*",iif(wcpp="compran",wgrancr,0),2)	
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
			values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifip*codd,wdifip,"D",wcier_act,wcier_act,wcpp,"+","N",wtxt,iif(!wnmra,"","I"))
		endif
		if winvn and !wnmrad  && si esta procesando cierre va a cupusu con nproceso
				insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,valret,tcupones,impiva,boleta,comercio,paisco);
					values(nacion,'-99',-99,"P",wgrp,wusu,0,"*",-99,wdifip*codd,wdifip,"D",wcier_act,wcier_act,"nada","+","N",0,0,wdifip*(piva/100),"-",iif(wcpp="compran",wgrancr,0),2)	
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
ENDIF 

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
			values(nacion,'-6',-6,"P",wgrp,wusu,0,"G",-6,&wtabin..servicio,&wtabin..servicio/codd,wmonservi,wcier_act,wcier_act,iif(wmonservi="D","gastod","gaston"),"+","N","Costo del Resumen",&wtabin..servicio,wimpiva,piva)
			
	if winvn   && si esta procesando cierre va a cupusu con nproceso
		insert into vcupusunp (ppais,item,item_madre,ingresado,cod1_cli,cod2_cli,cod3_cli,tipo_doc,motivo,importe,impmone,moneda,fecha,presenta,campo,signo,financia,iva,impiva,tasaiva);
			values(nacion,'-6',-6,"P",wgrp,wusu,0,"G",-6,&wtabin..servicio,&wtabin..servicio/codd,cparametros.monpais,wcier_act,wcier_act,"gaston","+","N",.t.,wimpiva,piva)
	endif	
	replace &wtabin..gaston with &wtabin..gaston+iif(wmonservi="D",0.00,&wtabin..servicio), ;
		&wtabin..gastod with &wtabin..gastod+iif(wmonservi="D",&wtabin..servicio,0.00) in (wtabin)

endif
*--fin ingreso de servicio
sele (wtabin)
if !ejecutable
	browse title "1-Mira el pago en Pagonf o Pagodf antes de procesarlo"
endif

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
***---- Proceso Saldo FUTURO ----------------------------------------------------------------------------------------------------------------*
sele (wtabcu)
GO TOP 
BROWSE TITLE "WTABCU"
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
	browse title "2-Mira el pago en Pagonf o Pagodf"
ENDIF 
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

csql5="select * from pluscor where scoring=?wescoring"
res=SQLEXEC(hnd,csql5,"cpluscor")
IF res<=0
   AERROR(verr)
   MESSAGEBOX(STR(verr[1],7)+" "+verr[2],16,"Error en PROCESARUS - SCORING")
   upa=CTOD("")
   THROW "NORMAL"
ENDIF 
if !EOF("cpluscor")
	wplus=1+(cpluscor.pluscor/100)
endif	 
if used("cpluscor")
	use in cpluscor
ENDIF 
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
	ENDIF 
ENDIF 
if nacion=2 or nacion=1 and wglobaiso="504736"
	if empty(wbolsatodo)
*		wxarchi="x"+quitaprefijodir(quitaextension(wtabin))
        wxarchi=wtabin
		wppai=elpais(wgrp) && pais del usuario que compra
		*--se refiere a los archivos locales de autorizacion en este caso IVR.---
		wctpeso=cparametros.cot_peso
		getautov9(wxarchi,nacion,wtrau,wctpeso,wppai)  && fncion que trae datos de autorizaciones
		wsspp=saldopos(wtrau,wxarchi)
		wpauto=wpauto+wsspp
	else
		sumauto(wbolsatodo,wtrau)
	endif
ENDIF 

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
	*browse title "wtabin  "+str(wimpsell,8,2)+" sdo Dispo "+str(sdo_dispo,8,2)
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
*!*	if empty(wbolsatodo)
*!*		set order to
*!*		index on utarjeta+dtos(presenta)+dtos(fecha) to (wcamino+wtabcu)+".idx"
*!*	ENDIF 
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


*!*					APPEND blank in (wtabcu) 
*!*					replace ppais with nacion IN (wtabcu) 
*!*					*replace item with ABS(wmt) IN (wtabcu) 
*!*					replace item_madre with wmt IN (wtabcu) 
*!*					replace ingresado with 'P' IN (wtabcu) 
*!*					replace cod1_cli with wgrp IN (wtabcu) 
*!*					replace cod2_cli with wusu IN (wtabcu) 
*!*					replace cod3_cli with 0 IN (wtabcu) 
*!*					replace tipo_doc with 'J' IN (wtabcu) 
*!*					replace motivo with wmt IN (wtabcu) 
*!*					replace importe with wtreinten IN (wtabcu) 
*!*					replace impmone with wtreinten/codd IN (wtabcu) 
*!*	 				replace moneda with wmnop IN (wtabcu) 
*!*					replace fecha with wfps IN (wtabcu) 
*!*					replace presenta with wprs IN (wtabcu) 
*!*					replace campo with 'ajusten' IN (wtabcu) 
*!*					replace signo with '-' IN (wtabcu) 
*!*					replace financia with 'N' IN (wtabcu) 
*!*					replace detalle with wdet IN (wtabcu) 
*!*					replace boleta with wbole IN (wtabcu) 
*!*					replace nrocupon with wnrcu IN (wtabcu) 
*!*					replace autoriza with wvtot IN (wtabcu) 
*!*					replace comercio WITH wcmop IN (wtabcu) 
*!*					replace codoper  with &weldes..codoper IN (wtabcu) 
*!*					
*!*					values(nacion,wmt,wmt,"P",wgrp,wusu,0,"J",wmt,wtreinten,wtreinten/codd,wmnop,wfps,wprs,"ajusten","-","N",wdet,wbole,wnrcu,wvtot,wcmop,&weldes..codoper)
