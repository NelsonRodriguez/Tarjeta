USE [TARJETA]
GO

INSERT INTO [dbo].[CUPUSU]
           ([INGRESADO]
           ,[PPAIS]
           ,[COD1_CLI]
           ,[COD2_CLI]
           ,[COD3_CLI]
           ,[COD4_CLI]
           ,[TIPO_DOC]
           ,[MOTIVO]
           ,[FECHA]
           ,[PAISCO]
           ,[IMPORTE]
           ,[COMERCIO]
           ,[SUCURSAL]
           ,[NROCUPON]
           ,[AUTORIZA]
           ,[BOLETA]
           ,[PRESENTA]
           ,[RESUMEN]
           ,[TIPOPAGO]
           ,[MONEDA]
           ,[IMPMONE]
           ,[INTERES]
           ,[COMISION]
           ,[CUOTAS]
           ,[CUOTA]
           ,[PLAN_C]
           ,[BONO]
           ,[IMPBONO]
           ,[VALRET]
           ,[TCUPONES]
           ,[ITEM_MADRE]
           ,[NPROCESO]
           ,[LOTE]
           ,[CODOPER]
           ,[SIGNO]
           ,[FINANCIA]
           ,[CAMPO]
           ,[IVA]
           ,[IMPIVA]
           ,[TASAIVA])

     SELECT top 500000 [INGRESADO]
      ,[PPAIS]
      ,[COD1_CLI]
      ,[COD2_CLI]
      ,[COD3_CLI]
      ,[COD4_CLI]
      ,[TIPO_DOC]
      ,[MOTIVO]
      ,[FECHA]
      ,[PAISCO]
      ,[IMPORTE]
      ,[COMERCIO]
      ,[SUCURSAL]
      ,[NROCUPON]
      ,[AUTORIZA]
      ,[BOLETA]
      ,[PRESENTA]
      ,[RESUMEN]
      ,[TIPOPAGO]
      ,[MONEDA]
      ,[IMPMONE]
      ,[INTERES]
      ,[COMISION]
      ,[CUOTAS]
      ,[CUOTA]
      ,[PLAN_C]
      ,[BONO]
      ,[IMPBONO]
      ,[VALRET]
      ,[TCUPONES]
      ,[ITEM_MADRE]
      ,[NPROCESO]
      ,[LOTE]
      ,[CODOPER]
      ,[SIGNO]
      ,[FINANCIA]
      ,[CAMPO]
      ,[IVA]
      ,[IMPIVA]
      ,[TASAIVA]
  FROM slg.dbo.a_cupusu 



