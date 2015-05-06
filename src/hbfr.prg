/* hbfr - FreeReport for (x)Harbour

  Copyright (C) 2015 Michal Gawrycki info..gmsystems.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "hbclass.ch"
#include "hbdyn.ch"
#include "hbfr.ch"
#include "hbgtinfo.ch"

STATIC nHbFrLibHandle := NIL

CREATE CLASS TFreeReport
   HIDDEN:
   DATA nObjHandle
   DATA lComposite

   METHOD CheckRes(nRes)
   METHOD GetLastError()

   EXPORTED:
   METHOD New(lComposite) CONSTRUCTOR
   METHOD Free()
   METHOD IsComposite()

   METHOD AddReport(oReport)
   METHOD ClearReports()

   METHOD LoadFromFile(cFileName)
   METHOD SaveToFile(cFileName)
   METHOD LoadPreparedReport(cFileName)
   METHOD LoadFromMemory(cData)

   METHOD AddValue(cValueName, xValue)
   METHOD AddRow(cTableName, aValues, aNames)
   METHOD AddDataset(cDatasetName)
   METHOD AddHbDataset(cDatasetName, cExprCheckEOF, cExprFirst, cExprNext)
   METHOD RowCount(cValueName)
   METHOD ClearData()

   METHOD ShowReport()
   METHOD DesignReport()
   METHOD PrepareReport()
   METHOD ShowPreparedReport()
   METHOD EditPreparedReport(nPageIndex)
   METHOD PrintPreparedReport(cPages, nCopies)

   METHOD SetPrinter(cPrinterName)

   DESTRUCTOR Free()

   ACCESS Title METHOD GetTitle
   ASSIGN Title METHOD SetTitle
   ACCESS InitialZoom METHOD GetInitialZoom
   ASSIGN InitialZoom METHOD SetInitialZoom
   ACCESS GrayedButtons METHOD GetGrayedButtons
   ASSIGN GrayedButtons METHOD SetGrayedButtons
   ACCESS ModifyPrepared METHOD GetModifyPrepared
   ASSIGN ModifyPrepared METHOD SetModifyPrepared
   ACCESS ReportType METHOD GetReportType
   ASSIGN ReportType METHOD SetReportType
   ACCESS ShowProgress METHOD GetShowProgress
   ASSIGN ShowProgress METHOD SetShowProgress
   ACCESS DoublePass METHOD GetDoublePass
   ASSIGN DoublePass METHOD SetDoublePass
   ACCESS ModalPreview METHOD GetModalPreview
   ASSIGN ModalPreview METHOD SetModalPreview

ENDCLASS

FUNCTION hbfr_IsInt(nVal)
   RETURN (Abs(nVal) - Int(Abs(nVal))) == 0

FUNCTION hbfr_ProcessMessages()
   LOCAL nRes
   IF nHbFrLibHandle == NIL
      RETURN -1
   ENDIF
   nRes := hb_DynCall({'hbfr_ProcessMessages', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL)})
   RETURN

FUNCTION hbfr_LoadLibrary(cLibName, lOemConvert)
   LOCAL nRes
   IF nHbFrLibHandle != NIL
      RETURN .T.
   ENDIF
   IF Empty(cLibName)
      cLibName := HBFR_LIB_NAME
   ENDIF
   IF Empty(lOemConvert)
      lOemConvert := .F.
   ENDIF
   nHbFrLibHandle := hb_libLoad(cLibName)
   IF nHbFrLibHandle == NIL
      RETURN .F.
   ELSE
      nRes := hb_DynCall({'hbfr_Init', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
         HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_VOID_PTR}, lOemConvert, GetHbPasFuncs())
      IF nRes != 0
         nHbFrLibHandle := NIL
         RETURN .F.
      ELSE
         hb_IdleAdd({|| hbfr_ProcessMessages() })
         RETURN .T.
      ENDIF
   ENDIF

METHOD CheckRes(nRes) CLASS TFreeReport
   LOCAL cErrMsg := '', oErr
   IF nRes < 0
      DO CASE
         CASE nRes == -1
            cErrMsg := 'Invalid object handle'
         CASE nRes == -2 .OR. nRes == -3
            cErrMsg := ::GetLastError()
         CASE nRes == -4
            cErrMsg := 'Invalid parametr type'
         CASE nRes == -5
            cErrMsg := 'Can not load ' + HBFR_LIB_NAME
         CASE nRes == -6
            cErrMsg := 'Can not create report object'
         OTHERWISE
            cErrMsg := 'Unknown error'
      ENDCASE
      oErr := ErrorNew()
      oErr:genCode := nRes
      oErr:description := cErrMsg
      Break oErr
   ENDIF

METHOD GetLastError() CLASS TFreeReport
   LOCAL nRes, cError := Space(255)
   nRes := hb_DynCall({'hbfr_GetErrorMsg', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, @cError)
   IF nRes = 0
      RETURN cError
   ENDIF
   RETURN ''

METHOD New(lComposite) CLASS TFreeReport
   IF !hbfr_LoadLibrary()
      ::CheckRes(-5)
   ENDIF
   IF Empty(lComposite)
      lComposite := .F.
   ENDIF
   ::lComposite := lComposite
   ::nObjHandle := hb_DynCall({'hbfr_New', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CALLCONV_STDCALL),;
     HB_DYN_CTYPE_BOOL}, lComposite)
   IF ::nObjHandle == 0
      ::CheckRes(-6)
   ENDIF
   RETURN Self

METHOD Free() CLASS TFreeReport
   RETURN hb_DynCall({'hbfr_Free', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle)

METHOD IsComposite() CLASS TFreeReport
   RETURN lComposite

METHOD AddReport(oReport) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_AddReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle, oReport:nObjHandle))
   RETURN

METHOD ClearReports() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ClearReports', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN

METHOD LoadFromFile(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_LoadFromFile', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN

METHOD SaveToFile(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SaveToFile', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN

METHOD LoadPreparedReport(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_LoadPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN

METHOD LoadFromMemory(cData) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_LoadFromMemory', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR + HB_DYN_ENC_RAW, HB_DYN_CTYPE_INT}, ::nObjHandle, cData, Len(cData)))
   RETURN

METHOD AddValue(cValueName, xValue, aNames) CLASS TFreeReport
   LOCAL cParamType, nRet
   cParamType := ValType(xValue)
   DO CASE
      CASE cParamType == 'C' .OR. cParamType == 'M'
         nRet := hb_DynCall({'hbfr_AddValueC', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
            HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cValueName, xValue)
      CASE cParamType == 'N'
         IF hbfr_IsInt(xValue)
            nRet := hb_DynCall({'hbfr_AddValueNI', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
               HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT}, ::nObjHandle, cValueName, xValue)
         ELSE
            nRet := hb_DynCall({'hbfr_AddValueNF', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
               HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_DOUBLE}, ::nObjHandle, cValueName, xValue)
         ENDIF
      CASE cParamType == 'D'
         nRet := hb_DynCall({'hbfr_AddValueD', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
            HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT}, ::nObjHandle,;
            cValueName, Year(xValue), Month(xValue), Day(xValue))
      CASE cParamType == 'L'
         nRet := hb_DynCall({'hbfr_AddValueL', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
            HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_BOOL}, ::nObjHandle, cValueName, xValue)
      OTHERWISE
         nRet := -4
   ENDCASE
   ::CheckRes(nRet)
   RETURN

METHOD AddRow(cTableName, aValues, aNames) CLASS TFreeReport
   LOCAL nRow := 0, i := 0, cVType, cValName
   cVType := ValType(aValues)
   IF cVType == 'H' .AND. aNames == NIL
      aNames := hb_HKeys(aValues)
   ENDIF
   nRow := ::RowCount(cTableName)
   FOR i := 1 TO Len(aValues)
      IF cVType == 'H'
         ::AddValue(cTableName + ':' + AllTrim(Str(nRow)) + ':' + aNames[i], hb_HValueAt(aValues, i))
      ELSE
         ::AddValue(cTableName + ':' + AllTrim(Str(nRow)) + ':' + aNames[i], aValues[i])
      ENDIF
   NEXT
   RETURN

METHOD AddDataset(cDatasetName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_AddDataset', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cDatasetName))
   RETURN

METHOD AddHbDataset(cDatasetName, cExprCheckEOF, cExprFirst, cExprNext) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_AddHbDataset', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR},;
	  ::nObjHandle, cDatasetName, cExprCheckEOF, cExprFirst, cExprNext))
   RETURN

METHOD RowCount(cTableName) CLASS TFreeReport
   LOCAL nRes := 0
   nRes := hb_DynCall({'hbfr_GetRowCount', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cTableName)
   IF nRes < 0
      ::CheckRes(nRes)
   ENDIF
   RETURN nRes

METHOD ClearData() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ClearData', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN

METHOD ShowReport()CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ShowReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN

METHOD DesignReport() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_DesignReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN

METHOD PrepareReport() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_PrepareReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN

METHOD ShowPreparedReport() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ShowPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN

METHOD EditPreparedReport(nPageIndex) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_EditPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, nPageIndex))
   RETURN

METHOD PrintPreparedReport(cPages, nCopies) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_PrintPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT}, ::nObjHandle, cPages, nCopies))
   RETURN

METHOD SetPrinter(cPrinterName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetPrinter', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cPrinterName))
   RETURN

METHOD GetTitle() CLASS TFreeReport
   LOCAL cTitle := Space(255)
   ::CheckRes(hb_DynCall({'hbfr_GetTitle', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR + HB_DYC_OPT_NULLTERM}, ::nObjHandle, @cTitle))
   RETURN cTitle

METHOD SetTitle(cTitle) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetTitle', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cTitle))
   RETURN

METHOD GetInitialZoom() CLASS TFreeReport
   LOCAL nZoom := 0
   ::CheckRes(hb_DynCall({'hbfr_GetInitialZoom', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, @nZoom))
   RETURN nZoom

METHOD SetInitialZoom(nZoom) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetInitialZoom', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, nZoom))
   RETURN

METHOD GetGrayedButtons() CLASS TFreeReport
   LOCAL lGButt := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetGrayedButtons', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lGButt))
   RETURN lGButt

METHOD SetGrayedButtons(lGButt) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetGrayedButtons', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lGButt))
   RETURN

METHOD GetModifyPrepared() CLASS TFreeReport
   LOCAL lMPrep := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetModifyPrepared', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lMPrep))
   RETURN lMPrep

METHOD SetModifyPrepared(lMPrep) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetModifyPrepared', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lMPrep))
   RETURN

METHOD GetReportType() CLASS TFreeReport
   LOCAL nRepTyp := 0
   ::CheckRes(hb_DynCall({'hbfr_GetReportType', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, @nRepTyp))
   RETURN nRepTyp

METHOD SetReportType(nRepTyp) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetReportType', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, nRepTyp))
   RETURN

METHOD GetShowProgress() CLASS TFreeReport
   LOCAL lProg := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetShowProgress', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lProg))
   RETURN lProg

METHOD SetShowProgress(lProg) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetShowProgress', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lProg))
   RETURN

METHOD GetDoublePass() CLASS TFreeReport
   LOCAL lVal := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetDoublePass', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lVal))
   RETURN lProg

METHOD SetDoublePass(lVal) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetDoublePass', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lVal))
   RETURN

METHOD GetModalPreview() CLASS TFreeReport
   LOCAL lVal := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetModalPreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lVal))
   RETURN lProg

METHOD SetModalPreview(lVal) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetModalPreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lVal))
   RETURN
   
FUNCTION hbfr_Eval(cExpr, p1, p2, p3, p4, p5)
   RETURN Eval(&(cExpr), p1, p2, p3, p4, p5)

FUNCTION hbfr_Exec(cExpr)
   RETURN &cExpr

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbvm.h"
#include "hbdate.h"

struct ExpPasFunc {
   void* Thb_dynsymFindName;
   void* Thb_dynsymSymbol;
   void* Thb_vmPushSymbol;
   void* Thb_vmPushNil;
   void* Thb_vmPushString;
   void* Thb_vmPushNumber;
   void* Thb_vmPushLogical;
   void* Thb_vmPushDate;
   void* Thb_vmFunction;
   void* Thb_parinfo;
   void* Thb_parc;
   void* Thb_parclen;
   void* Thb_parl;
   void* Thb_pardl;
   void* Thb_parnd;
   void* Thb_parni;
   void* Thb_dateDecode;
   void* Thb_dateEncode;
};

static struct ExpPasFunc sExtPasFunc;

HB_FUNC( GETHBPASFUNCS )
{
   sExtPasFunc.Thb_dynsymFindName = &hb_dynsymFindName;
   sExtPasFunc.Thb_dynsymSymbol = &hb_dynsymSymbol;
   sExtPasFunc.Thb_vmPushSymbol = &hb_vmPushSymbol;
   sExtPasFunc.Thb_vmPushNil = &hb_vmPushNil;
   sExtPasFunc.Thb_vmPushString = &hb_vmPushString;
   sExtPasFunc.Thb_vmPushNumber = &hb_vmPushNumber;
   sExtPasFunc.Thb_vmPushLogical = &hb_vmPushLogical;
   sExtPasFunc.Thb_vmPushDate = &hb_vmPushDate;
   sExtPasFunc.Thb_vmFunction = &hb_vmFunction;
   sExtPasFunc.Thb_parinfo = &hb_parinfo;
   sExtPasFunc.Thb_parc = &hb_parc;
   sExtPasFunc.Thb_parclen = &hb_parclen;
   sExtPasFunc.Thb_parl = &hb_parl;
   sExtPasFunc.Thb_pardl = &hb_pardl;
   sExtPasFunc.Thb_parnd = &hb_parnd;
   sExtPasFunc.Thb_parni = &hb_parni;
   sExtPasFunc.Thb_dateDecode = &hb_dateDecode;
   sExtPasFunc.Thb_dateEncode = &hb_dateEncode;
   hb_retptr( &sExtPasFunc );
}

#pragma ENDDUMP