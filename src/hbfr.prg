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
STATIC nHbFrLibIdle := NIL

CREATE CLASS TFreeReport
   HIDDEN:
   DATA nObjHandle
   DATA lComposite

   METHOD CheckRes(nRes)
   METHOD GetLastError()

   EXPORTED:

   // Create new report object
   // lComposite - if True, a composite report will be created
   METHOD New( lComposite ) CONSTRUCTOR

   // Returns .T. if current report object is composite report
   METHOD IsComposite()


   // Add report object to composite report
   METHOD AddReport( oReport )

   // Remove all reports objects from composite report
   METHOD ClearReports()


   // Loads report from file with cFileName name. File must have .frf extention (FastReport Form).
   METHOD LoadFromFile( cFileName )

   // Saves report to file with cFileName name. File must have .frf extention (FastReport Form).
   METHOD SaveToFile( cFileName )

   // Loads prepared report from file with cFileName name. File must have .frp extention (FastReport Prepared report).
   METHOD LoadPreparedReport( cFileName )

   // Saves prepared report to file with cFileName name. File must have .frp extetion (FastReport Prepared report).
   METHOD SavePreparedReport( cFileName )

   // Load report from string. Helpful when loading a report from database
   METHOD LoadFromMemory( cData )


   // Add value to report
   // cValueName - name of value
   // xValue - value (can be numeric, string, memo, date, timestamp, boolean)
   // lIsVariable - if true value will be assigned to FreeReport variable
   METHOD AddValue( cValueName, xValue, lIsVariable )

   // Add row to dataset
   // cTableName - dataset name - string
   // aValues - array with values or hash table
   // aNames - if aValues is simple array then aNames is string array containing column names
   METHOD AddRow( cTableName, aValues, aNames )

   // Register simple dataset
   METHOD AddDataset( cDatasetName )

   // Register harbour dataset - dataset controled by harbour side expressions or functions
   // cDatasetName - name of dataset
   // cExprCheckEOF - expression that checks end of data, should return true or false
   // cExprFirst - expression, go to first record
   // cExprNext - expression, skip to next record
   METHOD AddHbDataset( cDatasetName, cExprCheckEOF, cExprFirst, cExprNext )

   // Return row count of dataset - only for simple datasets, where data is added by AddRow
   METHOD RowCount( cTableName )

   // Remove all datasets and clear data
   METHOD ClearData()


   // Builds and shows report in preview window.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   // Prepared report clears after closing preview window.
   METHOD ShowReport()

   // Runs report designer.
   METHOD DesignReport()

   // Starts report building process. If user interrupts it, returns False.
   METHOD PrepareReport()

   // Shows prepared report in preview window.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   // Prepared report clears after closing preview window.
   METHOD ShowPreparedReport()

   // Edits page of prepared report. If no designer in compliled project, does nothing.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   METHOD EditPreparedReport( nPageIndex )

   // Prints prepared report. Printed pages taken from cPages string,
   // which can contains page numbers separated by comma, or page ranges (for example, "1,3,5-12").
   // If this string is empty, prints all pages.
   // nCopies parameter sets number of copies to print.
   // Assumed that report was builded with PrepareReport method or loaded from file by LoadPreparedReport method.
   METHOD PrintPreparedReport( cPages, nCopies )


   // Close preview window
   METHOD ClosePreview()

   // Returns true if preview window is visible
   METHOD IsPreviewVisible()


   // Set printer name
   METHOD SetPrinter( cPrinterName )


   // Get number of pages
   METHOD GetPageCount()

   // Set page margins
   METHOD SetMargins( nPage, nLeft, nRight, nTop, nBottom )

   DESTRUCTOR FreeFR()

   // Report title - shows in preview and progress windows and assigns to the printed job.
   // string
   ACCESS Title METHOD GetTitle
   ASSIGN Title METHOD SetTitle

   // Initial zoom for preview
   // numeric
   ACCESS InitialZoom METHOD GetInitialZoom
   ASSIGN InitialZoom METHOD SetInitialZoom

   // If this property is True, Preview window will has grayed buttons.
   // If your project contains report designer, you can set this option in "Designer options" dialog.
   // boolean
   ACCESS GrayedButtons METHOD GetGrayedButtons
   ASSIGN GrayedButtons METHOD SetGrayedButtons

   // Allows to modify prepared report
   // boolean
   ACCESS ModifyPrepared METHOD GetModifyPrepared
   ASSIGN ModifyPrepared METHOD SetModifyPrepared

   // Report type. Can be set to HBFR_RT_MULTIPLE or HBFR_RT_SIMPLE (default).
   // Multiple report uses Dataset property to build report for each record in this dataset.
   // numeric
   ACCESS ReportType METHOD GetReportType
   ASSIGN ReportType METHOD SetReportType

   // If True, shows progress window when building, printing or exporting report.
   // boolean
   ACCESS ShowProgress METHOD GetShowProgress
   ASSIGN ShowProgress METHOD SetShowProgress

   // If True, build the report twice, inserting total number of pages on the second run
   // boolean
   ACCESS DoublePass METHOD GetDoublePass
   ASSIGN DoublePass METHOD SetDoublePass

   // If this property is True, Preview window will be modal. Non-modal preview allows you to open several previews.
   // boolean
   ACCESS ModalPreview METHOD GetModalPreview
   ASSIGN ModalPreview METHOD SetModalPreview

   // expression that will be executed after closing preview window
   // string
   ACCESS OnClosePreview METHOD GetOnClosePreview
   ASSIGN OnClosePreview METHOD SetOnClosePreview

ENDCLASS

FUNCTION hbfr_IsInt(nVal)
   RETURN (Abs(nVal) - Int(Abs(nVal))) == 0

FUNCTION hbfr_ProcessMessages()
   LOCAL nRes
   IF nHbFrLibHandle == NIL
      RETURN -1
   ENDIF
   nRes := hb_DynCall({'hbfr_ProcessMessages', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL)})
   RETURN nRes

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
   RETURN .F.

FUNCTION hbfr_FreeLibrary()
   IF nHbFrLibIdle != NIL
      hb_idleDel(nHbFrLibIdle)
      nHbFrLibIdle := NIL
   ENDIF
   IF nHbFrLibHandle != NIL
      hb_libFree(nHbFrLibHandle)
      nHbFrLibHandle := NIL
   ENDIF
   RETURN NIL

METHOD CheckRes(nRes) CLASS TFreeReport
   LOCAL cErrMsg, oErr
   IF nRes < 0
      DO CASE
         CASE nRes == -1
            cErrMsg := 'Invalid object handle'
         CASE nRes == -2 .OR. nRes == -3
            cErrMsg := ::GetLastError()
         CASE nRes == -4
            cErrMsg := 'Invalid argument type'
         CASE nRes == -5
            cErrMsg := 'Can not load library ' + HBFR_LIB_NAME
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
   RETURN NIL

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

METHOD FreeFR() CLASS TFreeReport
   RETURN hb_DynCall({'hbfr_Free', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle)

METHOD IsComposite() CLASS TFreeReport
   RETURN ::lComposite

METHOD AddReport(oReport) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_AddReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle, oReport:nObjHandle))
   RETURN NIL

METHOD ClearReports() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ClearReports', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN NIL

METHOD LoadFromFile(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_LoadFromFile', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN NIL

METHOD SaveToFile(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SaveToFile', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN NIL

METHOD LoadPreparedReport(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_LoadPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN NIL

METHOD SavePreparedReport(cFileName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SavePreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cFileName))
   RETURN NIL

METHOD LoadFromMemory(cData) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_LoadFromMemory', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR + HB_DYN_ENC_RAW, HB_DYN_CTYPE_INT}, ::nObjHandle, cData, Len(cData)))
   RETURN NIL

METHOD AddValue(cValueName, xValue, lIsVariable) CLASS TFreeReport
   LOCAL cParamType, nRet
   hb_default(@lIsVariable, .F.)
   cParamType := ValType(xValue)
   DO CASE
      CASE cParamType == 'C' .OR. cParamType == 'M'
         nRet := hb_DynCall({'hbfr_AddValueC', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
            HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, lIsVariable, cValueName, xValue)
      CASE cParamType == 'N'
         IF hbfr_IsInt(xValue)
            nRet := hb_DynCall({'hbfr_AddValueNI', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
               HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT}, ::nObjHandle, lIsVariable, cValueName, xValue)
         ELSE
            nRet := hb_DynCall({'hbfr_AddValueNF', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
               HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_DOUBLE}, ::nObjHandle, lIsVariable, cValueName, xValue)
         ENDIF
      CASE cParamType == 'D'
         nRet := hb_DynCall({'hbfr_AddValueD', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
            HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT}, ::nObjHandle, lIsVariable,;
            cValueName, Year(xValue), Month(xValue), Day(xValue))
      CASE cParamType == 'L'
         nRet := hb_DynCall({'hbfr_AddValueL', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
            HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lIsVariable, cValueName, xValue)
      OTHERWISE
         nRet := -4
   ENDCASE
   ::CheckRes(nRet)
   RETURN NIL

METHOD AddRow(cTableName, aValues, aNames) CLASS TFreeReport
   LOCAL nRow, i, cVType
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
   RETURN NIL

METHOD AddDataset(cDatasetName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_AddDataset', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cDatasetName))
   RETURN NIL

METHOD AddHbDataset(cDatasetName, cExprCheckEOF, cExprFirst, cExprNext) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_AddHbDataset', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_CHAR_PTR},;
	  ::nObjHandle, cDatasetName, cExprCheckEOF, cExprFirst, cExprNext))
   RETURN NIL

METHOD RowCount(cTableName) CLASS TFreeReport
   LOCAL nRes
   nRes := hb_DynCall({'hbfr_GetRowCount', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cTableName)
   IF nRes < 0
      ::CheckRes(nRes)
   ENDIF
   RETURN nRes

METHOD ClearData() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ClearData', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN NIL

METHOD ShowReport()CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ShowReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN NIL

METHOD DesignReport() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_DesignReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN NIL

METHOD PrepareReport() CLASS TFreeReport
   LOCAL nRes, lRes
   nRes := hb_DynCall({'hbfr_PrepareReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle)
   SWITCH nRes
   CASE 0
      lRes := .T.
      EXIT
   CASE -3
      lRes := .F.
      EXIT
   OTHERWISE
      lRes := .F.
      ::CheckRes(nRes)
      EXIT
   ENDSWITCH
   RETURN lRes

METHOD ShowPreparedReport() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ShowPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN NIL

METHOD EditPreparedReport(nPageIndex) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_EditPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, nPageIndex))
   RETURN NIL

METHOD PrintPreparedReport(cPages, nCopies) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_PrintPreparedReport', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR, HB_DYN_CTYPE_INT}, ::nObjHandle, cPages, nCopies))
   RETURN NIL

METHOD ClosePreview() CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_ClosePreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED}, ::nObjHandle))
   RETURN NIL

METHOD IsPreviewVisible() CLASS TFreeReport
   LOCAL lGButt := .F.
   ::CheckRes(hb_DynCall({'hbfr_IsPreviewVisible', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lGButt))
   RETURN lGButt


METHOD SetPrinter(cPrinterName) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetPrinter', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL), ;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cPrinterName))
   RETURN NIL

METHOD GetPageCount() CLASS TFreeReport
   LOCAL nPages := 0
   ::CheckRes(hb_DynCall({'hbfr_GetPageCount', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, @nPages))
   RETURN nPages

METHOD SetMargins( nPage, nLeft, nRight, nTop, nBottom ) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetMargins', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT, HB_DYN_CTYPE_INT},;
      ::nObjHandle, nPage, nLeft, nRight, nTop, nBottom))
   RETURN NIL

METHOD GetTitle() CLASS TFreeReport
   LOCAL cTitle := Space(255)
   ::CheckRes(hb_DynCall({'hbfr_GetTitle', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR + HB_DYC_OPT_NULLTERM}, ::nObjHandle, @cTitle))
   RETURN cTitle

METHOD SetTitle(cTitle) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetTitle', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cTitle))
   RETURN NIL

METHOD GetInitialZoom() CLASS TFreeReport
   LOCAL nZoom := 0
   ::CheckRes(hb_DynCall({'hbfr_GetInitialZoom', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, @nZoom))
   RETURN nZoom

METHOD SetInitialZoom(nZoom) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetInitialZoom', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, nZoom))
   RETURN NIL

METHOD GetGrayedButtons() CLASS TFreeReport
   LOCAL lGButt := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetGrayedButtons', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lGButt))
   RETURN lGButt

METHOD SetGrayedButtons(lGButt) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetGrayedButtons', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lGButt))
   RETURN NIL

METHOD GetModifyPrepared() CLASS TFreeReport
   LOCAL lMPrep := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetModifyPrepared', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lMPrep))
   RETURN lMPrep

METHOD SetModifyPrepared(lMPrep) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetModifyPrepared', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lMPrep))
   RETURN NIL

METHOD GetReportType() CLASS TFreeReport
   LOCAL nRepTyp := 0
   ::CheckRes(hb_DynCall({'hbfr_GetReportType', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, @nRepTyp))
   RETURN nRepTyp

METHOD SetReportType(nRepTyp) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetReportType', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_INT}, ::nObjHandle, nRepTyp))
   RETURN NIL

METHOD GetShowProgress() CLASS TFreeReport
   LOCAL lProg := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetShowProgress', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lProg))
   RETURN lProg

METHOD SetShowProgress(lProg) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetShowProgress', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lProg))
   RETURN NIL

METHOD GetDoublePass() CLASS TFreeReport
   LOCAL lVal := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetDoublePass', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lVal))
   RETURN lVal

METHOD SetDoublePass(lVal) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetDoublePass', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lVal))
   RETURN NIL

METHOD GetModalPreview() CLASS TFreeReport
   LOCAL lVal := .F.
   ::CheckRes(hb_DynCall({'hbfr_GetModalPreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, @lVal))
   RETURN lVal

METHOD SetModalPreview(lVal) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetModalPreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_BOOL}, ::nObjHandle, lVal))
   RETURN NIL

METHOD GetOnClosePreview() CLASS TFreeReport
   LOCAL cTitle := Space(255)
   ::CheckRes(hb_DynCall({'hbfr_GetOnClosePreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR + HB_DYC_OPT_NULLTERM}, ::nObjHandle, @cTitle))
   RETURN cTitle

METHOD SetOnClosePreview(cTitle) CLASS TFreeReport
   ::CheckRes(hb_DynCall({'hbfr_SetOnClosePreview', nHbFrLibHandle, hb_bitOr(HB_DYN_CTYPE_INT, HB_DYN_CALLCONV_STDCALL),;
      HB_DYN_CTYPE_INT_UNSIGNED, HB_DYN_CTYPE_CHAR_PTR}, ::nObjHandle, cTitle))
   RETURN NIL

FUNCTION hbfr_Eval(cExpr, p1, p2, p3, p4, p5)
   RETURN Eval(&(cExpr), p1, p2, p3, p4, p5)

FUNCTION hbfr_Exec(cExpr)
   RETURN &cExpr

PROCEDURE hbfr_SetErrorBlock()
   ErrorBlock({|oE|Break(oE)})
   RETURN

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbvm.h"
#include "hbdate.h"
#include "hbxvm.h"
#include "hbstack.h"
#include "hbapierr.h"

struct ExpPasFunc {
   void* Thb_dynsymFindName;
   void* Thb_dynsymSymbol;
   void* Thb_vmPushSymbol;
   void* Thb_vmPushNil;
   void* Thb_vmPushString;
   void* Thb_vmPushNumber;
   void* Thb_vmPushLogical;
   void* Thb_vmPushDate;
   void* Thb_vmPushItemRef;
   void* Thb_vmPushTimeStamp;
   void* Thb_vmFunction;
   void* Thb_vmDo;
   void* Thb_vmRequestReenter;
   void* Thb_vmRequestRestore;
   void* Thb_vmRequestQuery;
   void* Thb_parinfo;
   void* Thb_parc;
   void* Thb_parclen;
   void* Thb_parl;
   void* Thb_pardl;
   void* Thb_parnd;
   void* Thb_parni;
   void* Thb_partd;
   void* Thb_dateDecode;
   void* Thb_dateEncode;
   void* Thb_timeStampUnpackDT;
   void* Thb_timeStampPack;
   void* Thb_timeStampUnpack;
   void* Thb_xvmSeqBegin;
   void* Thb_xvmSeqEnd;
   void* Thb_xvmSeqRecover;
   void* Thb_xvmSeqEndTest;
   void* Thb_stackPop;
   void* Thb_errorBlock;
   void* Thb_itemRelease;
   void* Thb_itemClone;
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
   sExtPasFunc.Thb_vmPushItemRef = &hb_vmPushItemRef;
   sExtPasFunc.Thb_vmPushTimeStamp = &hb_vmPushTimeStamp;
   sExtPasFunc.Thb_vmFunction = &hb_vmFunction;
   sExtPasFunc.Thb_vmDo = &hb_vmDo;
   sExtPasFunc.Thb_vmRequestReenter = &hb_vmRequestReenter;
   sExtPasFunc.Thb_vmRequestRestore = &hb_vmRequestRestore;
   sExtPasFunc.Thb_vmRequestQuery = &hb_vmRequestQuery;
   sExtPasFunc.Thb_parinfo = &hb_parinfo;
   sExtPasFunc.Thb_parc = &hb_parc;
   sExtPasFunc.Thb_parclen = &hb_parclen;
   sExtPasFunc.Thb_parl = &hb_parl;
   sExtPasFunc.Thb_pardl = &hb_pardl;
   sExtPasFunc.Thb_parnd = &hb_parnd;
   sExtPasFunc.Thb_parni = &hb_parni;
   sExtPasFunc.Thb_partd = &hb_partd;
   sExtPasFunc.Thb_dateDecode = &hb_dateDecode;
   sExtPasFunc.Thb_dateEncode = &hb_dateEncode;
   sExtPasFunc.Thb_timeStampUnpackDT = &hb_timeStampUnpackDT;
   sExtPasFunc.Thb_timeStampPack = &hb_timeStampPack;
   sExtPasFunc.Thb_timeStampUnpack = &hb_timeStampUnpack;
   sExtPasFunc.Thb_xvmSeqBegin = &hb_xvmSeqBegin;
   sExtPasFunc.Thb_xvmSeqEnd = &hb_xvmSeqEnd;
   sExtPasFunc.Thb_xvmSeqRecover = &hb_xvmSeqRecover;
   sExtPasFunc.Thb_xvmSeqEndTest = &hb_xvmSeqEndTest;
   sExtPasFunc.Thb_stackPop = &hb_stackPop;
   sExtPasFunc.Thb_errorBlock = &hb_errorBlock;
   sExtPasFunc.Thb_itemRelease = &hb_itemRelease;
   sExtPasFunc.Thb_itemClone = &hb_itemClone;
   hb_retptr( &sExtPasFunc );
}

#pragma ENDDUMP