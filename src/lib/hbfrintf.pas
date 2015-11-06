{ hbfr - FreeReport for (x)Harbour

  Copyright (C) 2015 Micha³ Gawrycki info..gmsystems.pl

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
}

unit hbfrintf;

interface

uses
  Classes, hbfrclass;

type
  Thb_dynsymFindName = function(AName: PChar): Pointer; cdecl;
  Thb_dynsymSymbol = function(ASym: Pointer): Pointer; cdecl;
  Thb_vmPushSymbol = procedure(ASym: Pointer); cdecl;
  Thb_vmPushNil = procedure; cdecl;
  Thb_vmPushString = procedure(AStr: PChar; ALen: LongInt); cdecl;
  Thb_vmPushNumber = procedure(AVal: Double; ADec: LongInt); cdecl;
  Thb_vmPushLogical = procedure(AVal: LongBool); cdecl;
  Thb_vmPushDate = procedure(AVal: LongInt); cdecl;
  Thb_vmPushItemRef = procedure(AItem: Pointer); cdecl;
  Thb_vmPushTimeStamp = procedure(AJulian, AMSec: LongInt); cdecl;
  Thb_vmFunction = procedure(AVal: Word); cdecl;
  Thb_vmDo = procedure(AVal: Word); cdecl;
  Thb_vmRequestReenter = function: LongBool; cdecl;
  Thb_vmRequestRestore = procedure; cdecl;
  Thb_vmRequestQuery = function: Word; cdecl;
  Thb_parinfo = function(AVal: LongInt): LongWord; cdecl;
  Thb_parc = function(AVal: LongInt): PChar; cdecl;
  Thb_parclen = function(AVal: LongInt): LongInt; cdecl;
  Thb_parl = function(AVal: LongInt): LongBool; cdecl;
  Thb_pardl = function(AVal: LongInt): LongInt; cdecl;
  Thb_parnd = function(AVal: LongInt): Double; cdecl;
  Thb_parni = function(AVal: LongInt): LongInt; cdecl;
  Thb_partd = function(AVal: LongInt): Double; cdecl;
  Thb_dateDecode = procedure(AVal: LongInt; var Y, M, D: LongInt); cdecl;
  Thb_dateEncode = function(Y, M, D: LongInt): LongInt; cdecl;
  Thb_timeStampUnpackDT = procedure(ATimeStamp: Double; var Yulian, MiliSec: Longint); cdecl;
  Thb_timeStampPack = function(Y, M, D, H, Mi, S, MS: Integer): Double; cdecl;
  Thb_timeStampUnpack = procedure(ATimeStamp: Double; var Y, M, D, H, Mi, S, MS: Integer); cdecl;
  Thb_xvmSeqBegin = procedure; cdecl;
  Thb_xvmSeqEnd = function: LongBool; cdecl;
  Thb_xvmSeqRecover = function: LongBool; cdecl;
  Thb_xvmSeqEndTest = function: LongBool; cdecl;
  Thb_stackPop = procedure; cdecl;
  Thb_errorBlock = function: Pointer; cdecl;
  Thb_itemRelease = function(AItem: Pointer): LongBool; cdecl;
  Thb_itemClone = function(AItem: Pointer): Pointer; cdecl;

  PHbFunctions = ^THbFunctions;
  THbFunctions = packed record
    hb_dynsymFindName: Thb_dynsymFindName;
    hb_dynsymSymbol: Thb_dynsymSymbol;
    hb_vmPushSymbol: Thb_vmPushSymbol;
    hb_vmPushNil: Thb_vmPushNil;
    hb_vmPushString: Thb_vmPushString;
    hb_vmPushNumber: Thb_vmPushNumber;
    hb_vmPushLogical: Thb_vmPushLogical;
    hb_vmPushDate: Thb_vmPushDate;
    hb_vmPushItemRef: Thb_vmPushItemRef;
    hb_vmPushTimeStamp: Thb_vmPushTimeStamp;
    hb_vmFunction: Thb_vmFunction;
    hb_vmDo: Thb_vmDo;
    hb_vmRequestReenter: Thb_vmRequestReenter;
    hb_vmRequestRestore: Thb_vmRequestRestore;
    hb_vmRequestQuery: Thb_vmRequestQuery;
    hb_parinfo: Thb_parinfo;
    hb_parc: Thb_parc;
    hb_parclen: Thb_parclen;
    hb_parl: Thb_parl;
    hb_pardl: Thb_pardl;
    hb_parnd: Thb_parnd;
    hb_parni: Thb_parni;
    hb_partd: Thb_partd;
    hb_dateDecode: Thb_dateDecode;
    hb_dateEncode: Thb_dateEncode;
    hb_timeStampUnpackDT: Thb_timeStampUnpackDT;
    hb_timeStampPack: Thb_timeStampPack;
    hb_timeStampUnpack: Thb_timeStampUnpack;
    hb_xvmSeqBegin: Thb_xvmSeqBegin;
    hb_xvmSeqEnd: Thb_xvmSeqEnd;
    hb_xvmSeqRecover: Thb_xvmSeqRecover;
    hb_xvmSeqEndTest: Thb_xvmSeqEndTest;
    hb_stackPop: Thb_stackPop;
    hb_errorBlock: Thb_errorBlock;
    hb_itemRelease: Thb_itemRelease;
    hb_itemClone: Thb_itemClone;
  end;

function hbfr_Init(AOemConvert: LongBool; AFunctions: PHbFunctions): Integer; stdcall;
function hbfr_ProcessMessages: Integer; StdCall;

function hbfr_New(AComposite: LongBool): LongWord; stdcall;
function hbfr_Free(AHandle: LongWord): Integer; stdcall;

function hbfr_AddValueC(AHandle: LongWord; AName: PChar; AValue: PChar): Integer; stdcall;
function hbfr_AddValueNI(AHandle: LongWord; AName: PChar; AValue: Integer): Integer; stdcall;
function hbfr_AddValueNF(AHandle: LongWord; AName: PChar; AValue: Double): Integer; stdcall;
function hbfr_AddValueL(AHandle: LongWord; AName: PChar; AValue: LongBool): Integer; stdcall;
function hbfr_AddValueD(AHandle: LongWord; AName: PChar; AYear, AMonth, ADay: Integer): Integer; stdcall;

function hbfr_AddDataset(AHandle: LongWord; AName: PChar): Integer; stdcall;
function hbfr_AddHbDataset(AHandle: LongWord; AName, AExprCheckEof, AExprFirst, AExprNext: PChar): Integer; stdcall;

function hbfr_GetRowCount(AHandle: LongWord; AName: PChar): Integer; stdcall;

function hbfr_ClearData(AHandle: LongWord): Integer; stdcall;

function hbfr_AddReport(AHandle: LongWord; AReport: LongWord): Integer; stdcall;
function hbfr_ClearReports(AHandle: LongWord): Integer; stdcall;

function hbfr_LoadFromFile(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_SaveToFile(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_LoadPreparedReport(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_SavePreparedReport(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_LoadFromMemory(AHandle: LongWord; AData: Pointer; ALength: Integer): Integer; stdcall;

function hbfr_PrepareReport(AHandle: LongWord): Integer; stdcall;
function hbfr_ShowReport(AHandle: LongWord): Integer; stdcall;
function hbfr_ShowPreparedReport(AHandle: LongWord): Integer; stdcall;
function hbfr_PrintPreparedReport(AHandle: LongWord; APages: PChar; ACopies: Integer): Integer; stdcall;
function hbfr_DesignReport(AHandle: LongWord): Integer; stdcall;
function hbfr_EditPreparedReport(AHandle: LongWord; APageIndex: Integer): Integer; stdcall;

function hbfr_ClosePreview(AHandle: LongWord): Integer; stdcall;
function hbfr_IsPreviewVisible(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;

function hbfr_GetTitle(AHandle: LongWord; ATitle: PChar): Integer; stdcall;
function hbfr_SetTitle(AHandle: LongWord; ATitle: PChar): Integer; stdcall;
function hbfr_GetInitialZoom(AHandle: LongWord; var AZoom: Integer): Integer; stdcall;
function hbfr_SetInitialZoom(AHandle: LongWord; AZoom: Integer): Integer; stdcall;
function hbfr_GetGrayedButtons(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
function hbfr_SetGrayedButtons(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
function hbfr_GetModifyPrepared(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
function hbfr_SetModifyPrepared(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
function hbfr_GetReportType(AHandle: LongWord; var AType: Integer): Integer; stdcall;
function hbfr_SetReportType(AHandle: LongWord; AType: Integer): Integer; stdcall;
function hbfr_GetShowProgress(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
function hbfr_SetShowProgress(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
function hbfr_GetDoublePass(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
function hbfr_SetDoublePass(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
function hbfr_GetModalPreview(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
function hbfr_SetModalPreview(AHandle: LongWord; AValue: LongBool): Integer; stdcall;

function hbfr_GetOnClosePreview(AHandle: LongWord; AEvent: PChar): Integer; stdcall;
function hbfr_SetOnClosePreview(AHandle: LongWord; AEvent: PChar): Integer; stdcall;

function hbfr_SetPrinter(AHandle: LongWord; APrinterName: PChar): Integer; stdcall;

function hbfr_GetErrorMsg(AHandle: LongWord; AMessage: PChar): Integer; stdcall;

function HbEval(AExpr: String; AParams: array of const; DoExec: Boolean = False): Variant;

implementation

uses
  SysUtils, Windows, Forms, FR_View, FR_Class, ExtCtrls, Variants, DateUtils;

var
  ReportList: TList;
  DoOemConvert: Boolean;
  HbFunc: THbFunctions;

function OemToStr(ASrc: PChar): String;
var
  PCh: PChar;
begin
  PCh := StrAlloc(StrLen(ASrc) + 1);
  OemToChar(ASrc, PCh);
  Result :=  String(PCh);
end;

function StrToOem(ASrc: String): PChar;
begin
  Result := StrAlloc(Length(ASrc) + 1);
  CharToOem(PChar(ASrc), Result);
end;

function CheckHandle(AHandle: LongWord): Boolean;
begin
  Result := ReportList.IndexOf(Pointer(AHandle)) >= 0;
end;

function hbfr_Init(AOemConvert: LongBool; AFunctions: PHbFunctions): Integer; stdcall;
begin
  try
    Application.Initialize;
    DoOemConvert := AOemConvert;
    HbFunc := AFunctions^; 
    Result := 0;
  except
    Result := -2;
  end;
end;

function hbfr_ProcessMessages: Integer; StdCall;
begin
  try
    Application.ProcessMessages;
    Result := 0;
  except
    Result := -2;
  end;
end;

function hbfr_New(AComposite: LongBool): LongWord; stdcall;
var
  HObj: THBFRObj;
begin
  Result := 0;
  try
    HObj := THBFRObj.CreateC(nil, AComposite);
    ReportList.Add(HObj);
    Result := LongWord(HObj);
  finally

  end;
end;

function hbfr_Free(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      ReportList.Remove(Pointer(AHandle));
      THBFRObj(AHandle).Free;
      Result := 0;
    end
    else
      Result := -1;
  except
    Result := -2;
  end;
end;

function hbfr_AddValueC(AHandle: LongWord; AName: PChar; AValue: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).AddValue(OemToStr(AName), OemToStr(AValue))
      else
        Result := THBFRObj(AHandle).AddValue(String(AName), String(AValue))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddValueNI(AHandle: LongWord; AName: PChar; AValue: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).AddValue(OemToStr(AName), AValue)
      else
        Result := THBFRObj(AHandle).AddValue(String(AName), AValue)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddValueNF(AHandle: LongWord; AName: PChar; AValue: Double): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).AddValue(OemToStr(AName), AValue)
      else
        Result := THBFRObj(AHandle).AddValue(String(AName), AValue)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddValueL(AHandle: LongWord; AName: PChar; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).AddValue(OemToStr(AName), AValue)
      else
        Result := THBFRObj(AHandle).AddValue(String(AName), AValue)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddValueD(AHandle: LongWord; AName: PChar; AYear, AMonth, ADay: Integer): Integer; stdcall;
var
  D: TDateTime;
begin
  try
    if CheckHandle(AHandle) then
      if TryEncodeDate(AYear, AMonth, ADay, D) then
        if DoOemConvert then
          Result := THBFRObj(AHandle).AddValue(OemToStr(AName), D)
        else
          Result := THBFRObj(AHandle).AddValue(String(AName), D)
      else
      begin
        THBFRObj(AHandle).LastErrorMsg := 'Invalid date';
        Result := -3
      end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddDataset(AHandle: LongWord; AName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).AddDataset(OemToStr(AName))
      else
        Result := THBFRObj(AHandle).AddDataset(String(AName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddHbDataset(AHandle: LongWord; AName, AExprCheckEof, AExprFirst, AExprNext: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).AddHbDataset(OemToStr(AName), OemToStr(AExprCheckEof),
          OemToStr(AExprFirst), OemToStr(AExprNext))
      else
        Result := THBFRObj(AHandle).AddHbDataset(String(AName), String(AExprCheckEof),
          String(AExprFirst), String(AExprNext))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetRowCount(AHandle: LongWord; AName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).GetRowCount(OemToStr(AName))
      else
        Result := THBFRObj(AHandle).GetRowCount(String(AName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_ClearData(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).ClearData
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_AddReport(AHandle: LongWord; AReport: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) and CheckHandle(AReport) then
      Result := THBFRObj(AHandle).AddReport(THBFRObj(AReport).Report)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_ClearReports(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).ClearReports
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_LoadFromFile(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).LoadFromFile(OemToStr(AFileName))
      else
        Result := THBFRObj(AHandle).LoadFromFile(String(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SaveToFile(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).SaveToFile(OemToStr(AFileName))
      else
        Result := THBFRObj(AHandle).SaveToFile(String(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_LoadPreparedReport(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).LoadPreparedReport(OemToStr(AFileName))
      else
        Result := THBFRObj(AHandle).LoadPreparedReport(String(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SavePreparedReport(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).SavePreparedReport(OemToStr(AFileName))
      else
        Result := THBFRObj(AHandle).SavePreparedReport(String(AFileName))
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_LoadFromMemory(AHandle: LongWord; AData: Pointer; ALength: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).LoadFromMemory(AData, ALength)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_PrepareReport(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).PrepareReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_ShowReport(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).ShowReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_ShowPreparedReport(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).ShowPreparedReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_PrintPreparedReport(AHandle: LongWord; APages: PChar; ACopies: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      if DoOemConvert then
        Result := THBFRObj(AHandle).PrintPreparedReport(OemToStr(APages), ACopies)
      else
        Result := THBFRObj(AHandle).PrintPreparedReport(String(APages), ACopies)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_DesignReport(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).DesignReport
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_EditPreparedReport(AHandle: LongWord; APageIndex: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
      Result := THBFRObj(AHandle).EditPreparedReport(APageIndex)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_ClosePreview(AHandle: LongWord): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.ClosePreview;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_IsPreviewVisible(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBFRObj(AHandle).Report.IsPreviewVisible;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetTitle(AHandle: LongWord; ATitle: PChar): Integer; stdcall;
var
  S: PChar;
begin
  try
    if CheckHandle(AHandle) then
    begin
      if DoOemConvert then
        S := StrToOem(Copy(THBFRObj(AHandle).Report.Title, 1, 255))
      else
        S := PChar(Copy(THBFRObj(AHandle).Report.Title, 1, 255));
      StrCopy(ATitle, S);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetTitle(AHandle: LongWord; ATitle: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      if DoOemConvert then
        THBFRObj(AHandle).Report.Title := OemToStr(ATitle)
      else
        THBFRObj(AHandle).Report.Title := String(ATitle);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetInitialZoom(AHandle: LongWord; var AZoom: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AZoom := Ord(THBFRObj(AHandle).Report.InitialZoom);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetInitialZoom(AHandle: LongWord; AZoom: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.InitialZoom := TfrPreviewZoom(AZoom);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetGrayedButtons(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBFRObj(AHandle).Report.GrayedButtons;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetGrayedButtons(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.GrayedButtons := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetModifyPrepared(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBFRObj(AHandle).Report.ModifyPrepared;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetModifyPrepared(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.ModifyPrepared := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetReportType(AHandle: LongWord; var AType: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AType := Ord(THBFRObj(AHandle).Report.ReportType);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetReportType(AHandle: LongWord; AType: Integer): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.ReportType := TfrReportType(AType);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetShowProgress(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBFRObj(AHandle).Report.ShowProgress;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetShowProgress(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.ShowProgress := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetDoublePass(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBFRObj(AHandle).Report.DoublePass;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetDoublePass(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.DoublePass := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetModalPreview(AHandle: LongWord; var AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      AValue := THBFRObj(AHandle).Report.ModalPreview;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetModalPreview(AHandle: LongWord; AValue: LongBool): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      THBFRObj(AHandle).Report.ModalPreview := AValue;
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetOnClosePreview(AHandle: LongWord; AEvent: PChar): Integer; stdcall;
var
  S: PChar;
begin
  try
    if CheckHandle(AHandle) then
    begin
      if DoOemConvert then
        S := StrToOem(Copy(THBFRObj(AHandle).OnClosePrev, 1, 255))
      else
        S := PChar(Copy(THBFRObj(AHandle).OnClosePrev, 1, 255));
      StrCopy(AEvent, S);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetOnClosePreview(AHandle: LongWord; AEvent: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      if DoOemConvert then
        THBFRObj(AHandle).OnClosePrev := OemToStr(AEvent)
      else
        THBFRObj(AHandle).OnClosePrev := String(AEvent);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_SetPrinter(AHandle: LongWord; APrinterName: PChar): Integer; stdcall;
begin
  try
    if CheckHandle(AHandle) then
    begin
      if DoOemConvert then
        THBFRObj(AHandle).SetPrinter(OemToStr(APrinterName))
      else
        THBFRObj(AHandle).SetPrinter(String(APrinterName));
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

function hbfr_GetErrorMsg(AHandle: LongWord; AMessage: PChar): Integer; stdcall;
var
  S: PChar;
begin
  try
    if CheckHandle(AHandle) then
    begin
      if DoOemConvert then
        S := StrToOem(Copy(THBFRObj(AHandle).LastErrorMsg, 1, 255))
      else
        S := PChar(Copy(THBFRObj(AHandle).LastErrorMsg, 1, 255));
      StrCopy(AMessage, S);
      Result := 0;
    end
    else
      Result := -1;
  except
    on E: Exception do
    begin
      THBFRObj(AHandle).LastErrorMsg := E.Message;
      Result := -2;
    end;
  end;
end;

const
  HB_IT_INTEGER = 2;
  HB_IT_LONG = 8;
  HB_IT_DOUBLE = $10;
  HB_IT_DATE = $20;
  HB_IT_TIMESTAMP = $40;
  HB_IT_LOGICAL = $80;
  HB_IT_STRING = $400;
  HB_IT_MEMO = $400 or $800;

function HbEval(AExpr: String; AParams: array of const; DoExec: Boolean): Variant;

procedure RestoreErrorBlock(ABlock: Pointer);
begin
  if ABlock <> nil then
  begin
    HbFunc.hb_vmPushSymbol(HbFunc.hb_dynsymSymbol(HbFunc.hb_dynsymFindName(PChar('ERRORBLOCK'))));
    HbFunc.hb_vmPushNil;
    HbFunc.hb_vmPushItemRef(ABlock);
    HbFunc.hb_vmFunction(1);
    HbFunc.hb_itemRelease(ABlock);
  end;
end;

var
  I: Integer;
  S: String;
  V: TVarRec;
  Pc: PChar;
  Y,M,D,H,MN,SC,MS: Integer;
  OldErrorBlock: Pointer;
  FncSym: Pointer;
  TmpY, TmpMS: LongInt;
begin
  Result := Null;
  OldErrorBlock := nil;
  if AExpr = '' then
    Exit;
  if not HbFunc.hb_vmRequestReenter then
    Exit;
  if DoExec then
    S := 'hbfr_Exec'
  else
    S := 'hbfr_Eval';

  OldErrorBlock := HbFunc.hb_itemClone(HbFunc.hb_errorBlock);

  FncSym := HbFunc.hb_dynsymFindName(PChar('hbfr_SetErrorBlock'));
  if FncSym = nil then
  begin
    RestoreErrorBlock(OldErrorBlock);
    raise Exception.Create('Function not found: hbfr_SetErrorBlock');
  end;
  HbFunc.hb_vmPushSymbol(HbFunc.hb_dynsymSymbol(FncSym));
  HbFunc.hb_vmPushNil;
  HbFunc.hb_vmDo(0);

  HbFunc.hb_xvmSeqBegin;

  FncSym := HbFunc.hb_dynsymFindName(PChar(S));
  if FncSym = nil then
  begin
    RestoreErrorBlock(OldErrorBlock);
    raise Exception.Create('Function not found: ' + S);
  end;
  HbFunc.hb_vmPushSymbol(HbFunc.hb_dynsymSymbol(FncSym));
  HbFunc.hb_vmPushNil;
  HbFunc.hb_vmPushString(PChar(AExpr), Length(AExpr));

  if Length(AParams) > 0 then
  begin
    for I := Low(AParams) to High(AParams) do
    begin
      V := AParams[I];
      case V.VType of
        vtInteger: HbFunc.hb_vmPushNumber(V.VInteger, 255);
        vtExtended: HbFunc.hb_vmPushNumber(V.VExtended^, 255);
        vtString: begin
          S := V.VString^;
          if DoOemConvert then
          begin
            Pc := StrToOem(S);
            HbFunc.hb_vmPushString(Pc, StrLen(Pc));
          end
          else
            HbFunc.hb_vmPushString(PChar(S), Length(S));
        end;
        vtAnsiString: begin
          S := AnsiString(V.VAnsiString^);
          if DoOemConvert then
          begin
            Pc := StrToOem(S);
            HbFunc.hb_vmPushString(Pc, StrLen(Pc));
          end
          else
            HbFunc.hb_vmPushString(PChar(S), Length(S));
        end;
        vtPChar: begin
          HbFunc.hb_vmPushString(V.VPChar, StrLen(V.VPChar));
        end;
        vtPWideChar: begin
          HbFunc.hb_vmPushString(V.VPChar, StrLen(V.VPChar));
        end;
        vtBoolean: begin
          HbFunc.hb_vmPushLogical(V.VBoolean);
        end;
        vtVariant:
          case VarType(V.VVariant^) of
            varDate: begin
              if TimeOf(V.VVariant^) = 0 then
                HbFunc.hb_vmPushDate(HbFunc.hb_dateEncode(YearOf(V.VVariant^),
                  MonthOf(V.VVariant^), DayOf(V.VVariant^)))
              else
              begin
                HbFunc.hb_timeStampUnpackDT(HbFunc.hb_timeStampPack(YearOf(V.VVariant^),
                  MonthOf(V.VVariant^), DayOf(V.VVariant^), HourOf(V.VVariant^),
                  MinuteOf(V.VVariant^), SecondOf(V.VVariant^), MilliSecondOf(V.VVariant^)),
                  TmpY, TmpMS);
                HbFunc.hb_vmPushTimeStamp(TmpY, TmpMS);
              end;
            end;
            varString: begin
              S := V.VVariant^;
              if DoOemConvert then
              begin
                Pc := StrToOem(S);
                HbFunc.hb_vmPushString(Pc, StrLen(Pc));
              end
              else
                HbFunc.hb_vmPushString(PChar(S), Length(S));
            end;
            varSmallint, varSingle, varShortInt, varInteger, varDouble,
            varCurrency, varByte, varWord, varLongWord: HbFunc.hb_vmPushNumber(V.VVariant^, 255);
            varBoolean: HbFunc.hb_vmPushLogical(V.VVariant^);
            else HbFunc.hb_vmPushNil;
          end;
        else HbFunc.hb_vmPushNil;
      end;
    end;
  end;
  //HbFunc.hb_vmDo(Length(AParams) + 1);
  HbFunc.hb_vmFunction(Length(AParams) + 1);
  if HbFunc.hb_xvmSeqEndTest then
  begin
    if HbFunc.hb_xvmSeqRecover then
      HbFunc.hb_stackPop;
    RestoreErrorBlock(OldErrorBlock);
    raise Exception.Create('An error occurred while executing harbour code')
  end
  else
  begin
    case HbFunc.hb_parinfo( -1 ) of
      HB_IT_INTEGER: Result := HbFunc.hb_parni( -1 );
      HB_IT_LONG, HB_IT_DOUBLE: Result := HbFunc.hb_parnd( -1 );
      HB_IT_DATE: begin
        HbFunc.hb_dateDecode( HbFunc.hb_pardl(-1), Y, M, D);
        Result := EncodeDate(Y, M, D);
      end;
      HB_IT_TIMESTAMP: begin
        HbFunc.hb_timeStampUnpack(HbFunc.hb_partd(-1), Y, M, D, H, MN, SC, MS);
        Result := EncodeDateTime(Y, M, D, H, MN, SC, MS);
      end;
      HB_IT_LOGICAL: begin
        Result := Boolean(HbFunc.hb_parl(-1));
      end;
      HB_IT_STRING, HB_IT_MEMO: begin
        if DoOemConvert then
          Result := OemToStr(HbFunc.hb_parc(-1))
        else
        begin
          SetLength(S, HbFunc.hb_parclen(-1));
          Move(HbFunc.hb_parc(-1)^, S[1], HbFunc.hb_parclen(-1));
          Result := S;
        end;
      end;
      else
        Result := Null;
    end;
  end;
  HbFunc.hb_vmRequestRestore;
  RestoreErrorBlock(OldErrorBlock);
end;

procedure FreeReports;
begin
  while ReportList.Count > 0 do
  begin
    THBFRObj(ReportList[0]).Free;
    ReportList.Delete(0);
  end;
end;

initialization
  ReportList := TList.Create;

finalization
  FreeReports;

end.
