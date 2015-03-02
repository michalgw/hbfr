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

function hbfr_Init(AOemConvert: LongBool): Integer; stdcall;
function hbfr_ProcessMessages: Integer; StdCall;

function hbfr_New(AComposite: LongBool): LongWord; stdcall;
function hbfr_Free(AHandle: LongWord): Integer; stdcall;

function hbfr_AddValueC(AHandle: LongWord; AName: PChar; AValue: PChar): Integer; stdcall;
function hbfr_AddValueNI(AHandle: LongWord; AName: PChar; AValue: Integer): Integer; stdcall;
function hbfr_AddValueNF(AHandle: LongWord; AName: PChar; AValue: Double): Integer; stdcall;
function hbfr_AddValueL(AHandle: LongWord; AName: PChar; AValue: LongBool): Integer; stdcall;
function hbfr_AddValueD(AHandle: LongWord; AName: PChar; AYear, AMonth, ADay: Integer): Integer; stdcall;

function hbfr_AddDataset(AHandle: LongWord; AName: PChar): Integer; stdcall;

function hbfr_GetRowCount(AHandle: LongWord; AName: PChar): Integer; stdcall;

function hbfr_ClearData(AHandle: LongWord): Integer; stdcall;

function hbfr_AddReport(AHandle: LongWord; AReport: LongWord): Integer; stdcall;
function hbfr_ClearReports(AHandle: LongWord): Integer; stdcall;

function hbfr_LoadFromFile(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_SaveToFile(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_LoadPreparedReport(AHandle: LongWord; AFileName: PChar): Integer; stdcall;
function hbfr_LoadFromMemory(AHandle: LongWord; AData: Pointer; ALength: Integer): Integer; stdcall;

function hbfr_PrepareReport(AHandle: LongWord): Integer; stdcall;
function hbfr_ShowReport(AHandle: LongWord): Integer; stdcall;
function hbfr_ShowPreparedReport(AHandle: LongWord): Integer; stdcall;
function hbfr_PrintPreparedReport(AHandle: LongWord; APages: PChar; ACopies: Integer): Integer; stdcall;
function hbfr_DesignReport(AHandle: LongWord): Integer; stdcall;
function hbfr_EditPreparedReport(AHandle: LongWord; APageIndex: Integer): Integer; stdcall;

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

function hbfr_SetPrinter(AHandle: LongWord; APrinterName: PChar): Integer; stdcall;

function hbfr_GetErrorMsg(AHandle: LongWord; AMessage: PChar): Integer; stdcall;

implementation

uses
  SysUtils, Windows, Forms, FR_View, FR_Class, ExtCtrls;

var
  ReportList: TList;
  DoOemConvert: Boolean;

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

function hbfr_Init(AOemConvert: LongBool): Integer; stdcall;
begin
  try
    Application.Initialize;
    DoOemConvert := AOemConvert;
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
