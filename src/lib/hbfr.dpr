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

library hbfr;

uses
  SysUtils,
  Classes,
  hbfrintf in 'hbfrintf.pas',
  hbfrclass in 'hbfrclass.pas' {HBFRObj: TDataModule},
  smpassocar in 'smpassocar.pas';

{$R *.res}

exports
  hbfr_Init,
  hbfr_ProcessMessages,
  hbfr_New,
  hbfr_Free,
  hbfr_AddValueC,
  hbfr_AddValueNI,
  hbfr_AddValueNF,
  hbfr_AddValueL,
  hbfr_AddValueD,
  hbfr_AddDataset,
  hbfr_AddHbDataset,
  hbfr_GetRowCount,
  hbfr_ClearData,
  hbfr_AddReport,
  hbfr_ClearReports,
  hbfr_LoadFromFile,
  hbfr_SaveToFile,
  hbfr_LoadFromMemory,
  hbfr_LoadPreparedReport,
  hbfr_ShowReport,
  hbfr_ShowPreparedReport,
  hbfr_PrepareReport,
  hbfr_PrintPreparedReport,
  hbfr_DesignReport,
  hbfr_EditPreparedReport,
  hbfr_GetTitle,
  hbfr_SetTitle,
  hbfr_GetInitialZoom,
  hbfr_SetInitialZoom,
  hbfr_GetGrayedButtons,
  hbfr_SetGrayedButtons,
  hbfr_GetModifyPrepared,
  hbfr_SetModifyPrepared,
  hbfr_GetReportType,
  hbfr_SetReportType,
  hbfr_GetShowProgress,
  hbfr_SetShowProgress,
  hbfr_GetDoublePass,
  hbfr_SetDoublePass,
  hbfr_GetModalPreview,
  hbfr_SetModalPreview,
  hbfr_SetPrinter,
  hbfr_GetErrorMsg;

begin
end.
