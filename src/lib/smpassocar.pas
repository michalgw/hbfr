{ smpassocar - simple associative array

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

unit smpassocar;

interface

uses
  Classes;

type
  TSmpAssocArray = class
  private
    FName: String;
    FValue: Variant;
    FItems: TList;
    FParent: TSmpAssocArray;
    function GetItem(AIndex: String): TSmpAssocArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ByName(AName: String): TSmpAssocArray;
    function Count: Integer;
    procedure Delete(AName: String);
    property Items[AIndex: String]: TSmpAssocArray read GetItem; default;
    property Name: String read FName write FName;
    property Value: Variant read FValue write FValue;
    property Parent: TSmpAssocArray read FParent;
  end;

implementation

{ TSmpAssocArray }

function TSmpAssocArray.ByName(AName: String): TSmpAssocArray;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if TSmpAssocArray(FItems[I]).Name = AName then
    begin
      Result := TSmpAssocArray(FItems[I]);
      Exit;
    end;
end;

procedure TSmpAssocArray.Clear;
begin
  while FItems.Count > 0 do
  begin
    TSmpAssocArray(FItems[0]).Clear;
    TSmpAssocArray(FItems[0]).Free;
    FItems.Delete(0);
  end;
end;

function TSmpAssocArray.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TSmpAssocArray.Create;
begin
  inherited;
  FItems := TList.Create;
  FParent := nil;
end;

procedure TSmpAssocArray.Delete(AName: String);
var
  It: TSmpAssocArray;
begin
  It := ByName(AName);
  if It <> nil then
  begin
    FItems.Remove(It);
    It.Clear;
    It.Free;
  end;
end;

destructor TSmpAssocArray.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TSmpAssocArray.GetItem(AIndex: String): TSmpAssocArray;
begin
  Result := ByName(AIndex);
  if Result = nil then
  begin
    Result := TSmpAssocArray.Create;
    Result.Name := AIndex;
    Result.FParent := Self;
    FItems.Add(Result);
  end;
end;

end.
