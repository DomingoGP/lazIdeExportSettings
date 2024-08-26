{ Interface to export / import lazarus options

  Copyright (C) 2024 Domingo Galmés dgalmesp@gmail.com

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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit lazexpimpcnfintf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TUInterface = class
    //aChildId must be > 0
    procedure AddSubOption(aGUID: TGuid; aChildId: cardinal; const aTitle: string; aChecked: boolean); virtual; abstract;
    // aChiId 0 father
    procedure SetChecked(aGUID: TGuid; aChildId: cardinal; aChecked: boolean); virtual; abstract;
    function GetChecked(aGUID: TGuid; aChildId: cardinal = 0): boolean; virtual; abstract;

    // transfers the AStream Ownership. We SHOULD not Free the AStream.
    procedure AddZipFileEntry(const AStream: TSTream; const AArchiveFileName: string); virtual; abstract;
    procedure AddZipFileEntry(const ADiskFileName, AArchiveFileName: string); virtual; abstract;
    procedure UnzipFile(AZippedFile: string; ADestStream: TStream); virtual; abstract;
    procedure UnZipFile(AZippedFile: string; AOutputPath: string; AFlat: boolean = True); virtual; abstract;
    // AStartingPath ->  dir1/dir2/start
    procedure UnZipGetFullFileNames(AOutStrings: TStrings; AStartingPath: string = ''); virtual; abstract;

    function GetConfigDir: string; virtual; abstract;
    function GetLazarusVersion: string; virtual; abstract;
    // get saved version of the current TExportImportSettings class.
    function GetSavedVersion: cardinal; virtual;
    function GetZipLazarusVersion: string; virtual; abstract;
  end;

  TExportImportSettings = class
  protected
    FGUID: TGuid;
    FUIInterface: TUInterface;
  public
    constructor Create(AGuid: TGuid; aInterface: TUInterface); virtual;
    class procedure AddOptions(const aUIInterface: TUInterface); virtual;
    procedure Import; virtual; abstract;
    procedure Export; virtual; abstract;
    class function GetVersion: cardinal; virtual; abstract;
    class function GetMinRequiredVersion: cardinal; virtual;
    function GetSavedVersion: cardinal;
  end;

  TExportSettingsRef = class of TExportImportSettings;

  rExpImpClases = record
    Guid: TGuid;
    Title: string;
    ExportImportClass: TExportSettingsRef;
    CheckedByDefault: boolean;
  end;

procedure RegisterExpImpCnfClass(aGUID: TGuid; const aTitle: string; aExportImportClass: TExportSettingsRef; aChecked: boolean;
  aInsertAtStart: boolean = False);
function SearchIndexExpImpCnfClass(aGUID: TGuid): integer;
procedure ClearExpImpCnfClasses;

var
  gExpImpClasses: array of rExpImpClases;

implementation

const
  MIN_REQUIRED_VERSION = 1;

function TUInterface.GetSavedVersion: cardinal;
begin
  Result := 0;
end;

constructor TExportImportSettings.Create(AGuid: TGuid; aInterface: TUInterface);
begin
  FGuid := AGuid;
  FUIInterface := aInterface;
end;

class procedure TExportImportSettings.AddOptions(const aUIInterface: TUInterface);
begin
  // by default we don't add options.
end;

class function TExportImportSettings.GetMinRequiredVersion: cardinal;
begin
  Result := MIN_REQUIRED_VERSION;
end;

function TExportImportSettings.GetSavedVersion: cardinal;
begin
  Result := FUIInterface.GetSavedVersion;
end;

procedure RegisterExpImpCnfClass(aGUID: TGuid; const aTitle: string; aExportImportClass: TExportSettingsRef; aChecked: boolean;
  aInsertAtStart: boolean);
var
  Index: integer;
begin
  if aExportImportClass = nil then
    Exit;
  if SearchIndexExpImpCnfClass(aGUID) > 0 then  // don't duplicate.
    Exit;
  SetLength(gExpImpClasses, Length(gExpImpClasses) + 1);
  if aInsertAtStart then
  begin
    if Length(gExpImpClasses) > 1 then
      System.Move(gExpImpClasses[0], gExpImpClasses[1], sizeof(gExpImpClasses[0]) * (Length(gExpImpClasses) - 1));
    Index := 0;
  end
  else
    Index := High(gExpImpClasses);
  gExpImpClasses[Index].Guid := aGUID;
  gExpImpClasses[Index].Title := ATitle;
  gExpImpClasses[Index].ExportImportClass := aExportImportClass;
  gExpImpClasses[Index].CheckedByDefault := aChecked;
end;

function SearchIndexExpImpCnfClass(aGUID: TGuid): integer;
var
  Index: integer;
begin
  Result := -1;
  Index := 0;
  while Index < length(gExpImpClasses) do
  begin
    if IsEqualGUID(gExpImpClasses[Index].Guid, aGUID) then
      Exit(Index);
    Inc(Index);
  end;
end;

procedure ClearExpImpCnfClasses;
begin
  SetLength(gExpImpClasses, 0);
end;

end.
