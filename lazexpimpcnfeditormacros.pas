{ Export / import lazarus editor macros

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
unit lazexpimpcnfeditormacros;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lazexpimpcnfintf;

type

  TExportImportSettingsEditorMacros = class(TExportImportSettings)
  public
    //constructor Create(AGuid:TGuid;aInterface:TUInterface);virtual;
    //class procedure AddOptions(const aUIInterface: TUInterface); override;
    procedure Import; override;
    procedure Export; override;
    class procedure Register;
    class function GetVersion: cardinal; override;
  end;

implementation

const
  ClassGUID = '{66B0CEEF-7120-47A4-849C-988BD577770C}';
  ClassCaption = 'Editor macros';
  ConfigFileName = 'EditorMacros.xml';
  Version = 1;

procedure TExportImportSettingsEditorMacros.Import;
begin
  FUIInterface.UnzipFile(ClassGUID + '/' + ConfigFileName, FUIInterface.GetConfigDir);
end;

procedure TExportImportSettingsEditorMacros.Export;
var
  FileName: string;
begin
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  FUIInterface.AddZipFileEntry(FileName, ClassGUID + '/' + ConfigFileName);
end;

class function TExportImportSettingsEditorMacros.GetVersion: cardinal;
begin
  Result := Version;
end;

class procedure TExportImportSettingsEditorMacros.Register;
begin
  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsEditorMacros, True, True);
end;

initialization
  //RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsEditorMacros, True);
end.
