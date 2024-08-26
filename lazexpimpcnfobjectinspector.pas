{ Interface to export / import lazarus object inspector options

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
unit lazexpimpcnfobjectinspector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lazexpimpcnfintf, laz2_dom, laz2_XMLRead, laz2_XMLWrite,
  xmllazutils;

type

  TExportImportSettingsObjectInspector = class(TExportImportSettings)
  private
    FCurrentIdeXmlDoc: TXMLDocument;
    procedure GetCurrentValues;
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
  ClassGUID = '{3D036CC2-7795-420F-8B6C-8EAFD96969CF}';
  ClassCaption = 'Object inspector';
  ConfigFileName = 'environmentoptions.xml';
  Version = 1;
  KEY_OBJECTINSPECTOR = '/CONFIG/ObjectInspectorOptions';

procedure TExportImportSettingsObjectInspector.Import;
var
  Doc: TXMLDocument;
  Stream: TMemoryStream;
  NewNode: TDOMNode;
begin
  GetCurrentValues;     // read current XML.
  try
    Doc := nil;
    Stream := TMemoryStream.Create;
    try
      FUIInterface.UnzipFile(ClassGUID + '/' + ConfigFileName, Stream);
      Stream.Position := 0;
      ReadXMLFile(Doc, Stream);
      if Doc <> nil then
      begin
        NewNode := XmlDomFindNode(Doc.DocumentElement, KEY_OBJECTINSPECTOR);
        if NewNode <> nil then
          NewNode := FCurrentIdeXmlDoc.ImportNode(NewNode, True);
        XmlDomReplaceNode(FCurrentIdeXmlDoc.DocumentElement, KEY_OBJECTINSPECTOR, NewNode);
        WriteXMLFile(FCurrentIdeXmlDoc, IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + ConfigFileName);
      end;
    finally
      Stream.Free;
      Doc.Free;
    end;
  finally
    FCurrentIdeXmlDoc.Free;
  end;
end;

procedure TExportImportSettingsObjectInspector.Export;
var
  FileName: string;
begin
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  FUIInterface.AddZipFileEntry(FileName, ClassGUID + '/' + ConfigFileName);
end;

class function TExportImportSettingsObjectInspector.GetVersion: cardinal;
begin
  Result := Version;
end;

class procedure TExportImportSettingsObjectInspector.Register;
begin
  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsObjectInspector, True, True);
end;

procedure TExportImportSettingsObjectInspector.GetCurrentValues;
var
  FileName: string;
begin
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  if FileExists(FileName) then
  begin
    ReadXMLFile(FCurrentIdeXmlDoc, FileName);
  end;
end;

initialization
  //RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsObjectInspector, True);
end.
