{ Interface to export / import lazarus codetools options

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
unit lazexpimpcnfcodetoolsoptions;


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lazexpimpcnfintf, laz2_dom, laz2_XMLRead, laz2_XMLWrite;

type

  TExportImportSettingsCodeToolsOptions = class(TExportImportSettings)
  public
    //constructor Create(AGuid:TGuid;aInterface:TUInterface);virtual;
    class procedure AddOptions(const aUIInterface: TUInterface); override;
    procedure Import; override;
    procedure Export; override;
    class procedure Register;
    class function GetVersion: cardinal; override;
  private
    FActualIdentationSampleFileFileName: string;
    FCurrentIdeXmlDoc: TXMLDocument;
    procedure GetCurrentValues;
  end;

implementation

uses
  FileUtil, xmllazutils;

const
  ClassGUID = '{63D1DE8C-B881-46D0-9DE6-FEE0C5DC3C5A}';
  ClassCaption = 'Code tools';
  ConfigFileName = 'codetoolsoptions.xml';
  Version = 1;
  IdentationSampleFileName = 'laz_indentation.pas';
  ID_IdentationSampleFile = 1;
  KEY_IDENTATION_SAMPLE_FILE = '/CONFIG/CodeToolsOptions/Indentation';

class procedure TExportImportSettingsCodeToolsOptions.AddOptions(const aUIInterface: TUInterface);
var
  guid: TGuid;
begin
  guid := StringToGUID(ClassGUID);
  aUIInterface.AddSubOption(guid, ID_IdentationSampleFile, 'Identation sample file', True);
end;

procedure TExportImportSettingsCodeToolsOptions.Import;
var
  guid: TGuid;
  Doc: TXMLDocument;
  Stream: TMemoryStream;
  auxNode: TDOMNode;
  FileList: TStringList;
  Index: integer;
  FileName: string;
begin
  guid := StringToGUID(ClassGUID);
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
        // keep old values if not checked.
        auxNode := XmlDomFindAttribute(Doc.DocumentElement, KEY_IDENTATION_SAMPLE_FILE, 'FileName');
        if auxNode <> nil then
        begin
          if FUIInterface.GetChecked(guid, ID_IdentationSampleFile) then
          begin
            auxNode.textContent := IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + IdentationSampleFileName;

            FileList := TStringList.Create;
            try
              FUIInterface.UnZipGetFullFileNames(FileList, ClassGUID + '/aux/');
              Index := 0;
              if FileList.Count > 0 then
              begin
                FileName := ExtractFileName(FileList[Index]);
                auxNode.textContent := IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + FileName;
                FUIInterface.UnzipFile(FileList[Index], FUIInterface.GetConfigDir);
              end;
            finally
              FileList.Free;
            end;
          end
          else
            auxNode.textContent := FActualIdentationSampleFileFileName;
        end;
        WriteXMLFile(Doc, IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + ConfigFileName);
      end;
    finally
      Stream.Free;
      Doc.Free;
    end;
  finally
    FCurrentIdeXmlDoc.Free;
  end;
end;

procedure TExportImportSettingsCodeToolsOptions.Export;
var
  FileName: string;
  guid: TGuid;
  Doc: TXMLDocument;
  Node: TDOMNode;
begin
  guid := StringToGUID(ClassGUID);
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  if FileExists(FileName) then
  begin
    FUIInterface.AddZipFileEntry(FileName, ClassGUID + '/' + ConfigFileName);
    if FUIInterface.GetChecked(guid, ID_IdentationSampleFile) then
    begin
      //retrieve file name
      ReadXMLFile(Doc, FileName);
      if Doc <> nil then
      begin
        try
          Node := XmlDomFindAttribute(Doc.DocumentElement, KEY_IDENTATION_SAMPLE_FILE, 'FileName');
          if Node <> nil then
          begin
            FileName := Node.TextContent;
            if FileExists(FileName) then
              FUIInterface.AddZipFileEntry(FileName, ClassGUID + '/aux/' + ExtractFileName(FileName));
          end;
        finally
          Doc.Free;
        end;
      end;
    end;
  end;
end;

class function TExportImportSettingsCodeToolsOptions.GetVersion: cardinal;
begin
  Result := Version;
end;

class procedure TExportImportSettingsCodeToolsOptions.Register;
begin
  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsCodeToolsOptions, True, True);
end;

procedure TExportImportSettingsCodeToolsOptions.GetCurrentValues;
var
  FileName: string;
  DOMNode: TDOMNode;
begin
  FActualIdentationSampleFileFileName := IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + IdentationSampleFileName;
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  if FileExists(FileName) then
  begin
    ReadXMLFile(FCurrentIdeXmlDoc, FileName);
    if FCurrentIdeXmlDoc <> nil then
    begin
      DOMNode := XmlDomFindAttribute(FCurrentIdeXmlDoc.DocumentElement, KEY_IDENTATION_SAMPLE_FILE, 'FileName');
      if DOMNode <> nil then
        FActualIdentationSampleFileFileName := DOMNode.TextContent;
    end;
  end;
end;

initialization
//  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsCodeToolsOptions, True, False);
end.
