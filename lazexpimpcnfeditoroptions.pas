{ Interface to export / import lazarus editor options

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
unit lazexpimpcnfeditoroptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lazexpimpcnfintf, laz2_dom, laz2_XMLRead, laz2_XMLWrite;

type

  TExportImportSettingsEditorOptions = class(TExportImportSettings)
  public
    //constructor Create(AGuid:TGuid;aInterface:TUInterface);virtual;
    class procedure AddOptions(const aUIInterface: TUInterface); override;
    procedure Import; override;
    procedure Export; override;
    class procedure Register;
    class function GetVersion: cardinal; override;
  private
    FActualCodeTemplatesFileName: string;
    FCurrentIdeXmlDoc: TXMLDocument;
    procedure GetCurrentValues;
  end;

implementation

uses
  FileUtil, xmllazutils;

const
  ClassGUID = '{3009AA20-C9D3-4930-8D59-962D0ABFF50C}';
  ClassCaption = 'Editor';
  ConfigFileName = 'editoroptions.xml';
  Version = 1;
  CodeTemplatesFileName = 'lazarus.dci';
  UserSchemesDirName = 'userschemes';
  ID_CodeTemplates = 1;
  ID_ColorSchemes = 2;
  KEY_COLOR = '/CONFIG/EditorOptions/Color';
  KEY_COLOR_PARENT = '/CONFIG/EditorOptions';
  KEY_CODE_TEMPLATES = '/CONFIG/EditorOptions/CodeTools';

class procedure TExportImportSettingsEditorOptions.AddOptions(const aUIInterface: TUInterface);
var
  guid: TGuid;
begin
  guid := StringToGUID(ClassGUID);
  aUIInterface.AddSubOption(guid, ID_CodeTemplates, 'Code templates', True);
  aUIInterface.AddSubOption(guid, ID_ColorSchemes, 'Color schemes', True);
end;

procedure TExportImportSettingsEditorOptions.Import;
var
  guid: TGuid;
  FileList: TStringList;
  Index: integer;
  Doc: TXMLDocument;
  Stream: TMemoryStream;
  oldColorDomNode, newColorDomNode, impNode, auxNode: TDOMNode;
  CodeTemplatesFileName: string;
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
        CodeTemplatesFileName := FActualCodeTemplatesFileName;
        //extract code templates file.
        if FUIInterface.GetChecked(guid, ID_CodeTemplates) then
        begin
          FileList := TStringList.Create;
          try
            FUIInterface.UnZipGetFullFileNames(FileList, ClassGUID + '/aux/');
            Index := 0;
            if FileList.Count > 0 then
            begin
              CodeTemplatesFileName := ExtractFileName(FileList[Index]);
              FUIInterface.UnzipFile(FileList[Index], FUIInterface.GetConfigDir);
            end;
          finally
            FileList.Free;
          end;
        end;
        // keep old values if not checked.
        if (FCurrentIdeXmlDoc <> nil) and (not FUIInterface.GetChecked(guid, ID_ColorSchemes)) then
        begin
          oldColorDomNode := XmlDomFindNode(FCurrentIdeXmlDoc.DocumentElement, KEY_COLOR);
          if OldColorDomNode <> nil then
          begin
            newColorDomNode := XmlDomFindNode(Doc.DocumentElement, KEY_COLOR);
            impNode := Doc.ImportNode(oldColorDomNode, True);
            if newColorDomNode <> nil then
              newColorDomNode.ParentNode.ReplaceChild(impNode, newColorDomNode)
            else
            begin
              auxNode := XmlDomFindNode(Doc.DocumentElement, KEY_COLOR_PARENT);
              if auxNode <> nil then
                auxNode.AppendChild(oldColorDomNode);
            end;
          end;
        end;
        auxNode := XmlDomFindAttribute(Doc.DocumentElement, KEY_CODE_TEMPLATES, 'CodeTemplateFileName');
        if auxNode <> nil then
        begin
          if FUIInterface.GetChecked(guid, ID_CodeTemplates) then
            auxNode.textContent := IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + CodeTemplatesFileName
          else
            auxNode.textContent := FActualCodeTemplatesFileName;
        end;
        WriteXMLFile(Doc, IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + ConfigFileName);
      end;
    finally
      Stream.Free;
      Doc.Free;
    end;
    //extract all user schemes
    if FUIInterface.GetChecked(guid, ID_ColorSchemes) then
    begin
      FileList := TStringList.Create;
      try
        FUIInterface.UnZipGetFullFileNames(FileList, ClassGUID + '/' + UserSchemesDirName);
        Index := 0;
        while Index < FileList.Count do
        begin
          FUIInterface.UnzipFile(FileList[Index], FUIInterface.GetConfigDir + UserSchemesDirName);
          Inc(Index);
        end;
      finally
        FileList.Free;
      end;
    end;
  finally
    FCurrentIdeXmlDoc.Free;
  end;
end;

procedure TExportImportSettingsEditorOptions.Export;
var
  FileName: string;
  guid: TGuid;
  configDir: string;
  FileInfo: TSearchRec;
  Doc: TXMLDocument;
  Node: TDOMNode;
begin
  guid := StringToGUID(ClassGUID);
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  FUIInterface.AddZipFileEntry(FileName, ClassGUID + '/' + ConfigFileName);

  configDir := FUIInterface.GetConfigDir;
  //code templates
  if FUIInterface.GetChecked(guid, ID_CodeTemplates) then
  begin
    //retrieve file name
    ReadXMLFile(Doc, FileName);
    if Doc <> nil then
    begin
      try
        Node := XmlDomFindAttribute(Doc.DocumentElement, KEY_CODE_TEMPLATES, 'CodeTemplateFileName');
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
  // color schemes
  if FUIInterface.GetChecked(guid, ID_ColorSchemes) then
  begin
    FileName := configDir + UserSchemesDirName;
    if DirectoryExists(FileName) then
    begin
      if FindFirst(IncludeTrailingPathDelimiter(FileName) + '*.*', 0, FileInfo) = 0 then
      begin
        FileName := IncludeTrailingPathDelimiter(FileName);
        repeat
          FUIInterface.AddZipFileEntry(FileName + FileInfo.Name, ClassGUID + '/' + UserSchemesDirName + '/' + FileInfo.Name);
        until FindNext(FileInfo) <> 0;
      end;
      FindClose(FileInfo);
    end;
  end;
end;

class function TExportImportSettingsEditorOptions.GetVersion: cardinal;
begin
  Result := Version;
end;

class procedure TExportImportSettingsEditorOptions.Register;
begin
  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsEditorOptions, True, True);
end;

procedure TExportImportSettingsEditorOptions.GetCurrentValues;
var
  FileName: string;
  DOMNode: TDOMNode;
begin
  FActualCodeTemplatesFileName := IncludeTrailingPathDelimiter(FUIInterface.GetConfigDir) + CodeTemplatesFileName;
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  if FileExists(FileName) then
  begin
    ReadXMLFile(FCurrentIdeXmlDoc, FileName);
    DOMNode := XmlDomFindAttribute(FCurrentIdeXmlDoc.DocumentElement, KEY_CODE_TEMPLATES, 'CodeTemplateFileName');
    if DOMNode <> nil then
      FActualCodeTemplatesFileName := DOMNode.TextContent;
  end;
end;

initialization
  //RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsEditorOptions, True, False);
end.
