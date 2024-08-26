{ Interface to export / import lazarus desktops

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
unit lazexpimpcnfdesktops;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lazexpimpcnfintf, laz2_dom, laz2_XMLRead, laz2_XMLWrite;

type

  TExportImportSettingsDesktops = class(TExportImportSettings)
  public
    //constructor Create(AGuid:TGuid;aInterface:TUInterface);virtual;
    //class procedure AddOptions(const aUIInterface: TUInterface); override;
    procedure Import; override;
    procedure Export; override;
    class procedure Register;
    class function GetVersion: cardinal; override;
  private
    FCurrentIdeXmlDoc: TXMLDocument;
    procedure GetCurrentValues;
  end;

implementation

uses
  FileUtil, lazexpimpinterfacevtv, xmllazutils;

const
  ClassGUID = '{9DE4B08B-C731-4563-89A7-4EE459C08FB4}';
  ClassCaption = 'Desktop(s)';
  ConfigFileName = 'environmentoptions.xml';
  Version = 1;
  AnchorDockingFileName = 'anchordockingoptions.xml';
  KEY_DESKTOPS = '/CONFIG/Desktops';
  KEY_DESKTOPS_PARENT = '/CONFIG';


procedure TExportImportSettingsDesktops.Import;
var
  Doc: TXMLDocument;
  Stream: TMemoryStream;
  oldDesktopsDomNode, newDesktopsDomNode, impNode, auxNode: TDOMNode;
begin
  GetCurrentValues;     // read current XML.
  try
    Doc := nil;
    Stream := TMemoryStream.Create;
    try
      FUIInterface.UnzipFile(ClassGUID + '/' + ConfigFileName, Stream);
      Stream.Position := 0;
      ReadXMLFile(Doc, Stream);
      // insert new desktop(s)
      if FCurrentIdeXmlDoc <> nil then
      begin
        oldDesktopsDomNode := XmlDomFindNode(FCurrentIdeXmlDoc.DocumentElement, KEY_DESKTOPS);
        if oldDesktopsDomNode <> nil then
        begin
          newDesktopsDomNode := XmlDomFindNode(Doc.DocumentElement, KEY_DESKTOPS);
          impNode := FCurrentIdeXmlDoc.ImportNode(newDesktopsDomNode, True);
          if newDesktopsDomNode <> nil then
            oldDesktopsDomNode.ParentNode.ReplaceChild(impNode, oldDesktopsDomNode)
          else
          begin
            auxNode := XmlDomFindNode(FCurrentIdeXmlDoc.DocumentElement, KEY_DESKTOPS_PARENT);
            if auxNode <> nil then
              auxNode.AppendChild(oldDesktopsDomNode);
          end;
        end;
      end;
      WriteXMLFile(FCurrentIdeXmlDoc, FUIInterface.GetConfigDir + DirectorySeparator + ConfigFileName);
    finally
      Stream.Free;
      Doc.Free;
    end;
    FUIInterface.UnzipFile(ClassGUID + '/' + AnchorDockingFileName, FUIInterface.GetConfigDir);
  finally
    FCurrentIdeXmlDoc.Free;
  end;
end;

procedure TExportImportSettingsDesktops.Export;
var
  FileName: string;
begin
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  FUIInterface.AddZipFileEntry(FileName, ClassGUID + '/' + ConfigFileName);
  if FileExists(FUIInterface.GetConfigDir + AnchorDockingFileName) then
    FUIInterface.AddZipFileEntry(FUIInterface.GetConfigDir + AnchorDockingFileName, ClassGUID + '/' + AnchorDockingFileName);
end;

class function TExportImportSettingsDesktops.GetVersion: cardinal;
begin
  Result := Version;
end;

class procedure TExportImportSettingsDesktops.Register;
begin
  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsDesktops, True, True);
end;

procedure TExportImportSettingsDesktops.GetCurrentValues;
var
  FileName: string;
begin
  FileName := FUIInterface.GetConfigDir + ConfigFileName;
  if FileExists(FileName) then
    ReadXMLFile(FCurrentIdeXmlDoc, FileName);
end;

initialization
  RegisterExpImpCnfClass(StringToGUID(ClassGUID), ClassCaption, TExportImportSettingsDesktops, True, False);
end.
