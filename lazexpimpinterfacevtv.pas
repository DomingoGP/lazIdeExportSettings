{ Interface to export / import lazarus options using lazVirtualStringTree

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
unit lazexpimpinterfacevtv;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Zipper, DateUtils,
  laz.VirtualTrees, laz2_dom, laz2_XMLRead, lazexpimpcnfintf, StreamUnzipper;

type

  rExpImpVTVNodeData = record
    Name: string[255]; // the identifier of the node
    Id: cardinal;        // 0 main other for childs
    Guid: TGuid;
    Version: cardinal;
    MinRequiredVersion: cardinal;
  end;


  TUInterfaceVT = class(TUInterface)
  protected
    FVT: TLazVirtualStringTree;
    FZipper: TZipper;
    FUnZipper: TStreamUnzipper;
    FConfigDir: string;
    FLazVersion: string;
    FSavedVersion: cardinal;
    FZipLazarusVersion: string;
    function FindNode(aGUID: TGuid; aChildId: cardinal): PVirtualNode;
    function MakeHeader(ADescription: string): string;
    procedure SetSavedVersion(AVersion: cardinal);
    function GetNodeVersion(aGUID: TGuid): cardinal;
  public
    constructor Create(aTree: TLazVirtualStringTree; AConfigDir: string; ALazVersion: string;AZipLazarusVersion:string='');
    //aChildId must be > 0
    procedure AddSubOption(aGUID: TGuid; aChildId: cardinal; const aTitle: string; aChecked: boolean); override;
    // aChiId 0 father
    procedure SetChecked(aGUID: TGuid; aChildId: cardinal; aChecked: boolean); override;
    function GetChecked(aGUID: TGuid; aChildId: cardinal = 0): boolean; override;
    procedure CheckVTNode(aNode: PVirtualNode; aChecked: boolean);

    // transfers the AStream Ownership. We SHOULD not Free the AStream.
    procedure AddZipFileEntry(const AStream: TSTream; const AArchiveFileName: string); override;
    procedure AddZipFileEntry(const ADiskFileName, AArchiveFileName: string); override;
    procedure UnzipFile(AZippedFile: string; ADestStream: TStream); override;
    //AFLat only file name without path.
    procedure UnZipFile(AZippedFile: string; AOutputPath: string; AFlat: boolean = True); override;
    // dir1/dir2/start
    procedure UnZipGetFullFileNames(AOutStrings: TStrings; AStartingPath: string = ''); override;


    property Zipper: TZipper read FZipper write FZipper;
    property Unzipper: TStreamUnzipper read FUnZipper write FUnZipper;

    function GetConfigDir: string; override;
    function GetSavedVersion: cardinal; override;
    procedure ExportToZipFile(const AZipFileName: string; ADescription: string);
    procedure ImportFromZipFile(const AZipFileName: string);
    function GetLazarusVersion: string; override;
    function GetZipLazarusVersion: string; override;
  end;



function CheckedStateToString(aCheckState: TCheckState): string;

implementation

function CheckedStateToString(aCheckState: TCheckState): string;
begin
  if aCheckState = csCheckedNormal then
    Result := 'True'
  else
    Result := 'False';
end;

function TUInterfaceVT.FindNode(aGUID: TGuid; aChildId: cardinal): PVirtualNode;
var
  Node: PVirtualNode;
  NodeD: ^rExpImpVTVNodeData;
begin
  Result := nil;
  Node := FVT.GetFirst;
  while Node <> nil do
  begin
    NodeD := FVT.GetNodeData(Node);
    if NodeD <> nil then
    begin
      if IsEqualGUID(NodeD^.Guid, aGUID) then
      begin
        if aChildId = 0 then
          Exit(Node);
        // find child node.
        Node := Node^.FirstChild;
        while Node <> nil do
        begin
          NodeD := FVT.GetNodeData(Node);
          if NodeD <> nil then
          begin
            if NodeD^.Id = aChildId then
              Exit(Node);
          end;
          Node := Node^.NextSibling;
        end;
        Exit(nil);
      end;
    end;
    Node := Node^.NextSibling;
  end;
end;

function TUInterfaceVT.MakeHeader(ADescription: string): string;
var
  Node, ChildNode: PVirtualNode;
  NodeD, ChildNodeD: ^rExpImpVTVNodeData;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<?xml version="1.0" ?>');
    Lines.Add('<LazExpImpCnf>');
    Lines.Add('  <WriteVersion> 1.0 </WriteVersion>');
    Lines.Add('  <WriteDateTimeUTC>' + DateToISO8601(LocalTimeToUniversal(Now)) + '</WriteDateTimeUTC>');
    Lines.Add('  <Description>' + StrToXMLValue(ADescription) + '</Description>');
    Lines.Add('  <LazarusVersion>' + GetLazarusVersion + '</LazarusVersion>');
    Lines.Add('  <LazarusConfigDir>' + GetConfigDir + '</LazarusConfigDir>');
    Lines.Add('  <Classes>');
    Node := FVT.GetFirst;
    while Node <> nil do
    begin
      NodeD := FVT.GetNodeData(Node);
      if NodeD <> nil then
      begin
        if FVT.CheckState[Node] = csCheckedNormal then
        begin
          Lines.Add('    <Class name="' + StrToXMLValue(NodeD^.Name) + '" guid="' + GuidToString(NodeD^.Guid) + '" id="' +
            IntToStr(NodeD^.Id) + '" checked="' + CheckedStateToString(FVT.CheckState[Node]) + '" version="' +
            IntToStr(NodeD^.Version) + '" minRequiredVersion="' + IntToStr(NodeD^.MinRequiredVersion) + '">');
          ChildNode := Node^.FirstChild;
          while ChildNode <> nil do
          begin
            ChildNodeD := FVT.GetNodeData(ChildNode);
            if ChildNodeD <> nil then
              Lines.Add('      <Option name="' + StrToXMLValue(ChildNodeD^.Name) + '" id="' + IntToStr(ChildNodeD^.Id) +
                '" checked="' + CheckedStateToString(FVT.CheckState[ChildNode]) + '"></Option>');
            ChildNode := ChildNode^.NextSibling;
          end;
          Lines.Add('    </Class>');
        end;
      end;
      Node := Node^.NextSibling;
    end;
    Lines.Add('  </Classes>');
    Lines.Add('</LazExpImpCnf>');
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

constructor TUInterfaceVT.Create(aTree: TLazVirtualStringTree;
  AConfigDir: string; ALazVersion: string; AZipLazarusVersion: string);
begin
  FVT := aTree;
  FConfigDir := AConfigDir;
  FLazVersion := ALazVersion;
  FSavedVersion := 0;
  FZipLazarusVersion := AZipLazarusVersion;
end;

procedure TUInterfaceVT.AddSubOption(aGUID: TGuid; aChildId: cardinal; const aTitle: string; aChecked: boolean);
var
  Node, NewNode: PVirtualNode;
  NodeD: ^rExpImpVTVNodeData;
begin
  Node := FindNode(aGUID, 0);
  if Node <> nil then
  begin
    NewNode := FVT.AddChild(Node, nil);
    NewNode^.CheckType := ctCheckBox;
    NodeD := FVT.GetNodeData(NewNode);
    NodeD^.Name := aTitle;
    NodeD^.Guid := aGuid;
    NodeD^.Id := aChildId;
    if aChecked then
      FVT.CheckState[NewNode] := csCheckedNormal
    else
      FVT.CheckState[NewNode] := csUncheckedNormal;
  end;
  FVT.Expanded[Node] := True;
end;

procedure TUInterfaceVT.SetChecked(aGUID: TGuid; aChildId: cardinal; aChecked: boolean);
begin
  CheckVTNode(FindNode(aGUID, aChildId), aChecked);
end;

function TUInterfaceVT.GetChecked(aGUID: TGuid; aChildId: cardinal): boolean;
var
  Node: PVirtualNode;
begin
  Result := False;
  Node := FindNode(aGUID, aChildId);
  if Node <> nil then
  begin
    Result := FVT.CheckState[Node] = csCheckedNormal;
  end;
end;

function TUInterfaceVT.GetNodeVersion(aGUID: TGuid): cardinal;
var
  Node: PVirtualNode;
  NodeD: ^rExpImpVTVNodeData;
begin
  Result := 0;
  Node := FindNode(aGUID, 0);
  if Node <> nil then
  begin
    NodeD := FVT.GetNodeData(Node);
    Result := NodeD^.Version;
  end;
end;

procedure TUInterfaceVT.CheckVTNode(aNode: PVirtualNode; aChecked: boolean);
begin
  if aNode <> nil then
  begin
    if aChecked then
      FVT.CheckState[aNode] := csCheckedNormal
    else
      FVT.CheckState[aNode] := csUncheckedNormal;
  end;
end;

procedure TUInterfaceVT.AddZipFileEntry(const AStream: TSTream; const AArchiveFileName: string);
begin
  if FZipper = nil then
    Exit;
  FZipper.Entries.AddFileEntry(AStream, AArchiveFileName);
end;

procedure TUInterfaceVT.AddZipFileEntry(const ADiskFileName, AArchiveFileName: string);
begin
  if FZipper = nil then
    Exit;
  FZipper.Entries.AddFileEntry(ADiskFileName, AArchiveFileName);
end;

procedure TUInterfaceVT.UnzipFile(AZippedFile: string; ADestStream: TStream);
begin
  if FUnzipper = nil then
    Exit;
  FUnzipper.UnzipFile(AZippedFile, ADestStream);
end;

procedure TUInterfaceVT.UnZipFile(AZippedFile: string; AOutputPath: string; AFlat: boolean);
begin
  if FUnzipper = nil then
    Exit;
  FUnzipper.UnZipFile(AZippedFile, AOutputPath, AFlat);
end;

procedure TUInterfaceVT.UnZipGetFullFileNames(AOutStrings: TStrings; AStartingPath: string);
begin
  if FUnZipper = nil then
    Exit;
  FUnzipper.GetFullFileNames(AOutStrings, AStartingPath);
end;

function TUInterfaceVT.GetConfigDir: string;
begin
  Result := FConfigDir;
end;

function TUInterfaceVT.GetLazarusVersion: string;
begin
  Result := FLazVersion;
end;

function TUInterfaceVT.GetSavedVersion: cardinal;
begin
  Result := FSavedVersion;
end;

procedure TUInterfaceVT.ExportToZipFile(const AZipFileName: string; ADescription: string);
var
  Index: integer;
  Exporter: TExportImportSettings;
  Header: string;
begin
  Zipper := TZipper.Create;
  try
    Zipper.FileName := AZipFileName;
    Header := MakeHeader(ADescription);
    AddZipFileEntry(TStringStream.Create(Header), 'header.xml');  // takes ownership of the stream.

    for Index := 0 to High(gExpImpClasses) do
    begin
      if GetChecked(gExpImpClasses[Index].Guid, 0) then
      begin
        Exporter := gExpImpClasses[Index].ExportImportClass.Create(gExpImpClasses[Index].GUID, Self);
        try
          SetSavedVersion(gExpImpClasses[Index].ExportImportClass.GetVersion);
          Exporter.Export;
          SetSavedVersion(0);
        finally
          Exporter.Free;
        end;
      end;
    end;
    Zipper.ZipAllFiles;
  finally
    // Free all streams.
    for Index := 0 to Zipper.Entries.Count - 1 do
      Zipper.Entries[Index].Stream.Free;
    Zipper.Free;
  end;
end;

procedure TUInterfaceVT.ImportFromZipFile(const AZipFileName: string);
var
  Index: integer;
  Importer: TExportImportSettings;
begin
  UnZipper := TStreamUnzipper.Create(AZipFileName);
  try
    for Index := 0 to High(gExpImpClasses) do
    begin
      if GetChecked(gExpImpClasses[Index].Guid, 0) then
      begin
        Importer := gExpImpClasses[Index].ExportImportClass.Create(gExpImpClasses[Index].GUID, Self);
        try
          SetSavedVersion(GetNodeVersion(gExpImpClasses[Index].GUID));
          Importer.Import;
          SetSavedVersion(0);
        finally
          Importer.Free;
        end;
      end;
    end;
  finally
    UnZipper.Free;
  end;
end;

function TUInterfaceVT.GetZipLazarusVersion: string;
begin
  Result := FZipLazarusVersion;
end;

procedure TUInterfaceVT.SetSavedVersion(AVersion: cardinal);
begin
  FSavedVersion := AVersion;
end;

end.
