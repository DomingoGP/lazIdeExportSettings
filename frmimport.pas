{ Import lazarus options form.

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
unit frmimport;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, StdCtrls, EditBtn, Dialogs, ExtCtrls, laz.VirtualTrees, Classes,
  lazexpimpcnfintf;

type
  TdlgImport = class(TForm)
    blImport: TLabel;
    btnImport: TButton;
    btnOpen: TButton;
    deLazarusConfigDir: TDirectoryEdit;
    feZipFileName: TFileNameEdit;
    lbConfigDir: TLabel;
    lbImportWarning: TStaticText;
    Memo1: TMemo;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    tvImport: TLazVirtualStringTree;
    procedure btnImportClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure feZipFileNameAcceptFileName(Sender: TObject; var Value: string);
    procedure FormCreate(Sender: TObject);
    procedure tvImportGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FZipLazarusVersion: string;
    procedure Open(AFileName: string);
  public

  end;

var
  dlgImport: TdlgImport;

implementation

{$R *.lfm}

uses
  SysUtils, FileUtil, Zipper, DateUtils, LazFileUtils, laz2_dom, laz2_XMLRead
  , lazexpimpinterfacevtv, StreamUnzipper, lazexpimpconsts, xmllazutils;

procedure TdlgImport.FormCreate(Sender: TObject);
begin
  dlgImport := Self;
  tvImport.NodeDataSize := SizeOf(rExpImpVTVNodeData);
end;

procedure TdlgImport.tvImportGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeD: ^rExpImpVTVNodeData;
begin
  NodeD := tvImport.GetNodeData(Node);
  if NodeD <> nil then
    CellText := NodeD^.Name;
end;

procedure TdlgImport.btnImportClick(Sender: TObject);
var
  UI: TUInterfaceVT;
  ConfigDir: string;

  tmpDOMNode: TDOMNode;
  Doc: TXMLDocument;
  LazarusVersion: string;
begin
  ConfigDir := AppendPathDelim(deLazarusConfigDir.Text);
  if not FileExists(ConfigDir + 'environmentoptions.xml') then
  begin
    ShowMessage(sLazExpImpInvalidConfigDirectory);
    Exit;
  end;
  try
    ReadXMLFile(Doc, ConfigDir + 'environmentoptions.xml');
    tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/CONFIG/EnvironmentOptions/Version');
    if (tmpDomNode = nil) or (tmpDOMNode.Attributes.GetNamedItem('Lazarus') = nil) then
    begin
      ShowMessage(sLazExpImpInvalidConfigFile);
      Exit;
    end;
    LazarusVersion := tmpDOMNode.Attributes.GetNamedItem('Lazarus').TextContent;
  finally
    Doc.Free;
  end;
  UI := TUInterfaceVT.Create(tvImport, ConfigDir, LazarusVersion,FZipLazarusVersion);
  try
    UI.ImportFromZipFile(feZipFileName.FileName);
  finally
    UI.Free;
  end;
  Close;
end;

procedure CheckVTVNode(AVTV: TLazVirtualStringTree; ANode: PVirtualNode; AChecked: boolean);
begin
  if AChecked then
    AVTV.CheckState[ANode] := csCheckedNormal
  else
    AVTV.CheckState[ANode] := csUncheckedNormal;
end;

procedure EnableVTVNode(ANode: PVirtualNode; AEnabled: boolean);
begin
  if AEnabled then
    Exclude(ANode^.States, vsDisabled)
  else
    Include(ANode^.States, vsDisabled);
end;

procedure TdlgImport.feZipFileNameAcceptFileName(Sender: TObject; var Value: string);
begin
  Open(Value);
end;

procedure TdlgImport.btnOpenClick(Sender: TObject);
begin
  Open(feZipFileName.FileName);
end;

procedure TdlgImport.Open(AFileName: string);
var
  Stream: TMemoryStream;
  tmpDOMNode, classDOMNode, optionDOMNode: TDOMNode;
  Doc: TXMLDocument;
  Guid: TGuid;

  Node, optionNode: PVirtualNode;
  NodeD, optionNodeD: ^rExpImpVTVNodeData;
  NodeChecked, NodeEnabled: boolean;
  Unzipper: TStreamUnzipper;
  Index, ClassIndex: integer;
  optionChecked: boolean;
begin
  Memo1.Lines.Clear;
  tvImport.Clear;
  if not FileExists(AFileName) then
  begin
    ShowMessage(sLazExpImpFileDoesntExist);
    Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    if not UnzipToStream(AFileName, 'header.xml', Stream) then
    begin
      ShowMessage(sLazExpImpUnsupportedFile);
      Exit;
    end;
    Memo1.Lines.BeginUpdate;
    try
      Stream.Position := 0;
      ReadXMLFile(Doc, Stream);
      try
        tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/LazExpImpCnf/Description');
        if tmpDomNode <> nil then
        begin
          Memo1.Lines.Add(sLazExpImpDescription);
          Memo1.Lines.Add(tmpDomNode.TextContent);
          Memo1.Lines.Add('');
        end
        else
        begin
          ShowMessage(sLazExpImpUnsupportedFile);
          Exit;
        end;
        tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/LazExpImpCnf/WriteDateTimeUTC');
        if tmpDomNode <> nil then
          Memo1.Lines.Add(sLazExpImpWriteDateUTC + tmpDomNode.TextContent);
        tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/LazExpImpCnf/LazarusVersion');
        if tmpDomNode <> nil then
        begin
          FZipLazarusVersion := tmpDomNode.TextContent;
          Memo1.Lines.Add(sLazExpImpLazarusVersion + tmpDomNode.TextContent);
        end;
        tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/LazExpImpCnf/WriteVersion');
        if tmpDomNode <> nil then
          Memo1.Lines.Add(sLazExpImpWriteVersion + tmpDomNode.TextContent);
        tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/LazExpImpCnf/LazarusConfigDir');
        if tmpDomNode <> nil then
          Memo1.Lines.Add(sLazExpImpLazarusConfigDir + tmpDomNode.TextContent);

        Memo1.Lines.Add('');
        Memo1.Lines.Add(sLazExpImpFileList);
        Memo1.Lines.Add('==========================================================');

        Unzipper := TStreamUnzipper.Create(AFileName);
        try
          if Unzipper.Entries.Count <= 0 then
            Unzipper.Examine;
          Index := 0;
          while Index < Unzipper.Entries.Count do
          begin
            Memo1.Lines.Add(Unzipper.Entries[Index].ArchiveFileName);
            Inc(Index);
          end;
        finally
          Unzipper.Free;
        end;

        tmpDomNode := XmlDomFindNode(Doc.DocumentElement, '/LazExpImpCnf/Classes');
        if tmpDomNode = nil then
          Exit;
        classDOMNode := tmpDomNode.FirstChild;
        while classDOMNode <> nil do
        begin
          ClassIndex := -1;
          Node := tvImport.AddChild(nil, nil);
          NodeD := tvImport.GetNodeData(Node);
          NodeChecked := True;
          NodeEnabled := True;
          Node^.CheckType := ctCheckBox;
          if classDOMNode.Attributes.GetNamedItem('name') <> nil then
            NodeD^.Name := classDOMNode.Attributes.GetNamedItem('name').TextContent;
          if classDOMNode.Attributes.GetNamedItem('guid') <> nil then
          begin
            Guid := StringToGUID(classDOMNode.Attributes.GetNamedItem('guid').TextContent);
            NodeD^.Guid := Guid;
            ClassIndex := SearchIndexExpImpCnfClass(Guid);
            if ClassIndex < 0 then  //Unknown class.
            begin
              NodeChecked := False;
              NodeEnabled := False;
            end;
          end;
          if classDOMNode.Attributes.GetNamedItem('version') <> nil then
            NodeD^.Version := StrToIntDef(classDOMNode.Attributes.GetNamedItem('version').TextContent, 0);
          if classDOMNode.Attributes.GetNamedItem('minRequiredVersion') <> nil then
            NodeD^.MinRequiredVersion := StrToIntDef(classDOMNode.Attributes.GetNamedItem('minRequiredVersion').TextContent, 1);
          if classDOMNode.Attributes.GetNamedItem('id') <> nil then
            NodeD^.Id := StrToIntDef(classDOMNode.Attributes.GetNamedItem('id').TextContent, 0);
          if ClassIndex >= 0 then
          begin
            if  gExpImpClasses[ClassIndex].ExportImportClass.GetVersion < NodeD^.MinRequiredVersion then
            begin
              NodeChecked := False;
              NodeEnabled := False;
            end;
          end;
          if NodeChecked then
            CheckVTVNode(tvImport, Node, True);
          if not NodeEnabled then
            EnableVTVNode(Node, False);
          optionDOMNode := classDOMNode.FirstChild;
          while optionDOMNode <> nil do
          begin
            optionNode := tvImport.AddChild(Node, nil);
            optionNodeD := tvImport.GetNodeData(optionNode);
            optionNode^.CheckType := ctCheckBox;
            if optionDOMNode.Attributes.GetNamedItem('name') <> nil then
              optionNodeD^.Name := optionDOMNode.Attributes.GetNamedItem('name').TextContent;
            if optionDOMNode.Attributes.GetNamedItem('id') <> nil then
              optionNodeD^.Id := StrToIntDef(optionDOMNode.Attributes.GetNamedItem('id').TextContent, -1);
            optionNodeD^.Guid := Guid;
            optionChecked := False;
            if optionDOMNode.Attributes.GetNamedItem('checked') <> nil then
              optionChecked := optionDOMNode.Attributes.GetNamedItem('checked').TextContent= 'True';

            CheckVTVNode(tvImport, optionNode, NodeChecked and optionChecked);
            if (not NodeEnabled) or (not optionChecked)  then
              EnableVTVNode(optionNode, False);
            optionDOMNode := optionDOMNode.NextSibling;
          end;
          tvImport.Expanded[Node] := True;
          classDOMNode := classDOMNode.NextSibling;
        end;
      finally
        Doc.Free;
      end;
    finally
      Stream.Free;
    end;
    Memo1.CaretPos := Point(1, 1);
  finally
    Memo1.Lines.EndUpdate;
  end;
  Panel1.Visible := True;
end;

end.
