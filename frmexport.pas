{ Export lazarus options form.

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
unit frmExport;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, StdCtrls, EditBtn, Dialogs, laz.VirtualTrees, Classes,
  lazexpimpcnfintf;

type
  TdlgExport = class(TForm)
    btnExport: TButton;
    deLazarusConfigDir: TDirectoryEdit;
    edDescription: TEdit;
    edLazarusVersion: TEdit;
    lbConfigDir: TLabel;
    lbDescription: TLabel;
    lbExport: TLabel;
    lbLazarusVersion: TLabel;
    SaveDialog: TSaveDialog;
    tvExport: TLazVirtualStringTree;
    procedure btnExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvExportGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FillExportTree(aTree: TLazVirtualStringTree);
  public

  end;

var
  dlgExport: TdlgExport;

implementation

{$R *.lfm}

uses
  SysUtils, FileUtil, DateUtils, LazFileUtils, laz2_dom, laz2_XMLRead,
  lazexpimpinterfacevtv, lazexpimpconsts, xmllazutils;

procedure TdlgExport.FormCreate(Sender: TObject);
begin
  dlgExport := Self;
  tvExport.NodeDataSize := SizeOf(rExpImpVTVNodeData);
  FillExportTree(tvExport);
end;

procedure TdlgExport.tvExportGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeD: ^rExpImpVTVNodeData;
begin
  NodeD := tvExport.GetNodeData(Node);
  if NodeD <> nil then
    CellText := NodeD^.Name;
end;

procedure TdlgExport.FillExportTree(aTree: TLazVirtualStringTree);
var
  Index: integer;
  Node: PVirtualNode;
  NodeD: ^rExpImpVTVNodeData;
  UI: TUInterfaceVT;
begin
  try
    UI := TUInterfaceVT.Create(aTree, '', '');
    for Index := 0 to High(gExpImpClasses) do
    begin
      Node := tvExport.AddChild(nil, nil); // adds a node as the last child
      Node^.CheckType := ctCheckBox;
      NodeD := tvExport.GetNodeData(Node);
      NodeD^.Name := gExpImpClasses[Index].Title;
      NodeD^.Guid := gExpImpClasses[Index].Guid;
      UI.CheckVTNode(Node, gExpImpClasses[Index].CheckedByDefault);
      NodeD^.Version := gExpImpClasses[Index].ExportImportClass.GetVersion;
      NodeD^.MinRequiredVersion := gExpImpClasses[Index].ExportImportClass.GetMinRequiredVersion;
      gExpImpClasses[Index].ExportImportClass.AddOptions(UI);
    end;
  finally
    UI.Free;
  end;
end;


procedure TdlgExport.btnExportClick(Sender: TObject);
var
  UI: TUInterfaceVT;
  ConfigDir: string;

  tmpDOMNode: TDOMNode;
  Doc: TXMLDocument;
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
    if (tmpDomNode <> nil) and (tmpDOMNode.Attributes.GetNamedItem('Lazarus') <> nil) then
      edLazarusVersion.Text := tmpDOMNode.Attributes.GetNamedItem('Lazarus').TextContent
    else
    begin
      ShowMessage(sLazExpImpInvalidConfigFile);
      Exit;
    end;
  finally
    Doc.Free;
  end;
  SaveDialog.InitialDir := GetUserDir;
  if SaveDialog.Execute then
  begin
    UI := TUInterfaceVT.Create(tvExport, ConfigDir, edLazarusVersion.Text);
    try
      UI.ExportToZipFile(SaveDialog.FileName, edDescription.Text);
    finally
      UI.Free;
    end;
  end;
  Close;
end;

end.
