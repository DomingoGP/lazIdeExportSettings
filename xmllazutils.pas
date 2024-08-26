{ utility functions for working with XML DOM

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
unit xmllazutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, laz2_dom, laz2_XMLRead;

function XmlDomFindNode(ARoot: TDOMNode; APath: string): TDOMNode;

function XmlDomFindAttribute(ARoot: TDOMNode; APath: string; AAttributeName: string): TDOMNode;
function XmlDomGetAttributeValue(ARoot: TDOMNode; APath: string; AAttributeName: string): string;
// /root/key1/key2   -->  /root/key1        /root  --> ''
function XmlDomGetParentPath(APath: string): string;
function XmlDomReplaceNode(ARoot: TDOMNode; APathToNode: string; ANewNode: TDOMNode): boolean;

implementation


// APath= 'key1/key2/key3'
// APath= '/key1/key2/key3'

// If starts with / then ARoot should be  key1.
// else search in child nodes or ARoot.

//Alternative call forms.
//tmpDomNode:=XmlDomFindNode(Doc.DocumentElement,'/CONFIG/EnvironmentOptions/Version');
//tmpDomNode:=XmlDomFindNode(Doc.DocumentElement,'EnvironmentOptions/Version');

function XmlInternalDomFindNode(ARoot: TDOMNode; APath: string; aStartAt: integer): TDomNode;
var
  EndAt: integer;
  Key: string;
begin
  Result := nil;
  if aStartAt < 1 then
    aStartAt := 1;
  if (APath = '') or (aStartAt > Length(aPath)) then
    Exit;
  // root node
  if (aStartAt = 1) and (APath[1] = '/') then
  begin
    EndAt := Pos('/', APath, 2);
    if EndAt <= 0 then
      EndAt := Length(APath) + 1;
    if EndAt > 0 then
    begin
      Key := Copy(APath, 2, EndAt - 2);
      if Key = ARoot.NodeName then
        Exit(XmlInternalDomFindNode(ARoot, APath, EndAt + 1));
    end;
    Exit(nil);
  end;
  EndAt := Pos('/', APath, aStartAt);
  if EndAt <= 0 then
    EndAt := Length(APath) + 1;
  if EndAt > 0 then
  begin
    Key := Copy(APath, aStartAt, EndAt - aStartAt);
    Result := ARoot.FindNode(Key);
    if Result <> nil then
    begin
      aStartAt := EndAt + 1;
      if aStartAt < Length(APath) then
        Result := XmlInternalDomFindNode(Result, APath, aStartAt);
    end;
  end;
end;

function XmlDomFindNode(ARoot: TDOMNode; APath: string): TDOMNode;
begin
  Result := XmlInternalDomFindNode(ARoot, APath, 1);
end;

function XmlDomFindAttribute(ARoot: TDOMNode; APath: string; AAttributeName: string): TDOMNode;
begin
  Result := XmlDomFindNode(ARoot, APath);
  if Result <> nil then
    Result := Result.Attributes.GetNamedItem(AAttributeName);
end;

function XmlDomGetAttributeValue(ARoot: TDOMNode; APath: string; AAttributeName: string): string;
var
  Node: TDOMNode;
begin
  Result := '';
  Node := XmlDomFindAttribute(ARoot, APath, AAttributeName);
  if Node <> nil then
    Result := Node.TextContent;
end;


// /root/key1/key2   -->  /root/key1        /root  --> ''
function XmlDomGetParentPath(APath: string): string;
var
  P: integer;
begin
  P := RPos('/', APath);
  Result := LeftStr(APath, P - 1);
end;

// we can import the node from other document
// NewNode := Doc.ImportNode(otherDocNode, True);
// XmlDomReplaceNode(Doc.DocumentElement,'/rootkey/key1/key2',NewNode;

function XmlDomReplaceNode(ARoot: TDOMNode; APathToNode: string; ANewNode: TDOMNode): boolean;
var
  Node: TDOMNode;
begin
  Result := True;
  Node := XmlDomFindNode(ARoot, APathToNode);
  // node not found, insert new node at parent if found.
  if Node = nil then
  begin
    Node := XmlDomFindNode(ARoot, XmlDomGetParentPath(APathToNode));
    if Node = nil then
      Exit(False);
    Node.AppendChild(ANewNode);
    Exit;
  end;
  Node.ParentNode.ReplaceChild(ANewNode, Node);
end;

end.
