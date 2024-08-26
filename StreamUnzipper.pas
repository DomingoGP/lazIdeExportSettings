{ Unzip files to streams.

  Copyright (C) 2021-2024 Domingo Galmés dgalmesp@gmail.com

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
unit StreamUnzipper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, zipper;

  //http://wiki.freepascal.org/paszlib

type
  TStreamUnzipper = class(TUnzipper)
  private
    FInputStream: TStream;
    FOutputStream: TStream;
    FSuccess: boolean;
    procedure CreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure OpenInputStream(Sender: TObject; var AStream: TStream);
  public
    //WARNING: AInputStreeam is Freed by TUnzipper.
    constructor Create(AInputStream: TStream); overload;
    constructor Create(const AZipFileName: string); overload;
    function UnzipFile(const AZippedFile: string; ADestStream: TStream): boolean; overload;
    function UnZipFile(AIndex: integer; ADestStream: TStream): boolean; overload;
    function UnZipFile(Item: TFullZipFileEntry; ADestStream: TStream): boolean; overload;

    //AFlat only filename without path.
    procedure UnZipFile(AZippedFile: string; AOutputPath: string; AFlat: boolean);
    procedure GetFullFileNames(AOutStrings: TStrings;  AStartingPath: string='');

  end;


procedure UnzipFile(AZipFileName, AZippedFile, ADestFolder: string);
//WARNING: AZipStream is Freed by TUnzipper.
function UnzipToStream(AZipStream: TStream; const AZippedFile: string; ADestStream: TStream): boolean;overload;
function UnzipToStream(AZipFileName: string; const AZippedFile: string; ADestStream: TStream): boolean;overload;


implementation

// if AinputStream = nil then opens the file FileName.
constructor TStreamUnzipper.Create(AInputStream: TStream);
begin
  inherited Create;
  OnCreateStream := @CreateStream;
  OnDoneStream := @DoneStream;
  OnOpenInputStream := @OpenInputStream;
  FInputStream := AInputStream;
end;

constructor TStreamUnzipper.Create(const AZipFileName: string);
begin
  inherited Create;
  OnCreateStream := @CreateStream;
  OnDoneStream := @DoneStream;
  OnOpenInputStream := @OpenInputStream;
  FInputStream := nil;
  FileName := AZipFileName;
end;

procedure TStreamUnzipper.CreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  FSuccess := True;
  AStream := FOutputStream;
end;

procedure TStreamUnzipper.DoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := nil;
end;

procedure TStreamUnzipper.OpenInputStream(Sender: TObject; var AStream: TStream);
begin
  AStream := FInputStream;
end;

function TStreamUnzipper.UnzipFile(const AZippedFile: string; ADestStream: TStream): boolean;
begin
  FOutputStream := ADestStream;
  FSuccess := False;
  Files.Clear;
  Files.Add(AZippedFile);
  UnZipAllFiles;
  Result := FSuccess;
end;

function TStreamUnzipper.UnZipFile(AIndex: integer; ADestStream: TStream): boolean;
begin
  FOutputStream := ADestStream;
  Result := False;
  FSuccess := False;

  if Entries.Count <= 0 then  //in case we haven't read it before.
    Examine;
  if (AIndex < 0) or (AIndex >= Entries.Count) then
    Exit;
  OpenInput;
  try
    UnZipOneFile(Entries[AIndex]);
  finally
    CloseInput;
  end;
  Result := FSuccess;
end;

function TStreamUnzipper.UnZipFile(Item: TFullZipFileEntry; ADestStream: TStream): boolean;
begin
  FOutputStream := ADestStream;
  FSuccess := False;
  OpenInput;
  try
    UnZipOneFile(Item);
  finally
    CloseInput;
  end;
  Result := FSuccess;
end;

procedure TStreamUnzipper.UnZipFile(AZippedFile: string; AOutputPath: string;AFlat: boolean);
var
  oldOnCreateStream: TOnCustomStreamEvent;
  oldOnDoneStream: TOnCustomStreamEvent;
  oldFlat: boolean;
begin
  oldOnCreateStream := OnCreateStream;
  oldOnDoneStream := OnDoneStream;
  OnCreateStream := nil;  // needed to take AOutputPath into account
  OnDoneStream := nil;    // Needed to close the output files
  oldFlat := Flat;
  try
    Flat := AFlat;
    OutputPath := AOutputPath;
    UnZipFile(AZippedFile);
  finally
    Flat := oldFlat;
    OnCreateStream := oldOnCreateStream;
    OnDoneStream := oldOnDoneStream;
  end;
end;

procedure TStreamUnzipper.GetFullFileNames(AOutStrings: TStrings; AStartingPath: string);
var
  Count, Index: integer;
  FullFileName: string;
begin
  if Entries.Count <= 0 then
    Examine;
  Count := Entries.Count;
  Index := 0;
  while Index < Count do
  begin
    FullFileName := Entries[Index].ArchiveFileName;
    if AStartingPath = '' then
      AOutStrings.Add(FullFileName)
    else
    begin
      if Pos(AStartingPath, FullFileName) > 0 then
        AOutStrings.Add(FullFileName);
    end;
    Inc(Index);
  end;
end;

procedure UnzipFile(AZipFileName, AZippedFile, ADestFolder: string);
var
  list: TStringList;
  unzip: TUnzipper;
begin
  list := TStringList.Create;
  try
    list.Add(AZippedFile);
    unzip := TUnzipper.Create;
    try
      Unzip.OutputPath := ADestFolder;
      Unzip.UnzipFiles(AZipFileName, list);
    finally
      unzip.Free;
    end;
  finally
    list.Free;
  end;
end;

//WARNING: AZipStream is Freed by TUnzipper.
function UnzipToStream(AZipStream: TStream; const AZippedFile: string; ADestStream: TStream): boolean;
var
  unzip: TStreamUnzipper;
  p: int64;
begin
  p := ADestStream.Position;
  unzip := TStreamUnzipper.Create(AZipStream);
  try
    Result := unzip.UnzipFile(AZippedFile, ADestStream);
    ADestStream.Position := p;
  finally
    unzip.Free;
  end;
end;

function UnzipToStream(AZipFileName: string; const AZippedFile: string;
  ADestStream: TStream): boolean;
var
  FileStream:TFileStream;
begin
  Result := false;
  if not FileExists(AZipFileName) then
    Exit(False);
  FileStream := TFileStream.Create(AZipFileName,fmOpenRead);
  FileStream.Position := 0;
  Result := UnzipToStream(FileStream, AZippedFile, ADestStream);
  //FileStream.Free;  //Freed in TUnzipper closeInput, not very intuitive behavior. :-(
end;

end.
