unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btnExport: TButton;
    btnimport: TButton;
    StaticText1: TStaticText;
    procedure btnExportClick(Sender: TObject);
    procedure btnimportClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  frmExport,frmimport;

{ TForm1 }

procedure TForm1.btnExportClick(Sender: TObject);
var
  ExportDialog:TdlgExport;
begin
  try
    ExportDialog:=TdlgExport.Create(nil);
    ExportDialog.ShowModal;
  finally
    ExportDialog.Free;
  end;
end;

procedure TForm1.btnimportClick(Sender: TObject);
var
  ImportDialog:TdlgImport;
begin
  try
    ImportDialog:=TdlgImport.Create(nil);
    ImportDialog.ShowModal;
  finally
    ImportDialog.Free;
  end;
end;

end.

