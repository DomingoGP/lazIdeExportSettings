program lazexportsettings;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, frmimport, lazexpimpcnfintf, lazexpimpcnfjcf,
  lazexpimpcnfeditormacros, lazexpimpinterfacevtv, frmExport, StreamUnzipper,
  lazexpimpregisterclasses, lazexpimpcnfeditoroptions,
  lazexpimpcnfdebuggeroptions, lazexpimpcnfdelphiconverter,
  lazexpimpcnfdesktops, lazexpimpconsts, xmllazutils, 
lazexpimpcnfcodetoolsoptions, lazexpimpcnfformeditoroptions, 
lazexpimpcnfobjectinspector
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.{%H-}MainFormOnTaskbar := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

