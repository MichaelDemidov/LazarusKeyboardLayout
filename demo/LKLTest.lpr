program LKLTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, _frmEnterPassword, KeyboardLayout
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Lazarus Keyboard Layout Test';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmEnterPassword, frmEnterPassword);
  if not frmEnterPassword.Success then
    Application.Terminate;
  Application.Run;
end.

