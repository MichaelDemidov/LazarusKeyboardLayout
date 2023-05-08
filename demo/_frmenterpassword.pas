unit _frmEnterPassword;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls;

type

  { TfrmEnterPassword }

  TfrmEnterPassword = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edtLogin: TEdit;
    edtPassword: TEdit;
    lblKeyboardLayout: TLabel;
    lblLogin: TLabel;
    lblPassword: TLabel;
    tmrKeyboardLayout: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrKeyboardLayoutTimer(Sender: TObject);
  end;

var
  frmEnterPassword: TfrmEnterPassword;

implementation

uses
  KeyboardLayout;

{$R *.lfm}

{ TfrmEnterPassword }

procedure TfrmEnterPassword.FormCreate(Sender: TObject);
begin
  lblKeyboardLayout.Font.Style := lblKeyboardLayout.Font.Style + [fsBold];
  lblKeyboardLayout.Font.Color := clCaptionText;
end;

procedure TfrmEnterPassword.FormShow(Sender: TObject);
begin
  tmrKeyboardLayout.OnTimer(tmrKeyboardLayout);
end;

procedure TfrmEnterPassword.tmrKeyboardLayoutTimer(Sender: TObject);
begin
  lblKeyboardLayout.Caption := UpperCase(GetKeyboardLayoutAbbr);
end;

end.

