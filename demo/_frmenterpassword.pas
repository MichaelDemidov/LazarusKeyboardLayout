unit _frmEnterPassword;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Classes;

type
  TLoginResult = (lrSuccess, lrFail, lrAbort);

  TOnVerifyPassword = function(const Login, Password: string; out ErrorMessage:
    string): TLoginResult of object;

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
    procedure btnOkClick(Sender: TObject);
    procedure edtLoginChange(Sender: TObject);
    procedure edtPasswordChange(Sender: TObject);
    procedure edtPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtPasswordKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrKeyboardLayoutTimer(Sender: TObject);
    procedure UpdateLayout;
  private
    {$IF defined(WINDOWS)}
    // The system hook handle for Windows
    FHook: THandle;
    {$ENDIF}

    // Verify password event
    FOnVerifyPassword: TOnVerifyPassword;

    // Getters and setters for the properties below
    function GetLogin: string;
    function GetPassword: string;
    procedure SetLogin(Value: string);
    procedure SetPassword(Value: string);

    // Update btnOK.Enabled property if the login and password are [not] empty
    procedure UpdateButtons;

    // Verify the password
    function DoVerifyPassword(out ErrorMessage: string): TLoginResult;
  public
    // User login
    property Login: string read GetLogin write SetLogin;

    // User password
    property Password: string read GetPassword write SetLogin;

    // Verify password event
    property OnVerifyPassword: TOnVerifyPassword read FOnVerifyPassword write
      FOnVerifyPassword;

    // True if the login attempt was successful
    function Success: Boolean;
  end;

var
  frmEnterPassword: TfrmEnterPassword;

implementation

{$R *.lfm}

uses
  {$IF defined(WINDOWS)}
  Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
  Graphics, Dialogs, KeyboardLayout;

{ TfrmEnterPassword }

function TfrmEnterPassword.GetLogin: string;
begin
  Result := edtLogin.Text;
end;

function TfrmEnterPassword.GetPassword: string;
begin
  Result := edtPassword.Text;
end;

procedure TfrmEnterPassword.SetLogin(Value: string);
begin
  edtLogin.Text := Value;
end;

procedure TfrmEnterPassword.SetPassword(Value: string);
begin
  edtPassword.Text := Value;
end;

procedure TfrmEnterPassword.UpdateButtons;
begin
  btnOk.Enabled := (Login <> '') and (Password <> '')
end;

procedure TfrmEnterPassword.UpdateLayout;
begin
  lblKeyboardLayout.Caption := UpperCase(GetKeyboardLayoutAbbr);
end;

function TfrmEnterPassword.Success: Boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEnterPassword.FormCreate(Sender: TObject);
begin
  // The initial field values
  {$IF defined(WINDOWS)}
  FHook := 0;
  {$ENDIF}
  FOnVerifyPassword := nil;

  // Appearance
  edtPassword.PasswordChar := #149;
  ClientHeight := btnOK.BoundsRect.Bottom + edtLogin.Top;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TfrmEnterPassword.FormDestroy(Sender: TObject);
begin
  FOnVerifyPassword := nil;
end;

procedure TfrmEnterPassword.edtLoginChange(Sender: TObject);
begin
  UpdateButtons;
end;

function TfrmEnterPassword.DoVerifyPassword(out ErrorMessage: string):
  TLoginResult;
begin
  Result := lrAbort;

  // Verify the password
  if Assigned(FOnVerifyPassword) then
    Result := FOnVerifyPassword(Login, Password, ErrorMessage);
end;

procedure TfrmEnterPassword.btnOkClick(Sender: TObject);
var
  ErrorMessage: string;
begin
  // Verify the password, set the ModalResult
  case DoVerifyPassword(ErrorMessage) of
    lrSuccess: // login & password successfully verified
      ModalResult := mrOk;
    lrFail: // wrong login or password, give the user another chance
      ;
    lrAbort: // the user has exhausted all their chances
      ModalResult := mrCancel;
  end;
  // If there is an error message, show it
  if ErrorMessage <> '' then
    MessageDlg(ErrorMessage, mtError, [mbOk], 0);
end;

procedure TfrmEnterPassword.edtPasswordChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmEnterPassword.edtPasswordKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Little trick: press the Ctrl key to view the password!
  if Key = VK_CONTROL then
    edtPassword.PasswordChar := #0;
end;

procedure TfrmEnterPassword.edtPasswordKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // The password is visible only while the Ctrl key is down
  if Key = VK_CONTROL then
    edtPassword.PasswordChar := #149
end;

procedure TfrmEnterPassword.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  {$IF defined(WINDOWS)}
  // Unhook the system hook
  if FHook <> 0 then
    UnhookWindowsHookEx(FHook);
  {$ENDIF}
  // Stop the timer (if running) and free memory
  tmrKeyboardLayout.Enabled := False;
  CloseAction := caFree;
end;

{$IF defined(WINDOWS)}
// The hook function
function HookProc(nCode: Longint; wParam: WParam; lParam: LParam): LResult;
  stdcall;
begin
  if nCode = HSHELL_LANGUAGE then
    frmEnterPassword.UpdateLayout;
  Result := CallNextHookEx(WH_SHELL, nCode, wParam, lParam);
end;
{$ENDIF}

procedure TfrmEnterPassword.FormShow(Sender: TObject);
begin
  UpdateButtons;
  {$IF defined(WINDOWS)}
  // Set the Windows hook
  FHook := SetWindowsHookEx(WH_SHELL, @HookProc, 0, MainThreadId);
  if FHook = 0 then // if failed, use the timer
  begin
    tmrKeyboardLayout.Enabled := True;
    UpdateLayout;
  end;
  {$ELSE}
  // Start the timer
  tmrKeyboardLayout.Enabled := True;
  UpdateLayout;
  {$ENDIF}
end;

procedure TfrmEnterPassword.tmrKeyboardLayoutTimer(Sender: TObject);
begin
  UpdateLayout;
end;

end.
