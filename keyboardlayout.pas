(**********************************************************************


    Cross-platform (Windows + GNU/Linux X11) class to retrieve
    the current keyboard layout from the OS in human-readable form
    ('us', 'it', etc.) written in FreePascal / Lazarus. It is also
    Delphi compatible, see warning below.

    Copyright (c) 2023, Michael Demidov

    Visit my GitHub page to check for updates, report issues, etc.:
    https://github.com/MichaelDemidov

    Drop me an e-mail at: michael.v.demidov@gmail.com

    DELPHI WARNING!
    You can use the module with Delphi on Windows. To do this,
    remove the {$mode ObjFPC}{$H+} line, all {$IF defined(WINDOWS)}
    lines, and all lines between the associated {$ELSE} and
    {$ENDIF}, including themselves. You may also need to fix the
    module names in the section 'uses' (depending on the version of
    Delphi)

 **********************************************************************)
unit KeyboardLayout;

{$mode ObjFPC}{$H+}

interface

uses
  Sysutils, Classes;

type
  { TKeyboardLayoutIndicator }

  TIndicatorEvent = procedure(LayoutText: string) of object;

  TKeyboardLayoutIndicator = class;

{$IF not defined(WINDOWS)}
  TXKbdThread = class(TThread)
  private
    // Current layout
    FCurrLayout: string;
    // Link to the TKeyboardLayoutIndicator object
    FKeyboardLayoutIndicator: TKeyboardLayoutIndicator;
    // Procedure to synchronize
    procedure UpdateLayout;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AOwner:
      TKeyboardLayoutIndicator);
    destructor Destroy; override;
  end;
{$ENDIF}

  TKeyboardLayoutIndicator = class
  private
    FOnUpdateIndicator: TIndicatorEvent;
{$IF defined(WINDOWS)}
    // See the Layouts property below
    FLayouts: TStringList;
    // The system hook handle for Windows
    FHook: THandle;
{$ELSE}
    FXKbdThread: TXKbdThread;
{$ENDIF}
  public
    // Constructor and destructor
    constructor Create;
    destructor Destroy; override;

    // Start keyboard layout watching
    procedure StartWatching;

    // Stop keyboard layout watching
    procedure StopWatching;

    // Update indicator event (see the note below)
    property OnUpdateIndicator: TIndicatorEvent read FOnUpdateIndicator write
      FOnUpdateIndicator;
{$IF defined(WINDOWS)}
    // List of all locale ids / keyboard layouts available in the Windows'
    // system registry: each string is a pair of locale id and abbreviation,
    // e.g. '00000409=US'
    property Layouts: TStringList read FLayouts;
{$ENDIF}
  end;

// Warning! Creating multiple instances of the TKeyboardLayoutIndicator class
// *in Windows* is pointless due to a system hook using this global variable.
// If you need more than one indicator in your application, please think of
// something to hold a list of them and use the single OnUpdateIndicator
// event handler. Or modify the TKeyboardLayoutIndicator class to create a list
// of event handlers instead of a single OnUpdateIndicator
var
  KeyboardLayoutIndicator: TKeyboardLayoutIndicator;

implementation

uses
{$IF defined(WINDOWS)}
  Windows, Registry;
{$ELSE}
  xlib, XKB, xkblib, keysym;
{$ENDIF}

{$IF defined(WINDOWS)}
// The hook function; it uses the global variable KeyboardLayoutIndicator
function HookProc(nCode: Longint; wParam: WParam;
  lParam: LParam): LResult; stdcall;
var
  Z: array[0..KL_NAMELENGTH] of Char;
  CurrLayout: string;
begin
  if Assigned(KeyboardLayoutIndicator.OnUpdateIndicator)
    and (nCode = HSHELL_LANGUAGE) then
  begin
    // Select the current layout from the list
    if GetKeyboardLayoutName(Z) then
    begin
      CurrLayout := KeyboardLayoutIndicator.Layouts.Values[Z];
      KeyboardLayoutIndicator.OnUpdateIndicator(CurrLayout);
    end;
  end;
  Result := CallNextHookEx(WH_SHELL, nCode, wParam, lParam);
end;

// Read locale list from the system registry. If anyone knows better way--
// let me know, please
procedure LoadLayouts(Layouts: TStrings);
var
  I: Integer;
begin
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Control\Keyboard ' +
      'Layout\DosKeybCodes') then
    begin
      GetValueNames(Layouts);
      for I := 0 to Layouts.Count - 1 do
        Layouts[I] := Layouts[I] + '=' + ReadString(Layouts[I]);
      CloseKey;
    end;
  finally
    Free;
  end;
end;
{$ELSE}

{ TXKbdThread }

constructor TXKbdThread.Create(CreateSuspended: Boolean; AOwner:
  TKeyboardLayoutIndicator);
begin
  inherited Create(CreateSuspended);
  FCurrLayout := '';
  FKeyboardLayoutIndicator := AOwner;
  FreeOnTerminate := False;
  Priority := tpNormal;
end;

destructor TXKbdThread.Destroy;
begin
  FKeyboardLayoutIndicator := nil;
  inherited Destroy;
end;

procedure TXKbdThread.UpdateLayout;
begin
  if Assigned(FKeyboardLayoutIndicator.OnUpdateIndicator) then
    FKeyboardLayoutIndicator.OnUpdateIndicator(FCurrLayout);
end;

procedure TXKbdThread.Execute;

  procedure UpdateCurrLayout(Display: PDisplay; Keyboard: PXkbDescPtr;
    State: TXkbStateRec);
  var
    PLayout: PChar;
    Layout: string;
  begin
    PLayout := XGetAtomName(Display, Keyboard^.names^.groups[State.group]);
    Layout := PLayout;
    Delete(Layout, 3, Length(Layout) - 2);
    if Layout <> FCurrLayout then
    begin
      FCurrLayout := Layout;
      Synchronize(@UpdateLayout);
    end;
    XFree(PLayout);
  end;

var
  Event: TXEvent;
  Display: PDisplay;
  State: TXkbStateRec;
  OldGroup: Integer = -1;
  Keyboard: PXkbDescPtr;
begin
  // Get the Display handle
  Display := XOpenDisplay(nil);

  // Catch the keyboard events
  if Assigned(Display) then
  begin
    if XkbSelectEvents(Display, XkbUseCoreKbd, XkbAllEventsMask,
      XkbAllEventsMask) then
    begin
      XKeysymToKeycode(Display, XK_F1);
      Keyboard := XkbGetKeyboard(Display, XkbAllComponentsMask, XkbUseCoreKbd);

      if XkbGetState(Display, XkbUseCoreKbd, @State) = 0 then
      begin
        UpdateCurrLayout(Display, Keyboard, State);
        OldGroup := State.group;
      end;

      while not Terminated do
      begin
        if XkbGetState(Display, XkbUseCoreKbd, @State) = 0 then
          while not Terminated and (XPending(Display) <> 0) do
          begin
            XNextEvent(Display, @Event);

            if State.group <> OldGroup then
            begin
              UpdateCurrLayout(Display, Keyboard, State);
              OldGroup := State.group;
            end;
          end;
        if not Terminated then
          XNextEvent(Display, @Event);
      end;

      XkbFreeKeyboard(Keyboard, XkbAllComponentsMask, True);
    end;

    // Release the resources used, destroy the window
    XkbSelectEvents(Display, XkbUseCoreKbd, XkbAllEventsMask, 0);
    XCloseDisplay(Display);
  end;
end;

{$ENDIF}

{ TKeyboardLayoutIndicator }

constructor TKeyboardLayoutIndicator.Create;
begin
  FOnUpdateIndicator := nil;
{$IF defined(WINDOWS)}
  FLayouts := TStringList.Create;
  LoadLayouts(FLayouts);
{$ELSE}
  FXKbdThread := TXKbdThread.Create(True, Self);
{$eNDIF}
end;

destructor TKeyboardLayoutIndicator.Destroy;
begin
  FOnUpdateIndicator := nil;
  StopWatching;
{$IF defined(WINDOWS)}
  FreeAndNil(FLayouts);
{$ELSE}
  FreeAndNil(FXKbdThread);
{$ENDIF}
  inherited Destroy;
end;

procedure TKeyboardLayoutIndicator.StartWatching;
begin
{$IF defined(WINDOWS)}
  // Set the system hook
  FHook := SetWindowsHookEx(WH_SHELL, @HookProc, 0, MainThreadId);
{$ELSE}
  // Start the thread
  FXKbdThread.Start;
{$ENDIF}
end;

procedure TKeyboardLayoutIndicator.StopWatching;
begin
{$IF defined(WINDOWS)}
  // Unhook the system hook
  if FHook <> 0 then
    UnhookWindowsHookEx(FHook);
{$ELSE}
  // Stop the thread
  if Assigned(FXKbdThread) then
    FXKbdThread.Terminate;
{$ENDIF}
end;

end.
