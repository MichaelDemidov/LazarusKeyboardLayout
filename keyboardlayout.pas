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
  Classes;

type
  { TKeyboardLayoutIndicator }

  TIndicatorEvent = procedure(LayoutText: string) of object;

{$IF not defined(WINDOWS)}
  TKeyboardLayoutIndicator = class;

  TXKbdThread = class(TThread)
  private
    // Current layout
    FCurrLayout: string;
    // Link to the TKeyboardLayoutIndicator object to call the event handler
    FKeyboardLayoutIndicator: TKeyboardLayoutIndicator;
    // Procedure called from Synchronize() method
    procedure UpdateLayout;
  protected
    procedure Execute; override;
  public
    // Constructor and destructor
    constructor Create(CreateSuspended: Boolean; AOwner:
      TKeyboardLayoutIndicator);
    destructor Destroy; override;
  end;
{$ENDIF}

  TKeyboardLayoutIndicator = class
  private
    FOnUpdateIndicator: TIndicatorEvent;
{$IF defined(WINDOWS)}
    // The system hook handle for Windows
    FHook: THandle;
{$ELSE}
    // The thread to handle the keyboard events
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
  end;

// Warning! *On Windows,* creating multiple instances of the
// TKeyboardLayoutIndicator class is pointless due to a system hook using this
// global variable. If you need more than one indicator in your application,
// please think of something to hold a list of them and use the single
// OnUpdateIndicator event handler. Or modify the TKeyboardLayoutIndicator class
// to create a list of event handlers instead of a single OnUpdateIndicator
var
  KeyboardLayoutIndicator: TKeyboardLayoutIndicator;

implementation

uses
  Sysutils,
{$IF defined(WINDOWS)}
  Windows;
{$ELSE}
  xlib, XKB, xkblib, keysym;
{$ENDIF}

{$IF defined(WINDOWS)}

// The hook function; it uses the global variable KeyboardLayoutIndicator--see
// the warning above
function HookProc(nCode: Longint; wParam: WParam;
  lParam: LParam): LResult; stdcall;
var
  LayoutId, LayoutName: array[0..KL_NAMELENGTH - 1] of Char;
begin
  if Assigned(KeyboardLayoutIndicator.OnUpdateIndicator)
    and (nCode = HSHELL_LANGUAGE) then
  begin
    // Get the current layout from the list
    if GetKeyboardLayoutName(LayoutId) then
    begin
      LayoutName[0] := #0;
      // MSDN marks GetLocaleInfo() as deprecated, but it works, and
      // the Lazarus developers don't import the new GetLocaleInfoEx() function.
      // You cas use LOCALE_SABBREVLANGNAME instead of LOCALE_SISO639LANGNAME
      // to retrieve slightly more detailed layout name: e.g. ENU (U.S. English)
      // instead of EN (English)
      if GetLocaleInfo(StrToInt('$' + StrPas(LayoutId)), LOCALE_SISO639LANGNAME,
        @LayoutName[0], SizeOf(LayoutName) - 1) <> 0 then
        KeyboardLayoutIndicator.OnUpdateIndicator(StrPas(LayoutName));
    end;
  end;
  Result := CallNextHookEx(WH_SHELL, nCode, wParam, lParam);
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
    // Get the keyboad layout name and call the update event handler
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

      // Initial layout name
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

    // Release the resources used
    XkbSelectEvents(Display, XkbUseCoreKbd, XkbAllEventsMask, 0);
    XCloseDisplay(Display);
  end;
end;

{$ENDIF}

{ TKeyboardLayoutIndicator }

constructor TKeyboardLayoutIndicator.Create;
begin
  FOnUpdateIndicator := nil;
{$IF not defined(WINDOWS)}
  FXKbdThread := TXKbdThread.Create(True, Self);
{$eNDIF}
end;

destructor TKeyboardLayoutIndicator.Destroy;
begin
  FOnUpdateIndicator := nil;
  StopWatching;
{$IF not defined(WINDOWS)}
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
