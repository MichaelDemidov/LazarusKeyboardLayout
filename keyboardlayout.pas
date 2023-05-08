(**********************************************************************

    Cross-platform (Windows + GNU/Linux X11) function to retrieve
    the current keyboard layout from the OS in human-readable form
    ('US', 'RU', etc.) written in FreePascal / Lazarus. It is also
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

// Get the current layout as two-letter string ('US', 'RU', etc.)
function GetKeyboardLayoutAbbr: string;

implementation

uses
  Classes, SysUtils,
{$IF defined(WINDOWS)}
  Windows, Registry;
{$ELSE}
  Process;
{$ENDIF}

var
  Locales: TStringList = nil; // list of all locales available in the OS:
  // in Windows: each string is a pair of locale id and abbreviation,
  //   e.g. 00000409=US
  // in X11: each string is an abbreviation, and is selected by index

{$IF defined(WINDOWS)}
// Read locale list from the system registry. If anyone knows better way--
// let me know, please
procedure LoadLocales;
var
  I: Integer;
begin
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    Locales := TStringList.Create;
    if OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Control\Keyboard ' +
      'Layout\DosKeybCodes') then
    begin
      GetValueNames(Locales);
      for I := 0 to Locales.Count - 1 do
        Locales[I] := Locales[I] + '=' + ReadString(Locales[I]);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

// Select the current layout from the list
function GetKeyboardLayoutAbbr: string;
var
  Z: array[0..KL_NAMELENGTH] of Char;
begin
  if GetKeyboardLayoutName(Z) then
    Result := Locales.Values[Z];
end;

{$ELSE}

// A helper function for parsing command output
function ExtractValue(const DataArray, Key: string): string;
var
  I, J, L: Integer;
begin
  Result := '';
  I := Pos(Key + ':', DataArray);
  if I > 0 then
  begin
    L := I + Length(Key) + 1;
    while (L < Length(DataArray)) and (DataArray[L] = ' ') do
      Inc(L);
    if L < Length(DataArray) then
    begin
      I := Pos(':', DataArray, L);
      J := Pos(#10, DataArray, L);
      if J < I then
        I := J - 1
      else
      begin
        if I = 0 then
          I := Length(DataArray)
        else
          while not (DataArray[I] in [' ', #10, #9]) do
            Dec(I);
      end;
      while DataArray[I] in [' ', #10, #9] do
        Dec(I);
      Result := DataArray.Substring(L - 1, I - L + 1);
    end;
  end;
end;

// Get a comma-separated layout list, e.g. 'us,ru'
procedure LoadLocales;
const
  CmdGetListEx = 'setxkbmap';
  CmdGetListParams = '-query';
var
  S: string;
begin
  if RunCommand(CmdGetListEx, [CmdGetListParams], S, [poNoConsole,
    poWaitOnExit]) then
  begin
    Locales := TStringList.Create;
    Locales.CommaText := ExtractValue(S, 'layout');
  end;
end;

// Get a number containing the identifier of the current layout in the format
// 0000X0YY, where X is the zero-based index of the current layout (i.e. 'ru' in
// the example above), and YY is a state of keyboard (CapsLock, NumLock, etc.)
// and can have values between 00 and 32--run xset in a terminal to see details
function GetKeyboardLayoutAbbr: string;
const
  CmdGetIdEx = 'xset';
  CmdGetIdParams = '-q';
var
  S: string;
  I, E: Integer;
begin
  Result := '';
  if RunCommand(CmdGetIdEx, [CmdGetIdParams], S, [poNoConsole, poWaitOnExit])
  then
  begin
    S := ExtractValue(S, 'LED mask');
    if S <> '' then
    begin
      Val(S.SubString(0, 5), I, E);
      if E = 0 then
        Result := Locales[I];
    end;
  end;
end;
{$ENDIF}

initialization
  LoadLocales;
finalization
  if Assigned(Locales) then
    Locales.Free;
end.
