////////////////////////////////////////////////////////////////////////////////
// File:       PegtopThemes.pas
// Classes:    TPegtopTheme
// Version:    1.00
// Date:       09 Sep 2004
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// DefaultTheme is the default instance of TPegtopTheme (created automatically).
// TPegtopTheme offers some methods for drawing XP themed controls. Some methods
// (like DrawButton) implement alternative drawing routines if XP themes are
// not supported.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit JPPegtopThemes;

interface

uses
  Windows, Forms, Graphics, StdCtrls;

type
  TPegtopThemeElement = (pteButton, pteComboBox, pteEdit, pteProgressBar,
    pteScrollBar, pteSpinButton, pteTrackBar);

  TPegtopThemeState = (ptsNormal, ptsHot, ptsPushed, ptsDisabled, ptsFocused);
  TPegtopElementOrientation = (peoHorizontal, peoVertical);
  TPegtopElementDirection = (pedUp, pedDown, pedLeft, pedRight);
  TPegtopSliderKind = (pskHorizontal, pskDown, pskUp, pskVertical, pskLeft, pskRight);
  TPegtopScrollBarPart = (pspUpper, pspLower);

  TPegtopTheme = class
  private
    FEnabled: Boolean;
    FThemeData: array[TPegtopThemeElement] of THandle;
    function GetThemeData(Element: TPegtopThemeElement): THandle;
    procedure DrawElement(Handle: HDC; DrawRect: TRect;
      Element: TPegtopThemeElement; Part, State: Integer);
    procedure SetEnabled(Value: Boolean);
    function GetSupported: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetColor(Element: TPegtopThemeElement; Part, State, Prop: Integer): TColor;
    function GetSysColor(ColorId: Integer): TColor;
    procedure DrawParent(Wnd: THandle; Handle: HDC; DrawRect: PRect);
    procedure DrawButton(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; WithFocusRect: Boolean = False);
    procedure DrawRadioButton(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Checked: Boolean);
    procedure DrawCheckButton(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; CheckState: TCheckBoxState);
    procedure DrawBevel(Handle: HDC; DrawRect: TRect);
    procedure DrawComboBox(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState);
    procedure DrawComboBoxButton(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState);
    procedure DrawEdit(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState);
    procedure DrawProgressBox(Handle: HDC; DrawRect: TRect;
      Orientation: TPegtopElementOrientation);
    procedure DrawProgressBar(Handle: HDC; DrawRect: TRect;
      Orientation: TPegtopElementOrientation);
    procedure DrawScrollButton(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Direction: TPegtopElementDirection);
    procedure DrawScrollHandle(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Orientation: TPegtopElementOrientation);
    procedure DrawScrollBar(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Orientation: TPegtopElementOrientation;
      Part: TPegtopScrollBarPart);
    procedure DrawScrollGrip(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Orientation: TPegtopElementOrientation);
    procedure DrawSizeGrip(Handle: HDC; DrawRect: TRect;
      RightToLeft: Boolean);
    procedure DrawSpinButton(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Direction: TPegtopElementDirection);
    procedure DrawTrackBar(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Orientation: TPegtopElementOrientation);
    procedure DrawTrackSlider(Handle: HDC; DrawRect: TRect;
      State: TPegtopThemeState; Kind: TPegtopSliderKind);
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Supported: Boolean read GetSupported;
  end;

var
  DefaultTheme: TPegtopTheme;

implementation

uses
  SysUtils;

const
  ElementName: array[TPegtopThemeElement] of PWideChar =
  ('button', 'combobox', 'edit', 'progress', 'scrollbar', 'spin', 'trackbar');

  StandardStateIndex: array[TPegtopThemeState] of Integer = (1, 2, 3, 4, 1);
  ButtonStateIndex: array[TPegtopThemeState] of Integer = (1, 2, 3, 4, 5);
  TrackStateIndex: array[TPegtopThemeState] of Integer = (1, 2, 3, 5, 4);

var
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): THandle; stdcall;
{$EXTERNALSYM OpenThemeData}

  CloseThemeData: function(hTheme: THandle): HRESULT; stdcall;
{$EXTERNALSYM CloseThemeData}

  DrawThemeBackground: function(hTheme: THandle; hHandle: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeBackground}

  DrawThemeParentBackground: function(hWnd: THandle; hHandle: HDC; Rect: PRect): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeParentBackground}

  GetThemeColor: function(hTheme: THandle; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall;
{$EXTERNALSYM GetThemeColor}

  GetThemeSysColor: function(hTheme: THandle; iColorId: Integer): COLORREF; stdcall;
{$EXTERNALSYM GetThemeSysColor}

  ThemeLibrary: THandle;

////////////////////////////////////////////////////////////////////////////////
// TPegtopTheme
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopTheme.Create;
var
  Element: TPegtopThemeElement;
begin
  FEnabled := Supported;
  for Element := Low(TPegtopThemeElement) to High(TPegtopThemeElement) do
    FThemeData[Element] := 0;
end;

destructor TPegtopTheme.Destroy;
var
  Element: TPegtopThemeElement;
begin
  if Assigned(CloseThemeData) then begin
    for Element := Low(TPegtopThemeElement) to High(TPegtopThemeElement) do begin
      if FThemeData[Element] <> 0 then begin
        CloseThemeData(FThemeData[Element]);
      end;
    end;
  end;
  inherited;
end;

function TPegtopTheme.GetThemeData(Element: TPegtopThemeElement): THandle;
begin
  if Assigned(OpenThemeData) and (FThemeData[Element] = 0) then begin
    FThemeData[Element] := OpenThemeData(Application.Handle, ElementName[Element]);
  end;
  Result := FThemeData[Element];
end;

procedure TPegtopTheme.DrawElement(Handle: HDC; DrawRect: TRect;
  Element: TPegtopThemeElement; Part, State: Integer);
var
  Data: THandle;
begin
  if Assigned(DrawThemeBackground) then begin
    Data := GetThemeData(Element);
    if Data <> 0 then DrawThemeBackground(Data, Handle, Part, State, DrawRect, NIL);
  end;
end;

procedure TPegtopTheme.DrawParent(Wnd: THandle; Handle: HDC; DrawRect: PRect);
begin
  if Assigned(DrawThemeParentBackground) then begin
    DrawThemeParentBackground(Wnd, Handle, DrawRect);
  end;
end;

function TPegtopTheme.GetColor(Element: TPegtopThemeElement; Part, State, Prop: Integer): TColor;
var
  Data: THandle;
  C: COLORREF;
begin
  Result := -1;
  if Assigned(GetThemeColor) then begin
    Data := GetThemeData(Element);
    if GetThemeColor(Data, Part, State, Prop, C) = S_OK then Result := C;
  end;
end;

function TPegtopTheme.GetSysColor(ColorId: Integer): TColor;
begin
  Result := -1;
  if Assigned(GetThemeSysColor) then begin
    Result := GetThemeSysColor(0, ColorId);
    // see COLOR_ in Windows.pas
  end;
end;

procedure TPegtopTheme.DrawButton(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; WithFocusRect: Boolean = False);
const
  StateFlags: array[TPegtopThemeState] of Integer = (0, 0, DFCS_FLAT, DFCS_INACTIVE, 0);
var
  FrameRect: TRect;
begin
  if FEnabled and (GetThemeData(pteButton) <> 0) then begin
    DrawElement(Handle, DrawRect, pteButton, 1, ButtonStateIndex[State]);
  end
  else begin
    FrameRect.Left := DrawRect.Left + 1;
    FrameRect.Top := DrawRect.Top + 1;
    FrameRect.Right := DrawRect.Right - 1;
    FrameRect.Bottom := DrawRect.Bottom - 1;
    if State in [ptsFocused, ptsPushed] then begin
      DrawEdge(Handle, FrameRect, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT or BF_MONO);
    end;
    DrawFrameControl(Handle, FrameRect, DFC_BUTTON,
      DFCS_BUTTONPUSH or StateFlags[State]);
  end;
  if WithFocusRect then begin
    FrameRect.Left := DrawRect.Left + 3;
    FrameRect.Top := DrawRect.Top + 3;
    FrameRect.Right := DrawRect.Right - 3;
    FrameRect.Bottom := DrawRect.Bottom - 3;
    DrawFocusRect(Handle, FrameRect);
  end;
end;

procedure TPegtopTheme.DrawRadioButton(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Checked: Boolean);
const
  StateFlags: array[TPegtopThemeState] of Integer = (0, 0, DFCS_PUSHED, DFCS_INACTIVE, 0);
  CheckFlags: array[Boolean] of Integer = (0, DFCS_CHECKED);
begin
  if FEnabled and (GetThemeData(pteButton) <> 0) then begin
    if Checked then
      DrawElement(Handle, DrawRect, pteButton, 2, StandardStateIndex[State] + 4)
    else
      DrawElement(Handle, DrawRect, pteButton, 2, StandardStateIndex[State]);
  end
  else begin
    DrawFrameControl(Handle, DrawRect, DFC_BUTTON,
      DFCS_BUTTONRADIO or StateFlags[State] or CheckFlags[Checked]);
  end;
end;

procedure TPegtopTheme.DrawCheckButton(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; CheckState: TCheckBoxState);
const
  StateFlags: array[TPegtopThemeState] of Integer = (0, 0, DFCS_PUSHED, DFCS_INACTIVE, 0);
  CheckFlags: array[TCheckBoxState] of Integer = (0, DFCS_CHECKED, DFCS_CHECKED or DFCS_INACTIVE);
var
  CheckRect: TRect;
  CheckSize: TSize;
begin
  if FEnabled and (GetThemeData(pteButton) <> 0) then begin
    if CheckState = cbUnchecked	then
      DrawElement(Handle, DrawRect, pteButton, 3, StandardStateIndex[State])
    else if CheckState = cbChecked then
      DrawElement(Handle, DrawRect, pteButton, 3, StandardStateIndex[State] + 4)
    else
      DrawElement(Handle, DrawRect, pteButton, 3, StandardStateIndex[State] + 8);
  end
  else begin
    CheckSize.cx := GetSystemMetrics(SM_CXMENUCHECK);
    CheckSize.cy := GetSystemMetrics(SM_CYMENUCHECK);
    CheckRect.Left := DrawRect.Left + (DrawRect.Right - DrawRect.Left - CheckSize.cx) div 2;
    CheckRect.Top := DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - CheckSize.cy) div 2;
    CheckRect.Right := CheckRect.Left + CheckSize.cx;
    CheckRect.Bottom := CheckRect.Top + CheckSize.cy;
    DrawFrameControl(Handle, CheckRect, DFC_BUTTON,
      DFCS_BUTTONCHECK or StateFlags[State] or CheckFlags[CheckState]);
  end;
end;

procedure TPegtopTheme.DrawBevel(Handle: HDC; DrawRect: TRect);
begin
  if FEnabled and (GetThemeData(pteButton) <> 0) then begin
    DrawElement(Handle, DrawRect, pteButton, 4, 1);
  end
  else begin
    DrawEdge(Handle, DrawRect, EDGE_ETCHED, BF_RECT);
  end;
end;

procedure TPegtopTheme.DrawComboBox(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState);
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteComboBox, 2, StandardStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawComboBoxButton(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState);
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteComboBox, 1, StandardStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawEdit(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState);
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteEdit, 1, StandardStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawProgressBox(Handle: HDC; DrawRect: TRect;
  Orientation: TPegtopElementOrientation);
begin
  if FEnabled and (GetThemeData(pteProgressBar) <> 0) then begin
    if Orientation = peoHorizontal then
      DrawElement(Handle, DrawRect, pteProgressBar, 1, 1)
    else
      DrawElement(Handle, DrawRect, pteProgressBar, 2, 1);
  end
  else begin
    DrawEdge(Handle, DrawRect, BDR_SUNKENOUTER, BF_RECT);
  end;
end;

procedure TPegtopTheme.DrawProgressBar(Handle: HDC; DrawRect: TRect;
  Orientation: TPegtopElementOrientation);
var
  AdjRect: TRect;
  Brush: THandle;
begin
  if FEnabled and (GetThemeData(pteProgressBar) <> 0) then begin
    if Orientation = peoHorizontal then
      DrawElement(Handle, DrawRect, pteProgressBar, 3, 1)
    else
      DrawElement(Handle, DrawRect, pteProgressBar, 4, 1);
  end
  else begin
    Brush := CreateSolidBrush(ColorToRGB(clHighlight));
    try
      if Orientation = peoHorizontal then begin
        AdjRect.Left := DrawRect.Left;
        AdjRect.Top := DrawRect.Top + 1;
        AdjRect.Right := DrawRect.Right;
        AdjRect.Bottom := DrawRect.Bottom - 1;
      end
      else begin
        AdjRect.Left := DrawRect.Left + 1;
        AdjRect.Top := DrawRect.Top;
        AdjRect.Right := DrawRect.Right - 1;
        AdjRect.Bottom := DrawRect.Bottom;
      end;
      FillRect(Handle, AdjRect, Brush);
    finally
      DeleteObject(Brush);
    end;
  end;
end;

procedure TPegtopTheme.DrawScrollButton(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Direction: TPegtopElementDirection);
const
  DirectionIndex: array[TPegtopElementDirection] of Integer = (0, 4, 8, 12);
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteScrollBar, 1,
      StandardStateIndex[State] + DirectionIndex[Direction]);
  end;
end;

procedure TPegtopTheme.DrawScrollHandle(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Orientation: TPegtopElementOrientation);
begin
  if FEnabled and (GetThemeData(pteProgressBar) <> 0) then begin
    if Orientation = peoHorizontal then
      DrawElement(Handle, DrawRect, pteScrollBar, 2, StandardStateIndex[State])
    else
      DrawElement(Handle, DrawRect, pteScrollBar, 3, StandardStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawScrollBar(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Orientation: TPegtopElementOrientation;
  Part: TPegtopScrollBarPart);
const
  BarIndex: array[TPegtopElementOrientation, TPegtopScrollBarPart] of Integer = ((4, 5), (6, 7));
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteScrollBar, BarIndex[Orientation, Part], StandardStateIndex[State])
  end;
end;

procedure TPegtopTheme.DrawScrollGrip(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Orientation: TPegtopElementOrientation);
begin
  if FEnabled and (GetThemeData(pteProgressBar) <> 0) then begin
    if Orientation = peoHorizontal then
      DrawElement(Handle, DrawRect, pteScrollBar, 8, StandardStateIndex[State])
    else
      DrawElement(Handle, DrawRect, pteScrollBar, 9, StandardStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawSizeGrip(Handle: HDC; DrawRect: TRect;
  RightToLeft: Boolean);
begin
  if FEnabled then begin
    if RightToLeft then
      DrawElement(Handle, DrawRect, pteScrollBar, 10, 2)
    else
      DrawElement(Handle, DrawRect, pteScrollBar, 10, 1);
  end;
end;

procedure TPegtopTheme.DrawSpinButton(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Direction: TPegtopElementDirection);
const
  DirectionIndex: array[TPegtopElementDirection] of Integer = (1, 2, 4, 3);
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteSpinButton, DirectionIndex[Direction],
      StandardStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawTrackBar(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Orientation: TPegtopElementOrientation);
begin
  if FEnabled and (GetThemeData(pteTrackBar) <> 0) then begin
    if Orientation = peoHorizontal then
      DrawElement(Handle, DrawRect, pteTrackBar, 1, TrackStateIndex[State])
    else
      DrawElement(Handle, DrawRect, pteTrackBar, 2, TrackStateIndex[State]);
  end;
end;

procedure TPegtopTheme.DrawTrackSlider(Handle: HDC; DrawRect: TRect;
  State: TPegtopThemeState; Kind: TPegtopSliderKind);
const
  SliderIndex: array[TPegtopSliderKind] of Integer = (3, 4, 5, 6, 7, 8);
begin
  if FEnabled then begin
    DrawElement(Handle, DrawRect, pteTrackBar, SliderIndex[Kind],
      TrackStateIndex[State]);
  end;
end;

procedure TPegtopTheme.SetEnabled(Value: Boolean);
begin
  FEnabled := Supported and Value;
end;

function TPegtopTheme.GetSupported: Boolean;
begin
  Result := (ThemeLibrary > 0);
end;

initialization
  OpenThemeData := NIL;
  CloseThemeData := NIL;
  DrawThemeBackground := NIL;
  DrawThemeParentBackground := NIL;
  GetThemeColor := NIL;
  GetThemeSysColor := NIL;
  if (Win32Platform = VER_PLATFORM_WIN32_NT)
  and ((Win32MajorVersion > 5)
  or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then // Win XP or higher
    ThemeLibrary := LoadLibrary('uxtheme.dll')
  else
    ThemeLibrary := 0;
  if ThemeLibrary > 0 then begin
    OpenThemeData := GetProcAddress(ThemeLibrary, 'OpenThemeData');
    CloseThemeData := GetProcAddress(ThemeLibrary, 'CloseThemeData');
    DrawThemeBackground := GetProcAddress(ThemeLibrary, 'DrawThemeBackground');
    DrawThemeParentBackground := GetProcAddress(ThemeLibrary, 'DrawThemeParentBackground');
    GetThemeColor := GetProcAddress(ThemeLibrary, 'GetThemeColor');
    GetThemeSysColor := GetProcAddress(ThemeLibrary, 'GetThemeSysColor');
  end;
  DefaultTheme := TPegtopTheme.Create;
finalization
  DefaultTheme.Free;
  if ThemeLibrary <> 0 then FreeLibrary(ThemeLibrary);
end.

