////////////////////////////////////////////////////////////////////////////////
// File:       PegtopNumEdits.pas
// Components: TPegtopIntEdit, TPegtopFloatEdit
// Version:    1.00
// Date:       02 Sep 2004
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopIntEdit and TPegtopFloatEdit (both derived from TPegtopNumEdit) are
// (right aligned) edit boxes for numeric values (only valid numbers allowed).
// Instead of a text property (string) a value property (integer / double)
// is provided. A range (defaults 0..100) can be specified as well as a caption
// (for showing a unit for the value).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit JPPegtopNumEdits;

interface

uses
  Windows, Messages, Graphics, SysUtils, Classes, Controls, StdCtrls;

type
  TPegtopNumEditOption = (pneFixLength, pneEmptyAllowed, pneSpecialAllowed);
  TPegtopNumEditOptions = set of TPegtopNumEditOption;

  TPegtopNumCaptionAlignment = (pcaLeft, pcaRight);

  TPegtopSpecialKeyEvent = procedure (Sender: TObject; var Key: Word; var DefaultHandler: Boolean) of object;

type
  TPegtopNumEdit = class(TCustomEdit)
  private
    FBeep: Boolean;
    FCanvas: TControlCanvas;
    FCaption: TCaption;
    FCaptionAlignment: TPegtopNumCaptionAlignment;
    FPaintRect: TRect;
    FOptions: TPegtopNumEditOptions;
    FDisableOnChange: Boolean;
    FSpecialChar: Char;
    FOnSpecialKey: TPegtopSpecialKeyEvent;
    procedure SetCaption(Value: TCaption);
    procedure AdjustMargin;
    procedure CMEnter(var Msg: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
    procedure SetOptions(V: TPegtopNumEditOptions);
    procedure SetCaptionAlignment(V: TPegtopNumCaptionAlignment);
    procedure SetSpecialChar(V: Char);
  protected
    procedure ValidateValue; virtual; abstract;
    function IsValidValue: Boolean; virtual; abstract;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(var Key: Char): Boolean; virtual; abstract;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Resize; override;
    function IsLikeEmpty: Boolean; virtual; abstract;
    procedure AdjustMaxLength; virtual; abstract;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsEmpty: Boolean; virtual;
    function IsSpecial: Boolean; virtual;
    procedure SetEmpty; virtual;
    procedure SetSpecial; virtual;
    function GetMaxLength: Integer;
  published
    property Beep: Boolean read FBeep write FBeep;
    property Caption: TCaption read FCaption write SetCaption;
    property CaptionAlignment: TPegtopNumCaptionAlignment read FCaptionAlignment write SetCaptionAlignment;
    property Options: TPegtopNumEditOptions read FOptions write SetOptions;
    property SpecialChar: Char read FSpecialChar write SetSpecialChar;
    property OnSpecialKey: TPegtopSpecialKeyEvent read FOnSpecialKey write FOnSpecialKey;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Ctl3D;
    property HideSelection;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TPegtopIntEdit = class(TPegtopNumEdit)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FLastValue: Integer;
    FBase: Integer;
    function ConvertStrToBase(S: String; var V: Integer): Boolean;
    function ConvertBaseToStr(V: Integer): String;
    function ConstrictValue(V: Integer): Integer;
    function GetValue: Integer;
    procedure SetValue(V: Integer);
    function GetModified: Boolean;
    procedure SetBase(V: Integer);
    procedure SetMinValue(V: Integer);
    procedure SetMaxValue(V: Integer);
    procedure CMEnter(var Msg: TCMGotFocus); message CM_ENTER;
    function GetEmptyValue: Integer;
  protected
    procedure ValidateValue; override;
    function IsValidValue: Boolean; override;
    function IsValidChar(var Key: Char): Boolean; override;
    function IsLikeEmpty: Boolean; override;
    procedure AdjustMaxLength; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property MinValue: Integer read FMinValue write SetMinValue;
    property Modified: Boolean read GetModified;
    property Base: Integer read FBase write SetBase;
    property Value: Integer read GetValue write SetValue;
  end;

  TPegtopFloatEdit = class(TPegtopNumEdit)
  private
    FMinValue: Double;
    FMaxValue: Double;
    FLastValue: Double;
    FDigits: Integer;
    Function ConvertStrToDouble(S: String; var V: Double): Boolean;
    Function ConvertDoubleToStr(V: Double): String;
    function GetValue: Double;
    function ConstrictValue(V: Double): Double;
    function GetModified: Boolean;
    procedure SetDigits(V: Integer);
    procedure SetValue(V: Double);
    procedure SetMinValue(V: Double);
    procedure SetMaxValue(V: Double);
    procedure CMEnter(var Msg: TCMGotFocus); message CM_ENTER;
    function GetEmptyValue: Double;
  protected
    procedure ValidateValue; override;
    function IsValidValue: Boolean; override;
    function IsValidChar(var Key: Char): Boolean; override;
    function IsLikeEmpty: Boolean; override;
    procedure AdjustMaxLength; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property Modified: Boolean read GetModified;
    property Digits: Integer read FDigits write SetDigits;
    property Value: Double read GetValue write SetValue;
  end;

implementation

const
  BaseDigits = '0123456789ABCDEF';

////////////////////////////////////////////////////////////////////////////////
// TPegtopNumEdit
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopNumEdit.Create(AOwner: TComponent);
begin
  FDisableOnChange := False;
  FCaption := '';
  FCaptionAlignment := pcaLeft;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FOptions := [];
  FSpecialChar := '*';
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
end;

destructor TPegtopNumEdit.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TPegtopNumEdit.KeyPress(var Key: Char);
begin
  if IsValidChar(Key) then begin
    inherited KeyPress(Key);
  end
  else begin
    Key := #0;
    if FBeep then MessageBeep(0);
  end;
end;

procedure TPegtopNumEdit.CMEnter(var Msg: TCMGotFocus);
begin
  inherited;
  if FCaption <> '' then Invalidate;
end;

procedure TPegtopNumEdit.CMExit(var Msg: TCMExit);
begin
  if IsValidValue then begin
    ValidateValue;    // reformat only
  end
  else begin
    ValidateValue;    // fix invalid value
    inherited Change; // value might have changed
  end;
  inherited;
  if FCaption <> '' then Invalidate;
end;

procedure TPegtopNumEdit.CMWantSpecialKey(var Msg: TWMKey);
var
  DefaultHandler: Boolean;
begin
  if Assigned(FOnSpecialKey) Then Begin
    DefaultHandler := True;
    FOnSpecialKey(Self, Msg.CharCode, DefaultHandler);
    if DefaultHandler then inherited;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopNumEdit.Change;
begin
  if IsValidValue and not FDisableOnChange then inherited Change;
  if FCaption <> '' then Invalidate;
end;

procedure TPegtopNumEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_RIGHT;
end;

procedure TPegtopNumEdit.WMPaste(var Msg: TWMPaste);
begin
  inherited;
  ValidateValue;
end;

procedure TPegtopNumEdit.WMCut(var Msg: TWMCut);
begin
  inherited;
  ValidateValue;
end;

{procedure TPegtopNumEdit.EMReplaceSel(var Msg: TEMReplaceSel);
begin
  inherited;
  ValidateValue;
end;}

procedure TPegtopNumEdit.CreateWnd;
begin
  inherited;
  Perform(EM_GETRECT, 0, LongInt(@FPaintRect));
  AdjustMargin;
end;

procedure TPegtopNumEdit.WMPaint(var Msg: TMessage);
begin
  inherited;
  if FCaption <> '' then begin
    FCanvas.Brush.Color := Color;
    FCanvas.Font.Assign(Font);
    if not Enabled then FCanvas.Font.Color := clGrayText;
    if FCaptionAlignment = pcaRight then
      FCanvas.TextOut(FPaintRect.Right - FCanvas.TextWidth(FCaption), FPaintRect.Top, FCaption)
    else
      FCanvas.TextOut(FPaintRect.Left, FPaintRect.Top, FCaption);
  end;
end;

function TPegtopNumEdit.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  FPaintRect.Right := FPaintRect.Right + NewWidth - Width;
  FPaintRect.Bottom := FPaintRect.Bottom + NewHeight - Height;
end;

procedure TPegtopNumEdit.Resize;
begin
  inherited;
  AdjustMargin;
end;

procedure TPegtopNumEdit.AdjustMargin;
begin
  if FCaption = '' then
    Perform(EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, 0)
  else if FCaptionAlignment = pcaRight then
    Perform(EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, (FCanvas.TextWidth(FCaption) + 2) SHL 16) // set right margin
  else
    Perform(EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, FCanvas.TextWidth(FCaption) + 2); // set left margin
end;

procedure TPegtopNumEdit.SetOptions(V: TPegtopNumEditOptions);
begin
  if V <> FOptions then begin
    if (pneSpecialAllowed in FOptions) and not (pneSpecialAllowed in V) then begin
      if Text = FSpecialChar then Text := '';
    end;
    if (pneEmptyAllowed in V) and not (pneEmptyAllowed in FOptions) then begin
      if IsLikeEmpty then Text := '';
    end;
    FOptions := V;
    AdjustMaxLength;
    ValidateValue;
  end;
end;

procedure TPegtopNumEdit.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    AdjustMargin;
    Invalidate;
  end;
end;

procedure TPegtopNumEdit.Loaded;
begin
  inherited;
  if pneEmptyAllowed in FOptions then begin
    if IsLikeEmpty then Text := '';
  end;
end;

function TPegtopNumEdit.IsEmpty: Boolean;
begin
  Result := (Text = '');
end;

function TPegtopNumEdit.IsSpecial: Boolean;
begin
  Result := (Text = FSpecialChar);
end;

procedure TPegtopNumEdit.SetEmpty;
begin
  if pneEmptyAllowed in FOptions then begin
    // set new text:
    FDisableOnChange := True;
    try
      Text := '';
    finally
      FDisableOnChange := False;
    end;
  end;
end;

procedure TPegtopNumEdit.SetSpecial;
begin
  if pneSpecialAllowed in FOptions then begin
    // set new text:
    FDisableOnChange := True;
    try
      Text := FSpecialChar;
    finally
      FDisableOnChange := False;
    end;
  end;
end;

procedure TPegtopNumEdit.SetCaptionAlignment(V: TPegtopNumCaptionAlignment);
begin
  if FCaptionAlignment <> V then begin
    FCaptionAlignment := V;
    AdjustMargin;
    Invalidate;
  end;
end;

procedure TPegtopNumEdit.SetSpecialChar(V: Char);
begin
  if FSpecialChar <> V then begin
    if Text = FSpecialChar then Text := V;
    FSpecialChar := V;
  end;
end;

function TPegtopNumEdit.GetMaxLength: Integer;
begin
  Result := MaxLength;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopIntEdit
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopIntEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBase := 10;
  FMinValue := 0;
  FMaxValue := 100;
  AdjustMaxLength;
  Options := [];
  SetValue(FMinValue);
end;

function TPegtopIntEdit.ConvertStrToBase(S: String; var V: Integer): Boolean;
var
  I, P: Integer;
  MinusFound: Boolean;
begin
  S := Trim(S);
  if S = '' then begin
    if pneEmptyAllowed in Options then begin
      V := GetEmptyValue;
      Result := True;
    end
    else begin
      Result := False;
    end;
  end
  else if S = FSpecialChar then begin
    if pneSpecialAllowed in FOptions then begin
      V := GetEmptyValue;
      Result := True;
    end
    else begin
      Result := False;
    end;
  end
  else begin
    Result := True;
    V := 0;
    I := 0;
    MinusFound := False;
    if S[1] = '-' then begin
      MinusFound := True;
      Inc(I);
    end;
    while Result and (I < Length(S)) do begin
      Inc (I);
      P := Pos(S[I], BaseDigits);
      if (P > 0) and (P <= FBase) then V := V * FBase + (P-1)
      else Result := False;
    end;
    if MinusFound then V := -V;
  end;
end;

function TPegtopIntEdit.ConvertBaseToStr(V: Integer): String;
var
  S: String;
  A, I: Integer;
begin
  A := Abs(V);
  I := 1;
  S := '';
  repeat
    S := BaseDigits[((A div I) mod FBase)+1] + S;
    I := I * FBase;
  until I > A;
  if V < 0 then Result := '-' + S else Result := S;
End;

function TPegtopIntEdit.IsValidChar(var Key: Char): Boolean;
var
  P: Integer;
begin
  Key := UpCase(Key); // for hex numbers
  P := Pos(UpCase(Key), BaseDigits);
  Result := ((P > 0) and (P <= FBase))           // number
    or ((Key = '-') and (FMinValue < 0))         // minus
    or ((Key < #32) and (Key <> Chr(VK_RETURN))) // special key
    or ((pneSpecialAllowed in FOptions) and (Key = FSpecialChar) and ((Text = '') or (SelLength = Length(Text)))); // special char
end;

function TPegtopIntEdit.IsValidValue: Boolean;
var
  V: Integer;
begin
  Result := (ConvertStrToBase(Text, V)) and (V >= FMinValue) and (V <= FMaxValue);
end;

function TPegtopIntEdit.GetModified: Boolean;
Begin
  Result := (FLastValue <> GetValue);
End;

function TPegtopIntEdit.GetValue: Integer;
begin
  ConvertStrToBase(Text, Result);
end;

function TPegtopIntEdit.GetEmptyValue: Integer;
begin
  if (FMinValue <= 0) and (FMaxValue >= 0) then Result := 0
  else Result := FMinValue;
end;

procedure TPegtopIntEdit.ValidateValue;
var
  V: Integer;
begin
  if not ((pneSpecialAllowed in Options) and (Text = FSpecialChar)) then begin
    if not ((pneEmptyAllowed in Options) and IsEmpty) then begin
      if ConvertStrToBase(Text, V) then SetValue(V)
      else if pneEmptyAllowed in FOptions then SetEmpty
      else SetValue(V);
    end;
  end;
end;

function TPegtopIntEdit.IsLikeEmpty: Boolean;
begin
  Result := (Text = '') or (Value = GetEmptyValue);
end;

procedure TPegtopIntEdit.SetBase(V: Integer);
var
  N: Integer;
begin
  if V < 2 then V := 2 else if V > 16 then V := 16;
  if V <> FBase then begin
    N := GetValue;
    FBase := V;
    AdjustMaxLength;
    SetValue(N);
  end;
end;

procedure TPegtopIntEdit.SetValue(V: Integer);
var
  N: Integer;
  S: String;
begin
  N := ConstrictValue(V);
  if (pneFixLength in Options) then begin
    S := ConvertBaseToStr(Abs(N));
    if N < 0 then begin
      S := '-' + StringOfChar('0', MaxLength - Length(S) - 1) + S;
    end
    else begin
      if FMinValue < 0 then begin
        S := StringOfChar('0', MaxLength - Length(S) - 1) + S; // save one space for minus
      end
      else begin
        S := StringOfChar('0', MaxLength - Length(S)) + S;
      end;
    end;
  end
  else begin
    S := ConvertBaseToStr(N);
  end;
  // set new text:
  FDisableOnChange := True;
  try
    Text := S;
  finally
    FDisableOnChange := False;
  end;
end;

procedure TPegtopIntEdit.SetMinValue(V: Integer);
begin
  if FMinValue <> V then begin
    if V > FMaxValue then FMaxValue := V;
    FMinValue := V;
    AdjustMaxLength;
    ValidateValue;
  end;
end;

procedure TPegtopIntEdit.SetMaxValue(V: Integer);
begin
  if FMaxValue <> V then begin
    if V < FMinValue then FMinValue := V;
    FMaxValue := V;
    AdjustMaxLength;
    ValidateValue;
  End;
end;

procedure TPegtopIntEdit.AdjustMaxLength;
var
  LengthOfMin, LengthOfMax: Integer;
begin
  LengthOfMin := Length(ConvertBaseToStr(FMinValue));
  LengthOfMax := Length(ConvertBaseToStr(FMaxValue));
  if LengthOfMin > LengthOfMax then MaxLength := LengthOfMin else MaxLength := LengthOfMax;
end;

function TPegtopIntEdit.ConstrictValue(V: Integer): Integer;
begin
  if V < FMinValue then Result := FMinValue
  else if V > FMaxValue then Result := FMaxValue
  else Result := V;
end;

procedure TPegtopIntEdit.CMEnter(var Msg: TCMGotFocus);
begin
  FLastValue := GetValue;
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopFloatEdit
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDigits := 2;
  FMinValue := 0.0;
  FMaxValue := 100.0;
  AdjustMaxLength;
  Options := [pneFixLength];
  SetValue(FMinValue);
end;

function TPegtopFloatEdit.ConvertStrToDouble(S: String; var V: Double): Boolean;
begin
  S := Trim(S);
  if S = '' then begin
    if pneEmptyAllowed in Options then begin
      V := GetEmptyValue;
      Result := True;
    end
    else begin
      Result := False;
    end;
  end
  else if S = FSpecialChar then begin
    if pneSpecialAllowed in FOptions then begin
      V := GetEmptyValue;
      Result := True;
    end
    else begin
      Result := False;
    end;
  end
  else begin
    try
      V := StrToFloat(S);
      Result := True;
    except
      on EConvertError do Result := False;
    end;
  end;
end;

function TPegtopFloatEdit.ConvertDoubleToStr(V: Double): String;
begin
  if pneFixLength in Options then begin
    Result := FloatToStrF(V, ffFixed, 15, FDigits);
  end
  else begin
    Result := FloatToStrF(V, ffGeneral, 15, 0);
  end;
end;

function TPegtopFloatEdit.IsValidValue: Boolean;
var
  V: Double;
begin
  Result := ConvertStrToDouble(Text, V) and (V >= FMinValue) and (V <= FMaxValue);
end;

function TPegtopFloatEdit.IsValidChar(var Key: Char): Boolean;
begin
  Result := ((Key >= '0') and (Key <= '9'))       // number
    or (Key = FormatSettings.DecimalSeparator)                   // decimal point
    or ((Key = '-') and (FMinValue < 0))          // minus
    or ((Key < #32) and (Key <> Chr(VK_RETURN)))  // special key
    or ((pneSpecialAllowed in FOptions) and (Key = FSpecialChar) and ((Text = '') or (SelLength = Length(Text)))); // special char
end;

function TPegtopFloatEdit.GetModified: Boolean;
Begin
  Result := (FLastValue <> GetValue);
End;

function TPegtopFloatEdit.GetValue: Double;
begin
  ConvertStrToDouble(Text, Result);
end;

function TPegtopFloatEdit.GetEmptyValue: Double;
begin
  if (FMinValue <= 0.0) and (FMaxValue >= 0.0) then Result := 0.0
  else Result := FMinValue;
end;

procedure TPegtopFloatEdit.SetDigits(V: Integer);
var
  N: Double;
begin
  if V < 0 then V := 0 else if V > 10 then V := 10;
  if V <> FDigits then begin
    N := GetValue;
    FDigits := V;
    SetValue(N);
  End;
end;

procedure TPegtopFloatEdit.ValidateValue;
var
  V: Double;
begin
  if not ((pneSpecialAllowed in Options) and (Text = FSpecialChar)) then begin
    if not ((pneEmptyAllowed in Options) and IsEmpty) then begin
      if ConvertStrToDouble(Text, V) then SetValue(V)
      else if pneEmptyAllowed in FOptions then SetEmpty
      else SetValue(V);
    end;
  end;
end;

function TPegtopFloatEdit.IsLikeEmpty: Boolean;
begin
  Result := (Text = '') or (Value = GetEmptyValue);
end;

procedure TPegtopFloatEdit.SetValue(V: Double);
Var
  N: Double;
  S: String;
begin
  N := ConstrictValue(V);
  S := ConvertDoubleToStr(N);
  // set new text:
  FDisableOnChange := True;
  try
    Text := S;
  finally
    FDisableOnChange := False;
  end;
end;

procedure TPegtopFloatEdit.SetMinValue(V: Double);
begin
  If FMinValue <> V Then Begin
    If V > FMaxValue Then FMaxValue := V;
    FMinValue := V;
    AdjustMaxLength;
    ValidateValue;
  End;
end;

procedure TPegtopFloatEdit.SetMaxValue(V: Double);
begin
  If FMaxValue <> V Then Begin
    If V < FMinValue Then FMinValue := V;
    FMaxValue := V;
    AdjustMaxLength;
    ValidateValue;
  End;
end;

procedure TPegtopFloatEdit.AdjustMaxLength;
begin
  MaxLength := 17;
end;

function TPegtopFloatEdit.ConstrictValue(V: Double): Double;
begin
  if V < FMinValue then Result := FMinValue
  else if V > FMaxValue then Result := FMaxValue
  else Result := V;
end;

procedure TPegtopFloatEdit.CMEnter(var Msg: TCMGotFocus);
begin
  FLastValue := GetValue;
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

end.

