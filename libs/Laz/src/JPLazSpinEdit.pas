unit JPLazSpinEdit;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ELSE}
  //{$I JPJVCL_DCC.inc}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, Messages, Dialogs, Menus, Forms,
  ActnList, Controls, StdCtrls, ExtCtrls, Types,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls,
  Spin
  ;


type

  TJPLazCustomFloatSpinEdit = class(TCustomFloatSpinEdit)
  private
    FAnchoredControls: TJppAnchoredControls;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FRightLabel: TJppControlBoundLabel;
    FRightLabelPosition: TLabelPosition;
    FRightLabelSpacing: Integer;
    FTagExt: TJppTagExt;
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure SetRightLabelPosition(const Value: TLabelPosition);
    procedure SetRightLabelSpacing(const Value: Integer);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure AdjustRightLabelBounds(Sender: TObject);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property RightLabel: TJppControlBoundLabel read FRightLabel;
    property RightLabelPosition: TLabelPosition read FRightLabelPosition write SetRightLabelPosition default lpRight;
    property RightLabelSpacing: Integer read FRightLabelSpacing write SetRightLabelSpacing default 4;

    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupInternalLabel;
    procedure SetupRightLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure UpdateLabelsPosition;
  end;

  TJPLazFloatSpinEdit = class(TJPLazCustomFloatSpinEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DecimalPlaces;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;

    // jacek
    property TagExt;
    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;
    property RightLabel;
    property RightLabelPosition;
    property RightLabelSpacing;
    property AnchoredControls;
  end;

  TJPLazCustomSpinEdit = class(TJPLazCustomFloatSpinEdit)
  private
    function GetIncrement: integer;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetValue: integer;
  protected
    procedure SetMaxValue(const AValue: integer); overload; //virtual;
    procedure SetMinValue(const AValue: integer); overload; //virtual;
    procedure SetIncrement(const AValue: integer); overload; //virtual;
    procedure SetValue(const AValue: integer); overload; //virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetLimitedValue(const AValue: Double): Double; override;
  public
    property Value: integer read GetValue write SetValue default 0;
    property MinValue: integer read GetMinValue write SetMinValue default 0;
    property MaxValue: integer read GetMaxValue write SetMaxValue default 0; //100;
    property Increment: integer read GetIncrement write SetIncrement default 1;
  end;

  TJPLazSpinEdit = class(TJPLazCustomSpinEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;

    // jacek
    property TagExt;
    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;
    property RightLabel;
    property RightLabelPosition;
    property RightLabelSpacing;
    property AnchoredControls;
  end;


implementation



{ TJPLazCustomFloatSpinEdit }

constructor TJPLazCustomFloatSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FRightLabelPosition := lpRight;
  FRightLabelSpacing := 4;
  SetupRightLabel;

  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJPLazCustomFloatSpinEdit.Destroy;
begin
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited Destroy;
end;

procedure TJPLazCustomFloatSpinEdit.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  FBoundLabel.OnChangeBounds := AdjustLabelBounds;
end;

procedure TJPLazCustomFloatSpinEdit.SetupRightLabel;
begin
  if Assigned(FRightLabel) then Exit;
  FRightLabel := TJppControlBoundLabel.Create(Self, 'RightLabel');
  FRightLabel.FreeNotification(Self);
  FRightLabel.OnAdjustBounds := AdjustRightLabelBounds;
  FRightLabel.FocusControl := Self;
  FRightLabel.OnChangeBounds := AdjustRightLabelBounds;
end;

procedure TJPLazCustomFloatSpinEdit.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetBoundLabelPosition(FBoundLabelPosition);
  SetRightLabelPosition(FRightLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJPLazCustomFloatSpinEdit.UpdateLabelsPosition;
begin
  SetBoundLabelPosition(FBoundLabelPosition);
  SetRightLabelPosition(FRightLabelPosition);
end;

procedure TJPLazCustomFloatSpinEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
  if FRightLabel <> nil then
  begin
    FRightLabel.Parent := AParent;
    FRightLabel.Visible := True;
  end;
end;

procedure TJPLazCustomFloatSpinEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  if FRightLabel <> nil then FRightLabel.Enabled := Enabled;
end;

procedure TJPLazCustomFloatSpinEdit.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
  if FRightLabel <> nil then FRightLabel.Visible := Visible;
end;

procedure TJPLazCustomFloatSpinEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
  if FRightLabel <> nil then FRightLabel.BiDiMode := BiDiMode;
end;

procedure TJPLazCustomFloatSpinEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
    begin
      if Assigned(FBoundLabel) and (AComponent = FBoundLabel) then FBoundLabel := nil
      else if Assigned(FRightLabel) and (AComponent = FRightLabel) then FRightLabel := nil
      else if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
    end;
end;

procedure TJPLazCustomFloatSpinEdit.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJPLazCustomFloatSpinEdit.AdjustRightLabelBounds(Sender: TObject);
begin
  SetRightLabelPosition(FRightLabelPosition);
end;

procedure TJPLazCustomFloatSpinEdit.SetBoundLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FBoundLabel = nil then Exit;
  FBoundLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FBoundLabel.Height - FBoundLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FBoundLabelSpacing);
    lpLeft : P := Point(Left - FBoundLabel.Width - FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
  end;

  FBoundLabel.SetBounds({%H-}P.x, {%H-}P.y, FBoundLabel.Width, FBoundLabel.Height);
end;

procedure TJPLazCustomFloatSpinEdit.SetRightLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FRightLabel = nil then Exit;
  FRightLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FRightLabel.Height - FRightLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FRightLabelSpacing);
    lpLeft : P := Point(Left - FRightLabel.Width - FRightLabelSpacing, Top + ((Height - FRightLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FRightLabelSpacing, Top + ((Height - FRightLabel.Height) div 2));
  end;

  FRightLabel.SetBounds({%H-}P.x, {%H-}P.y, FRightLabel.Width, FRightLabel.Height);
end;

procedure TJPLazCustomFloatSpinEdit.SetBoundLabelSpacing(const Value: Integer);
begin
  if FBoundLabelSpacing = Value then Exit;
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJPLazCustomFloatSpinEdit.SetRightLabelSpacing(const Value: Integer);
begin
  if FRightLabelSpacing = Value then Exit;
  FRightLabelSpacing := Value;
  SetRightLabelPosition(FRightLabelPosition);
end;

procedure TJPLazCustomFloatSpinEdit.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJPLazCustomFloatSpinEdit.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;




{ TJPLazCustomSpinEdit }

constructor TJPLazCustomSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetDecimals(0);
end;

function TJPLazCustomSpinEdit.GetIncrement: integer;
begin
  Result := Round(inherited Increment);
end;

function TJPLazCustomSpinEdit.GetMaxValue: integer;
begin
  Result := Round(inherited MaxValue);
end;

function TJPLazCustomSpinEdit.GetMinValue: integer;
begin
  Result := Round(inherited MinValue);
end;

function TJPLazCustomSpinEdit.GetValue: integer;
begin
  Result := Round(inherited Value);
end;

procedure TJPLazCustomSpinEdit.SetIncrement(const AValue: integer);
begin
  if Increment = AValue then Exit;
  inherited SetIncrement(AValue);
end;

procedure TJPLazCustomSpinEdit.SetMaxValue(const AValue: integer);
begin
  if MaxValue = AValue then Exit;
  inherited SetMaxValue(AValue);
end;

procedure TJPLazCustomSpinEdit.SetMinValue(const AValue: integer);
begin
  if MinValue = AValue then Exit;
  inherited SetMinValue(AValue);
end;

procedure TJPLazCustomSpinEdit.SetValue(const AValue: integer);
begin
  if Value = AValue then Exit;
  inherited SetValue(AValue);
end;

function TJPLazCustomSpinEdit.GetLimitedValue(const AValue: Double): Double;
begin
  Result := inherited GetLimitedValue(AValue);
  if Result > MaxInt then Result := MaxInt;
  if Result < Low(Integer) then Result := Low(Integer);
end;

end.
