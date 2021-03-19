unit JpJvSpinEdit;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$I JPJVCL_DCC.inc}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, Messages, Dialogs, Menus, Forms,
  ActnList, Controls, StdCtrls, ExtCtrls, Types,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls,
  JvSpin, JvJCLUtils, JclSysUtils
  ;

type

//  TJPJvSpinEdit = class(TJvCustomSpinEdit)
  TJPJvSpinEdit = class(TJvSpinEdit)
  private
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FAnchoredControls: TJppAnchoredControls;
    FPropagateEnabled: Boolean;
    FRightLabel: TJppControlBoundLabel;
    FRightLabelPosition: TLabelPosition;
    FRightLabelSpacing: Integer;
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustBoundLabelBounds(Sender: TObject);
    procedure AdjustRightLabelBounds(Sender: TObject);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
    procedure SetPropagateEnabled(const Value: Boolean);
    procedure SetIntValue(const AValue: integer);
    function GetIntValue: integer;
    procedure SetRightLabelPosition(const Value: TLabelPosition);
    procedure SetRightLabelSpacing(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure UpdateEnbledState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupBoundLabel;
    procedure SetupRightLabel;

    property IntValue: integer read GetIntValue write SetIntValue;
  published
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property RightLabel: TJppControlBoundLabel read FRightLabel;
    property RightLabelPosition: TLabelPosition read FRightLabelPosition write SetRightLabelPosition default lpRight;
    property RightLabelSpacing: Integer read FRightLabelSpacing write SetRightLabelSpacing default 4;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

    property PropagateEnabled: Boolean read FPropagateEnabled write SetPropagateEnabled default True;
  end;




implementation




constructor TJPJvSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //Text := '0';

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupBoundLabel;

  FRightLabelPosition := lpRight;
  FRightLabelSpacing := 4;
  SetupRightLabel;

  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FPropagateEnabled := True;

  //ButtonKind := bkClassic;
end;

destructor TJPJvSpinEdit.Destroy;
begin
  FAnchoredControls.Free;
  inherited;
end;

procedure TJPJvSpinEdit.Loaded;
begin
  inherited;
end;

function RemoveThousands(const AValue: string): string;
begin
  if JclFormatSettings.DecimalSeparator <> JclFormatSettings.ThousandSeparator then
    Result := DelChars(AValue, JclFormatSettings.ThousandSeparator)
  else
    Result := AValue;
end;

procedure TJPJvSpinEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
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

procedure TJPJvSpinEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  SetRightLabelPosition(FRightLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

function TJPJvSpinEdit.GetIntValue: integer;
begin
  Result := Round(Value);
end;

procedure TJPJvSpinEdit.SetIntValue(const AValue: integer);
begin
  Value := AValue;
end;

procedure TJPJvSpinEdit.AdjustBoundLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJPJvSpinEdit.AdjustRightLabelBounds(Sender: TObject);
begin
  SetRightLabelPosition(FRightLabelPosition);
end;

procedure TJPJvSpinEdit.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJPJvSpinEdit.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJPJvSpinEdit.SetRightLabelPosition(const Value: TLabelPosition);
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

procedure TJPJvSpinEdit.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJPJvSpinEdit.SetRightLabelSpacing(const Value: Integer);
begin
  FRightLabelSpacing := Value;
  SetRightLabelPosition(FRightLabelPosition);
end;

procedure TJPJvSpinEdit.SetupBoundLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustBoundLabelBounds;
  FBoundLabel.FocusControl := Self;
end;

procedure TJPJvSpinEdit.SetupRightLabel;
begin
  if Assigned(FRightLabel) then Exit;
  FRightLabel := TJppControlBoundLabel.Create(Self, 'RightLabel');
  FRightLabel.FreeNotification(Self);
  FRightLabel.OnAdjustBounds := AdjustRightLabelBounds;
  FRightLabel.FocusControl := Self;
end;

procedure TJPJvSpinEdit.SetParent(AParent: TWinControl);
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

procedure TJPJvSpinEdit.SetPropagateEnabled(const Value: Boolean);
begin
  if FPropagateEnabled = Value then Exit;
  FPropagateEnabled := Value;
  UpdateEnbledState;
end;

procedure TJPJvSpinEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
  if FRightLabel <> nil then FRightLabel.BiDiMode := BiDiMode;
end;

procedure TJPJvSpinEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateEnbledState;
end;

procedure TJPJvSpinEdit.UpdateEnbledState;
begin
  if FPropagateEnabled then // and Assigned(FAnchoredControls) then
  begin
    if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
    if FRightLabel <> nil then FRightLabel.Enabled := Enabled;
//    if FAnchoredControls.Top.Enabled and (FAnchoredControls.Top.Control <> nil) then FAnchoredControls.Top.Control.Enabled := Enabled;
//    if FAnchoredControls.Bottom.Enabled and (FAnchoredControls.Bottom.Control <> nil) then FAnchoredControls.Bottom.Control.Enabled := Enabled;
//    if FAnchoredControls.Left.Enabled and (FAnchoredControls.Left.Control <> nil) then FAnchoredControls.Left.Control.Enabled := Enabled;
//    if FAnchoredControls.Right.Enabled and (FAnchoredControls.Right.Control <> nil) then FAnchoredControls.Right.Control.Enabled := Enabled;
  end;
end;

procedure TJPJvSpinEdit.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
  if FRightLabel <> nil then FRightLabel.Visible := Visible;
end;

end.