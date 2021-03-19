unit JpJvRadioButton;

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
  JvRadioButton, JvJCLUtils, JclSysUtils
  ;

type

  TJPJvRadioButton = class(TJvRadioButton)
  private
    FAnchoredControls: TJppAnchoredControls;
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;




implementation




constructor TJPJvRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJPJvRadioButton.Destroy;
begin
  FAnchoredControls.Free;
  inherited;
end;

procedure TJPJvRadioButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
    begin
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
    end;
end;

procedure TJPJvRadioButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJPJvRadioButton.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;



end.