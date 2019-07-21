////////////////////////////////////////////////////////////////////////////////
// File:       PegtopNumForms.pas
// Version:    1.00
// Date:       15 Jun 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Popup forms with numeric edit controls (TPegtopIntEdit / TPegtopFloatEdit).
// Used by track bars (see PegtopTrackBars.pas).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit JPPegtopNumForms;

interface

uses
  Windows, Messages, Classes, Forms, Controls,
  JPPegtopNumEdits;

type
  TPegtopPopupForm = class(TCustomForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Deactivate; override;
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
  published
    property OnClose;
  end;

  TPegtopNumEditForm = class(TPegtopPopupForm)
  private
    FEdit: TPegtopNumEdit;
    FOnEditChange: TNotifyEvent;
    // procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
  protected
    procedure DoShow; override;
    procedure EditSpecialKey(Sender: TObject; var Key: Word;
      var DefaultHandler: Boolean);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CreateEdit(var NewEdit: TPegtopNumEdit); virtual; abstract;
  public
    procedure AfterConstruction; override;
    property NumEdit: TPegtopNumEdit read FEdit;
  published
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
  end;

  TPegtopIntEditForm = class(TPegtopNumEditForm)
  protected
    procedure CreateEdit(var NewEdit: TPegtopNumEdit); override;
  end;

  TPegtopFloatEditForm = class(TPegtopNumEditForm)
  protected
    procedure CreateEdit(var NewEdit: TPegtopNumEdit); override;
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TPegtopPopupForm
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopPopupForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner); // bypass loading of the .dfm resource for this form
end;

procedure TPegtopPopupForm.AfterConstruction;
begin
  inherited;
  BorderIcons := [];
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
end;

procedure TPegtopPopupForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  if (Win32Platform = VER_PLATFORM_WIN32_NT)
  and ((Win32MajorVersion > 5)
  or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then // Win XP or higher
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TPegtopPopupForm.Deactivate;
begin
  inherited;
  Close;
end;

procedure TPegtopPopupForm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopNumEditForm
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopNumEditForm.AfterConstruction;
begin
  FOnEditChange := NIL;
  inherited;
  CreateEdit(FEdit);
  FEdit.Parent := Self;
  FEdit.Left := 0;
  FEdit.Top := 0;
  FEdit.Ctl3D := False;
  FEdit.OnSpecialKey := EditSpecialKey;
  FEdit.OnKeyDown := EditKeyDown;
end;

{procedure TPegtopNumEditForm.WMEraseBkgnd(var Msg: TMessage);
Begin
  Msg.Result := 1;
End;}

procedure TPegtopNumEditForm.EditSpecialKey(Sender: TObject;
  var Key: Word; var DefaultHandler: Boolean);
begin
  if Key in [VK_RETURN, VK_ESCAPE] then DefaultHandler := False;
end;

procedure TPegtopNumEditForm.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    if Assigned(FOnEditChange) then FOnEditChange(Self);
    Close;
  end
  else if Key = VK_ESCAPE then begin
    Close;
  end;
end;

procedure TPegtopNumEditForm.DoShow;
begin
  ClientWidth := FEdit.Width;
  ClientHeight := FEdit.Height;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopIntEditForm
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopIntEditForm.CreateEdit(var NewEdit: TPegtopNumEdit);
begin
  NewEdit := TPegtopIntEdit.Create(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopFloatEditForm
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopFloatEditForm.CreateEdit(var NewEdit: TPegtopNumEdit);
begin
  NewEdit := TPegtopFloatEdit.Create(Self);
end;

end.
