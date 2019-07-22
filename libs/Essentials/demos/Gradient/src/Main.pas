unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JPEsGrad, System.Actions, Vcl.ActnList, Vcl.StdCtrls, JPP.ColorListBox, Vcl.ExtCtrls, JPP.ColorSwatch, Vcl.ComCtrls,
  JPP.Edit, JPP.SimplePanel, JPL.Colors.List;

type

  TForm1 = class(TForm)
    Grad: TJPEsGradient;
    ActionList1: TActionList;
    actEsc: TAction;
    btnExit: TButton;
    clbColors: TJppColorListBox;
    cswStartColor: TJppColorSwatchEx;
    cswEndColor: TJppColorSwatchEx;
    edSteps: TJppEdit;
    udSteps: TUpDown;
    btnUpdate: TButton;
    actUpdateColors: TAction;
    pnColors: TJppSimplePanel;
    lbl1: TLabel;
    lbl2: TLabel;
    dlgSave: TSaveDialog;
    actSaveColorsToGimpPaletteFile: TAction;
    btn1: TButton;
    procedure actEscExecute(Sender: TObject);
    procedure actSaveColorsToGimpPaletteFileExecute(Sender: TObject);
    procedure actUpdateColorsExecute(Sender: TObject);
    procedure cswStartColorSelectedColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure udStepsClick(Sender: TObject; Button: TUDBtnType);
    procedure UpdateGradient;
    procedure SaveToArray(var Arr: TColorListArray);
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Gradient Colors';
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  clbColors.Clear;
  clbColors.Align := alClient;
  udSteps.Min := MinColorBands;
  udSteps.Max := MaxColorBands;
  UpdateGradient;
  actUpdateColors.Execute;
end;

procedure TForm1.SaveToArray(var Arr: TColorListArray);
var
  i, xNo, xInd: integer;
  ItemData: TJppColorListBoxItemData;
begin
  SetLength(Arr, 0);
  xNo := 0;
  for i := 0 to clbColors.Count - 1 do
  begin
    if not clbColors.IsColorItem(i) then Continue;
    clbColors.GetItemInfo(i, ItemData);
    SetLength(Arr, Length(Arr) + 1);
    xInd := High(Arr);
    Inc(xNo);
    Arr[xInd].No := xNo;
    Arr[xInd].Color := ItemData.Color;
    Arr[xInd].ColorName := ItemData.Name;
  end;
end;

procedure TForm1.UpdateGradient;
begin
  Grad.FromColor := cswStartColor.SelectedColor;
  Grad.ToColor := cswEndColor.SelectedColor;
  Grad.ColorBands := udSteps.Position;
end;

procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actSaveColorsToGimpPaletteFileExecute(Sender: TObject);
var
  Arr: TColorListArray;
  CList: TColorList;
begin
  if not dlgSave.Execute then Exit;
  SaveToArray(Arr);
  CList := TColorList.Create;
  try
    CList.AddColorsFromArray(Arr);
    CList.SaveToGimpPaletteFile(dlgSave.FileName, 'Gradient colors');
  finally
    CList.Free;
  end;
end;

procedure TForm1.actUpdateColorsExecute(Sender: TObject);
var
  i: integer;
  cl: TColor;
begin
  clbColors.BeginUpdate;
  try
    clbColors.Clear;
    for i := 0 to Grad.ColorBands - 1 do
    begin
      cl := Grad.gGradColors[i];
      clbColors.AddColor(cl, '');
    end;
  finally
    clbColors.EndUpdate;
  end;
end;

procedure TForm1.cswStartColorSelectedColorChanged(Sender: TObject);
begin
  UpdateGradient;
end;

procedure TForm1.udStepsClick(Sender: TObject; Button: TUDBtnType);
begin
  UpdateGradient;
end;

end.
