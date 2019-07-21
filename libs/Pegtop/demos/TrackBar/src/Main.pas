unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JPPegtopTrackBars, Vcl.ExtCtrls, JPP.SimplePanel, System.Actions, Vcl.ActnList, Vcl.StdCtrls, JPP.BasicPanel, JPP.Panel,
  JPP.ColorSwatch;

type
  TForm1 = class(TForm)
    JppSimplePanel1: TJppSimplePanel;
    JPPegtopColorTrackBar1: TJPPegtopColorTrackBar;
    ActionList1: TActionList;
    actEsc: TAction;
    tbBool: TJPPegtopTrackBar;
    lblBool: TLabel;
    pnEqualizer: TJppBasicPanel;
    lblEq1: TLabel;
    lblEq2: TLabel;
    lblEq3: TLabel;
    tbEq1: TJPPegtopTrackBar;
    tbEq2: TJPPegtopTrackBar;
    tbEq3: TJPPegtopTrackBar;
    pnRgb: TJppPanel;
    lblRgbRed: TLabel;
    lblRgbGreen: TLabel;
    lblRgbBlue: TLabel;
    lblRgb: TLabel;
    tbRgbRed: TJPPegtopColorTrackBar;
    tbRgbGreen: TJPPegtopColorTrackBar;
    tbRgbBlue: TJPPegtopColorTrackBar;
    btnExit: TButton;
    csw: TJppColorSwatch;
    procedure actEscExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbBoolLabel(Sender: TObject; var Caption: string);
    procedure tbRgbRedScroll(Sender: TObject; ScrollCode: TJPPegtopScrollCode; var ScrollPos: Integer);
    procedure UpdateColorSwatch;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateColorSwatch;
end;

procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.tbBoolLabel(Sender: TObject; var Caption: string);
begin
  if tbBool.Position = 0 then Caption := 'False' else Caption := 'True';
end;

procedure TForm1.tbRgbRedScroll(Sender: TObject; ScrollCode: TJPPegtopScrollCode; var ScrollPos: Integer);
begin
  UpdateColorSwatch;
end;

procedure TForm1.UpdateColorSwatch;
begin
  csw.SelectedColor := RGB(tbRgbRed.Position, tbRgbGreen.Position, tbRgbBlue.Position);
end;

end.
