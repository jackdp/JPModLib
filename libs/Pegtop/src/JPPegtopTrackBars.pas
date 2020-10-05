////////////////////////////////////////////////////////////////////////////////
// File:       PegtopTrackBars.pas
// Classes:    TPegtopSlideBar, TPegtopLabelSlideBar, TPegtopTrackBar,
//             TPegtopRangeBar
// Version:    1.02
// Date:       09 Sep 2004 created 1.00
//             19 Jan 2005 modified 1.01 (OnDrawTrack event handler added)
//             23 Mar 2005 modified 1.02 (Font and other properties added,
//                                        optional label hint added,
//                                        more label options added,
//                                        vertical labels implemented,
//                                        constrained rangebar implemented,
//                                        transparency (supports XP themes),
//                                        bugfix: order of properties changed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004, 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopTrackBar is a better track bar for changing integer values (floating
// point values can be "simulated" by changing the LabelMode property).
// TPegtopColorTrackBar is a track bar with a colored button.
// TPegtopRangeBar works like TPegtopTrackBar, but defines a range (two values).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

{
 jp mod 16-19.05.2019
}

unit JPPegtopTrackBars;

  {$IF CompilerVersion >= 23}
    //{$DEFINE DELPHIXE2_OR_ABOVE}
    {$DEFINE HAS_SYSTEM_UITYPES}
  {$IFEND}

interface

uses
  Windows, Classes, Messages, Graphics, Forms, Controls, JPPegtopThemes, GraphUtil, StdCtrls, ComCtrls, Types,
  {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}
  JPL.Rects,
  JPP.Common;

const
  WM_PEGTOPSLIDEBAR_EDITVALUE = WM_USER + $E + 1;

type

  TJPPegtopTrackBarButtonStateVisualParams = record
    Color: TColor;
    ColorTo: TColor;
    BorderColor: TColor;
    GripColor: TColor;
    GradientDirection: GraphUtil.TGradientDirection;
  end;

  TJPPegtopTrackBarButtonVisualParams = record
    Normal: TJPPegtopTrackBarButtonStateVisualParams;
    Hot: TJPPegtopTrackBarButtonStateVisualParams;
    Pushed: TJPPegtopTrackBarButtonStateVisualParams;
    Focused: TJPPegtopTrackBarButtonStateVisualParams;
    Disabled: TJPPegtopTrackBarButtonStateVisualParams;
  end;

  TJPPegtopTrackBarButtonVisualStyle = (bvsCustom, bvsWin10);

  TJPPegtopSlideBarOrientation = (psoHorizontal, psoVertical);

  TJPPegtopLabelMode = (plmPos, plmMul, plmDiv, plmShl, plmShr, plmBin, plmInv, plmLog, plmExp, plmSqr, plmAdd, plmSub);
  TJPPegtopLabelOption = (ploVisible, ploPlusMinusZero, ploExplicitSign, ploHint, ploDisableEdit, ploDisableCopy, ploDisablePaste, ploFlip, ploRotate);
  TJPPegtopLabelOptions = set of TJPPegtopLabelOption;
  TJPPegtopRangeOption = (proDisableConstrain);
  TJPPegtopRangeOptions = set of TJPPegtopRangeOption;

  TJPPegtopScrollCode = (pscLineUp, pscLineDown, pscPageUp, pscPageDown, pscPosition, pscTrack, pscTop, pscBottom, pscEndScroll);
  TJPPegtopScrollEvent = procedure(Sender: TObject; ScrollCode: TJPPegtopScrollCode; var ScrollPos: Integer) of object;
  TJPPegtopLabelEvent = procedure(Sender: TObject; var Caption: String) of object;
  TJPPegtopDrawTrackEvent = procedure(Sender: TObject; Canvas: TCanvas; Orientation: TJPPegtopSlideBarOrientation; BoundsRect: TRect; Center: TPoint) of object;

  {$Region ' --- TJPPegtopButtonStateParams --- '}
  TJPPegtopButtonStateParams = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColor: TColor;
    FColorTo: TColor;
    FBorderColor: TColor;
    FGradientDirection: TGradientDirection;
    FGripColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetGradientDirection(const Value: TGradientDirection);
    procedure SetGripColor(const Value: TColor);
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignVisualParams(const bsvp: TJPPegtopTrackBarButtonStateVisualParams);
    procedure Assign(const Src: TJPPegtopButtonStateParams); reintroduce;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection;// default Vcl.GraphUtil.gdVertical;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnShadow;
    property GripColor: TColor read FGripColor write SetGripColor default clGray;
  end;
  {$endregion TJPPegtopButtonStateParams}

  {$Region ' --- TJPPegtopButtonParams --- '}
  TJPPegtopButtonParams = class(TPersistent)
  private
    FWidth: integer;
    FOnChange: TNotifyEvent;
    FHeight: integer;
    FNormal: TJPPegtopButtonStateParams;
    FHot: TJPPegtopButtonStateParams;
    FDefaultDrawing: Boolean;
    FPushed: TJPPegtopButtonStateParams;
    FDisabled: TJPPegtopButtonStateParams;
    FFocused: TJPPegtopButtonStateParams;
    FDrawFocusRect: Boolean;
    FDrawGrip: Boolean;
    FVisualStyle: TJPPegtopTrackBarButtonVisualStyle;
    procedure SetWidth(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetNormal(const Value: TJPPegtopButtonStateParams);
    procedure SetHot(const Value: TJPPegtopButtonStateParams);
    procedure SetDefaultDrawing(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetPushed(const Value: TJPPegtopButtonStateParams);
    procedure SetDisabled(const Value: TJPPegtopButtonStateParams);
    procedure SetFocused(const Value: TJPPegtopButtonStateParams);
    procedure SetDrawFocusRect(const Value: Boolean);
    procedure SetDrawGrip(const Value: Boolean);
    procedure SetVisualStyle(const Value: TJPPegtopTrackBarButtonVisualStyle);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignVisualParams(const bvs: TJPPegtopTrackBarButtonVisualParams);
    procedure Assign(const Src: TJPPegtopButtonParams); reintroduce;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Width: integer read FWidth write SetWidth default 25;
    property Height: integer read FHeight write SetHeight default 17;
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default True;
    property DrawFocusRect: Boolean read FDrawFocusRect write SetDrawFocusRect default False; // used only when DefaultDrawing = True
    property DrawGrip: Boolean read FDrawGrip write SetDrawGrip default True;
    property Normal: TJPPegtopButtonStateParams read FNormal write SetNormal;
    property Hot: TJPPegtopButtonStateParams read FHot write SetHot;
    property Pushed: TJPPegtopButtonStateParams read FPushed write SetPushed;
    property Disabled: TJPPegtopButtonStateParams read FDisabled write SetDisabled;
    property Focused: TJPPegtopButtonStateParams read FFocused write SetFocused;
    property VisualStyle: TJPPegtopTrackBarButtonVisualStyle read FVisualStyle write SetVisualStyle default bvsCustom;
  end;
  {$endregion TJPPegtopButtonParams}

  {$REGION ' --- TJPPegtopSlideBar --- '}
  TJPPegtopSlideBar = class(TCustomControl)
  private
    FOrientation: TJPPegtopSlideBarOrientation;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: Integer;
    FMouseDown: Boolean;
    FMouseDelta: TPoint;
    FOnChange: TNotifyEvent;
    FOnScroll: TJPPegtopScrollEvent;
    FOnDrawTrack: TJPPegtopDrawTrackEvent;
    FTrackShadowColor: TColor;
    FTrackHighlightColor: TColor;
    FTrackInnerColor: TColor;
    FTrackHeight: Integer;
    FTickMarkPos: TTickMark;
    FTickColor: TColor;
    FTickLength: SmallInt;
    FTickSpacing: SmallInt;
    FTickCount: SmallInt;
    FTicksVisible: Boolean;
    FTrackInnerColorTo: TColor;
    FTrackInnerGradientDirection: TGradientDirection;
    FTrackDisabledColor: TColor;
    FTrackDisabledColorTo: TColor;
    FTrackDisabledBorderColor: TColor;
    FTickStyle: TPenStyle;
    FButtonParams: TJPPegtopButtonParams;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure SetOrientation(V: TJPPegtopSlideBarOrientation);
    procedure SetMin(V: Integer);
    procedure SetMax(V: Integer);
    procedure SetSmallChange(V: Integer);
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
    procedure SetOnDrawTrack(V: TJPPegtopDrawTrackEvent);
    procedure SetTrackShadowColor(const Value: TColor);
    procedure SetTrackHighlightColor(const Value: TColor);
    procedure SetTrackInnerColor(const Value: TColor);
    procedure SetTrackHeight(const Value: Integer);
    procedure SetTickMarkPos(const Value: TTickMark);
    procedure SetTickColor(const Value: TColor);
    procedure SetTickLength(const Value: SmallInt);
    procedure SetTickSpacing(const Value: SmallInt);
    procedure SetTickCount(const Value: SmallInt);
    procedure SetTicksVisible(const Value: Boolean);
    procedure SetTrackInnerColorTo(const Value: TColor);
    procedure SetTrackInnerGradientDirection(const Value: TGradientDirection);
    procedure SetTrackDisabledColor(const Value: TColor);
    procedure SetTrackDisabledColorTo(const Value: TColor);
    procedure SetTrackDisabledBorderColor(const Value: TColor);
    procedure SetTickStyle(const Value: TPenStyle);
    procedure SetButtonParams(const Value: TJPPegtopButtonParams);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintTo(const ACanvas: TCanvas); virtual;
    procedure Paint; override;
    procedure DrawTrack(const ACanvas: TCanvas; const AOrientation: TJPPegtopSlideBarOrientation; const P: TPoint); virtual;
    function TrackEnabled: Boolean; virtual;
    procedure EndScroll;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure ValidatePosition; virtual; abstract;
    procedure DrawText(const ACanvas: TCanvas; const X, Y: Integer; const S: String);
    procedure DrawButton(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState; const WithFocusRect: Boolean = False);
    procedure DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState); virtual;
    function DoStartScroll(const X, Y: Integer): TPoint; virtual; abstract;
    procedure DoScroll(const X, Y: Integer); virtual; abstract;
    procedure DoEndScroll; virtual; abstract;
    procedure PopupContextMenu(const X, Y: Integer); virtual;
    procedure ApplyPosition(const P: Integer); virtual; abstract;
    procedure ChangePosition(const Delta: Integer); virtual; abstract;
    function GetLinePoint: TPoint; virtual;
    function GetScrollableRect: TRect; virtual;
    function GetButtonSize: TSize; virtual; abstract;
    procedure PropsChanged(Sender: TObject);
    property IsMouseDown: Boolean read FMouseDown;
    property OnDrawTrack: TJPPegtopDrawTrackEvent read FOnDrawTrack write SetOnDrawTrack;
    property Orientation: TJPPegtopSlideBarOrientation read FOrientation write SetOrientation default psoHorizontal;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TJPPegtopScrollEvent read FOnScroll write FOnScroll;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowContextMenu;
    procedure ResetToDefault; virtual;
    procedure AssignTrackParams(const Src: TJPPegtopSlideBar);
    procedure AssignTickParams(const Src: TJPPegtopSlideBar);
    procedure AssignButtonParams(const Src: TJPPegtopSlideBar);
  published
    property TabOrder;
    property OnEnter;
    property OnExit;
    property TrackShadowColor: TColor read FTrackShadowColor write SetTrackShadowColor default clBtnShadow;
    property TrackHighlightColor: TColor read FTrackHighlightColor write SetTrackHighlightColor default clBtnHighlight;
    property TrackInnerColor: TColor read FTrackInnerColor write SetTrackInnerColor default clBlack;
    property TrackInnerColorTo: TColor read FTrackInnerColorTo write SetTrackInnerColorTo default clNone;
    property TrackInnerGradientDirection: TGradientDirection read FTrackInnerGradientDirection write SetTrackInnerGradientDirection default GraphUtil.gdVertical;
    property TrackDisabledColor: TColor read FTrackDisabledColor write SetTrackDisabledColor default clBtnFace;// $00E2E2E2;
    property TrackDisabledColorTo: TColor read FTrackDisabledColorTo write SetTrackDisabledColorTo default clNone;
    property TrackDisabledBorderColor: TColor read FTrackDisabledBorderColor write SetTrackDisabledBorderColor default clSilver;
    property TrackHeight: Integer read FTrackHeight write SetTrackHeight default 3;
    property TickMarkPos: TTickMark read FTickMarkPos write SetTickMarkPos default tmBottomRight;
    property TickColor: TColor read FTickColor write SetTickColor default clGray;
    property TickLength: SmallInt read FTickLength write SetTickLength default 4;
    property TickSpacing: SmallInt read FTickSpacing write SetTickSpacing default 8; // TrackRect to tick distance
    property TickCount: SmallInt read FTickCount write SetTickCount default 5;
    property TicksVisible: Boolean read FTicksVisible write SetTicksVisible default False;
    property TickStyle: TPenStyle read FTickStyle write SetTickStyle default psSolid;
    property ButtonParams: TJPPegtopButtonParams read FButtonParams write SetButtonParams;

  end;
  {$ENDREGION TJPPegtopSlideBar}

  {$REGION ' --- TJPPegtopLabelSlideBar --- '}
  TJPPegtopLabelSlideBar = class(TJPPegtopSlideBar)
  private
    FLabelCaption: TCaption;
    FLabelMin: TCaption;
    FLabelMax: TCaption;
    FLabelMode: TJPPegtopLabelMode;
    FLabelParam: Double;
    FLabelOptions: TJPPegtopLabelOptions;
    FCaptionRect: TRect;
    FCursor: TCursor;
    FOnLabel: TJPPegtopLabelEvent;
    FPositionLabel: TCustomLabel;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure ChangeCursor(const NewCursor: TCursor);
    procedure SetLabelCaption(V: TCaption);
    procedure SetLabelMin(V: TCaption);
    procedure SetLabelMax(V: TCaption);
    procedure SetLabelMode(V: TJPPegtopLabelMode);
    procedure SetLabelParam(V: Double);
    procedure SetLabelOptions(V: TJPPegtopLabelOptions);
    procedure SetCursor(Value: TCursor);
    procedure SetPositionLabel(const Value: TCustomLabel);
  protected
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function TransformCaption(const S: String): String; virtual;
    function PositionToString(const V: Integer): String;
    function PositionToValue(const V: Integer): Double;
    function ValueToPosition(const V: Double): Integer;
    function GetLinePoint: TPoint; override;
    function GetCaptionPoint: TPoint; virtual;
    procedure PropsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LabelCaption: TCaption read FLabelCaption write SetLabelCaption;
    property LabelMin: TCaption read FLabelMin write SetLabelMin;
    property LabelMax: TCaption read FLabelMax write SetLabelMax;
    property LabelMode: TJPPegtopLabelMode read FLabelMode write SetLabelMode;
    property LabelParam: Double read FLabelParam write SetLabelParam;
    property LabelOptions: TJPPegtopLabelOptions read FLabelOptions write SetLabelOptions;
    property Cursor: TCursor read FCursor write SetCursor;
    property OnLabel: TJPPegtopLabelEvent read FOnLabel write FOnLabel;
    property PositionLabel: TCustomLabel read FPositionLabel write SetPositionLabel;

  end;
  {$ENDREGION TJPPegtopLabelSlideBar}

  {$REGION ' --- TJPPegtopTrackBar --- '}
  TJPPegtopTrackBar = class(TJPPegtopLabelSlideBar)
  private
    FPosition: Integer;
    FDefaultPosition: Integer;
    FPositionCaptionRect: TRect;
    FMouseHover: Boolean;
    FTagExt: TJppTagExt;
    procedure WMEditValue(var Msg: TMessage); message WM_PEGTOPSLIDEBAR_EDITVALUE;
    procedure NumEditChange(Sender: TObject);
    procedure NumEditClose(Sender: TObject; var Action: TCloseAction);
    function PointToPosition(const X, Y: Integer): Integer;
    procedure MenuItemClick(Sender: TObject);
    procedure SetPosition(V: Integer);
    procedure SetDefaultPosition(V: Integer);
    function GetValue: Double;
    procedure SetValue(V: Double);
    procedure SetTagExt(const Value: TJppTagExt);
  protected
    procedure Loaded; override;
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure ValidatePosition; override;
    function TransformCaption(const S: String): String; override;
    function DoStartScroll(const X, Y: Integer): TPoint; override;
    procedure DoScroll(const X, Y: Integer); override;
    procedure DoEndScroll; override;
    procedure ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ApplyPosition(const P: Integer); override;
    procedure ChangePosition(const Delta: Integer); override;
    function GetButtonSize: TSize; override;
    function GetButtonPoint: TPoint;
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditValue;
    procedure CopyValue;
    procedure PasteValue;
    procedure ResetToDefault; override;
    property DefaultPosition: Integer read FDefaultPosition write SetDefaultPosition;
  published
    property Value: Double read GetValue write SetValue stored False;
    property Transparent;
    property Orientation;
    property Min;
    property Max;
    property SmallChange;
    //property OnChange;
    property OnScroll;
    property Enabled;
    property Visible;
    property OnDrawTrack;
    property Font;
    property ParentFont;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property Position: Integer read FPosition write SetPosition; // should be last property
  end;
  {$ENDREGION TJPPegtopTrackBar}

  {$Region ' --- TJPPegtopButtonColorRect --- '}
  TJPPegtopButtonColorRect = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FNormalBgColor: TColor;
    FNormalBorderColor: TColor;
    FHotBgColor: TColor;
    FHotBorderColor: TColor;
    FPushedBgColor: TColor;
    FPushedBorderColor: TColor;
    FDisabledBgColor: TColor;
    FDisabledBorderColor: TColor;
    FFocusedBgColor: TColor;
    FFocusedBorderColor: TColor;
    FMargins: TMargins;
    procedure SetOnChange(const Value: TNotifyEvent);

    procedure SetNormalBgColor(const Value: TColor);
    procedure SetNormalBorderColor(const Value: TColor);
    procedure SetHotBgColor(const Value: TColor);
    procedure SetHotBorderColor(const Value: TColor);
    procedure SetPushedBgColor(const Value: TColor);
    procedure SetPushedBorderColor(const Value: TColor);
    procedure SetDisabledBgColor(const Value: TColor);
    procedure SetDisabledBorderColor(const Value: TColor);
    procedure SetFocusedBgColor(const Value: TColor);
    procedure SetFocusedBorderColor(const Value: TColor);
    procedure SetMargins(const Value: TMargins);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property NormalBgColor: TColor read FNormalBgColor write SetNormalBgColor default clBlack;
    property NormalBorderColor: TColor read FNormalBorderColor write SetNormalBorderColor default clBlack;
    property HotBgColor: TColor read FHotBgColor write SetHotBgColor default clBlack;
    property HotBorderColor: TColor read FHotBorderColor write SetHotBorderColor default clBlack;
    property PushedBgColor: TColor read FPushedBgColor write SetPushedBgColor default clBlack;
    property PushedBorderColor: TColor read FPushedBorderColor write SetPushedBorderColor default clBlack;
    property DisabledBgColor: TColor read FDisabledBgColor write SetDisabledBgColor default clBtnFace;
    property DisabledBorderColor: TColor read FDisabledBorderColor write SetDisabledBorderColor default clGrayText;
    property FocusedBgColor: TColor read FFocusedBgColor write SetFocusedBgColor default clBlack;
    property FocusedBorderColor: TColor read FFocusedBorderColor write SetFocusedBorderColor default clBlack;
    property Margins: TMargins read FMargins write SetMargins;
  end;
  {$endregion TJPPegtopButtonColorRect}

  {$REGION ' --- TJPPegtopColorTrackBar --- '}
  TJPPegtopColorTrackBar = class(TJPPegtopTrackBar)
  private
    FColorRect: TJPPegtopButtonColorRect;
    procedure SetColorRect(const Value: TJPPegtopButtonColorRect);
  protected
    procedure DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnDrawTrack;
    property ColorRect: TJPPegtopButtonColorRect read FColorRect write SetColorRect;
  end;
  {$ENDREGION TJPPegtopColorTrackBar}

  {$REGION ' --- TJPPegtopRangeBar --- '}
  TJPPegtopRangeBar = class(TJPPegtopLabelSlideBar)
  private
    FPosition: array [0 .. 1] of Integer;
    FDefaultPosition: array [0 .. 1] of Integer;
    FPositionCaptionRect: array [0 .. 1] of TRect;
    FButtonFocus: Integer;
    FMouseHover: Integer;
    FConstrained: Boolean;
    FRangeOptions: TJPPegtopRangeOptions;
    FTagExt: TJppTagExt;
    FDrawTriangleWhenDisabled: Boolean;
    procedure WMEditValue(var Msg: TMessage); message WM_PEGTOPSLIDEBAR_EDITVALUE;
    procedure NumEditChange(Sender: TObject);
    procedure NumEditClose(Sender: TObject; var Action: TCloseAction);
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    function PointToPosition(const Index, X, Y: Integer): Integer;
    procedure MenuItemClick(Sender: TObject);
    function GetPosition(Index: Integer): Integer;
    procedure SetPosition(Index: Integer; V: Integer);
    function GetDefaultPosition(Index: Integer): Integer;
    procedure SetDefaultPosition(Index: Integer; V: Integer);
    function GetValue(Index: Integer): Double;
    procedure SetValue(Index: Integer; V: Double);
    procedure SetConstrained(V: Boolean);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetDrawTriangleWhenDisabled(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure ValidatePosition; override;
    function TransformCaption(const S: String): String; override;
    function DoStartScroll(const X, Y: Integer): TPoint; override;
    procedure DoScroll(const X, Y: Integer); override;
    procedure DoEndScroll; override;
    procedure ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ApplyPosition(const P: Integer); override;
    procedure ChangePosition(const Delta: Integer); override;
    function GetButtonSize: TSize; override;
    function GetButtonPoint(Index: Integer): TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditValueMin;
    procedure EditValueMax;
    procedure CopyValueMin;
    procedure CopyValueMax;
    procedure PasteValueMin;
    procedure PasteValueMax;
    procedure ResetToDefault; override;
    property DefaultPositionMin: Integer index 0 read GetDefaultPosition write SetDefaultPosition;
    property DefaultPositionMax: Integer index 1 read GetDefaultPosition write SetDefaultPosition;
  published
    property RangeOptions: TJPPegtopRangeOptions read FRangeOptions write FRangeOptions;
    property Constrained: Boolean read FConstrained write SetConstrained default False;
    property Transparent;
    property Orientation;
    property Min;
    property Max;
    property SmallChange;
    property OnChange;
    property OnScroll;
    property Enabled;
    property Visible;
    property OnDrawTrack;
    property Font;
    property ParentFont;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property DrawTriangleWhenDisabled: Boolean read FDrawTriangleWhenDisabled write SetDrawTriangleWhenDisabled default True;
    property PositionMin: Integer index 0 read GetPosition write SetPosition; // should be last property
    property PositionMax: Integer index 1 read GetPosition write SetPosition; // should be last property
    property ValueMin: Double index 0 read GetValue write SetValue stored False; // should be last property
    property ValueMax: Double index 1 read GetValue write SetValue stored False; // should be last property
  end;
  {$ENDREGION TJPPegtopRangeBar}

  {$REGION ' --- TJPPegtopHintWindow --- '}
  TJPPegtopHintWindow = class(THintWindow)
  private
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
  end;
  {$ENDREGION TJPPegtopHintWindow}



var
  JPPegtopTrackBarButtonVisualStyle_Win10: TJPPegtopTrackBarButtonVisualParams;

procedure InflateRectEx(var R: TRect; const dxLeft, dxTop, dxRight, dxBottom: integer);


implementation

uses
  Menus, SysUtils, Clipbrd, JPPegtopNumForms, JPPegtopNumEdits;

{$R *.res}

resourcestring
  PegtopTrackBarReset = '&Reset';
  PegtopTrackBarEdit = '&Edit value...';
  PegtopTrackBarCopy = '&Copy value';
  PegtopTrackBarPaste = '&Paste value';
  PegtopRangeBarReset = '&Reset';
  PegtopRangeBarConstrained = '&Constrained';
  PegtopRangeBarEditMin = 'Edit &lower limit...';
  PegtopRangeBarEditMax = 'Edit &upper limit...';
  PegtopRangeBarCopyMin = 'Copy &lower limit';
  PegtopRangeBarCopyMax = 'Copy &upper limit';
  PegtopRangeBarPasteMin = 'Paste &lower limit';
  PegtopRangeBarPasteMax = 'Paste &upper limit';

const
  ExpTable: array [0 .. 9] of Double = (1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);

var
  TrackBarPopupMenu: TPopupMenu;
  RangeBarPopupMenu: TPopupMenu;
  BitmapsLoaded: Boolean;
  Bitmaps: array [0 .. 3] of TBitmap;
  LabelHintWindow: THintWindow;


{$REGION ' ---------- Helpers ------------ '}

procedure InflateRectEx(var R: TRect; const dxLeft, dxTop, dxRight, dxBottom: integer);
begin
  R.Left := R.Left + dxLeft;
  R.Top := R.Top + dxTop;
  R.Right := R.Right + dxRight;
  R.Bottom := R.Bottom + dxBottom;
end;

// JppFrame3D: JPP.Common.Procs (JPPack)
procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; LeftColor, RightColor, TopColor, BottomColor: TColor; Width: Integer); overload;

  procedure DoRect;
  begin
    with Canvas, Rect do
    begin
      Pen.Color := TopColor;
      MoveTo(Left, Top);
      LineTo(Right, Top);

      Pen.Color := RightColor;
      LineTo(Right, Bottom);

      Pen.Color := BottomColor;
      LineTo(Left, Bottom);

      Pen.Color := LeftColor;
      LineTo(Left, Top);
    end;
  end;

begin
  Canvas.Pen.Width := 1;

  Dec(Rect.Bottom);
  Dec(Rect.Right);

  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;

  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

procedure JppFrame3D(Canvas: TCanvas; var Rect: TRect; Color: TColor; Width: integer); overload;
begin
  JppFrame3D(Canvas, Rect, Color, Color, Color, Color, Width);
end;


function GetLabelHintWindow: THintWindow;
begin
  if LabelHintWindow = NIL then
  begin
    LabelHintWindow := HintWindowClass.Create(Application);
    LabelHintWindow.Visible := False;
  end;
  Result := LabelHintWindow;
end;

function HasExtent(R: TRect): Boolean;
begin
  Result := (R.Right > R.Left) and (R.Bottom > R.Top);
end;

function IsValidFloat(S: String): Boolean;
var
  Dummy: Extended;
begin
  Result := TextToFloat(PChar(S), Dummy, fvExtended);
end;

procedure InitBitmaps;
var
  I: Integer;
begin
  if not BitmapsLoaded then
  begin
    for I := Low(Bitmaps) to High(Bitmaps) do
    begin
      Bitmaps[I] := TBitmap.Create;
      Bitmaps[I].LoadFromResourceName(HInstance, 'JPPEGTOPTRACKBAR' + Chr(48 + I div 10) + Chr(48 + I mod 10));
    end;
    BitmapsLoaded := True;
  end;
end;

procedure CloseBitmaps;
var
  I: Integer;
begin
  if BitmapsLoaded then
  begin
    for I := Low(Bitmaps) to High(Bitmaps) do Bitmaps[I].Free;
    BitmapsLoaded := False;
  end;
end;
{$ENDREGION Helpers}


{$REGION ' ------------------------------- TJPPegtopSlideBar -------------------------------- '}

  {$Region ' ----------------- TJPPegtopSlideBar: Create & Destroy -------------------- '}
constructor TJPPegtopSlideBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls, csOpaque];
  Width := 120;
  Height := 32;
  TabStop := True;
  DoubleBuffered := True;
  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FOrientation := psoHorizontal;
  FTrackShadowColor := clBtnShadow;
  FTrackHighlightColor := clBtnHighlight;
  FTrackInnerColor := clBlack;
  FTrackHeight := 3;
  FTickMarkPos := tmBottomRight;
  FTickColor := clGray;
  FTickLength := 4;
  FTickSpacing := 8;
  FTickCount := 5;
  FTicksVisible := False;
  FTickStyle := psSolid;
  FTrackInnerColorTo := clNone;
  FTrackInnerGradientDirection := GraphUtil.gdVertical;
  FTrackDisabledColor := clBtnFace; // $00E2E2E2;
  FTrackDisabledColorTo := clNone;
  FTrackDisabledBorderColor := clSilver;
  FButtonParams := TJPPegtopButtonParams.Create;
  FButtonParams.OnChange := PropsChanged;
end;

destructor TJPPegtopSlideBar.Destroy;
begin
  FreeAndNil(FButtonParams);
  inherited;
end;

procedure TJPPegtopSlideBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;
  {$endregion TJPPegtopSlideBar: Create & Destroy}


procedure TJPPegtopSlideBar.PropsChanged(Sender: TObject);
begin
  //if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJPPegtopSlideBar.AssignButtonParams(const Src: TJPPegtopSlideBar);
begin
  FButtonParams.Assign(Src.ButtonParams);
  PropsChanged(Self);
end;

procedure TJPPegtopSlideBar.AssignTickParams(const Src: TJPPegtopSlideBar);
begin
  FTickMarkPos := Src.TickMarkPos;
  FTickColor := Src.TickColor;
  FTickLength := Src.TickLength;
  FTickSpacing := Src.TickSpacing;
  FTickCount := Src.TickCount;
  FTicksVisible := Src.TicksVisible;
  FTickStyle := Src.TickStyle;
  PropsChanged(Self);
end;

procedure TJPPegtopSlideBar.AssignTrackParams(const Src: TJPPegtopSlideBar);
begin
  FTrackShadowColor := Src.TrackShadowColor;
  FTrackHighlightColor := Src.TrackHighlightColor;
  FTrackInnerColor := Src.TrackInnerColor;
  FTrackInnerColorTo := Src.TrackInnerColorTo;
  FTrackInnerGradientDirection := Src.TrackInnerGradientDirection;
  FTrackDisabledColor := Src.TrackDisabledColor;
  FTrackDisabledColorTo := Src.TrackDisabledColorTo;
  FTrackDisabledBorderColor := Src.TrackDisabledBorderColor;
  FTrackHeight := Src.TrackHeight;
  PropsChanged(Self);
end;

procedure TJPPegtopSlideBar.Paint;
begin
  if Transparent then
  begin
    DefaultTheme.DrawParent(Handle, Canvas.Handle, NIL);
  end
  else
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
  PaintTo(Canvas);
end;

procedure TJPPegtopSlideBar.PaintTo(const ACanvas: TCanvas);
var
  P: TPoint;
begin
  P := GetLinePoint;
  DrawTrack(ACanvas, FOrientation, P);
end;

  {$Region ' ----------------- TJPPegtopSlideBar.DrawText ------------------ '}
procedure TJPPegtopSlideBar.DrawText(const ACanvas: TCanvas; const X, Y: Integer; const S: String);
begin
  if S <> '' then
  begin
    if Enabled then
    begin
      // ACanvas.Font.Color := clWindowText;
      ACanvas.TextOut(X, Y, S);
    end
    else
    begin
      ACanvas.Font.Color := clBtnHighlight;
      ACanvas.TextOut(X + 1, Y + 1, S);
      ACanvas.Font.Color := clBtnShadow;
      ACanvas.TextOut(X, Y, S);
    end;
  end;
end;
  {$endregion TJPPegtopSlideBar.DrawText}

  {$Region ' -------------------- TJPPegtopSlideBar.DrawButton ------------------- '}
procedure TJPPegtopSlideBar.DrawButton(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState; const WithFocusRect: Boolean);
var
  bps: TJPPegtopButtonStateParams;
  bFocusRect: Boolean;
  R: TRect;
begin
  { DefaultTheme.Enabled := False; }


  // --------------- draw default button and exit ------------------
  if FButtonParams.DefaultDrawing then
  begin
    bFocusRect := (AState = ptsFocused) and (FButtonParams.DrawFocusRect);
    SetBkColor(ACanvas.Handle, ColorToRGB(Font.Color));
    SetTextColor(ACanvas.Handle, ColorToRGB(Color));
    DefaultTheme.DrawButton(ACanvas.Handle, ARect, AState, bFocusRect);
    Exit;
  end;



  R := ARect;
  bps := FButtonParams.Normal;

  case AState of
    ptsHot: bps := FButtonParams.Hot;
    ptsPushed: bps := FButtonParams.Pushed;
    ptsDisabled: bps := FButtonParams.Disabled;
    ptsFocused: bps := FButtonParams.Focused;
  end;

  if (bps.Color = clNone) and (bps.BorderColor = clNone) then Exit;

  with ACanvas do
  begin

    if bps.Color = clNone then Brush.Style := bsClear
    else
    begin
      Brush.Style := bsSolid;
      Brush.Color := bps.Color;
    end;

    InflateRect(R, -1, -1);

    // --------- draw background ----------
    if bps.ColorTo = clNone then
    begin
      if bps.Color <> clNone then FillRect(R);
    end
    else GradientFillCanvas(ACanvas, bps.Color, bps.ColorTo, R, bps.GradientDirection);


    // ------------ draw border ----------
    if bps.BorderColor <> clNone then
    begin
      Brush.Style := bsClear;
      Pen.Style := psSolid;
      Pen.Color := bps.BorderColor;
      if AState = ptsFocused then Pen.Width := 2
      else Pen.Width := 1;
      JppFrame3D(ACanvas, R, Pen.Color, Pen.Width);
      Brush.Style := bsSolid;
      Pen.Width := 1;
    end;

  end; // with ACanvas

end;
  {$endregion TJPPegtopSlideBar.DrawButton}

  {$Region ' ------------------ TJPPegtopSlideBar.DrawButtonGrip ----------------- '}
procedure TJPPegtopSlideBar.DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState);
var
  X, Y: Integer;
  cl: TColor;
begin
  if not FButtonParams.DrawGrip then Exit;

  cl := clBtnText;

  case AState of
    ptsNormal: cl := FButtonParams.Normal.GripColor;
    ptsHot: cl := FButtonParams.Hot.GripColor;
    ptsPushed: cl := FButtonParams.Pushed.GripColor;
    ptsDisabled: cl := FButtonParams.Disabled.GripColor;
    ptsFocused: cl := FButtonParams.Focused.GripColor;
  end;

  //if Enabled then
  if cl <> clNone then
  begin
    Y := ARect.Top + 6;
    while Y < ARect.Bottom - 6 do
    begin
      X := ARect.Left + 6;
      while X < ARect.Right - 6 do
      begin
        ACanvas.Pixels[X, Y] :=  cl;
        Inc(X, 2);
      end;
      Inc(Y, 2);
    end;
  end;
end;
  {$endregion TJPPegtopSlideBar.DrawButtonGrip}

  {$Region ' ---------------------- TJPPegtopSlideBar.DrawTrack ------------------------ '}
// P - center point
procedure TJPPegtopSlideBar.DrawTrack(const ACanvas: TCanvas; const AOrientation: TJPPegtopSlideBarOrientation; const P: TPoint);
var
  P1, P2: Integer;
  HalfHeight, HalfWidth, dx, DeltaX, xStart, DeltaY, yStart: Integer;
  TrackRect: TRect;
begin
  if Assigned(FOnDrawTrack) then FOnDrawTrack(Self, ACanvas, AOrientation, ClientRect, P)
  else
    with ACanvas do
    begin
      HalfHeight := FTrackHeight div 2;
      dx := 1;

      // ----------------------- Vertical position -------------------
      if FOrientation = psoHorizontal then
      begin

        TrackRect.Left := dx;
        TrackRect.Right := ClientWidth - dx;
        TrackRect.Top := P.Y - HalfHeight;
        TrackRect.Height := FTrackHeight;

        if Self.Enabled then
        begin
          Brush.Color := FTrackInnerColor;
          Pen.Style := psClear;
          if FTrackInnerColorTo = clNone then Rectangle(TrackRect)
          else GradientFillCanvas(ACanvas, FTrackInnerColor, FTrackInnerColorTo, TrackRect, FTrackInnerGradientDirection);
          Pen.Style := psSolid;

          P1 := dx;
          P2 := ClientWidth - dx;
          Pen.Color := FTrackShadowColor;
          MoveTo(P2 - 1, P.Y - HalfHeight);
          LineTo(P1, P.Y - HalfHeight);
          LineTo(P1, P.Y + HalfHeight);
          Pen.Color := FTrackHighlightColor;
          LineTo(P2 - 1, P.Y + HalfHeight);
          LineTo(P2 - 1, P.Y - HalfHeight);

          //if TrackEnabled then Pen.Color := FTrackInnerColor
          //else Pen.Color := clBtnFace;
          //      MoveTo(P1 + 1, P.Y);
          //      LineTo(P2 - 1, P.Y);
        end
        else
        begin
          Brush.Color := FTrackDisabledColor;
          if FTrackDisabledColorTo = clNone then Rectangle(TrackRect)
          else GradientFillCanvas(ACanvas, FTrackDisabledColor, FTrackDisabledColorTo, TrackRect, FTrackInnerGradientDirection);
          Pen.Color := FTrackDisabledBorderColor;
          Brush.Style := bsClear;
          Rectangle(TrackRect);
          Brush.Style := bsSolid;
        end;


        // -------------- ticks ----------------
        if FTicksVisible then
        begin

          Pen.Color := FTickColor;
          Pen.Style := FTickStyle;
          Brush.Style := bsClear;
          Pen.Width := 1;

          if FTickCount > 1 then
          begin

            DeltaX := TrackRect.Width div (FTickCount - 1);
            if DeltaX = 0 then DeltaX := 1;

            if (FTickMarkPos = tmBottomRight) or (FTickMarkPos = tmBoth) then
            begin
              xStart := TrackRect.Left;
              while True do
              begin
                if xStart = TrackRect.Left then dx := 1
                else if xStart = TrackRect.Right then dx := -1
                else dx := 0;
                MoveTo(xStart + dx, TrackRect.Bottom + FTickSpacing);
                LineTo(xStart + dx, TrackRect.Bottom + FTickSpacing + FTickLength);
                xStart := xStart + DeltaX;
                if xStart > TrackRect.Right then Break;
              end;
            end;

            if (FTickMarkPos = tmTopLeft) or (FTickMarkPos = tmBoth) then
            begin
              xStart := TrackRect.Left;
              while True do
              begin
                if xStart = TrackRect.Left then dx := 1
                else if xStart = TrackRect.Right then dx := -1
                else dx := 0;
                MoveTo(xStart + dx, TrackRect.Top - FTickSpacing);
                LineTo(xStart + dx, TrackRect.Top - FTickSpacing - FTickLength);
                xStart := xStart + DeltaX;
                if xStart > TrackRect.Right then Break;
              end;
            end;

          end;

          Brush.Style := bsSolid;
          Pen.Style := psSolid;

        end; // if FTicksVisible

      end

      else

      // ----------------------- Horizontal position ------------------------------
      begin

        dx := 1;
        HalfWidth := FTrackHeight div 2;

        TrackRect.Left := P.X - HalfWidth + dx;
        if Odd(TrackHeight) then TrackRect.Left := TrackRect.Left - 1;
        TrackRect.Width := FTrackHeight;
        TrackRect.Top := dx;
        TrackRect.Height := ClientHeight - dx - dx;

        if Self.Enabled then
        begin
          Brush.Color := FTrackInnerColor;
          Pen.Style := psClear;
          if FTrackInnerColorTo = clNone then Rectangle(TrackRect)
          else GradientFillCanvas(ACanvas, FTrackInnerColor, FTrackInnerColorTo, TrackRect, FTrackInnerGradientDirection);
          Pen.Style := psSolid;

          P1 := 1;
          P2 := ClientHeight - 1;
          Pen.Color := FTrackShadowColor;
          MoveTo(P.X - HalfWidth, P2 - 1);
          LineTo(P.X - HalfWidth, P1);
          LineTo(P.X + HalfWidth, P1);
          Pen.Color := FTrackHighlightColor;
          LineTo(P.X + HalfWidth, P2 - 1);
          LineTo(P.X - HalfWidth, P2 - 1);

          //if TrackEnabled then Pen.Color := FTrackInnerColor
          //else Pen.Color := clBtnFace;
          //      MoveTo(P.X, P1 + 1);
          //      LineTo(P.X, P2 - 1);
        end
        else
        begin
          Brush.Color := FTrackDisabledColor;
          if FTrackDisabledColorTo = clNone then Rectangle(TrackRect)
          else GradientFillCanvas(ACanvas, FTrackDisabledColor, FTrackDisabledColorTo, TrackRect, FTrackInnerGradientDirection);
          Pen.Color := FTrackDisabledBorderColor;
          Brush.Style := bsClear;
          Rectangle(TrackRect);
          Brush.Style := bsSolid;
        end;


        // -------------- ticks ----------------
        if FTicksVisible then
        begin

          Pen.Color := FTickColor;
          Pen.Style := FTickStyle;
          Brush.Style := bsClear;
          Pen.Width := 1;

          if FTickCount > 1 then
          begin

            DeltaY := TrackRect.Height div (FTickCount - 1);
            if DeltaY = 0 then DeltaY := 1;

            if (FTickMarkPos = tmBottomRight) or (FTickMarkPos = tmBoth) then
            begin
              yStart := TrackRect.Top;
              while True do
              begin
                if yStart = TrackRect.Top then dx := 1
                else if yStart = TrackRect.Bottom then dx := -1
                else dx := 0;
                MoveTo(TrackRect.Right + FTickSpacing, yStart + dx);
                LineTo(TrackRect.Right + FTickSpacing + FTickLength, yStart + dx);
                yStart := yStart + DeltaY;
                if yStart > TrackRect.Bottom then Break;
              end;
            end;

            if (FTickMarkPos = tmTopLeft) or (FTickMarkPos = tmBoth) then
            begin
              yStart := TrackRect.Top;
              while True do
              begin
                if yStart = TrackRect.Top then dx := 1
                else if yStart = TrackRect.Bottom then dx := -1
                else dx := 0;
                MoveTo(TrackRect.Left - FTickSpacing, yStart + dx);
                LineTo(TrackRect.Left - FTickSpacing - FTickLength, yStart + dx);
                yStart := yStart + DeltaY;
                if yStart > TrackRect.Bottom then Break;
              end;
            end;

          end;

          Brush.Style := bsSolid;
          Pen.Style := psSolid;

        end; // if FTicksVisible

      end;

    end; // with ACanvas

end;
  {$endregion TJPPegtopSlideBar.DrawTrack}

  {$Region ' ------------------- TJPPegtopSlideBar - Mouse procs: Down, Move, Up, Wheel . . . ---------------------- '}

procedure TJPPegtopSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ScrollableRect: TRect;
Begin
  if Visible and Enabled then
  begin
    if Button = mbLeft then
    begin
      ScrollableRect := GetScrollableRect;
      if (X >= ScrollableRect.Left) and (X < ScrollableRect.Right) and (Y >= ScrollableRect.Top) and (Y < ScrollableRect.Bottom) then
      begin
        FMouseDelta := DoStartScroll(X, Y);
        FMouseDown := True;
        if CanFocus then
        begin
          SetFocus;
          Invalidate;
        end;
      end
      else
      begin
        ExtraMouseDown(Button, Shift, X, Y);
      end;
    end
    else if Button = mbRight then
    begin
      if FMouseDown then
      begin
        FMouseDown := False;
        DoEndScroll;
      end;
    end;
  end;
  inherited;
end;

procedure TJPPegtopSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then
  begin
    DoScroll(X - FMouseDelta.X, Y - FMouseDelta.Y);
  end
  else
  begin
    ExtraMouseMove(Shift, X, Y);
  end;
  inherited;
end;

procedure TJPPegtopSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then
  begin
    FMouseDown := False;
    DoEndScroll;
  end;
  // if Button = mbRight then PopupContextMenu(X, Y);
  inherited;
end;


function TJPPegtopSlideBar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if FOrientation = psoVertical then ChangePosition(FSmallChange)
  else ChangePosition(-FSmallChange);
  Result := True;
end;


function TJPPegtopSlideBar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if FOrientation = psoVertical then ChangePosition(-FSmallChange)
  else ChangePosition(FSmallChange);
  Result := True;
end;

procedure TJPPegtopSlideBar.CMMouseLeave(var Msg: TMessage);
begin
  ExtraMouseMove([], -1, -1);
  inherited;
end;

procedure TJPPegtopSlideBar.ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TJPPegtopSlideBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;
  {$endregion TJPPegtopSlideBar - Mouse procs: Down, Move, Up, Wheel . . .}

  {$Region ' ---------------- TJPPegtopSlideBar: Messages ------------------- '}
procedure TJPPegtopSlideBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  // Msg.Result := 1;
  // Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.DC, Msg.Unused);
  inherited;
end;

procedure TJPPegtopSlideBar.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TJPPegtopSlideBar.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TJPPegtopSlideBar.CMWantSpecialKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_LEFT, VK_RIGHT: if FOrientation = psoHorizontal then Msg.Result := 1 else inherited;
    VK_UP, VK_DOWN: if FOrientation = psoVertical then Msg.Result := 1 else inherited;
  else
    inherited;
  end;
end;


procedure TJPPegtopSlideBar.CMEnabledChanged(var Msg: TMessage);
begin
  FMouseDown := False;
  inherited;
  Invalidate;
end;

procedure TJPPegtopSlideBar.WMContextMenu(var Msg: TWMContextMenu);
begin
  if (Msg.XPos = -1) and (Msg.YPos = -1) then ShowContextMenu
  else PopupContextMenu(Msg.XPos, Msg.YPos);
  // inherited is not called because we already did popup the context menu
end;
  {$endregion TJPPegtopSlideBar: Messages}

function TJPPegtopSlideBar.TrackEnabled: Boolean;
begin
  Result := Enabled;
end;

procedure TJPPegtopSlideBar.EndScroll;
begin
  if FMouseDown then
  begin
    FMouseDown := False;
    DoEndScroll;
  end;
end;

procedure TJPPegtopSlideBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT: if FOrientation = psoHorizontal then ChangePosition(-FSmallChange);
    VK_RIGHT: if FOrientation = psoHorizontal then ChangePosition(FSmallChange);
    VK_UP: if FOrientation = psoVertical then ChangePosition(FSmallChange);
    VK_DOWN: if FOrientation = psoVertical then ChangePosition(-FSmallChange);
  end;
  inherited;
end;

procedure TJPPegtopSlideBar.ShowContextMenu;
var
  P: TPoint;
begin
  P := GetLinePoint;
  Dec(P.Y, 2);
  P := ClientToScreen(P);
  PopupContextMenu(P.X, P.Y);
end;

procedure TJPPegtopSlideBar.PopupContextMenu(const X, Y: Integer);
begin
  //
end;

procedure TJPPegtopSlideBar.ResetToDefault;
begin
  //
end;

function TJPPegtopSlideBar.GetLinePoint: TPoint;
begin
  Result.X := ClientWidth div 2;
  Result.Y := ClientHeight div 2;
end;

function TJPPegtopSlideBar.GetScrollableRect: TRect;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if FOrientation = psoHorizontal then Result := Bounds(0, LinePoint.Y - ButtonSize.CY div 2, ClientWidth, ButtonSize.CY)
  else Result := Bounds(LinePoint.X - ButtonSize.CX div 2, 0, ButtonSize.CX, ClientHeight);
end;

function TJPPegtopSlideBar.GetTransparent: Boolean;
begin
  Result := not(csOpaque in ControlStyle);
end;


  {$Region ' ----------------- TJPPegtopSlideBar: Setters ------------------ '}
procedure TJPPegtopSlideBar.SetOrientation(V: TJPPegtopSlideBarOrientation);
begin
  if FOrientation <> V then
  begin
    FOrientation := V;
    Invalidate;
  end;
end;

procedure TJPPegtopSlideBar.SetMin(V: Integer);
begin
  if FMin <> V then
  begin
    FMin := V;
    if FMax < FMin Then FMax := FMin;
    ValidatePosition;
    Invalidate;
  end;
end;

procedure TJPPegtopSlideBar.SetButtonParams(const Value: TJPPegtopButtonParams);
begin
  FButtonParams := Value;
  FButtonParams.OnChange := PropsChanged;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetMax(V: Integer);
begin
  if FMax <> V then
  begin
    FMax := V;
    if FMin > FMax Then FMin := FMax;
    ValidatePosition;
    Invalidate;
  end;
end;

procedure TJPPegtopSlideBar.SetSmallChange(V: Integer);
begin
  if V < 1 then V := 1
  else if V > (FMax - FMin) then V := (FMax - FMin);
  if FSmallChange <> V then
  begin
    FSmallChange := V;
  end;
end;

procedure TJPPegtopSlideBar.SetTickColor(const Value: TColor);
begin
  if FTickColor = Value then Exit;
  FTickColor := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTickCount(const Value: SmallInt);
begin
  if FTickCount = Value then Exit;
  FTickCount := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTickLength(const Value: SmallInt);
begin
  if FTickLength = Value then Exit;
  FTickLength := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTickMarkPos(const Value: TTickMark);
begin
  if FTickMarkPos = Value then Exit;
  FTickMarkPos := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTickSpacing(const Value: SmallInt);
begin
  if FTickSpacing = Value then Exit;
  FTickSpacing := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTickStyle(const Value: TPenStyle);
begin
  if FTickStyle = Value then Exit;
  FTickStyle := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTicksVisible(const Value: Boolean);
begin
  if FTicksVisible = Value then Exit;
  FTicksVisible := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackDisabledBorderColor(const Value: TColor);
begin
  if FTrackDisabledBorderColor = Value then Exit;
  FTrackDisabledBorderColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackDisabledColor(const Value: TColor);
begin
  if FTrackDisabledColor = Value then Exit;
  FTrackDisabledColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackDisabledColorTo(const Value: TColor);
begin
  if FTrackDisabledColorTo = Value then Exit;
  FTrackDisabledColorTo := Value;
  if not Enabled then Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackHeight(const Value: Integer);
begin
  if FTrackHeight = Value then Exit;
  FTrackHeight := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackHighlightColor(const Value: TColor);
begin
  if FTrackHighlightColor = Value then Exit;
  FTrackHighlightColor := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackInnerColor(const Value: TColor);
begin
  if FTrackInnerColor = Value then Exit;
  FTrackInnerColor := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackInnerColorTo(const Value: TColor);
begin
  if FTrackInnerColorTo = Value then Exit;
  FTrackInnerColorTo := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackInnerGradientDirection(const Value: TGradientDirection);
begin
  if FTrackInnerGradientDirection = Value then Exit;
  FTrackInnerGradientDirection := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTrackShadowColor(const Value: TColor);
begin
  if FTrackShadowColor = Value then Exit;
  FTrackShadowColor := Value;
  Invalidate;
end;

procedure TJPPegtopSlideBar.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then ControlStyle := ControlStyle - [csOpaque]
    else ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TJPPegtopSlideBar.SetOnDrawTrack(V: TJPPegtopDrawTrackEvent);
begin
  if (TMethod(FOnDrawTrack).Code <> TMethod(V).Code) or (TMethod(FOnDrawTrack).Data <> TMethod(V).Data) then
  begin
    FOnDrawTrack := V;
    Invalidate;
  end;
end;
  {$endregion TJPPegtopSlideBar: Setters}


{$ENDREGION TJPPegtopSlideBar}


{$REGION ' ----------------------------------- TJPPegtopLabelSlideBar ----------------------------------- '}

constructor TJPPegtopLabelSlideBar.Create(AOwner: TComponent);
begin
  inherited;
  FCursor := Cursor;
  FLabelCaption := Name;
  FLabelMin := '';
  FLabelMax := '';
  FLabelMode := plmPos;
  FLabelParam := 1.0;
  FLabelOptions := [ploVisible];
  FPositionLabel := nil;
end;

destructor TJPPegtopLabelSlideBar.Destroy;
begin
  inherited;
end;

procedure TJPPegtopLabelSlideBar.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

  {$Region ' ------------------ TJPPegtopLabelSlideBar.PaintTo -------------------- '}
procedure TJPPegtopLabelSlideBar.PaintTo(const ACanvas: TCanvas);
var
  S: String;
  CaptionPoint: TPoint;
  CaptionSize: TSize;
  LogFont: TLogFont;
  TextMetric: TTextMetric;
  RotFont, OrgFont: THandle;
begin
  ACanvas.Font.Assign(Font);
  S := TransformCaption(FLabelCaption);
  if Assigned(FOnLabel) then FOnLabel(Self, S);
  if Assigned(FPositionLabel) then FPositionLabel.Caption := S;
  if ploVisible in FLabelOptions then
  begin
    //if Assigned(FOnLabel) then FOnLabel(Self, S);
    CaptionSize := Canvas.TextExtent(S);
    CaptionPoint := GetCaptionPoint;
    if Orientation = psoHorizontal then
    begin
      FCaptionRect.Left := CaptionPoint.X - CaptionSize.CX div 2;
      FCaptionRect.Right := FCaptionRect.Left + CaptionSize.CX;
      FCaptionRect.Top := CaptionPoint.Y;
      FCaptionRect.Bottom := FCaptionRect.Top + CaptionSize.CY;
      ACanvas.Brush.Style := bsClear;
      DrawText(ACanvas, FCaptionRect.Left, FCaptionRect.Top, S);
      DrawText(ACanvas, 0, FCaptionRect.Top, FLabelMin);
      DrawText(ACanvas, ClientWidth - ACanvas.TextWidth(FLabelMax), FCaptionRect.Top, FLabelMax);
    end
    else
    begin
      FCaptionRect.Left := CaptionPoint.X;
      FCaptionRect.Right := FCaptionRect.Left + CaptionSize.CY;
      FCaptionRect.Top := CaptionPoint.Y - CaptionSize.CX div 2;
      FCaptionRect.Bottom := FCaptionRect.Top + CaptionSize.CX;
      ACanvas.Brush.Style := bsClear;
      GetObject(ACanvas.Font.Handle, SizeOf(TLogFont), @LogFont);
      GetTextMetrics(ACanvas.Handle, TextMetric);
      if (TextMetric.tmPitchAndFamily and (TMPF_TRUETYPE or TMPF_VECTOR)) = 0 then // no truetype font
          StrCopy(LogFont.lfFaceName, 'Arial');
      if ploRotate in FLabelOptions then
      begin
        LogFont.lfEscapement := 2700; // 270.0°
        LogFont.lfOrientation := 2700; // 270.0°
      end
      else
      begin
        LogFont.lfEscapement := 900; // 90.0°
        LogFont.lfOrientation := 900; // 90.0°
      end;
      RotFont := CreateFontIndirect(LogFont);
      try
        OrgFont := SelectObject(ACanvas.Handle, RotFont);
        try
          if ploRotate in FLabelOptions then
          begin
            DrawText(ACanvas, FCaptionRect.Right, FCaptionRect.Top, S);
            DrawText(ACanvas, FCaptionRect.Right, ClientHeight - ACanvas.TextWidth(FLabelMin), FLabelMin);
            DrawText(ACanvas, FCaptionRect.Right, 0, FLabelMax);
          end
          else
          begin
            DrawText(ACanvas, FCaptionRect.Left, FCaptionRect.Bottom, S);
            DrawText(ACanvas, FCaptionRect.Left, ClientHeight, FLabelMin);
            DrawText(ACanvas, FCaptionRect.Left, ACanvas.TextWidth(FLabelMax), FLabelMax);
          end;
        finally
          SelectObject(ACanvas.Handle, OrgFont);
        end;
      finally
        DeleteObject(RotFont);
      end;
    end;
  end;
  inherited;
end;
  {$endregion TJPPegtopLabelSlideBar.PaintTo}


  {$Region ' --------------- TJPPegtopLabelSlideBar: MouseDown, MouseMove, MouseUp ----------------- '}
procedure TJPPegtopLabelSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HintWindow: THintWindow;
  HintRect: TRect;
  HintPoint: TPoint;
  S: String;
begin
  inherited;
  if FMouseDown and (ploHint in FLabelOptions) then
  begin
    HintWindow := GetLabelHintWindow;
    if not HintWindow.Visible then
    begin
      HintWindow.Color := Application.HintColor;
      HintWindow.Visible := True;
    end;
    S := TransformCaption(FLabelCaption);
    if Assigned(FOnLabel) then FOnLabel(Self, S);
    HintRect := HintWindow.CalcHintRect(200, S, NIL);
    HintPoint := GetCaptionPoint;
    HintPoint.X := HintPoint.X - (HintRect.Right - HintRect.Left) div 2;
    HintPoint.Y := HintPoint.Y - 6;
    HintPoint := ClientToScreen(HintPoint);
    OffsetRect(HintRect, HintPoint.X, HintPoint.Y);
    HintWindow.ActivateHint(HintRect, S);
    HintWindow.Update;
  end;
end;

procedure TJPPegtopLabelSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  HintWindow: THintWindow;
  HintRect: TRect;
  HintPoint: TPoint;
  S: String;
begin
  inherited;
  if FMouseDown and (ploHint in FLabelOptions) then
  begin
    HintWindow := GetLabelHintWindow;
    if not HintWindow.Visible then
    begin
      HintWindow.Color := Application.HintColor;
      HintWindow.Visible := True;
    end;
    S := TransformCaption(FLabelCaption);
    if Assigned(FOnLabel) then FOnLabel(Self, S);
    HintRect := HintWindow.CalcHintRect(200, S, NIL);
    HintPoint := GetCaptionPoint;
    HintPoint.X := HintPoint.X - (HintRect.Right - HintRect.Left) div 2;
    HintPoint.Y := HintPoint.Y - 6;
    HintPoint := ClientToScreen(HintPoint);
    OffsetRect(HintRect, HintPoint.X, HintPoint.Y);
    {    if (HintWindow.Width <> HintRect.Right - HintRect.Left)
      or (HintWindow.Height <> HintRect.Bottom - HintRect.Top) then
     // deactivate befor resizing to avoid graphic errors
     HintWindow.ReleaseHandle;}
    HintWindow.ActivateHint(HintRect, S);
    HintWindow.Update;
  end;
end;

procedure TJPPegtopLabelSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HintWindow: THintWindow;
begin
  inherited;
  if not FMouseDown then
  begin
    HintWindow := LabelHintWindow;
    if (HintWindow <> NIL) and HintWindow.Visible then
    begin
      HintWindow.Visible := False;
      HintWindow.ReleaseHandle;
    end;
  end;
end;



{$endregion TJPPegtopLabelSlideBar: MouseDown, MouseMove, MouseUp}


  {$Region ' ------------------ TJPPegtopLabelSlideBar: Position related -------------- '}
function TJPPegtopLabelSlideBar.ValueToPosition(const V: Double): Integer;
begin
  Result := Low(Integer); // undefined
  case FLabelMode of
    plmPos: Result := Round(V);
    plmMul: Result := Round(V / FLabelParam);
    plmDiv: Result := Round(V * FLabelParam);
    plmShl: if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then Result := Round(V / ExpTable[Trunc(FLabelParam)]);
    plmShr: if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then Result := Round(V * ExpTable[Trunc(FLabelParam)]);
    plmBin: if (Trunc(FLabelParam) >= 0) and (Trunc(FLabelParam) <= 10) then Result := Round(Ln(V / Trunc(FLabelParam)) / Ln(2));
    plmInv: if (FLabelParam > 0) and (FLabelParam <= 1000000) and (V <> 0) then Result := Round(FLabelParam / V);
    plmLog: if (FLabelParam > 0) and (FLabelParam <= 1000) then Result := Round(Exp(Ln(FLabelParam) * V));
    plmExp: if (FLabelParam > 0) and (FLabelParam <= 1000) then Result := Round(Ln(V) / Ln(FLabelParam));
    plmSqr: Result := Round(Sqrt(V));
    plmAdd: Result := Round(V - FLabelParam);
    plmSub: Result := Round(FLabelParam - V);
  end;
end;

function TJPPegtopLabelSlideBar.PositionToValue(const V: Integer): Double;
begin
  Result := -10000000000000.0;
  case FLabelMode of
    plmPos: Result := V;
    plmMul: Result := V * FLabelParam;
    plmDiv: Result := V / FLabelParam;
    plmShl: if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then Result := Round(V * ExpTable[Trunc(FLabelParam)]);
    plmShr: if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then Result := V / ExpTable[Trunc(FLabelParam)];
    plmBin: if (Trunc(FLabelParam) >= 0) and (FLabelParam <= 10) then Result := Trunc(FLabelParam) shl V;
    plmInv: if (FLabelParam > 0) and (FLabelParam <= 1000000) and (V <> 0) then Result := FLabelParam / V;
    plmLog: if (FLabelParam > 0) and (FLabelParam <= 1000) and (V > 0) then Result := Ln(V) / Ln(FLabelParam);
    plmExp: if (FLabelParam > 0) and (FLabelParam <= 1000) then Result := Exp(Ln(FLabelParam) * V);
    plmSqr: Result := Sqr(V);
    plmAdd: Result := FLabelParam + V;
    plmSub: Result := FLabelParam - V;
  end;
end;

function TJPPegtopLabelSlideBar.PositionToString(const V: Integer): String;

  function AddSign(const S: String; const IsNull, IsPositive: Boolean): String;
  begin
    if IsNull and (ploPlusMinusZero in FLabelOptions) then Result := '±' + S
    else if (V > 0) and (ploExplicitSign in FLabelOptions) then Result := '+' + S
    else Result := S;
  end;

begin
  Result := '***';
  case FLabelMode of
    plmPos: Result := AddSign(IntToStr(V), V = 0, V > 0);
    plmMul: Result := AddSign(FloatToStrF(V * FLabelParam, ffGeneral, 7, 2), V = 0, V > 0);
    plmDiv: Result := AddSign(FloatToStrF(V / FLabelParam, ffNumber, 16, Trunc(Ln(FLabelParam) / Ln(10) + 0.9999)), V = 0, V > 0);
    plmShl: if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
          Result := AddSign(IntToStr(Round(V * ExpTable[Trunc(FLabelParam)])), V = 0, V > 0);
    plmShr: if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
          Result := AddSign(FloatToStrF(V / ExpTable[Trunc(FLabelParam)], ffNumber, 16, Trunc(FLabelParam)), V = 0, V > 0);
    plmBin: if (Trunc(FLabelParam) >= 0) and (Trunc(FLabelParam) <= 10) then Result := AddSign(IntToStr(Trunc(FLabelParam) shl V), False, True);
    plmInv: if (FLabelParam > 0) and (FLabelParam <= 1000000) then
      begin
        if V = 0 then Result := 'inf.'
        else Result := AddSign(FloatToStrF(FLabelParam / V, ffNumber, 16, Trunc(Ln(Abs(V) / FLabelParam) / Ln(10) + 2.4999)), False, V > 0);
      end;
    plmLog: if (FLabelParam > 0) and (FLabelParam <= 1000) then
      begin
        if V <= 0 then Result := 'undef.'
        else Result := AddSign(FloatToStrF(Ln(V) / Ln(FLabelParam), ffNumber, 16, 2), V = 1, V < 1);
      end;
    plmExp: if (FLabelParam >= 1) and (FLabelParam <= 1000) then Result := AddSign(FloatToStrF(Exp(Ln(FLabelParam) * V), ffNumber, 16, 0), False, True);
    plmSqr: Result := AddSign(IntToStr(Sqr(V)), V = 0, True);
    plmAdd: Result := AddSign(FloatToStrF(FLabelParam + V, ffGeneral, 7, 2), FLabelParam + V = 0, FLabelParam + V > 0);
    plmSub: Result := AddSign(FloatToStrF(FLabelParam - V, ffGeneral, 7, 2), FLabelParam - V = 0, FLabelParam - V > 0);
  end;
end;
  {$endregion TJPPegtopLabelSlideBar: Position related}


procedure TJPPegtopLabelSlideBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FPositionLabel) and (Operation = opRemove) then FPositionLabel := nil;
end;

function TJPPegtopLabelSlideBar.TransformCaption(const S: String): String;
begin
  Result := S; // no transformation
end;

procedure TJPPegtopLabelSlideBar.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FPositionLabel) then FPositionLabel.Enabled := Self.Enabled;
end;

procedure TJPPegtopLabelSlideBar.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJPPegtopLabelSlideBar.ChangeCursor(const NewCursor: TCursor);
begin
  inherited Cursor := NewCursor;
end;

procedure TJPPegtopLabelSlideBar.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    ChangeCursor(FCursor);
  end;
end;

  {$Region ' ------------------ TJPPegtopLabelSlideBar.GetLinePoint ------------------ '}
function TJPPegtopLabelSlideBar.GetLinePoint: TPoint;
var
  ButtonSize: TSize;
  CaptionHeight: Integer;
begin
  if not(ploVisible in FLabelOptions) then
  begin
    Result := inherited GetLinePoint;
  end
  else
  begin
    ButtonSize := GetButtonSize;
    Canvas.Font.Assign(Font);
    CaptionHeight := Canvas.TextHeight('0');
    if FOrientation = psoHorizontal then
    begin
      Result.X := ClientWidth div 2;
      if ploFlip in FLabelOptions then Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2 + ButtonSize.CY div 2
      else Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2 + CaptionHeight + ButtonSize.CY div 2;
    end
    else
    begin
      if ploFlip in FLabelOptions then Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2 + ButtonSize.CX div 2
      else Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2 + CaptionHeight + ButtonSize.CX div 2;
      Result.Y := ClientHeight div 2;
    end;
  end;
end;
  {$endregion TJPPegtopLabelSlideBar.GetLinePoint}


  {$Region ' ------------------ TJPPegtopLabelSlideBar.GetCaptionPoint ---------------- '}
function TJPPegtopLabelSlideBar.GetCaptionPoint: TPoint;
var
  ButtonSize: TSize;
  CaptionHeight: Integer;
begin
  ButtonSize := GetButtonSize;
  Canvas.Font.Assign(Font);
  CaptionHeight := Canvas.TextHeight('0');
  if FOrientation = psoHorizontal then
  begin
    Result.X := ClientWidth div 2;
    if ploFlip in FLabelOptions then Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2 + ButtonSize.CY
    else Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2;
  end
  else
  begin
    if ploFlip in FLabelOptions then Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2 + ButtonSize.CX
    else Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2;
    Result.Y := ClientHeight div 2;
  end;
end;
  {$endregion TJPPegtopLabelSlideBar.GetCaptionPoint}


  {$Region ' --------------- TJPPegtopLabelSlideBar: Setters ------------------ '}
procedure TJPPegtopLabelSlideBar.SetLabelCaption(V: TCaption);
begin
  if FLabelCaption <> V then
  begin
    FLabelCaption := V;
    Invalidate;
  end;
end;

procedure TJPPegtopLabelSlideBar.SetLabelMin(V: TCaption);
begin
  if FLabelMin <> V then
  begin
    FLabelMin := V;
    Invalidate;
  end;
end;

procedure TJPPegtopLabelSlideBar.SetLabelMax(V: TCaption);
begin
  if FLabelMax <> V then
  begin
    FLabelMax := V;
    Invalidate;
  end;
end;

procedure TJPPegtopLabelSlideBar.SetLabelMode(V: TJPPegtopLabelMode);
begin
  if FLabelMode <> V then
  begin
    FLabelMode := V;
    Invalidate;
  end;
end;

procedure TJPPegtopLabelSlideBar.SetLabelParam(V: Double);
begin
  if FLabelParam <> V then
  begin
    FLabelParam := V;
    Invalidate;
  end;
end;

procedure TJPPegtopLabelSlideBar.SetPositionLabel(const Value: TCustomLabel);
begin
  if FPositionLabel = Value then Exit;
  FPositionLabel := Value;
  if Assigned(FPositionLabel) then
  begin
    FPositionLabel.FreeNotification(Self);
    FPositionLabel.Enabled := Self.Enabled;
  end;
  Invalidate;
end;

procedure TJPPegtopLabelSlideBar.SetLabelOptions(V: TJPPegtopLabelOptions);
begin
  if FLabelOptions <> V then
  begin
    FLabelOptions := V;
    Invalidate;
  end;
end;
  {$endregion TJPPegtopLabelSlideBar: Setters}


{$ENDREGION TJPPegtopLabelSlideBar}


{$REGION ' ---------------------------------- TJPPegtopTrackBar ----------------------------------- '}

  {$Region ' -------------------- TJPPegtopTrackBar: Create & Destroy ---------------------- '}
constructor TJPPegtopTrackBar.Create(AOwner: TComponent);
const
  MenuCaption: array [0 .. 4] of String = (PegtopTrackBarReset, '-', PegtopTrackBarEdit, PegtopTrackBarCopy, PegtopTrackBarPaste);
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  if TrackBarPopupMenu = NIL then
  begin
    TrackBarPopupMenu := TPopupMenu.Create(Application);
    for I := Low(MenuCaption) to High(MenuCaption) do
    begin
      MenuItem := TMenuItem.Create(TrackBarPopupMenu);
      MenuItem.Caption := MenuCaption[I];
      MenuItem.Tag := I;
      TrackBarPopupMenu.Items.Add(MenuItem);
    end;
  end;
  FPosition := 0;
  FDefaultPosition := 0;
  inherited;
  LabelCaption := 'Position: <pos>';
  FTagExt := TJppTagExt.Create(Self);
end;

destructor TJPPegtopTrackBar.Destroy;
begin
  FTagExt.Free;
  inherited;
end;
  {$endregion TJPPegtopTrackBar: Create & Destroy}

procedure TJPPegtopTrackBar.Loaded;
begin
  inherited;
  FDefaultPosition := FPosition;
end;

procedure TJPPegtopTrackBar.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJPPegtopTrackBar.PaintTo(const ACanvas: TCanvas);
var
  ButtonPoint: TPoint;
  ButtonSize: TSize;
  ButtonRect: TRect;
  State: TPegtopThemeState;
begin
  inherited;
  if not Enabled then State := ptsDisabled
  else if FMouseDown then State := ptsPushed
  else if FMouseHover then State := ptsHot
  else if Focused then State := ptsFocused
  else State := ptsNormal;
  ButtonSize := GetButtonSize;
  ButtonPoint := GetButtonPoint;
  ButtonRect := Bounds(ButtonPoint.X, ButtonPoint.Y, ButtonSize.CX, ButtonSize.CY);
  DrawButton(ACanvas, ButtonRect, State, Focused);
  DrawButtonGrip(ACanvas, ButtonRect, State);
end;


  {$Region ' ------------------ TJPPegtopTrackBar - Scroll procs ------------------ '}
function TJPPegtopTrackBar.DoStartScroll(const X, Y: Integer): TPoint;
var
  ButtonSize: TSize;
  ButtonPoint: TPoint;
  NewPos: Integer;
begin
  ButtonSize := GetButtonSize;
  ButtonPoint := GetButtonPoint;
  if (X < ButtonPoint.X) or (Y < ButtonPoint.Y) or (X >= ButtonPoint.X + ButtonSize.CX) or (Y >= ButtonPoint.Y + ButtonSize.CY) then
  begin
    // clicked outside the button
    ButtonPoint.X := X - (ButtonSize.CX div 2);
    ButtonPoint.Y := Y - (ButtonSize.CY div 2);
    NewPos := PointToPosition(ButtonPoint.X, ButtonPoint.Y);
    if NewPos <> FPosition then
    begin
      FPosition := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition);
      if Assigned(FOnChange) then FOnChange(Self);
    end;
    ButtonPoint := GetButtonPoint;
  end;
  Result.X := X - ButtonPoint.X;
  Result.Y := Y - ButtonPoint.Y;
end;

procedure TJPPegtopTrackBar.DoScroll(const X, Y: Integer);
var
  NewPos: Integer;
begin
  NewPos := PointToPosition(X, Y);
  if NewPos <> FPosition then
  begin
    FPosition := NewPos;
    Invalidate;
    if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJPPegtopTrackBar.DoEndScroll;
begin
  if Assigned(FOnScroll) then FOnScroll(Self, pscEndScroll, FPosition);
  Invalidate;
end;
  {$endregion TJPPegtopTrackBar - Scroll procs}


  {$Region ' ------------------ TJPPegtopTrackBar.WMEditValue ------------------------ '}
procedure TJPPegtopTrackBar.WMEditValue(var Msg: TMessage);
var
  NumEditForm: TPegtopNumEditForm;
  UpperLeft: TPoint;
  MinVal, MaxVal: Double;
begin
  if not(ploDisableEdit in LabelOptions) then
  begin
    if CanFocus then
    begin
      SetFocus;
      Invalidate;
    end;
    UpperLeft := ClientToScreen(Point(FCaptionRect.Left, FCaptionRect.Top));
    MinVal := PositionToValue(FMin);
    MaxVal := PositionToValue(FMax);
    case FLabelMode of
      plmPos, plmMul, plmShl, plmBin, plmSqr, plmAdd, plmSub:
        begin
          NumEditForm := TPegtopIntEditForm.CreateNew(Application);
          if MinVal <= MaxVal then
          begin // could be inversed (plmSub)
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MinVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MaxVal);
          end
          else
          begin
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MaxVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MinVal);
          end;
          TPegtopIntEdit(NumEditForm.NumEdit).Value := Round(PositionToValue(FPosition));
        end;
      plmDiv, plmShr, plmInv, plmLog, plmExp:
        begin
          NumEditForm := TPegtopFloatEditForm.CreateNew(Application);
          case FLabelMode of
            plmDiv: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(FLabelParam) / Ln(10) + 0.9999);
            plmShr: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(FLabelParam);
            plmInv: if Abs(FMin) > Abs(FMax) then TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMin) / FLabelParam) / Ln(10) + 2.4999)
              else TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMax) / FLabelParam) / Ln(10) + 2.4999);
            plmLog: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 2;
            plmExp: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 0;
          end;
          if MinVal <= MaxVal then
          begin // could be inversed (plmInv)
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMin);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMax);
          end
          else
          begin
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMax);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMin);
          end;
          TPegtopFloatEdit(NumEditForm.NumEdit).Value := PositionToValue(FPosition);
        end;
      else NumEditForm := NIL; // NumEditForm must be initialized
    end;
    if Assigned(NumEditForm) then
    begin
      NumEditForm.NumEdit.Width := Canvas.TextWidth(StringOfChar('0', NumEditForm.NumEdit.GetMaxLength)) + 8;
      NumEditForm.NumEdit.SelectAll;
      NumEditForm.OnEditChange := NumEditChange;
      NumEditForm.OnClose := NumEditClose;
      NumEditForm.Left := UpperLeft.X + ((FPositionCaptionRect.Left + FPositionCaptionRect.Right) div 2) - (NumEditForm.NumEdit.Width div 2);
      NumEditForm.Top := UpperLeft.Y + FPositionCaptionRect.Bottom - NumEditForm.NumEdit.Height + 4;
      NumEditForm.Show;
    end;
    // SendMessage (ParentWindow, WM_NCACTIVATE, 1, 0);
  end;
end;
  {$endregion TJPPegtopTrackBar.WMEditValue}


  {$Region ' ------------------ TJPPegtopTrackBar.ExtraMouse - Down & Move -------------------- '}
procedure TJPPegtopTrackBar.ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not(ploDisableEdit in LabelOptions)) and (ploVisible in LabelOptions) and (X >= FCaptionRect.Left + FPositionCaptionRect.Left) and
    (X < FCaptionRect.Left + FPositionCaptionRect.Right) and (Y >= FCaptionRect.Top + FPositionCaptionRect.Top) and
    (Y < FCaptionRect.Top + FPositionCaptionRect.Bottom) and HasExtent(FPositionCaptionRect) then
  begin
    EditValue;
  end;
  inherited;
end;

procedure TJPPegtopTrackBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonSize: TSize;
  ButtonPoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  ButtonPoint := GetButtonPoint;
  if (X >= ButtonPoint.X) and (Y >= ButtonPoint.Y) and (X < ButtonPoint.X + ButtonSize.CX) and (Y < ButtonPoint.Y + ButtonSize.CY) then
  begin
    if not FMouseHover then
    begin
      FMouseHover := True;
      Invalidate;
    end;
  end
  else
  begin
    if FMouseHover then
    begin
      FMouseHover := False;
      Invalidate;
    end;
  end;
  if (not(ploDisableEdit in LabelOptions)) and (ploVisible in LabelOptions) and (X >= FCaptionRect.Left + FPositionCaptionRect.Left) and
    (X < FCaptionRect.Left + FPositionCaptionRect.Right) and (Y >= FCaptionRect.Top + FPositionCaptionRect.Top) and
    (Y < FCaptionRect.Top + FPositionCaptionRect.Bottom) and HasExtent(FPositionCaptionRect) then
  begin
    ChangeCursor(crIBeam);
  end
  else
  begin
    ChangeCursor(FCursor);
  end;
  inherited;
end;
  {$endregion TJPPegtopTrackBar.ExtraMouse - Down & Move}


  {$Region ' ------------------ TJPPegtopTrackBar - PopupMenu & NumEdit ------------------ '}
procedure TJPPegtopTrackBar.NumEditChange(Sender: TObject);
begin
  if Sender is TPegtopIntEditForm then ApplyPosition(ValueToPosition(TPegtopIntEdit(TPegtopIntEditForm(Sender).NumEdit).Value))
  else if Sender is TPegtopFloatEditForm then ApplyPosition(ValueToPosition(TPegtopFloatEdit(TPegtopFloatEditForm(Sender).NumEdit).Value));
end;

procedure TJPPegtopTrackBar.NumEditClose(Sender: TObject; var Action: TCloseAction);
begin
  if CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
end;

procedure TJPPegtopTrackBar.PopupContextMenu(const X, Y: Integer);
var
  I: Integer;
begin
  TrackBarPopupMenu.Items[0].Enabled := FPosition <> FDefaultPosition;
  TrackBarPopupMenu.Items[2].Visible := not(ploDisableEdit in LabelOptions);
  TrackBarPopupMenu.Items[2].Enabled := HasExtent(FPositionCaptionRect);
  TrackBarPopupMenu.Items[3].Visible := not(ploDisableCopy in LabelOptions);
  TrackBarPopupMenu.Items[4].Visible := not(ploDisablePaste in LabelOptions);
  TrackBarPopupMenu.Items[4].Enabled := Clipboard.HasFormat(CF_TEXT) and IsValidFloat(Clipboard.AsText);
  for I := 0 to TrackBarPopupMenu.Items.Count - 1 do TrackBarPopupMenu.Items[I].OnClick := MenuItemClick;
  TrackBarPopupMenu.PopupComponent := Self;
  TrackBarPopupMenu.Popup(X, Y);
end;

procedure TJPPegtopTrackBar.MenuItemClick(Sender: TObject);
begin
  if Sender is TComponent then
  begin
    case TComponent(Sender).Tag of
      0: ResetToDefault;
      2: EditValue;
      3: CopyValue;
      4: PasteValue;
    end;
  end;
end;

procedure TJPPegtopTrackBar.EditValue;
begin
  PostMessage(Handle, WM_PEGTOPSLIDEBAR_EDITVALUE, 0, 0);
end;

procedure TJPPegtopTrackBar.CopyValue;
begin
  Clipboard.AsText := PositionToString(FPosition);
end;

procedure TJPPegtopTrackBar.PasteValue;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    try
      ApplyPosition(ValueToPosition(StrToFloat(Clipboard.AsText)));
    except
    end;
end;
  {$endregion TJPPegtopTrackBar - PopupMenu & NumEdit}


  {$Region ' ----------------- TJPPegtopTrackBar - Position: Change, Apply, Reset --------------- '}
procedure TJPPegtopTrackBar.ApplyPosition(const P: Integer);
var
  OldPos: Integer;
begin
  OldPos := FPosition;
  SetPosition(P);
  if FPosition <> OldPos then
  begin
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJPPegtopTrackBar.ChangePosition(const Delta: Integer);
var
  OldPos: Integer;
begin
  OldPos := FPosition;
  SetPosition(FPosition + Delta);
  if FPosition <> OldPos then
  begin
    if Assigned(FOnScroll) then
    begin
      if Delta < 0 then FOnScroll(Self, pscLineUp, FPosition)
      else FOnScroll(Self, pscLineDown, FPosition);
    end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJPPegtopTrackBar.ResetToDefault;
begin
  if FPosition <> FDefaultPosition then
  begin
    FPosition := FDefaultPosition;
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition);
    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;
  {$endregion TJPPegtopTrackBar - Position: Change, Apply, Reset}


function TJPPegtopTrackBar.TransformCaption(const S: String): String;
var
  T: String;
  E: TSize;
  P: Integer;
begin
  Result := S;
  T := PositionToString(FPosition);
  P := Pos('<pos>', AnsiLowerCase(Result));
  if P > 0 then
  begin
    E := Canvas.TextExtent(T);
    FPositionCaptionRect := Bounds(Canvas.TextWidth(Copy(Result, 1, P - 1)), 0, E.CX, E.CY);
    Result := StringReplace(Result, '<pos>', T, [rfReplaceAll, rfIgnoreCase]);
  end
  else FPositionCaptionRect := Rect(0, 0, 0, 0);
end;

function TJPPegtopTrackBar.GetButtonSize: TSize;
begin
  if Orientation = psoHorizontal then
  begin
    Result.CX := FButtonParams.Width; // 25;
    Result.CY := FButtonParams.Height; // 17;
  end
  else
  begin
    Result.CX := FButtonParams.Height; // 17;
    Result.CY := FButtonParams.Width; // 25;
  end;
end;

  {$Region ' ----------------- TJPPegtopTrackBar.GetButtonPoint ------------------ '}
function TJPPegtopTrackBar.GetButtonPoint: TPoint;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if Orientation = psoHorizontal then
  begin
    if FMax = FMin then
    begin
      Result.X := 0;
    end
    else
    begin
      Result.X := MulDiv(FPosition - FMin, ClientWidth - ButtonSize.CX, FMax - FMin);
    end;
    Result.Y := LinePoint.Y - ButtonSize.CY div 2;
  end
  else
  begin
    if FMax = FMin then
    begin
      Result.Y := 0;
    end
    else
    begin
      Result.Y := MulDiv(FMax - FPosition, ClientHeight - ButtonSize.CY, FMax - FMin);
    end;
    Result.X := LinePoint.X - ButtonSize.CX div 2;
  end;
end;
  {$endregion TJPPegtopTrackBar.GetButtonPoint}


  {$Region ' ------------------ TJPPegtopTrackBar: Position related ------------------- '}
procedure TJPPegtopTrackBar.ValidatePosition;
begin
  if FPosition < Min then FPosition := Min
  else if FPosition > Max then FPosition := Max;
  if FDefaultPosition < Min then FDefaultPosition := Min
  else if FDefaultPosition > Max then FDefaultPosition := Max;
end;

function TJPPegtopTrackBar.PointToPosition(const X, Y: Integer): Integer;
var
  ButtonSize: TSize;
begin
  if FMax = FMin then
  begin
    Result := FMin;
  end
  else
  begin
    ButtonSize := GetButtonSize;
    if FOrientation = psoHorizontal then
        Result := ((X * (FMax - FMin) div FSmallChange + ((ClientWidth - ButtonSize.CX) div 2)) div (ClientWidth - ButtonSize.CX)) * FSmallChange + FMin
    else Result := FMax - ((Y * (FMax - FMin) div FSmallChange + ((ClientHeight - ButtonSize.CY) div 2)) div (ClientHeight - ButtonSize.CY)) * FSmallChange;
    if Result < FMin then Result := FMin
    else if Result > FMax then Result := FMax;
  end;
end;

procedure TJPPegtopTrackBar.SetPosition(V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FPosition <> V then
  begin
    FPosition := V;
    Invalidate;
  end;
end;

procedure TJPPegtopTrackBar.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJPPegtopTrackBar.SetDefaultPosition(V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FDefaultPosition <> V then
  begin
    FDefaultPosition := V;
    Invalidate;
  end;
end;

function TJPPegtopTrackBar.GetValue: Double;
begin
  Result := PositionToValue(FPosition);
end;

procedure TJPPegtopTrackBar.SetValue(V: Double);
begin
  SetPosition(ValueToPosition(V));
end;
  {$endregion TJPPegtopTrackBar: Position related}


{$ENDREGION TJPPegtopTrackBar}


{$REGION ' ------------------------------------ TJPPegtopColorTrackBar ------------------------------------- '}

constructor TJPPegtopColorTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorRect := TJPPegtopButtonColorRect.Create;
  FColorRect.OnChange := PropsChanged;
  FColorRect.Margins.OnChange := PropsChanged;
end;

destructor TJPPegtopColorTrackBar.Destroy;
begin
  FreeAndNil(FColorRect);
  inherited;
end;


  {$Region ' ------------------ TJPPegtopColorTrackBar.DrawButtonGrip (override) --------------------- '}
procedure TJPPegtopColorTrackBar.DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState);

  procedure BlendPixel(const Canvas: TCanvas; const X, Y: Integer; const Color: TColor; const Alpha: Integer);
  var
    C: TColor;
    R, G, B: Integer;
  begin
    C := Canvas.Pixels[X, Y];
    R := (C and $FF) + ((Color and $FF) - (C and $FF)) * Alpha div 256;
    G := (C shr 8 and $FF) + ((Color shr 8 and $FF) - (C shr 8 and $FF)) * Alpha div 256;
    B := (C shr 16 and $FF) + ((Color shr 16 and $FF) - (C shr 16 and $FF)) * Alpha div 256;
    Canvas.Pixels[X, Y] := R or (G shl 8) or (B shl 16);
  end;

var
  //C: TColor;
  clBg, clBorder: TColor;
  CRect: TRect;
begin

  // TPegtopThemeState = (ptsNormal, ptsHot, ptsPushed, ptsDisabled, ptsFocused);
//  if Enabled then
//  begin
    //C := ColorToRGB(FButtonColor);
//    ACanvas.Brush.Style := bsSolid;
//    ACanvas.Brush.Color := C;
//    ACanvas.FillRect(Rect(ARect.Left + 7, ARect.Top + 6, ARect.Right - 7, ARect.Top + 7));
//    ACanvas.FillRect(Rect(ARect.Left + 6, ARect.Top + 7, ARect.Right - 6, ARect.Bottom - 7));
//    ACanvas.FillRect(Rect(ARect.Left + 7, ARect.Bottom - 7, ARect.Right - 7, ARect.Bottom - 6));
//    BlendPixel(ACanvas, ARect.Left + 6, ARect.Top + 6, C, 128);
//    BlendPixel(ACanvas, ARect.Right - 7, ARect.Top + 6, C, 128);
//    BlendPixel(ACanvas, ARect.Left + 6, ARect.Bottom - 7, C, 128);
//    BlendPixel(ACanvas, ARect.Right - 7, ARect.Bottom - 7, C, 128);
    with Canvas do
    begin

      CRect := ARect;

      InflateRectEx(CRect, FColorRect.Margins.Left, FColorRect.Margins.Top, -FColorRect.Margins.Right, -FColorRect.Margins.Bottom);

      clBg := FColorRect.NormalBgColor;
      clBorder := FColorRect.NormalBorderColor;

      case AState of
        ptsHot:
          begin
            clBg := FColorRect.HotBgColor;
            clBorder := FColorRect.HotBorderColor;
          end;
        ptsPushed:
          begin
            clBg := FColorRect.PushedBgColor;
            clBorder := FColorRect.PushedBorderColor;
          end;
        ptsDisabled:
          begin
            clBg := FColorRect.DisabledBgColor;
            clBorder := FColorRect.DisabledBorderColor;
          end;
        ptsFocused:
          begin
            clBg := FColorRect.FocusedBgColor;
            clBorder := FColorRect.FocusedBorderColor;
          end;
      end;

      Brush.Color := clBg;
      Brush.Style := bsSolid;
      Pen.Style := psSolid;
      Pen.Color := clBorder;

      Rectangle(CRect);

    end;  // with Canvas

//  end;

  if FButtonParams.DrawGrip then inherited DrawButtonGrip(ACanvas, ARect, AState);
end;
  {$endregion TJPPegtopColorTrackBar.DrawButtonGrip}


procedure TJPPegtopColorTrackBar.SetColorRect(const Value: TJPPegtopButtonColorRect);
begin
  FColorRect := Value;
  FColorRect.OnChange := PropsChanged;
  FColorRect.Margins.OnChange := PropsChanged;
  //Invalidate;
  PropsChanged(Self);
end;


{$ENDREGION TJPPegtopColorTrackBar}


{$REGION ' ------------------------------- TJPPegtopRangeBar ----------------------------------- '}


  {$Region ' ---------------- TJPPegtopRangeBar: Create & Loaded ----------------- '}
constructor TJPPegtopRangeBar.Create(AOwner: TComponent);
const
  MenuCaption: array [0 .. 9] of String = (
    PegtopRangeBarReset, PegtopRangeBarConstrained, '-', PegtopRangeBarEditMin, PegtopRangeBarCopyMin,
    PegtopRangeBarPasteMin, '-', PegtopRangeBarEditMax, PegtopRangeBarCopyMax, PegtopRangeBarPasteMax
  );
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  if RangeBarPopupMenu = NIL then
  begin
    RangeBarPopupMenu := TPopupMenu.Create(Application);
    for I := Low(MenuCaption) to High(MenuCaption) do
    begin
      MenuItem := TMenuItem.Create(RangeBarPopupMenu);
      MenuItem.Caption := MenuCaption[I];
      MenuItem.Tag := I;
      RangeBarPopupMenu.Items.Add(MenuItem);
    end;
  end;
  InitBitmaps;
  FPosition[0] := 0;
  FPosition[1] := 0;
  FDefaultPosition[0] := 0;
  FDefaultPosition[1] := 0;
  FButtonFocus := 0;
  FMouseHover := -1;
  inherited;
  LabelCaption := 'Position: <min> - <max>';
  FTagExt := TJppTagExt.Create(Self);
  FDrawTriangleWhenDisabled := True;
end;

destructor TJPPegtopRangeBar.Destroy;
begin
  FTagExt.Free;
  inherited;
end;

procedure TJPPegtopRangeBar.Loaded;
begin
  inherited;
  FDefaultPosition[0] := FPosition[0];
  FDefaultPosition[1] := FPosition[1];
end;
  {$endregion TJPPegtopRangeBar: Create & Loaded}


procedure TJPPegtopRangeBar.CMWantSpecialKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_TAB: if (FButtonFocus = 0) xor ((GetKeyState(VK_SHIFT) and 128) <> 0) then Msg.Result := 1
      else inherited;
    else inherited;
  end;
end;

procedure TJPPegtopRangeBar.DoEnter;
begin
  if (GetKeyState(VK_TAB) and 128) <> 0 then
  begin
    if (GetKeyState(VK_SHIFT) and 128) <> 0 then FButtonFocus := 1
    else FButtonFocus := 0;
  end;
  inherited;
end;

  {$Region ' -------------------- TJPPegtopRangeBar.PaintTo ---------------------- '}
procedure TJPPegtopRangeBar.PaintTo(const ACanvas: TCanvas);
var
  ButtonPoint: array [0 .. 1] of TPoint;
  ButtonSize: TSize;
  ButtonRect: TRect;
  State: TPegtopThemeState;
  BitmapIndex: Integer;
begin
  inherited;
  ButtonSize := GetButtonSize;
  if FConstrained then
  begin
    // draw min button:
    if not Enabled then State := ptsDisabled
    else if FMouseDown then State := ptsPushed
    else if FMouseHover = 0 then State := ptsHot
    else if Focused then State := ptsFocused
    else State := ptsNormal;
    ButtonPoint[0] := GetButtonPoint(0);
    ButtonPoint[1] := GetButtonPoint(1);
    ButtonRect := Rect(ButtonPoint[0].X, ButtonPoint[0].Y, ButtonPoint[1].X + ButtonSize.CX, ButtonPoint[1].Y + ButtonSize.CY);
    DrawButton(ACanvas, ButtonRect, State, Focused);
    if Enabled or FDrawTriangleWhenDisabled then
    begin
      if FOrientation = psoHorizontal then BitmapIndex := 1
      else BitmapIndex := 2;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[0].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width + 1) div 2,
        ButtonPoint[0].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1) div 2, Bitmaps[BitmapIndex]);
      if FOrientation = psoHorizontal then BitmapIndex := 0
      else BitmapIndex := 3;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[1].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width) div 2, ButtonPoint[1].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1)
        div 2, Bitmaps[BitmapIndex]);
    end;
  end
  else
  begin
    // draw min button:
    if not Enabled then State := ptsDisabled
    else if (FButtonFocus = 0) and FMouseDown then State := ptsPushed
    else if FMouseHover = 0 then State := ptsHot
    else if (FButtonFocus = 0) and Focused then State := ptsFocused
    else State := ptsNormal;
    ButtonPoint[0] := GetButtonPoint(0);
    ButtonRect := Bounds(ButtonPoint[0].X, ButtonPoint[0].Y, ButtonSize.CX, ButtonSize.CY);
    DrawButton(ACanvas, ButtonRect, State, (FButtonFocus = 0) and Focused);
    if Enabled or FDrawTriangleWhenDisabled then
    begin
      if FOrientation = psoHorizontal then BitmapIndex := 1
      else BitmapIndex := 2;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[0].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width + 1) div 2,
        ButtonPoint[0].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1) div 2, Bitmaps[BitmapIndex]);
    end;
    // draw max button:
    if not Enabled then State := ptsDisabled
    else if (FButtonFocus = 1) and FMouseDown then State := ptsPushed
    else if FMouseHover = 1 then State := ptsHot
    else if (FButtonFocus = 1) and Focused then State := ptsFocused
    else State := ptsNormal;
    ButtonPoint[1] := GetButtonPoint(1);
    ButtonRect := Bounds(ButtonPoint[1].X, ButtonPoint[1].Y, ButtonSize.CX, ButtonSize.CY);
    DrawButton(ACanvas, ButtonRect, State, (FButtonFocus = 1) and Focused);
    if Enabled or FDrawTriangleWhenDisabled then
    begin
      if FOrientation = psoHorizontal then BitmapIndex := 0
      else BitmapIndex := 3;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[1].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width) div 2, ButtonPoint[1].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1)
        div 2, Bitmaps[BitmapIndex]);
    end;
  end;
end;
  {$endregion TJPPegtopRangeBar.PaintTo}

procedure TJPPegtopRangeBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_TAB) then
  begin
    FButtonFocus := FButtonFocus xor 1;
    Invalidate;
  end;
  inherited;
end;

procedure TJPPegtopRangeBar.ValidatePosition;
var
  I: Integer;
begin
  for I := 0 to 1 do
  begin
    if FPosition[I] < Min then FPosition[I] := Min
    else if FPosition[I] > Max then FPosition[I] := Max;
    if FDefaultPosition[I] < Min then FDefaultPosition[I] := Min
    else if FDefaultPosition[I] > Max then FDefaultPosition[I] := Max;
  end;
end;

  {$Region ' ----------------- TJPPegtopRangeBar: Scrolling ------------------- '}
function TJPPegtopRangeBar.DoStartScroll(const X, Y: Integer): TPoint;
var
  ButtonSize: TSize;
  ButtonPoint: array [0 .. 1] of TPoint;
  NewPos: Integer;
begin
  ButtonSize := GetButtonSize;
  ButtonPoint[0] := GetButtonPoint(0);
  ButtonPoint[1] := GetButtonPoint(1);
  // which button is nearer?
  if Abs(X - (ButtonPoint[1].X + ButtonSize.CX div 2) + Y - (ButtonPoint[1].Y + ButtonSize.CY div 2)) <
    Abs(X - (ButtonPoint[0].X + ButtonSize.CX div 2) + Y - (ButtonPoint[0].Y + ButtonSize.CY div 2)) then FButtonFocus := 1
  else FButtonFocus := 0;
  if FConstrained then
  begin
    if (X < ButtonPoint[0].X) or (Y < ButtonPoint[1].Y) or (X >= ButtonPoint[1].X + ButtonSize.CX) or (Y >= ButtonPoint[0].Y + ButtonSize.CY) then
    begin
      // clicked outside the button
      ButtonPoint[FButtonFocus].X := X - (ButtonSize.CX div 2);
      ButtonPoint[FButtonFocus].Y := Y - (ButtonSize.CY div 2);
      NewPos := PointToPosition(FButtonFocus, ButtonPoint[FButtonFocus].X, ButtonPoint[FButtonFocus].Y);
      if NewPos <> FPosition[FButtonFocus] then
      begin
        FPosition[FButtonFocus xor 1] := FPosition[FButtonFocus xor 1] + NewPos - FPosition[FButtonFocus];
        FPosition[FButtonFocus] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[0]);
        if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[1]);
        if Assigned(FOnChange) then FOnChange(Self);
      end;
      ButtonPoint[FButtonFocus] := GetButtonPoint(FButtonFocus);
    end;
  end
  else
  begin
    if (X < ButtonPoint[FButtonFocus].X) or (Y < ButtonPoint[FButtonFocus].Y) or (X >= ButtonPoint[FButtonFocus].X + ButtonSize.CX) or
      (Y >= ButtonPoint[FButtonFocus].Y + ButtonSize.CY) then
    begin
      // clicked outside the button
      ButtonPoint[FButtonFocus].X := X - (ButtonSize.CX div 2);
      ButtonPoint[FButtonFocus].Y := Y - (ButtonSize.CY div 2);
      NewPos := PointToPosition(FButtonFocus, ButtonPoint[FButtonFocus].X, ButtonPoint[FButtonFocus].Y);
      if NewPos <> FPosition[FButtonFocus] then
      begin
        FPosition[FButtonFocus] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[FButtonFocus]);
        if Assigned(FOnChange) then FOnChange(Self);
      end;
      ButtonPoint[FButtonFocus] := GetButtonPoint(FButtonFocus);
    end;
  end;
  Result.X := X - ButtonPoint[FButtonFocus].X;
  Result.Y := Y - ButtonPoint[FButtonFocus].Y;
end;

procedure TJPPegtopRangeBar.DoScroll(const X, Y: Integer);
var
  NewPos: Integer;
begin
  NewPos := PointToPosition(FButtonFocus, X, Y);
  if NewPos <> FPosition[FButtonFocus] then
  begin
    if FConstrained then
    begin
      FPosition[FButtonFocus xor 1] := FPosition[FButtonFocus xor 1] + NewPos - FPosition[FButtonFocus];
      FPosition[FButtonFocus] := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[0]);
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[1]);
      if Assigned(FOnChange) then FOnChange(Self);
    end
    else
    begin
      FPosition[FButtonFocus] := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[FButtonFocus]);
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

procedure TJPPegtopRangeBar.DoEndScroll;
begin
  if Assigned(FOnScroll) then FOnScroll(Self, pscEndScroll, FPosition[FButtonFocus]);
  Invalidate;
end;
  {$endregion TJPPegtopRangeBar: Scrolling}


  {$Region ' ------------------ TJPPegtopRangeBar.WMEditValue ------------------ '}
procedure TJPPegtopRangeBar.WMEditValue(var Msg: TMessage);
var
  NumEditForm: TPegtopNumEditForm;
  UpperLeft: TPoint;
  MinVal, MaxVal: Double;
begin
  if not(ploDisableEdit in LabelOptions) then
  begin
    if Msg.WParam in [0, 1] then
    begin
      FButtonFocus := Msg.WParam;
    end;
    if CanFocus then
    begin
      SetFocus;
      Invalidate;
    end;
    UpperLeft := ClientToScreen(Point(FCaptionRect.Left, FCaptionRect.Top));
    MinVal := PositionToValue(FMin);
    MaxVal := PositionToValue(FMax);
    case FLabelMode of
      plmPos, plmMul, plmShl, plmBin, plmSqr, plmAdd, plmSub:
        begin
          NumEditForm := TPegtopIntEditForm.CreateNew(Application);
          if MinVal <= MaxVal then
          begin // could be inversed (plmSub)
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MinVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MaxVal);
          end
          else
          begin
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MaxVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MinVal);
          end;
          TPegtopIntEdit(NumEditForm.NumEdit).Value := Round(PositionToValue(FPosition[FButtonFocus]));
        end;
      plmDiv, plmShr, plmInv, plmLog, plmExp:
        begin
          NumEditForm := TPegtopFloatEditForm.CreateNew(Application);
          case FLabelMode of
            plmDiv: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(FLabelParam) / Ln(10) + 0.9999);
            plmShr: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(FLabelParam);
            plmInv: if Abs(FMin) > Abs(FMax) then TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMin) / FLabelParam) / Ln(10) + 2.4999)
              else TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMax) / FLabelParam) / Ln(10) + 2.4999);
            plmLog: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 2;
            plmExp: TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 0;
          end;
          if MinVal <= MaxVal then
          begin // could be inversed (plmInv)
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMin);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMax);
          End
          Else
          Begin
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMax);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMin);
          End;
          TPegtopFloatEdit(NumEditForm.NumEdit).Value := PositionToValue(FPosition[FButtonFocus]);
        end;
      else NumEditForm := NIL; // NumEditForm must be initialized
    end;
    if Assigned(NumEditForm) then
    begin
      NumEditForm.NumEdit.Width := Canvas.TextWidth(StringOfChar('0', NumEditForm.NumEdit.GetMaxLength)) + 8;
      NumEditForm.NumEdit.SelectAll;
      NumEditForm.OnEditChange := NumEditChange;
      NumEditForm.OnClose := NumEditClose;
      NumEditForm.Left := UpperLeft.X + ((FPositionCaptionRect[FButtonFocus].Left + FPositionCaptionRect[FButtonFocus].Right) div 2) -
        (NumEditForm.NumEdit.Width div 2);
      NumEditForm.Top := UpperLeft.Y + FPositionCaptionRect[FButtonFocus].Bottom - NumEditForm.NumEdit.Height + 4;
      NumEditForm.Show;
    end;
    // SendMessage (ParentWindow, WM_NCACTIVATE, 1, 0);
  end;
end;
  {$endregion TJPPegtopRangeBar.WMEditValue}

procedure TJPPegtopRangeBar.NumEditChange(Sender: TObject);
begin
  if Sender is TPegtopIntEditForm then ApplyPosition(ValueToPosition(TPegtopIntEdit(TPegtopIntEditForm(Sender).NumEdit).Value))
  else if Sender is TPegtopFloatEditForm then ApplyPosition(ValueToPosition(TPegtopFloatEdit(TPegtopFloatEditForm(Sender).NumEdit).Value));
end;

procedure TJPPegtopRangeBar.NumEditClose(Sender: TObject; var Action: TCloseAction);
begin
  if CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
end;

  {$Region ' ----------------- TJPPegtopRangeBar: ExtraMouse Down & Move ------------------- '}
procedure TJPPegtopRangeBar.ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not(ploDisableEdit in LabelOptions)) and (ploVisible in LabelOptions) and (X >= FCaptionRect.Left + FPositionCaptionRect[0].Left) and
    (X < FCaptionRect.Left + FPositionCaptionRect[0].Right) and (Y >= FCaptionRect.Top + FPositionCaptionRect[0].Top) and
    (Y < FCaptionRect.Top + FPositionCaptionRect[0].Bottom) and HasExtent(FPositionCaptionRect[0]) then
  begin
    EditValueMin;
  end
  else if (not(ploDisableEdit in LabelOptions)) and (ploVisible in LabelOptions) and (X >= FCaptionRect.Left + FPositionCaptionRect[1].Left) and
    (X < FCaptionRect.Left + FPositionCaptionRect[1].Right) and (Y >= FCaptionRect.Top + FPositionCaptionRect[1].Top) and
    (Y < FCaptionRect.Top + FPositionCaptionRect[1].Bottom) and HasExtent(FPositionCaptionRect[1]) then
  begin
    EditValueMax;
  end;
  inherited;
end;

procedure TJPPegtopRangeBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonSize: TSize;
  ButtonPoint: array [0 .. 1] of TPoint;
  NewHover: Integer;
begin
  NewHover := -1;
  ButtonSize := GetButtonSize;
  ButtonPoint[0] := GetButtonPoint(0);
  ButtonPoint[1] := GetButtonPoint(1);
  if FConstrained then
  begin
    if (X >= ButtonPoint[0].X) and (Y >= ButtonPoint[0].Y) and (X < ButtonPoint[1].X + ButtonSize.CX) and (Y < ButtonPoint[1].Y + ButtonSize.CY) then
        NewHover := 0;
  end
  else
  begin
    if (X >= ButtonPoint[0].X) and (Y >= ButtonPoint[0].Y) and (X < ButtonPoint[0].X + ButtonSize.CX) and (Y < ButtonPoint[0].Y + ButtonSize.CY) then
        NewHover := 0
    else if (X >= ButtonPoint[1].X) and (Y >= ButtonPoint[1].Y) and (X < ButtonPoint[1].X + ButtonSize.CX) and (Y < ButtonPoint[1].Y + ButtonSize.CY) then
        NewHover := 1;
  end;
  if FMouseHover <> NewHover then
  begin
    FMouseHover := NewHover;
    Invalidate;
  end;
  if (not(ploDisableEdit in LabelOptions)) and (ploVisible in LabelOptions) and (X >= FCaptionRect.Left + FPositionCaptionRect[0].Left) and
    (X < FCaptionRect.Left + FPositionCaptionRect[0].Right) and (Y >= FCaptionRect.Top + FPositionCaptionRect[0].Top) and
    (Y < FCaptionRect.Top + FPositionCaptionRect[0].Bottom) and HasExtent(FPositionCaptionRect[0]) then
  begin
    ChangeCursor(crIBeam);
  end
  else if (not(ploDisableEdit in LabelOptions)) and (ploVisible in LabelOptions) and (X >= FCaptionRect.Left + FPositionCaptionRect[1].Left) and
    (X < FCaptionRect.Left + FPositionCaptionRect[1].Right) and (Y >= FCaptionRect.Top + FPositionCaptionRect[1].Top) and
    (Y < FCaptionRect.Top + FPositionCaptionRect[1].Bottom) and HasExtent(FPositionCaptionRect[1]) then
  begin
    ChangeCursor(crIBeam);
  end
  else
  begin
    ChangeCursor(FCursor);
  end;
  inherited;
end;
  {$endregion TJPPegtopRangeBar: ExtraMouse Down & Move}


procedure TJPPegtopRangeBar.PopupContextMenu(const X, Y: Integer);
var
  I: Integer;
begin
  RangeBarPopupMenu.Items[0].Enabled := (FPosition[0] <> FDefaultPosition[0]) or (FPosition[1] <> FDefaultPosition[1]);
  RangeBarPopupMenu.Items[1].Visible := not(proDisableConstrain in FRangeOptions);
  RangeBarPopupMenu.Items[1].Enabled := FConstrained or (FPosition[0] > FMin) or (FPosition[1] < FMax);
  RangeBarPopupMenu.Items[1].Checked := FConstrained;
  RangeBarPopupMenu.Items[3].Visible := not(ploDisableEdit in LabelOptions);
  RangeBarPopupMenu.Items[3].Enabled := HasExtent(FPositionCaptionRect[0]);
  RangeBarPopupMenu.Items[4].Visible := not(ploDisableCopy in LabelOptions);
  RangeBarPopupMenu.Items[5].Visible := not(ploDisablePaste in LabelOptions);
  RangeBarPopupMenu.Items[5].Enabled := Clipboard.HasFormat(CF_TEXT) and IsValidFloat(Clipboard.AsText);
  RangeBarPopupMenu.Items[7].Visible := not(ploDisableEdit in LabelOptions);
  RangeBarPopupMenu.Items[7].Enabled := HasExtent(FPositionCaptionRect[1]);
  RangeBarPopupMenu.Items[8].Visible := not(ploDisableCopy in LabelOptions);
  RangeBarPopupMenu.Items[9].Visible := not(ploDisablePaste in LabelOptions);
  RangeBarPopupMenu.Items[9].Enabled := Clipboard.HasFormat(CF_TEXT) and IsValidFloat(Clipboard.AsText);
  for I := 0 to RangeBarPopupMenu.Items.Count - 1 do RangeBarPopupMenu.Items[I].OnClick := MenuItemClick;
  RangeBarPopupMenu.PopupComponent := Self;
  RangeBarPopupMenu.Popup(X, Y);
end;

procedure TJPPegtopRangeBar.EditValueMin;
begin
  PostMessage(Handle, WM_PEGTOPSLIDEBAR_EDITVALUE, 0, 0);
end;

procedure TJPPegtopRangeBar.EditValueMax;
begin
  PostMessage(Handle, WM_PEGTOPSLIDEBAR_EDITVALUE, 1, 0);
end;

procedure TJPPegtopRangeBar.CopyValueMin;
begin
  Clipboard.AsText := PositionToString(FPosition[0]);
end;

procedure TJPPegtopRangeBar.CopyValueMax;
begin
  Clipboard.AsText := PositionToString(FPosition[1]);
end;

procedure TJPPegtopRangeBar.PasteValueMin;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    try
      FButtonFocus := 0;
      ApplyPosition(ValueToPosition(StrToFloat(Clipboard.AsText)));
    except
    end;
end;

procedure TJPPegtopRangeBar.PasteValueMax;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    try
      FButtonFocus := 1;
      ApplyPosition(ValueToPosition(StrToFloat(Clipboard.AsText)));
    except
    end;
end;

procedure TJPPegtopRangeBar.ApplyPosition(const P: Integer);
var
  NewPos: Integer;
begin
  NewPos := P;
  if NewPos < Min then NewPos := Min
  else if NewPos > Max then NewPos := Max
  else if (FButtonFocus = 0) and (NewPos > FPosition[1]) then NewPos := FPosition[1]
  else if (FButtonFocus = 1) and (NewPos < FPosition[0]) then NewPos := FPosition[0];
  if FPosition[FButtonFocus] <> NewPos then
  begin
    FPosition[FButtonFocus] := NewPos;
    Invalidate;
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition[FButtonFocus]);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

  {$Region ' -------------------- TJPPegtopRangeBar.ChangePosition ----------------------- '}
procedure TJPPegtopRangeBar.ChangePosition(const Delta: Integer);
var
  NewPos: Integer;
begin
  if FConstrained then
  begin
    if Delta < 0 then
    begin
      NewPos := FPosition[0] + Delta;
      if NewPos < Min then NewPos := Min;
      if FPosition[0] <> NewPos then
      begin
        FPosition[1] := FPosition[1] + NewPos - FPosition[0];
        FPosition[0] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then
        begin
          if Delta < 0 then
          begin
            FOnScroll(Self, pscLineUp, FPosition[0]);
            FOnScroll(Self, pscLineUp, FPosition[1]);
          end
          else
          begin
            FOnScroll(Self, pscLineDown, FPosition[0]);
            FOnScroll(Self, pscLineDown, FPosition[1]);
          end;
        end;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    end
    else
    begin
      NewPos := FPosition[1] + Delta;
      if NewPos > Max then NewPos := Max;
      if FPosition[1] <> NewPos then
      begin
        FPosition[0] := FPosition[0] + NewPos - FPosition[1];
        FPosition[1] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then
        begin
          if Delta < 0 then
          begin
            FOnScroll(Self, pscLineUp, FPosition[0]);
            FOnScroll(Self, pscLineUp, FPosition[1]);
          end
          else
          begin
            FOnScroll(Self, pscLineDown, FPosition[0]);
            FOnScroll(Self, pscLineDown, FPosition[1]);
          end;
        end;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    end;
  end
  else
  begin
    NewPos := FPosition[FButtonFocus] + Delta;
    if NewPos < Min then NewPos := Min
    else if NewPos > Max then NewPos := Max
    else if (FButtonFocus = 0) and (NewPos > FPosition[1]) then NewPos := FPosition[1]
    else if (FButtonFocus = 1) and (NewPos < FPosition[0]) then NewPos := FPosition[0];
    if FPosition[FButtonFocus] <> NewPos then
    begin
      FPosition[FButtonFocus] := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then
      begin
        if Delta < 0 then FOnScroll(Self, pscLineUp, FPosition[FButtonFocus])
        else FOnScroll(Self, pscLineDown, FPosition[FButtonFocus]);
      end;
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;
  {$endregion TJPPegtopRangeBar.ChangePosition}

procedure TJPPegtopRangeBar.ResetToDefault;
begin
  if (FPosition[0] <> FDefaultPosition[0]) or (FPosition[1] <> FDefaultPosition[1]) then
  begin
    FPosition[0] := FDefaultPosition[0];
    FPosition[1] := FDefaultPosition[1];
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition[0]);
    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

  {$Region ' --------------------- TJPPegtopRangeBar.TransformCaption ---------------------- '}
function TJPPegtopRangeBar.TransformCaption(const S: String): String;
var
  L, T: String;
  E: TSize;
  P: array [0 .. 1] of Integer;
begin
  Result := S;
  L := AnsiLowerCase(Result);
  P[0] := Pos('<min>', L);
  P[1] := Pos('<max>', L);
  if P[0] <= P[1] then
  begin
    if P[0] > 0 then
    begin
      T := PositionToString(FPosition[0]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[0] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[0] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<min>', T, [rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[0] := Rect(0, 0, 0, 0);
    P[1] := Pos('<max>', AnsiLowerCase(Result));
    if P[1] > 0 then
    begin
      T := PositionToString(FPosition[1]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[1] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[1] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<max>', T, [rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[1] := Rect(0, 0, 0, 0);
  end
  else
  begin
    if P[1] > 0 then
    begin
      T := PositionToString(FPosition[1]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[1] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[1] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<max>', T, [rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[1] := Rect(0, 0, 0, 0);
    P[0] := Pos('<min>', AnsiLowerCase(Result));
    if P[0] > 0 then
    begin
      T := PositionToString(FPosition[0]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[0] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[0] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<min>', T, [rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[0] := Rect(0, 0, 0, 0);
  end;
end;
  {$endregion TJPPegtopRangeBar.TransformCaption}


function TJPPegtopRangeBar.GetButtonSize: TSize;
begin
  if Orientation = psoHorizontal then
  begin
    Result.CX := 17;
    Result.CY := 17;
  end
  else
  begin
    Result.CX := 17;
    Result.CY := 17;
  end;
end;

  {$Region ' ----------------- TJPPegtopRangeBar.GetButtonPoint ---------------- '}
function TJPPegtopRangeBar.GetButtonPoint(Index: Integer): TPoint;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if Orientation = psoHorizontal then
  begin
    if FMax = FMin then
    begin
      Result.X := 0;
    end
    else
    begin
      Result.X := MulDiv(FPosition[Index] - FMin, ClientWidth - 2 * ButtonSize.CX, FMax - FMin) + Index * ButtonSize.CX;
    end;
    Result.Y := LinePoint.Y - ButtonSize.CY div 2;
  end
  else
  begin
    if FMax = FMin then
    begin
      Result.Y := 0;
    end
    else
    begin
      Result.Y := MulDiv(FMax - FPosition[Index], ClientHeight - 2 * ButtonSize.CY, FMax - FMin) + (1 - Index) * ButtonSize.CX;
    end;
    Result.X := LinePoint.X - ButtonSize.CX div 2;
  end;
end;
  {$endregion TJPPegtopRangeBar.GetButtonPoint}


  {$Region ' ------------------- TJPPegtopRangeBar: Position related ------------------- '}
function TJPPegtopRangeBar.PointToPosition(const Index, X, Y: Integer): Integer;
var
  ButtonSize: TSize;
begin
  if FMax = FMin then
  begin
    Result := FMin;
  end
  else
  begin
    ButtonSize := GetButtonSize;
    if FOrientation = psoHorizontal then
        Result := (((X - Index * ButtonSize.CX) * (FMax - FMin) div FSmallChange + ((ClientWidth - ButtonSize.CX * 2) div 2))
        div (ClientWidth - ButtonSize.CX * 2)) * FSmallChange + FMin
    else Result := FMax - (((Y - Index * ButtonSize.CY) * (FMax - FMin) div FSmallChange + ((ClientHeight - ButtonSize.CY * 2) div 2))
        div (ClientHeight - ButtonSize.CY * 2)) * FSmallChange;
    if FConstrained then
    begin
      if Index = 1 then
      begin
        if Result - FPosition[1] + FPosition[0] < FMin then Result := FMin + FPosition[1] - FPosition[0]
        else if Result > FMax then Result := FMax;
      end
      else
      begin
        if Result < FMin then Result := FMin
        else if Result + FPosition[1] - FPosition[0] > FMax then Result := FMax - FPosition[1] + FPosition[0];
      end;
    end
    else
    begin
      if Index = 1 then
      begin
        if Result < FPosition[0] then Result := FPosition[0]
        else if Result > FMax then Result := FMax;
      end
      else
      begin
        if Result < FMin then Result := FMin
        else if Result > FPosition[1] then Result := FPosition[1];
      end;
    end;
  end;
end;


function TJPPegtopRangeBar.GetPosition(Index: Integer): Integer;
begin
  Result := FPosition[Index];
end;

procedure TJPPegtopRangeBar.SetPosition(Index: Integer; V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FPosition[Index] <> V then
  begin
    FPosition[Index] := V;
    if Index = 1 then
    begin
      if FPosition[0] > FPosition[1] then FPosition[0] := FPosition[1];
    end
    else
    begin
      if FPosition[1] < FPosition[0] then FPosition[1] := FPosition[0];
    end;
    Invalidate;
  end;
end;

procedure TJPPegtopRangeBar.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

function TJPPegtopRangeBar.GetDefaultPosition(Index: Integer): Integer;
begin
  Result := FDefaultPosition[Index];
end;

procedure TJPPegtopRangeBar.SetDefaultPosition(Index: Integer; V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FDefaultPosition[Index] <> V then
  begin
    FDefaultPosition[Index] := V;
    if Index = 1 then
    begin
      if FDefaultPosition[0] > FDefaultPosition[1] then FDefaultPosition[0] := FDefaultPosition[1];
    end
    else
    begin
      if FDefaultPosition[1] < FDefaultPosition[0] then FDefaultPosition[1] := FDefaultPosition[0];
    end;
  end;
end;

procedure TJPPegtopRangeBar.SetDrawTriangleWhenDisabled(const Value: Boolean);
begin
  FDrawTriangleWhenDisabled := Value;
  PropsChanged(Self);
end;

{$endregion TJPPegtopRangeBar: Position related}


procedure TJPPegtopRangeBar.MenuItemClick(Sender: TObject);
begin
  if Sender is TComponent then
  begin
    case TComponent(Sender).Tag of
      0: ResetToDefault;
      1: Constrained := not Constrained;
      3: EditValueMin;
      4: CopyValueMin;
      5: PasteValueMin;
      7: EditValueMax;
      8: CopyValueMax;
      9: PasteValueMax;
    end;
  end;
end;

function TJPPegtopRangeBar.GetValue(Index: Integer): Double;
begin
  Result := PositionToValue(FPosition[Index]);
end;

procedure TJPPegtopRangeBar.SetValue(Index: Integer; V: Double);
begin
  SetPosition(Index, ValueToPosition(V));
end;

procedure TJPPegtopRangeBar.SetConstrained(V: Boolean);
begin
  if FConstrained <> V then
  begin
    FConstrained := V;
    Invalidate;
  end;
end;
{$ENDREGION TJPPegtopRangeBar}


{$REGION ' ----------------------------- TJPPegtopHintWindow ----------------------------- '}

procedure TJPPegtopHintWindow.WMNCPaint(var Msg: TMessage);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    R := Rect(0, 0, Width, Height);
    Rectangle(DC, R.Left, R.Top, R.Right, R.Bottom);
    // DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RECT);
  finally
    ReleaseDC(Handle, DC);
  end;
end;
{$ENDREGION TJPPegtopHintWindow}


{$Region ' ------------------------- TJPPegtopButtonColorRect ------------------------ '}
constructor TJPPegtopButtonColorRect.Create;
begin
  inherited;
  FOnChange := nil;
  FNormalBgColor := clBlack;
  FNormalBorderColor := clBlack;
  FHotBgColor := clBlack;
  FHotBorderColor := clBlack;
  FPushedBgColor := clBlack;
  FPushedBorderColor := clBlack;
  FDisabledBgColor := clBtnFace;
  FDisabledBorderColor := clGrayText;
  FFocusedBgColor := clBlack;
  FFocusedBorderColor := clBlack;
  FMargins := TMargins.Create(nil);
  FMargins.OnChange := PropsChanged;
  FMargins.Left := 5;
  FMargins.Top := 5;
  FMargins.Right := 5;
  FMargins.Bottom := 5;
end;

destructor TJPPegtopButtonColorRect.Destroy;
begin
  FreeAndnil(FMargins);
  inherited;
end;

procedure TJPPegtopButtonColorRect.PropsChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJPPegtopButtonColorRect.SetDisabledBgColor(const Value: TColor);
begin
  if FDisabledBgColor = Value then Exit;
  FDisabledBgColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetDisabledBorderColor(const Value: TColor);
begin
  if FDisabledBorderColor = Value then Exit;
  FDisabledBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetFocusedBgColor(const Value: TColor);
begin
  if FFocusedBgColor = Value then Exit;
  FFocusedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetFocusedBorderColor(const Value: TColor);
begin
  if FFocusedBorderColor = Value then Exit;
  FFocusedBorderColor := Value;
  PropsChanged(Self);
end;


procedure TJPPegtopButtonColorRect.SetHotBgColor(const Value: TColor);
begin
  if FHotBgColor = Value then Exit;
  FHotBgColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetHotBorderColor(const Value: TColor);
begin
  if FHotBorderColor = Value then Exit;
  FHotBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetMargins(const Value: TMargins);
begin
  if FMargins = Value then Exit;
  FMargins := Value;
  FMargins.OnChange := PropsChanged;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetNormalBgColor(const Value: TColor);
begin
  if FNormalBgColor = Value then Exit;
  FNormalBgColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetNormalBorderColor(const Value: TColor);
begin
  if FNormalBorderColor = Value then Exit;
  FNormalBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  PropsChanged(Self);
end;


procedure TJPPegtopButtonColorRect.SetPushedBgColor(const Value: TColor);
begin
  if FPushedBgColor = Value then Exit;
  FPushedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonColorRect.SetPushedBorderColor(const Value: TColor);
begin
  if FPushedBorderColor = Value then Exit;
  FPushedBorderColor := Value;
  PropsChanged(Self);
end;
{$endregion TJPPegtopButtonColorRect}


{$Region ' --------------------------- TJPPegtopButtonParams -------------------------------- '}


constructor TJPPegtopButtonParams.Create;
begin
  inherited;
  FWidth := 25;
  FHeight := 17;
  FDefaultDrawing := True;
  FDrawFocusRect := False;
  FDrawGrip := True;

  FNormal := TJPPegtopButtonStateParams.Create;
  FHot := TJPPegtopButtonStateParams.Create;
  FPushed := TJPPegtopButtonStateParams.Create;
  FDisabled := TJPPegtopButtonStateParams.Create;
  FFocused := TJPPegtopButtonStateParams.Create;

  FNormal.OnChange := PropsChanged;
  FHot.OnChange := PropsChanged;
  FPushed.OnChange := PropsChanged;
  FDisabled.OnChange := PropsChanged;
  FFocused.OnChange := PropsChanged;

  FVisualStyle := bvsCustom;

end;

destructor TJPPegtopButtonParams.Destroy;
begin
  FreeAndNil(FNormal);
  FreeAndNil(FHot);
  FreeAndNil(FPushed);
  FreeAndNil(FDisabled);
  FreeAndNil(FFocused);
  inherited;
end;

procedure TJPPegtopButtonParams.PropsChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJPPegtopButtonParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
//  FNormal.OnChange := FOnChange;
//  FHot.OnChange := FOnChange;
//  FPushed.OnChange := FOnChange;
//  FDisabled.OnChange := FOnChange;
//  FFocused.OnChange := FOnChange;
  //PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.Assign(const Src: TJPPegtopButtonParams);
begin
  FNormal.Assign(Src.Normal);
  FHot.Assign(Src.Hot);
  FPushed.Assign(Src.Pushed);
  FDisabled.Assign(Src.Disabled);
  FFocused.Assign(Src.Focused);
  FWidth := Src.Width;
  FHeight := Src.Height;
  FDefaultDrawing := Src.DefaultDrawing;
  FDrawFocusRect := Src.DrawFocusRect;
  FDrawGrip := Src.DrawGrip;
  FVisualStyle := Src.VisualStyle;
end;

procedure TJPPegtopButtonParams.AssignVisualParams(const bvs: TJPPegtopTrackBarButtonVisualParams);
begin
  FNormal.AssignVisualParams(bvs.Normal);
  FHot.AssignVisualParams(bvs.Hot);
  FPushed.AssignVisualParams(bvs.Pushed);
  FDisabled.AssignVisualParams(bvs.Disabled);
  FFocused.AssignVisualParams(bvs.Focused);
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetDefaultDrawing(const Value: Boolean);
begin
  if FDefaultDrawing = Value then Exit;
  FDefaultDrawing := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetDrawFocusRect(const Value: Boolean);
begin
  if FDrawFocusRect = Value then Exit;
  FDrawFocusRect := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetDrawGrip(const Value: Boolean);
begin
  if FDrawGrip = Value then Exit;
  FDrawGrip := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetDisabled(const Value: TJPPegtopButtonStateParams);
begin
  if FDisabled = Value then Exit;
  FDisabled := Value;
  FDisabled.OnChange := PropsChanged;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetFocused(const Value: TJPPegtopButtonStateParams);
begin
  if FFocused = Value then Exit;
  FFocused := Value;
  FFocused.OnChange := PropsChanged;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetHot(const Value: TJPPegtopButtonStateParams);
begin
  if FHot = Value then Exit;
  FHot := Value;
  FHot.OnChange := PropsChanged;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetNormal(const Value: TJPPegtopButtonStateParams);
begin
  if FNormal = Value then Exit;
  FNormal := Value;
  FNormal.OnChange := PropsChanged;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetPushed(const Value: TJPPegtopButtonStateParams);
begin
  if FPushed = Value then Exit;
  FPushed := Value;
  FPushed.OnChange := PropsChanged;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetVisualStyle(const Value: TJPPegtopTrackBarButtonVisualStyle);
begin
  FVisualStyle := Value;
  if FVisualStyle = bvsCustom then Exit; // just leave all params unchanged
  case FVisualStyle of
    bvsWin10: AssignVisualParams(JPPegtopTrackBarButtonVisualStyle_Win10);
    // TODO: Carbon
  end;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetWidth(const Value: integer);
begin
  if FWidth = Value then Exit;
  FWidth := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonParams.SetHeight(const Value: integer);
begin
  if FHeight = Value then Exit;
  FHeight := Value;
  PropsChanged(Self);
end;


{$endregion TJPPegtopButtonParams}


{$Region ' ---------------------------- TJPPegtopButtonStateParams ------------------------------ '}

constructor TJPPegtopButtonStateParams.Create;
begin
  inherited;
  FColor := clBtnFace;
  FColorTo := clNone;
  FBorderColor := clBtnShadow;
  FGradientDirection := gdVertical;
  FGripColor := clGray;
end;

destructor TJPPegtopButtonStateParams.Destroy;
begin
  inherited;
end;

procedure TJPPegtopButtonStateParams.PropsChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJPPegtopButtonStateParams.Assign(const Src: TJPPegtopButtonStateParams);
begin
  FColor := Src.Color;
  FColorTo := Src.ColorTo;
  FBorderColor := Src.BorderColor;
  FGripColor := Src.GripColor;
  FGradientDirection := Src.GradientDirection;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonStateParams.AssignVisualParams(const bsvp: TJPPegtopTrackBarButtonStateVisualParams);
begin
  FColor := bsvp.Color;
  FColorTo := bsvp.ColorTo;
  FBorderColor := bsvp.BorderColor;
  FGripColor := bsvp.GripColor;
  FGradientDirection := bsvp.GradientDirection;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonStateParams.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonStateParams.SetColor(const Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonStateParams.SetColorTo(const Value: TColor);
begin
  if FColorTo = Value then Exit;
  FColorTo := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonStateParams.SetGradientDirection(const Value: TGradientDirection);
begin
  if FGradientDirection = Value then Exit;
  FGradientDirection := Value;
  PropsChanged(Self);
end;


procedure TJPPegtopButtonStateParams.SetGripColor(const Value: TColor);
begin
  if FGripColor = Value then Exit;
  FGripColor := Value;
  PropsChanged(Self);
end;

procedure TJPPegtopButtonStateParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  PropsChanged(Self);
end;

{$endregion TJPPegtopButtonStateParams}





initialization

  TrackBarPopupMenu := NIL;
  RangeBarPopupMenu := NIL;
  BitmapsLoaded := False;
  LabelHintWindow := NIL;
  HintWindowClass := TJPPegtopHintWindow;


  // ------- Trackbar button Win10 color scheme --------
  with JPPegtopTrackBarButtonVisualStyle_Win10 do
  begin
    Normal.BorderColor := $00A2A2A2;
    Normal.Color := $00F1F1F1;
    Normal.ColorTo := $00DDDDDD;
    Normal.GripColor := $002A2A2A;

    Hot.BorderColor := $00B17F3C;
    Hot.Color := $00FDF6EA;
    Hot.ColorTo := $00FDE6BE;
    Hot.GripColor := $0050391B;

    Pushed.BorderColor := $0059401E;
    Pushed.Color := $00F6E5C4;
    Pushed.ColorTo := $00F3DCAF;
    Pushed.GripColor := $002E2110;

    Focused.BorderColor := $00A3851D;
    Focused.Color := $00F1F1F1;
    Focused.ColorTo := $00DDDDDD;
    Focused.GripColor := $002A2A2A;

    Disabled.BorderColor := $00B5B2AD;
    Disabled.Color := $00F4F4F4;
    Disabled.ColorTo := $00EFEFEF;
    Disabled.GripColor := clGray;
  end;


finalization

  CloseBitmaps;

end.
