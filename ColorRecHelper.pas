unit ColorRecHelper;

interface

uses
  Winapi.Windows, System.SysUtils, Vcl.Graphics,
  System.Math;

(* Original Authors = Jerry Dodge et.all
  TColorRec - Helper record for TColor to easily interchange for simplicity of reading not only RGB but also CMYK and HSV.
  Supports directly assigning a TColor using implicit class operators.
*)
type //Extra Goodie (TNaturalColorHTMLHex and mapping function)
   TNaturalColorHTMLHex = (white=$ffffff, silver=$C0C0C0,	gray=$808080,	black=$000000,	navy=$000080,	blue=$0000FF,	cerulean=$007BA7,	skyblue=$87CEEB,	turquoise=$40E0D0,bluegreen=$0095B6,	azure=$007FFF,	teal=$008080,	cyan=$00FFFF,green=$00FF00,lime=$BFFF00,chartreuse=$7FFF00
                  , olive=$808000,	yellow=$FFFF00,	gold=$FFD700,	amber=$FFBF00,	orange=$FFA500,	brown=$964B00,	orangered=$FF4500,	red=$FF0000,	maroon=800000,	rose=$FF007F,	redviolet=$C71585,	pink=$FFC0CB,	magenta=$FF00FF,blueviolet=$8A2BE2, purple=$800080,	indigo=$4B0082
 	 	 	 	 	 	 	 	 	,violet=$EE82EE,	peach=$FFE5B4,	apricot=$FBCEB1,	ochre=$CC7722,	plum=$8E4585);

type
  TColorRec = record
  {private}
  public
    FRed: Byte;
    FGreen: Byte;
    FBlue: Byte;
    function GetBrightness: Double;
    function GetHue: Double;
    function GetHueDegrees: Double;
    function GetSaturation: Double;
    procedure SetBrightness(const Value: Double);
    procedure SetHue(const Value: Double);
    procedure SetHueDegrees(const Value: Double);
    procedure SetSaturation(const Value: Double);
    function GetBlack: Integer;
    function GetCyan: Integer;
    function GetMagenta: Integer;
    function GetYellow: Integer;
    procedure SetBlack(const Value: Integer);
    procedure SetCyan(const Value: Integer);
    procedure SetMagenta(const Value: Integer);
    procedure SetYellow(const Value: Integer);
    //My Extra for Hex manipulation
    function GetHTMLHexWithoutHash: String;
    function GetDelphiHexWithoutDollar: String;
    procedure SetHTMLHexWithoutHashInput(const ValueHTMLHex: String);
    procedure SetDelphiHexWithoutDollarInput(const ValueDelphiHex: String);
    procedure SetHTMLHexWithDollar(const ValueHTMLHex: Integer);
    procedure SetDelphiHexWithDollar(const ValueDelphiHex: Integer);
    procedure SetColorFromNaturalString(const col: string);   //Extra Goodie
//  public
    class operator implicit(Value: TColorRec): TColor;
    class operator implicit(Value: TColor): TColorRec;
    property Red: Byte read FRed write FRed;
    property Green: Byte read FGreen write FGreen;
    property Blue: Byte read FBlue write FBlue;
    property Hue: Double read GetHue write SetHue;
    property Saturation: Double read GetSaturation write SetSaturation;
    property Brightness: Double read GetBrightness write SetBrightness;
    property Cyan: Integer read GetCyan write SetCyan;
    property Magenta: Integer read GetMagenta write SetMagenta;
    property Yellow: Integer read GetYellow write SetYellow;
    property Black: Integer read GetBlack write SetBlack;
  end;


implementation



//My Extra for Hex manipulation
function ConvertHtmlHexToTColor(Color: String):TColor ;
var
    rColor : TColor;
begin
//    Color := CheckHexForHash(Color);    Not required if you don't want the # infront of the string

    if (length(color) = 6) then
    begin
        {remember that TColor is bgr not rgb: so you need to switch then around}
        color := '$00' + copy(color,5,2) + copy(color,3,2) + copy(color,1,2);
        rColor := StrToInt(color);
    end;

    result := rColor;
end;

//My Extra for Hex manipulation
function TColorToHTMLHex(Color: integer): string;
var
r,g,b: byte;
begin
r:=GetRValue(Color);
g:=GetGValue(Color);
b:=GetBValue(Color);
Result:=IntToHex(r,2)+IntToHex(g,2)+IntToHex(b,2);
end;

function RGBToHSV(R, G, B: Byte; var H, S, V: Double): Boolean;
var
  minRGB, maxRGB, delta: Double;
begin
  h := 0.0;
  minRGB := Min(Min(R, G), B);
  maxRGB := Max(Max(R, G), B);
  delta := (maxRGB - minRGB);
  V := maxRGB;
  if (maxRGB <> 0.0) then
    S := 255.0 * delta / maxRGB
  else
    S := 0.0;
  if (S <> 0.0) then begin
    if R = maxRGB then
      H := (G - B) / delta
    else if G = maxRGB then
      H := 2.0 + (B - R) / delta
    else if B = maxRGB then
      H := 4.0 + (R - G) / delta
  end else
    H := -1.0;
  H := h * 60;
  if H < 0.0 then
    H := H + 360.0;

  H := H/360.0; //for making the 0..360 map to 0..1 as required in the assert

  //S := S * 100 / 255;
  //V := B * 100 / 255;
  S := S / 255;
  V := V / 255;

  Result:= True;
end;

function HSVToRGB(H, S, V: Double; var R, G, B: Byte): Boolean;
var
  i: Integer;
  f, p, q, t: Double;
  procedure CopyOutput(const RV, GV, BV: Double);
  const
    RGBmax = 255;
  begin
    R:= Round(RGBmax * RV);
    G:= Round(RGBmax * GV);
    B:= Round(RGBmax * BV);
  end;
begin
  Assert(InRange(H, 0.0, 1.0));
  Assert(InRange(S, 0.0, 1.0));
  Assert(InRange(V, 0.0, 1.0));
  if S = 0.0 then begin
    // achromatic (grey)
    CopyOutput(B, B, B);
    Result:= True;
    exit;
  end;
  H := H * 6.0; // sector 0 to 5
  i := floor(H);
  f := H - i; // fractional part of H
  p := V * (1.0 - S);
  q := V * (1.0 - S * f);
  t := V * (1.0 - S * (1.0 - f));
  case i of
    0: CopyOutput(V, t, p);
    1: CopyOutput(q, V, p);
    2: CopyOutput(p, V, t);
    3: CopyOutput(p, q, V);
    4: CopyOutput(t, p, V);
    else CopyOutput(V, p, q);
  end;
  Result:= True;
end;

//Extra Goodie (TNaturalColorHTMLHex and mapping function)
procedure TColorRec.SetColorFromNaturalString(const col: string);
var
   colHTMLHex : TNaturalColorHTMLHex;
begin
    if col = 'white' then colHTMLHex := TNaturalColorHTMLHex.white
    else if col = 'silver' then colHTMLHex := TNaturalColorHTMLHex.silver
    else if col = 'gray' then colHTMLHex := TNaturalColorHTMLHex.gray
    else if col = 'black' then colHTMLHex := TNaturalColorHTMLHex.black
    else if col = 'navy' then colHTMLHex := TNaturalColorHTMLHex.navy
    else if col = 'blue' then colHTMLHex := TNaturalColorHTMLHex.blue
    else if col = 'cerulean' then colHTMLHex :=TNaturalColorHTMLHex.cerulean
    else if col = 'skyblue' then colHTMLHex :=  TNaturalColorHTMLHex.skyblue
    else if col = 'turquoise' then colHTMLHex :=  TNaturalColorHTMLHex.turquoise
    else if col = 'bluegreen' then colHTMLHex := TNaturalColorHTMLHex.bluegreen
    else if col = 'azure' then colHTMLHex :=  TNaturalColorHTMLHex.azure
    else if col = 'teal' then colHTMLHex := TNaturalColorHTMLHex.teal
    else if col = 'cyan' then colHTMLHex := TNaturalColorHTMLHex.cyan
    else if col = 'green' then colHTMLHex := TNaturalColorHTMLHex.green
    else if col = 'lime' then colHTMLHex := TNaturalColorHTMLHex.lime
    else if col = 'chartreuse' then colHTMLHex := TNaturalColorHTMLHex.chartreuse
    else if col = 'olive' then colHTMLHex :=  TNaturalColorHTMLHex.olive
    else if col = 'yellow' then colHTMLHex := TNaturalColorHTMLHex. yellow
    else if col = 'gold' then colHTMLHex := TNaturalColorHTMLHex.gold
    else if col = 'amber' then colHTMLHex :=  TNaturalColorHTMLHex.amber
    else if col = 'orange' then colHTMLHex := TNaturalColorHTMLHex.orange
    else if col = 'brown' then colHTMLHex :=  TNaturalColorHTMLHex.brown
    else if col = 'orangered' then colHTMLHex := TNaturalColorHTMLHex.orangered
    else if col = 'red' then colHTMLHex := TNaturalColorHTMLHex.red
    else if col = 'maroon' then colHTMLHex := TNaturalColorHTMLHex.maroon
    else if col = 'rose' then colHTMLHex := TNaturalColorHTMLHex.rose
    else if col = 'redviolet' then colHTMLHex :=  TNaturalColorHTMLHex.redviolet
    else if col = 'pink' then colHTMLHex := TNaturalColorHTMLHex.pink
    else if col = 'magenta' then colHTMLHex :=TNaturalColorHTMLHex.magenta
    else if col = 'blueviolet' then colHTMLHex := TNaturalColorHTMLHex.blueviolet
    else if col = 'purple' then colHTMLHex := TNaturalColorHTMLHex.purple
    else if col = 'indigo' then colHTMLHex := TNaturalColorHTMLHex.indigo
    else if col = 'violet' then colHTMLHex := TNaturalColorHTMLHex.violet
    else if col = 'peach' then colHTMLHex := TNaturalColorHTMLHex.peach
    else if col = 'apricot' then colHTMLHex := TNaturalColorHTMLHex.apricot
    else if col = 'ochre' then colHTMLHex := TNaturalColorHTMLHex.ochre
    else if col = 'plum' then colHTMLHex := TNaturalColorHTMLHex.plum
    else colHTMLHex := TNaturalColorHTMLHex.white;

    Self.SetHTMLHexWithDollar(Ord(colHTMLHex));
end;

{ TColorRec }

class operator TColorRec.implicit(Value: TColorRec): TColor;
begin
  with Value do
    Result:= RGB(Red, Green, Blue);
end;

class operator TColorRec.implicit(Value: TColor): TColorRec;
begin
  with Result do begin
    FRed:= GetRValue(Value);
    FGreen:= GetGValue(Value);
    FBlue:= GetBValue(Value);
  end;
end;

function TColorRec.GetHue: Double;
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  Result:= H;
end;

function TColorRec.GetHueDegrees: Double;
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  Result:= H*360;      //Assuming H is always in the range 0..1 (for 0..360)
end;

function TColorRec.GetSaturation: Double;
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  Result:= S;
end;

function TColorRec.GetBrightness: Double;
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  Result:= V;
end;

function TColorRec.GetCyan: Integer;
begin
  Result:= GetCValue(RGB(FRed, FGreen, FBlue));
end;

function TColorRec.GetMagenta: Integer;
begin
  Result:= GetMValue(RGB(FRed, FGreen, FBlue));
end;

function TColorRec.GetYellow: Integer;
begin
  Result:= GetYValue(RGB(FRed, FGreen, FBlue));
end;

function TColorRec.GetBlack: Integer;
begin
  Result:= GetKValue(RGB(FRed, FGreen, FBlue));
end;

procedure TColorRec.SetBrightness(const Value: Double);
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  V:= Value;
  HSVToRGB(H, S, V, FRed, FGreen, FBlue);
end;

procedure TColorRec.SetHue(const Value: Double);
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  H:= Value;
  HSVToRGB(H, S, V, FRed, FGreen, FBlue);
end;

procedure TColorRec.SetHueDegrees(const Value: Double);
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  H:= Value/360;   //Assuming H is always in the range 0..1 (for 0..360)
  HSVToRGB(H, S, V, FRed, FGreen, FBlue);
end;

procedure TColorRec.SetSaturation(const Value: Double);
var
  H, S, V: Double;
begin
  RGBToHSV(FRed, FGreen, FBlue, H, S, V);
  S:= Value;
  HSVToRGB(H, S, V, FRed, FGreen, FBlue);
end;

procedure TColorRec.SetCyan(const Value: Integer);
begin
  Self:= CMYK(Value, Magenta, Yellow, Black);
end;

procedure TColorRec.SetMagenta(const Value: Integer);
begin
  Self:= CMYK(Cyan, Value, Yellow, Black);
end;

procedure TColorRec.SetYellow(const Value: Integer);
begin
  Self:= CMYK(Cyan, Magenta, Value, Black);
end;

procedure TColorRec.SetBlack(const Value: Integer);
begin
  Self:= CMYK(Cyan, Magenta, Yellow, Value);
end;

//My Extra for Hex manipulation

procedure TColorRec.SetDelphiHexWithoutDollarInput(const ValueDelphiHex: string);
begin
   //Delphi Hex to TColor
   Self := StrToInt('$'+ValueDelphiHex);
end;

function TColorRec.GetDelphiHexWithoutDollar;
begin
   Result := '';
   //internalRGB to TColor
   //TColor to Delphi HEX
   Result := '$'+IntToHex(RGB(FRed,FGreen,FBlue),6);
   Result := StringReplace(Result,'$','',[rfReplaceAll]);
end;

procedure TColorRec.SetHTMLHexWithoutHashInput(const ValueHTMLHex: string);
begin
  //HTMLHex to TColor
  //TColor set method (implicit)
   Self := ConvertHtmlHexToTColor(ValueHTMLHex);
end;

function TColorRec.GetHTMLHexWithoutHash;
begin
   Result := '';
   Result := TColorToHTMLHex(RGB(FRed, FGreen, FBlue));
end;


procedure TColorRec.SetDelphiHexWithDollar(const ValueDelphiHex: Integer);
begin
    //Delphi Hex to TColor
   Self := (ValueDelphiHex);
end;

procedure TColorRec.SetHTMLHexWithDollar(const ValueHTMLHex: Integer);
var
  dollarStrippedHexString : string;
begin
   dollarStrippedHexString := IntToHex(ValueHTMLHex,6);
   Self.SetHTMLHexWithoutHashInput(dollarStrippedHexString);
end;
end.


{* Some use cases

Natural String to color from editor based input (Goodie Part)


**Trick to make tints of color**
#Convert RGB to HSV and play with Value/Brightness(V field)
#Back convert HSV to RGB.



procedure TForm1.btnHexDemosViaRecordHelperClick(Sender: TObject);
var
 col1 : TColorRec;
 myHTMLColor : TNaturalColorHTMLHex;
begin
    //Delphi to Hex
    col1 := clNone;
    edtHexDelphi.Text := col1.GetDelphiHexWithoutDollar;
    shp2.Brush.Color := col1;
    edtDelphiRGB.Text := IntToStr(col1.FRed) + ', '+IntToStr(col1.FGreen) + ', '+IntToStr(col1.FBlue);

    //Delphi Hex to TColor
    col1.SetDelphiHexWithoutDollarInput(edtHexDelphi.Text);
    shp2.Brush.Color := col1;
    edtDelphiRGB.Text := IntToStr(col1.FRed) + ', '+IntToStr(col1.FGreen) + ', '+IntToStr(col1.FBlue);

    //Delphi Hex to HTML Hex
    col1.SetDelphiHexWithoutDollarInput(edtHexDelphi.Text);
    shp2.Brush.Color := col1;
    edtHexHTML.Text := col1.GetHTMLHexWithoutHash;

    //HTML Hex to TColor
    col1.SetHTMLHexWithoutHashInput(edtHexHTML.Text);
    shp2.Brush.Color := col1;


    col1.SetHTMLHexWithDollar($FFD700);
    myHTMLColor := TNaturalColorHTMLHex.green;
    col1.SetHTMLHexWithDollar(Ord(myHTMLColor));

    col1.SetColorFromNaturalString('orangered');
    shp2.Brush.Color := col1;
end;


*}
