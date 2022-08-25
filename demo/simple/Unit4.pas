unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Navigator4D.Interfaces,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TForm4 = class(TForm, INavigator4DComponent, INavigator4DComponetParams)
    Layout1: TLayout;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  public
    function Render: TFmxObject;
    procedure UnRender;
    procedure ProcessParams(AParams: INavigator4DParams);
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses Navigator4D;

{ TForm4 }

procedure TForm4.Button1Click(Sender: TObject);
begin
  TNavigator4D.&To.Pop;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  TNavigator4D.&To.PushNamed('/form2');
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  TNavigator4D.&To.PushNamed('/form3');
end;

procedure TForm4.ProcessParams(AParams: INavigator4DParams);
var
  I: Integer;
  param: INavigator4DParam;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    param := AParams.GetParam(I);
    Memo1.Lines.Add( param.Key + ' = ' + VarToStr(param.Value) );
  end;
end;

function TForm4.Render: TFmxObject;
begin
  Result := Layout1;
end;

procedure TForm4.UnRender;
begin

end;

end.
