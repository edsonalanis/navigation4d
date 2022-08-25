unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Navigator4D.Interfaces;

type
  TForm3 = class(TForm, INavigator4DComponent)
    Layout1: TLayout;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  public
    function Render: TFmxObject;
    procedure UnRender;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses Navigator4D;

{ TForm3 }

procedure TForm3.Button1Click(Sender: TObject);
begin
  TNavigator4D.&To.Pop;
end;

function TForm3.Render: TFmxObject;
begin
  Result := Layout1
end;

procedure TForm3.UnRender;
begin

end;

end.
