unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Navigator4D.Interfaces;

type
  TForm2 = class(TForm, INavigator4DComponent)
    Layout1: TLayout;
    Label1: TLabel;
    Button1: TButton;
    Unit4: TButton;
    back: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure backClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Unit4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
    function Render: TFmxObject;
    procedure UnRender;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Navigator4D, Navigator4D.Params;

{ TForm2 }

procedure TForm2.backClick(Sender: TObject);
begin
  TNavigator4D.&To.Pop;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  TNavigator4D.&To.PushNamed('/form3');
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  TNavigator4D.&To.PushNamed('/form4.1');
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
//
end;

function TForm2.Render: TFmxObject;
begin
  Result := Layout1;
end;

procedure TForm2.Unit4Click(Sender: TObject);
begin
  TNavigator4D
    .&To
    .Navigate(
      '/form4',
      TNavigator4DParams.Create
        .AddParam( TNavigator4DParam.Create('aa', 'Valor 1') )
        .AddParam( TNavigator4DParam.Create('bb', 1) )
    );
end;

procedure TForm2.UnRender;
begin

end;

end.
