unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Navigator4D, Routers;

procedure TForm1.FormShow(Sender: TObject);
begin
  TRouters.Register;
//  TRouters.Register;
  TNavigator4D
    .New
    .InitRender(Layout1)
    .&To.Navigate('/form2');
end;

end.
