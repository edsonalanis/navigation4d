unit Template1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, Navigator4D.Interfaces;

type
  TfrmTemplate1 = class(TForm, INavigator4DTemplate)
    Layout1: TLayout;
    Rectangle1: TRectangle;
    Layout2: TLayout;
    procedure FormDestroy(Sender: TObject);
  public
    function EmbedIn: TFmxObject;
    function Render: TFmxObject;
  end;

var
  frmTemplate1: TfrmTemplate1;

implementation

{$R *.fmx}

{ TForm5 }

function TfrmTemplate1.EmbedIn: TFmxObject;
begin
  Result := Layout2;
end;

procedure TfrmTemplate1.FormDestroy(Sender: TObject);
begin
//
end;

function TfrmTemplate1.Render: TFmxObject;
begin
  Result := Layout1;
end;

end.
