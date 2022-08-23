unit Routers;

interface

uses
  Navigator4D,
  Navigator4D.Interfaces;

type
  TRouters = class
  public
    class procedure Register;
  end;

implementation

{ TRotas }

uses
  Unit2,
  Unit3,
  Unit4,
  Template1;

class procedure TRouters.Register;
begin
  TNavigator4D
    .Templates
      .Add('template1', TfrmTemplate1);

  TNavigator4D
    .Router
      .Add('/form2', TForm2, 'template1')
      .Add('/form3', TForm3)
      .Add('/form4', TForm4)
      .Add('/form4.1', TForm4);
end;

end.
