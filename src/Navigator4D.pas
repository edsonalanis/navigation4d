unit Navigator4D;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.UITypes,
  SysUtils,
  FMX.Types,
  Navigator4D.Interfaces;

type
  TNavigator4D = class
  private
    class var FInstance: TNavigator4D;
  protected
    class destructor Destroy;
  public
    function InitRender(ARender: TFmxObject): TNavigator4D;
    class function New: TNavigator4D;
    class function Router: INavigator4DRouter;
    class function Templates: INavigator4DTemplates;
    class function &To: INavigator4DTo;
  end;

implementation

{ TNavigator4D }

uses
  Navigator4D.Navigate,
  Navigator4D.Router,
  Navigator4D.Templates;

class function TNavigator4D.Templates: INavigator4DTemplates;
begin
  Result := TNavigator4DTemplates.New;
end;

class function TNavigator4D.&To: INavigator4DTo;
begin
  Result := TNavigator4DTo.New;
end;

class destructor TNavigator4D.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TNavigator4D.InitRender(ARender: TFmxObject): TNavigator4D;
begin
  Result := Self;
  TNavigator4DTo.New.InitRender(ARender);
end;

class function TNavigator4D.New: TNavigator4D;
begin
  if not Assigned(FInstance) then
    FInstance := TNavigator4D.Create;

  Result := FInstance;
end;

class function TNavigator4D.Router: INavigator4DRouter;
begin
  Result := TNavigator4DRouter.New;
end;

end.
