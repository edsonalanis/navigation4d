unit Navigator4D.Router;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.UITypes,
  SysUtils,
  FMX.Forms,
  FMX.Types,
  Navigator4D.Interfaces;

type
  TRouterPersistent = record
    Patch: String;
    PersistentClass: TPersistentClass;
    Template: String;
  end;

  TNavigator4DRouter = class(TInterfacedObject, INavigator4DRouter)
  private
    class var FInstance: INavigator4DRouter;
  private
    FList: TDictionary<String, TRouterPersistent>;
  public
    class function New: INavigator4DRouter;

    constructor Create;
    destructor Destroy; override;

    function CreateInstancePersistent(APath: String): TComponent;
    function CreateInstanceTemplate(APath: String): TComponent;
    function HasTemplateClass(APath: String): Boolean;
    function Add(APath: string; AClass: TPersistentClass; ATemplate: String = ''): INavigator4DRouter;
    function Remove(APath: string): INavigator4DRouter;
  end;

implementation

{ TNavigator4DRouter }

uses Navigator4D;

function TNavigator4DRouter.Add(APath: string; AClass: TPersistentClass;
  ATemplate: String = ''): INavigator4DRouter;
var
  cache, temp: TRouterPersistent;
begin
  Result := Self;

  RegisterClass(AClass);

  cache.Patch := APath;
  cache.PersistentClass := AClass;
  cache.Template := ATemplate;

  if not FList.TryGetValue(APath, temp) then
    FList.Add(APath, cache);
end;

constructor TNavigator4DRouter.Create;
begin
  FList := TDictionary<String, TRouterPersistent>.Create;
end;

function TNavigator4DRouter.CreateInstancePersistent(APath: String): TComponent;
var
  cache: TRouterPersistent;
begin
  if not FList.TryGetValue(APath, cache) then
    raise Exception.Create('Router not registered: ' + APath);

  Result := TComponentClass(FindClass(cache.PersistentClass.ClassName))
    .Create(Application);
end;

function TNavigator4DRouter.CreateInstanceTemplate(APath: String): TComponent;
var
  cache: TRouterPersistent;
begin
  if not FList.TryGetValue(APath, cache) then
    raise Exception.Create('Router not registered: ' + APath);

//  Result := cache.Template <> '';
  Result := TNavigator4D.Templates.CreateInstanceTemplate(cache.Template);
end;

destructor TNavigator4DRouter.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TNavigator4DRouter.HasTemplateClass(APath: String): Boolean;
var
  cache: TRouterPersistent;
begin
  if not FList.TryGetValue(APath, cache) then
    raise Exception.Create('Router not registered: ' + APath);

  Result := cache.Template <> '';
end;

class function TNavigator4DRouter.New: INavigator4DRouter;
begin
  if not Assigned(FInstance) then
    FInstance := TNavigator4DRouter.Create;

  Result := FInstance;
end;

function TNavigator4DRouter.Remove(APath: string): INavigator4DRouter;
begin
  Result := Self;

  FList.Remove(APath);
end;

end.
