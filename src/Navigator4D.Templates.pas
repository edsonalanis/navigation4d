unit Navigator4D.Templates;

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
  TTemplatePersistent = record
    Patch: String;
    TemplateClass: TPersistentClass;
  end;

  TNavigator4DTemplates = class(TInterfacedObject, INavigator4DTemplates)
  private
    class var FInstance: INavigator4DTemplates;
  private
    FList: TDictionary<String, TTemplatePersistent>;
  public
    class function New: INavigator4DTemplates;

    constructor Create;
    destructor Destroy; override;

    function CreateInstanceTemplate(APath: String): TComponent;
    function Add(APath: string; AClass: TPersistentClass): INavigator4DTemplates;
    function Remove(APath: string): INavigator4DTemplates;
  end;

implementation

{ TNavigator4DTemplates }

function TNavigator4DTemplates.Add(APath: string; AClass: TPersistentClass): INavigator4DTemplates;
var
  cache, temp: TTemplatePersistent;
begin
  Result := Self;

  RegisterClass(AClass);

  cache.Patch := APath;
  cache.TemplateClass := AClass;

  if not FList.TryGetValue(APath, temp) then
    FList.Add(APath, cache);
end;

constructor TNavigator4DTemplates.Create;
begin
  FList := TDictionary<String, TTemplatePersistent>.Create;
end;

function TNavigator4DTemplates.CreateInstanceTemplate(APath: String): TComponent;
var
  cache: TTemplatePersistent;
begin
  if not FList.TryGetValue(APath, cache) then
    raise Exception.Create('Template not registered: ' + APath);

  Result := TComponentClass(FindClass(cache.TemplateClass.ClassName))
    .Create(Application);
end;

destructor TNavigator4DTemplates.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

class function TNavigator4DTemplates.New: INavigator4DTemplates;
begin
  if not Assigned(FInstance) then
    FInstance := TNavigator4DTemplates.Create;

  Result := FInstance;
end;

function TNavigator4DTemplates.Remove(APath: string): INavigator4DTemplates;
begin
  Result := Self;

  FList.Remove(APath);
end;

end.
