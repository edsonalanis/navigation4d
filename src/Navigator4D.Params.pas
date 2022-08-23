unit Navigator4D.Params;

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
  TNavigator4DParam = class(TInterfacedObject, INavigator4DParam)
  private
    FKey: String;
    FValue: Variant;
  public
    constructor Create(AKey: String; AValue: Variant); reintroduce;
    function Key: string;
    function Value: Variant;
  end;

  TParamCache = record
    Param: INavigator4DParam;
  end;

  TNavigator4DParams = class(TInterfacedObject, INavigator4DParams)
  private
    FList: TDictionary<String, TParamCache>;
  public
    constructor Create;
    destructor Destroy; override;

    function AddParam(AParam: INavigator4DParam): INavigator4DParams;
    function Count: Integer;
    function GetParam(AIndex: Integer): INavigator4DParam; overload;
    function GetParam(AKey: String): INavigator4DParam; overload;
  end;

implementation

//
{ TNavigator4DParams }

function TNavigator4DParams.AddParam(AParam: INavigator4DParam): INavigator4DParams;
var
  cache: TParamCache;
begin
  Result := Self;
  cache.Param := AParam;
  FList.Add(AParam.Key, cache);
end;

function TNavigator4DParams.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TNavigator4DParams.Create;
begin
  FList := TDictionary<String, TParamCache>.Create;
end;

destructor TNavigator4DParams.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TNavigator4DParams.GetParam(AIndex: Integer): INavigator4DParam;
var
  aList: TArray<TPair<String, TParamCache>>;
begin
  aList := FList.ToArray;
  Result := aList[AIndex].Value.Param;
end;

function TNavigator4DParams.GetParam(AKey: String): INavigator4DParam;
begin
  Result := FList.Items[AKey].Param;
end;

{ TNavigator4DParam<T> }

constructor TNavigator4DParam.Create(AKey: String; AValue: Variant);
begin
  FKey := AKey;
  FValue := AValue;
end;

function TNavigator4DParam.Key: string;
begin
  Result := FKey;
end;

function TNavigator4DParam.Value: Variant;
begin
  Result := FValue;
end;

end.
