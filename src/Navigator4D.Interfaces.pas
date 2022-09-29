unit Navigator4D.Interfaces;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.UITypes,
  SysUtils,
  FMX.Types;

type
  INavigator4DParam = interface
  ['{B288262F-32A6-471F-A439-54CB1EB04DCD}']
    function Key: String;
    function Value: Variant;
    function IValue: IInterface;
    function OValue: TObject;
  end;


  INavigator4DParams = interface
  ['{573B8E5E-A548-472A-8977-52018A68224D}']
    function AddParam(AParam: INavigator4DParam): INavigator4DParams;
    function Count: Integer;
    function GetParam(AIndex: Integer): INavigator4DParam; overload;
    function GetParam(AKey: String): INavigator4DParam; overload;
    function ContainsKey(AKey: String): Boolean;
  end;

  INavigator4DComponetParams = interface
  ['{E2792FCC-B08A-4CC5-B6C6-E37C194B384A}']
    procedure ProcessParams(AParams: INavigator4DParams);
  end;

  INavigator4DTemplate = interface
  ['{F669051A-3883-4116-AD13-E6EE3D0B232E}']
    function EmbedIn: TFmxObject;
    function Render: TFmxObject;
  end;

  INavigator4DTemplates = interface
  ['{CF5908E8-79F4-4353-AF8E-1CFEE1C94EA6}']
    function CreateInstanceTemplate(APath: String): TComponent;
    function Add(APath: String; ATemplateClass: TPersistentClass): INavigator4DTemplates;
    function Remove(APath: String): INavigator4DTemplates;
  end;

  INavigator4DComponent = interface
  ['{4341F8E4-B48B-42CE-9B07-78BB6E1D9E97}']
    function Render: TFmxObject;
    procedure UnRender;
    procedure SetTemplate(ATemplate: INavigator4DTemplate);
  end;

  INavigator4DRouter = interface
  ['{FF3C2B3D-F575-4F1D-9F6B-05F471D35482}']
    function CreateInstancePersistent(APath: String): TComponent;
    function CreateInstanceTemplate(APath: String): TComponent;
    function HasTemplateClass(APath: String): Boolean;
    function Add(APath: String; AClass: TPersistentClass; ATemplate: String = ''): INavigator4DRouter;
    function Remove(APath: String): INavigator4DRouter;
  end;

  INavigator4DTo = interface
  ['{72BC4C85-7618-4B46-AB0A-9BA9245E467E}']
    procedure Navigate(APath: String; AParams: INavigator4DParams = nil);
    procedure PushNamed(APath: String; AParams: INavigator4DParams = nil);
    procedure Pop(AParams: INavigator4DParams = nil);
    procedure PopUntil(APath: String; AParams: INavigator4DParams = nil);
    procedure PopAndPushNamed(APath: String; AParams: INavigator4DParams = nil);
    procedure PopUntilAndPushNamed(APath, AToPath: String; AParams: INavigator4DParams = nil);
    procedure InitRender(AObject: TFmxObject);
    function HistoryCount: Integer;
  end;

implementation

end.
