unit Navigator4D.Navigate;

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
  THistoryNavigator = record
    Path: String;
    PersistentInstance: TComponent;
    TemplateInstance: TComponent;
  end;

  TNavigator4DTo = class(TInterfacedObject, INavigator4DTo)
  private
    class var FInstance: INavigator4DTo;
  public
    class function New: INavigator4DTo;
  private
    FHistory: TStack<THistoryNavigator>;
    FHistoryTemplates: TStack<TComponent>;
    FInitRender: TFmxObject;
    procedure _Render(AHistory: THistoryNavigator; AParams: INavigator4DParams = nil);
    procedure _UnRender(AHistory: THistoryNavigator);
    procedure _Pop;
    procedure _PushNamed(APath: String);
  public
    constructor Create;
    destructor Destroy; override;

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

uses
  Navigator4D;

{ TNavigator4DTo }

constructor TNavigator4DTo.Create;
begin
  FHistory := TStack<THistoryNavigator>.Create;
  FHistoryTemplates := TStack<TComponent>.Create;
end;

destructor TNavigator4DTo.Destroy;
begin
  FreeAndNil(FHistoryTemplates);
  FreeAndNil(FHistory);
  inherited;
end;

function TNavigator4DTo.HistoryCount: Integer;
begin
  Result := FHistory.Count;
end;

procedure TNavigator4DTo.InitRender(AObject: TFmxObject);
begin
  FInitRender := AObject;
end;

procedure TNavigator4DTo.Navigate(APath: String; AParams: INavigator4DParams = nil);
begin
  var thr := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FInitRender.RemoveObject(0);

          while (FHistory.Count > 0) do
            _Pop;

          _PushNamed(APath);

          _Render(FHistory.Peek, AParams);
        end
      );
    end);
  thr.FreeOnTerminate := True;
  thr.Start;
end;

class function TNavigator4DTo.New: INavigator4DTo;
begin
  if not Assigned(FInstance) then
    FInstance := TNavigator4DTo.Create;

  Result := FInstance;
end;

procedure TNavigator4DTo.Pop(AParams: INavigator4DParams = nil);
begin
  var thr := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FInitRender.RemoveObject(0);

          if (FHistory.Count > 1) then
            _Pop;

          _Render(FHistory.Peek, AParams);
        end
      );
    end);
  thr.FreeOnTerminate := True;
  thr.Start;
end;

procedure TNavigator4DTo.PopAndPushNamed(APath: String; AParams: INavigator4DParams);
begin
  var thr := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FInitRender.RemoveObject(0);

          if (FHistory.Count > 1) then
            _Pop;
          _PushNamed(APath);

          _Render(FHistory.Peek, AParams);
        end
      );
    end);
  thr.FreeOnTerminate := True;
  thr.Start;
end;

procedure TNavigator4DTo.PopUntil(APath: String; AParams: INavigator4DParams = nil);
begin
  if (FHistory.Count = 1) then
    Exit;

  var thr := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          history: THistoryNavigator;
        begin
          FInitRender.RemoveObject(0);

          history := FHistory.Peek;
          while (FHistory.Count > 1) and (not history.Path.Equals(APath)) do
          begin
            _Pop;
            history := FHistory.Peek;
          end;

          if (FHistory.Count > 0) then
            _Render(FHistory.Peek, AParams);
        end
      );
    end);
  thr.FreeOnTerminate := True;
  thr.Start;
end;

procedure TNavigator4DTo.PopUntilAndPushNamed(APath, AToPath: String;
  AParams: INavigator4DParams);
begin
  if (FHistory.Count = 1) then
    Exit;

  var thr := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          history: THistoryNavigator;
        begin
          FInitRender.RemoveObject(0);

          history := FHistory.Peek;
          while (FHistory.Count > 1) and (not history.Path.Equals(APath)) do
          begin
            _Pop;
            history := FHistory.Peek;
          end;
          _PushNamed(AToPath);

          if (FHistory.Count > 0) then
            _Render(FHistory.Peek);
        end
      );
    end);
  thr.FreeOnTerminate := True;
  thr.Start;
end;

procedure TNavigator4DTo.PushNamed(APath: String; AParams: INavigator4DParams = nil);
begin
  var thr := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FInitRender.RemoveObject(0);

          _PushNamed(APath);

          _Render(FHistory.Peek, AParams);
        end
      );
    end);
  thr.FreeOnTerminate := True;
  thr.Start;
end;

procedure TNavigator4DTo._Render(AHistory: THistoryNavigator; AParams: INavigator4DParams = nil);
var
  component: INavigator4DComponent;
  template: INavigator4DTemplate;
  params: INavigator4DComponetParams;
begin
  if not Supports(AHistory.PersistentInstance, INavigator4DComponent, component) then
    raise Exception.Create('Object does not implement INavigator4DComponent interface');

  if Assigned(AHistory.TemplateInstance) then
    if not Supports(AHistory.TemplateInstance, INavigator4DTemplate, template) then
      raise Exception.Create('Object does not implement INavigator4DTemplate interface');

  if Assigned(AParams) then
  begin
    if not Supports(AHistory.PersistentInstance, INavigator4DComponetParams, params) then
      raise Exception.Create('Object does not implement INavigator4DComponetParams interface');

    params.ProcessParams(AParams);
  end;

  if Assigned(template) then
  begin
    template.EmbedIn.AddObject(component.Render);
    component.SetTemplate(template);
    FInitRender.AddObject(template.Render);
  end
  else
  begin
    if FHistoryTemplates.Count > 0 then
      if not Supports(FHistoryTemplates.Peek, INavigator4DTemplate, template) then
        raise Exception.Create('Object does not implement INavigator4DTemplate interface');

    if Assigned(template) then
    begin
      template.EmbedIn.RemoveObject(0);
      template.EmbedIn.AddObject(component.Render);
      component.SetTemplate(template);
      FInitRender.AddObject(template.Render);
    end
    else
      FInitRender.AddObject(component.Render);
  end;
end;

procedure TNavigator4DTo._UnRender(AHistory: THistoryNavigator);
var
  component: INavigator4DComponent;
begin
  if not Supports(AHistory.PersistentInstance, INavigator4DComponent, component) then
    raise Exception.Create('Object does not implement INavigator4DComponent interface');

  component.UnRender;
end;

procedure TNavigator4DTo._Pop;
var
  history: THistoryNavigator;
begin
  _UnRender(FHistory.Peek);

  history := FHistory.Pop;
  history.PersistentInstance.Free;
  if Assigned(history.TemplateInstance) then
  begin
    FHistoryTemplates.Pop;
    history.TemplateInstance.Free;
  end;
end;

procedure TNavigator4DTo._PushNamed(APath: String);
var
  history: THistoryNavigator;
begin
  if (FHistory.Count > 0) then
    _UnRender(FHistory.Peek);

  history.Path := APath;
  history.PersistentInstance := TNavigator4D.Router.CreateInstancePersistent(APath);

  history.TemplateInstance := nil;
  if TNavigator4D.Router.HasTemplateClass(APath) then
  begin
    history.TemplateInstance := TNavigator4D.Router.CreateInstanceTemplate(APath);
    FHistoryTemplates.Push(history.TemplateInstance);
  end;

  FHistory.Push(history);
end;

end.
