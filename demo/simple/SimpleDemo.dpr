program SimpleDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Navigator4D in '..\..\src\Navigator4D.pas',
  Navigator4D.Interfaces in '..\..\src\Navigator4D.Interfaces.pas',
  Navigator4D.Router in '..\..\src\Navigator4D.Router.pas',
  Unit2 in 'Unit2.pas' {Form2},
  Navigator4D.Templates in '..\..\src\Navigator4D.Templates.pas',
  Routers in 'Routers.pas',
  Navigator4D.Navigate in '..\..\src\Navigator4D.Navigate.pas',
  Unit3 in 'Unit3.pas' {Form3},
  Unit4 in 'Unit4.pas' {Form4},
  Navigator4D.Params in '..\..\src\Navigator4D.Params.pas',
  Template1 in 'Template1.pas' {frmTemplate1};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
