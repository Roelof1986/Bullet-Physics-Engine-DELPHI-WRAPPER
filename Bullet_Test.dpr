program Bullet_Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas';

{$R *.res}
{$APPTYPE GUI}
begin
  Application.Initialize;
  Form1 := TForm1.CreateNew(nil);
  Form1.Caption := 'Bullet 3D Test';
  Form1.Width := 1024;
  Form1.Height := 720;
  Form1.Position := TFormPosition.poScreenCenter;
  Form1.InitializeScene;
//  Application.CreateForm(TForm1, Form1);
  Application.MainForm := Form1;
  Form1.Show;
  Application.Run;
  Form1.Free;
end.
