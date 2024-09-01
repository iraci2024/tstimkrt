//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SSHClientDemo.h"
#include <assert.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ScBridge"
#pragma link "ScSSHChannel"
#pragma link "ScSSHClient"
#pragma link "ScSSHSocket"
#pragma resource "*.dfm"
TDemoForm *DemoForm;
//---------------------------------------------------------------------------
__fastcall TDemoForm::TDemoForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::FormCreate(TObject *Sender)
{
  FLockEvent = new TCriticalSection();
  FEvStrings = new TStringList();

  LoadState();
  edSSHHost->Text = ScSSHClient->HostName;
  seSSHPort->Text = IntToStr(ScSSHClient->Port);
  edSSHUserName->Text = ScSSHClient->User;

  StringGrid->Cells[0][0] = "Num";
  StringGrid->Cells[1][0] = "Status";
  StringGrid->Cells[2][0] = "From host";
  StringGrid->Cells[3][0] = "From port";
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::FormDestroy(TObject *Sender)
{
  DisconnectAll();
  SaveState();

  FEvStrings->Free();
  FLockEvent->Free();
  if (FVio)
    FVio->Free();
}
//---------------------------------------------------------------------------

void TDemoForm::SaveState()
{
  TRegistry *Registry = new TRegistry();
  try {
    Registry->OpenKey(KeyPath() + "\\" + this->ClassName(), true);
    Registry->WriteString("SSHHost", ScSSHClient->HostName);
    Registry->WriteInteger("SSHPort", ScSSHClient->Port);
    Registry->WriteString("SSHUserName", ScSSHClient->User);
    Registry->WriteString("DestHost", edDestHost->Text);
    Registry->WriteInteger("DestPort", StrToIntDef(seDestPort->Text, 0));
    Registry->WriteInteger("SourcePort", StrToIntDef(seSourcePort->Text, 0));
  }
  __finally {
    Registry->Free();
  }
}
//---------------------------------------------------------------------------

void TDemoForm::LoadState()
{
  TRegistry *Registry = new TRegistry();
  try {
    if (Registry->OpenKey(KeyPath() + "\\" + this->ClassName(), false)) {
      if (Registry->ValueExists("SSHHost"))
        ScSSHClient->HostName = Registry->ReadString("SSHHost");
      if (Registry->ValueExists("SSHPort"))
        ScSSHClient->Port = Registry->ReadInteger("SSHPort");
      if (Registry->ValueExists("SSHUserName"))
        ScSSHClient->User = Registry->ReadString("SSHUserName");
      if (Registry->ValueExists("DestHost"))
        edDestHost->Text = Registry->ReadString("DestHost");
      if (Registry->ValueExists("DestPort"))
        seDestPort->Text = IntToStr(Registry->ReadInteger("DestPort"));
      if (Registry->ValueExists("SourcePort"))
        seSourcePort->Text = IntToStr(Registry->ReadInteger("SourcePort"));
    }
  }
  __finally {
    Registry->Free();
  }
}
//---------------------------------------------------------------------------

String TDemoForm::KeyPath()
{
  return "\SOFTWARE\Devart\SecureBridge\Demos";
}
//---------------------------------------------------------------------------

void TDemoForm::CheckRandomize()
{
  if (!FRandomized)
    Randomize();

  if (!FRandomized)
    throw Exception("Data for the random generator has not been generated");
}
//---------------------------------------------------------------------------

void TDemoForm::Randomize()
{
  FRandomized = true;
  
//  TfmRandom *fmRandom = TfmRandom(this);
//  try {
//    if (fmRandom->ShowModal() == mrOk) {
//      Random->Randomize(fmRandom->Data);
//      FRandomized = true;
//    }
//  }
//  finally {
//    fmRandom->Free();
//  }
}
//---------------------------------------------------------------------------

void TDemoForm::DisconnectAll()
{
  DisconnectChannel();
  ScSSHClient->Disconnect();
  ShowSSHButtons(false);
}
//---------------------------------------------------------------------------

void TDemoForm::ShowSSHButtons(bool Connected)
{
  btConnectSSH->Enabled = !Connected;
  btDisconnectSSH->Enabled = Connected;
  btStart->Enabled = Connected;
}
//---------------------------------------------------------------------------

void TDemoForm::DisconnectChannel()
{
  try {
    ScSSHChannel->Disconnect();
  }
  __finally {
    btStart->Enabled = ScSSHClient->Connected;
    btStop->Enabled = false;
    btSConnect->Enabled = false;
    btSDisconnect->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::ScSSHClientAfterConnect(TObject *Sender)
{
  ShowSSHButtons(true);
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::ScSSHClientAfterDisconnect(TObject *Sender)
{
  ShowSSHButtons(false);
  DisconnectChannel();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btConnectSSHClick(TObject *Sender)
{
  TCursor OldCursor = Screen->Cursor;
  try {
    Screen->Cursor = crHourGlass;
    ScSSHClient->Connect();
  }
  __finally {
    Screen->Cursor = OldCursor;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btDisconnectSSHClick(TObject *Sender)
{
  DisconnectAll();
}
//---------------------------------------------------------------------------

void TDemoForm::ShowPasswordAuth(bool pa)
{
  pnPassword->Visible = pa;
  pnPrivateKey->Visible = !pa;
  Repaint();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::rbPasswordClick(TObject *Sender)
{
  ShowPasswordAuth(true);
  DisconnectAll();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::rbPublicKeyClick(TObject *Sender)
{
  ShowPasswordAuth(false);
  DisconnectAll();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::edSSHUserNameChange(TObject *Sender)
{
  DisconnectAll();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::cbPrivateKeyDropDown(TObject *Sender)
{
  ScFileStorage->Keys->GetKeyNames(cbPrivateKey->Items);
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btKeyGenClick(TObject *Sender)
{
  CheckRandomize();

  TCursor OldCursor = Screen->Cursor;
  try {
    Screen->Cursor = crHourGlass;

    if (cbPrivateKey->Text == "")
      cbPrivateKey->Text = "client_key";

    TScKey *Key = ScFileStorage->Keys->FindKey(cbPrivateKey->Text);
    TScAsymmetricAlgorithm Algorithm;
    int BitCount;

    if (Key == NULL) {
      Key = new TScKey(ScFileStorage->Keys);
      Key->KeyName = cbPrivateKey->Text;
      Algorithm = aaRSA;
      BitCount = 1024;
    }
    else {
      Key->Ready = true;
      Algorithm = Key->Algorithm;
      BitCount = Key->BitCount;
    }

    try {
      Key->Generate(Algorithm, BitCount, Scbridge::Random);
      Key->ExportTo(Key->KeyName + ".pub", true, "", saTripleDES_cbc, kfDefault, "");

      String msg = "The client key file has been generated in the current application directory.\n";
      msg += "To connect using authentication by key, you should pass the '" + Key->KeyName;
      msg += ".pub' file to the server and setup the server to work with this file.";
      MessageDlg(msg, mtInformation, TMsgDlgButtons() << mbOK, 0);
    }
    catch (Exception &ex) {
      MessageDlg("Cannot generate key: " + ex.Message, mtWarning, TMsgDlgButtons() << mbOK, 0);
    }
  }
  __finally {
    Screen->Cursor = OldCursor;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::ScSSHClientServerKeyValidate(TObject *Sender,
      TScKey *NewServerKey, bool &Accept)
{
  String fp, msg;
  TScKey *Key = ScFileStorage->Keys->FindKey(ScSSHClient->HostName);
  if ((Key == NULL) || (!Key->Ready))  {
    NewServerKey->GetFingerprint(haMD5, fp);
    msg = "The authenticity of the server can not be verified.\n";
    msg += "Fingerprint for the key received from server: " + fp + ".\n";
    msg += "Key length: " + IntToStr(NewServerKey->BitCount) + " bits.\n";
    msg += "Are you sure you want to continue connecting?";

    if (MessageDlg(msg, mtConfirmation, TMsgDlgButtons() << mbOK << mbCancel, 0) == mrOk) {
      Key = new TScKey(NULL);
      try {
        Key->Assign(NewServerKey);
        Key->KeyName = ScSSHClient->HostName;
        ScFileStorage->Keys->Add(Key);
      }
      catch (Exception &exception) {
        Key->Free();
        throw;
      }

      Accept = true;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::ScSSHClientBeforeConnect(TObject *Sender)
{
  CheckRandomize();

  ScSSHClient->HostName = edSSHHost->Text;
  ScSSHClient->Port = StrToInt(seSSHPort->Text);
  ScSSHClient->User = edSSHUserName->Text;

  if (rbPassword->Checked) {
    ScSSHClient->Authentication = atPassword;
    ScSSHClient->Password = edSSHPassword->Text;
  }
  else {
    ScSSHClient->Authentication = atPublicKey;
    ScSSHClient->PrivateKeyName = cbPrivateKey->Text;
    if (ScFileStorage->Keys->FindKey(ScSSHClient->PrivateKeyName) == NULL)
      throw Exception("Private key can not be empty");
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btStartClick(TObject *Sender)
{
  ScSSHChannel->SourcePort = StrToInt(seSourcePort->Text);
  ScSSHChannel->DestHost = edDestHost->Text;
  ScSSHChannel->DestPort = StrToInt(seDestPort->Text);
  ScSSHChannel->Connect();

  btStart->Enabled = false;
  btStop->Enabled = true;
  btSConnect->Enabled = true;
  btSDisconnect->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btStopClick(TObject *Sender)
{
  DisconnectChannel();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btSConnectClick(TObject *Sender)
{
  FVio = new TCRVioTcp();

  FVio->Host = "localhost";
  FVio->Port = ScSSHChannel->SourcePort;
  FVio->ConnectionTimeout = ScSSHChannel->Timeout;
  FVio->Connect();

  btSConnect->Enabled = false;
  btSDisconnect->Enabled = true;
}

//---------------------------------------------------------------------------

void __fastcall TDemoForm::btSDisconnectClick(TObject *Sender)
{
  FVio->Close();
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::edDestHostChange(TObject *Sender)
{
  DisconnectChannel();
}
//---------------------------------------------------------------------------

void TDemoForm::WriteLog(String Event, PSOCKADDR SockAddr)
{
  TIPEndPoint *Sock = new TIPEndPoint(PSockAddrIn(SockAddr));

  FLockEvent->Acquire();
  try {
    FEvStrings->AddObject(Event, Sock);
  }
  __finally {
    FLockEvent->Release();
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::ScSSHChannelSocketConnect(TObject *Sender, const PSOCKADDR SockAddr)
{
  WriteLog("Connect", SockAddr);
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::ScSSHChannelSocketDisconnect(TObject *Sender, const PSOCKADDR SockAddr)
{
  btSConnect->Enabled = ScSSHChannel->Connected;
  btSDisconnect->Enabled = false;
  if (FVio) {
    FVio->Free();
    FVio = NULL;
  }

  WriteLog("Disconnect", SockAddr);
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::TimerTimer(TObject *Sender)
{
  FLockEvent->Acquire();
  try {
    while (FEvStrings->Count > 0) {
      TIPEndPoint *Sock = (TIPEndPoint*)(FEvStrings->Objects[0]);

      try {
        if (StringGrid->Cells[0][1] != "")
          StringGrid->RowCount = StringGrid->RowCount + 1;

        StringGrid->Cells[0][StringGrid->RowCount - 1] = IntToStr(StringGrid->RowCount - 1);
        StringGrid->Cells[1][StringGrid->RowCount - 1] = FEvStrings->Strings[0];
        StringGrid->Cells[2][StringGrid->RowCount - 1] = Sock->ToString();
        StringGrid->Cells[3][StringGrid->RowCount - 1] = IntToStr(Sock->Port);
      }
      __finally {
        Sock->Free();
        FEvStrings->Delete(0);
      }
    }
  }
  __finally {
    FLockEvent->Release();
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoForm::btImportKeyClick(TObject *Sender)
{
  TScKey *Key, *StorageKey;

  if (OpenDialog->Execute()) {
    bool MustFree = true;
    Key = new TScKey(NULL);
    try {
      String Comment, fp;
      Key->ImportFrom(OpenDialog->FileName, "", Comment);
      Key->GetFingerprint(haMD5, fp);
      String msg = "Fingerprint for the key imported from server: " + fp + ".\n";
      msg += "Key length: " + IntToStr(Key->BitCount) + " bits.\n";
      msg += "Do you want to save the server key?";

      if (MessageDlg(msg, mtConfirmation, TMsgDlgButtons() << mbOK << mbCancel, 0) == mrOk) {
        if (Trim(edSSHHost->Text) == "")
          edSSHHost->Text = ChangeFileExt(ExtractFileName(OpenDialog->FileName), "");

        StorageKey = ScFileStorage->Keys->FindKey(edSSHHost->Text);
        if (StorageKey != NULL) {
          StorageKey->Assign(Key);
        }
        else {
          Key->KeyName = edSSHHost->Text;
          ScFileStorage->Keys->Add(Key);
          MustFree = false;
        }
      }
    }
    __finally {
      if (MustFree)
        Key->Free();
    }
  }
}
//---------------------------------------------------------------------------

