//---------------------------------------------------------------------------

#ifndef SSHClientDemoH
#define SSHClientDemoH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "ScBridge.hpp"
#include "ScSSHChannel.hpp"
#include "ScSSHClient.hpp"
//---------------------------------------------------------------------------
class TDemoForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *MainPanel;
    TPanel *pnTopLabel;
    TLabel *lbTitle;
    TPanel *Panel1;
    TPanel *Panel2;
    TPanel *Panel3;
    TPanel *Panel6;
    TPanel *Panel7;
    TPanel *Panel5;
    TLabel *Label1;
    TLabel *Label6;
    TEdit *edSSHUserName;
    TLabel *Label7;
    TEdit *edSSHPassword;
    TLabel *Label8;
    TComboBox *cbPrivateKey;
    TPanel *pnPassword;
    TPanel *pnPrivateKey;
    TSpeedButton *btConnectSSH;
    TSpeedButton *btDisconnectSSH;
    TScSSHClient *ScSSHClient;
    TLabel *Label4;
    TLabel *Label5;
    TEdit *edSSHHost;
    TLabel *Label3;
    TRadioButton *rbPassword;
    TRadioButton *rbPublicKey;
    TPanel *Panel12;
    TSpeedButton *btKeyGen;
    TScFileStorage *ScFileStorage;
    TScSSHChannel *ScSSHChannel;
    TLabel *Label2;
    TEdit *edDestHost;
    TLabel *Label9;
    TEdit *seDestPort;
    TPanel *Panel8;
    TSpeedButton *btStart;
    TSpeedButton *btStop;
    TLabel *Label10;
    TEdit *seSourcePort;
    TStringGrid *StringGrid;
    TTimer *Timer;
    TPanel *Panel9;
    TPanel *Panel4;
    TSpeedButton *btSConnect;
    TSpeedButton *btSDisconnect;
    TPanel *Panel10;
    TSpeedButton *btImportKey;
    TEdit *seSSHPort;
    TOpenDialog *OpenDialog;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall ScSSHClientAfterConnect(TObject *Sender);
    void __fastcall ScSSHClientAfterDisconnect(TObject *Sender);
    void __fastcall btConnectSSHClick(TObject *Sender);
    void __fastcall btDisconnectSSHClick(TObject *Sender);
    void __fastcall rbPasswordClick(TObject *Sender);
    void __fastcall rbPublicKeyClick(TObject *Sender);
    void __fastcall edSSHUserNameChange(TObject *Sender);
    void __fastcall cbPrivateKeyDropDown(TObject *Sender);
    void __fastcall btKeyGenClick(TObject *Sender);
    void __fastcall ScSSHClientServerKeyValidate(TObject *Sender,
          TScKey *NewServerKey, bool &Accept);
    void __fastcall ScSSHClientBeforeConnect(TObject *Sender);
    void __fastcall btStartClick(TObject *Sender);
    void __fastcall btStopClick(TObject *Sender);
    void __fastcall btSConnectClick(TObject *Sender);
    void __fastcall btSDisconnectClick(TObject *Sender);
    void __fastcall edDestHostChange(TObject *Sender);
    void __fastcall TimerTimer(TObject *Sender);
    void __fastcall btImportKeyClick(TObject *Sender);
	void __fastcall ScSSHChannelSocketConnect(TObject *Sender, const PSOCKADDR SockAddr);
	void __fastcall ScSSHChannelSocketDisconnect(TObject *Sender, const PSOCKADDR SockAddr);



private:	// User declarations
    TCRVioTcp *FVio;
    TCriticalSection *FLockEvent;
    TStringList *FEvStrings;
    bool FRandomized;

    void SaveState();
    void LoadState();
    String KeyPath();
    void CheckRandomize();
    void Randomize();
    void DisconnectAll();
    void DisconnectChannel();
    void ShowSSHButtons(bool Connected);
    void ShowPasswordAuth(bool pa);
	void WriteLog(String Event, PSOCKADDR SockAddr);

public:		// User declarations
        __fastcall TDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoForm *DemoForm;
//---------------------------------------------------------------------------
#endif
