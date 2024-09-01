// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWhatsAppReceiver.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncwhatsappreceiverHPP
#define Fmx_TmsfncwhatsappreceiverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <FMX.TMSFNCWebSocketClient.hpp>
#include <FMX.TMSFNCWebSocketCommon.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwhatsappreceiver
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCWhatsAppReceiverContactAddress;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactEmail;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactPhone;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactURL;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactName;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactOrganization;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactAddresses;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactEmails;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactPhones;
class DELPHICLASS TTMSFNCWhatsAppReceiverContactURLs;
class DELPHICLASS TTMSFNCWhatsAppReceiverContact;
class DELPHICLASS TTMSFNCWhatsAppReceiverContacts;
class DELPHICLASS TTMSFNCWhatsAppReceiverText;
class DELPHICLASS TTMSFNCWhatsAppReceiverMedia;
class DELPHICLASS TTMSFNCWhatsAppReceiverLocation;
class DELPHICLASS TTMSFNCWhatsAppReceiverMessageFrom;
class DELPHICLASS TTMSFNCWhatsAppReceiverMessage;
class DELPHICLASS TTMSFNCCustomWhatsAppReceiver;
class DELPHICLASS TTMSFNCWhatsAppReceiver;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCWhatsAppReceiverMessageType : unsigned char { wamtText, wamtImage, wamtDocument, wamtContacts, wamtAudio, wamtLocation, wamtVideo, wamtSticker };

enum DECLSPEC_DENUM TTMSFNCWhatsAppReceiverContactType : unsigned char { wactHome, wactWork };

enum DECLSPEC_DENUM TTMSFNCWhatsAppReceiverContactPhoneType : unsigned char { waptHome, waptWork, waptCell, waptMain, waptIphone };

class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactAddress : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FStreet;
	System::UnicodeString FZIP;
	System::UnicodeString FState;
	System::UnicodeString FCountryCode;
	TTMSFNCWhatsAppReceiverContactType FAddressType;
	System::UnicodeString FCountry;
	System::UnicodeString FCity;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString Street = {read=FStreet, write=FStreet};
	__property System::UnicodeString City = {read=FCity, write=FCity};
	__property System::UnicodeString State = {read=FState, write=FState};
	__property System::UnicodeString ZIP = {read=FZIP, write=FZIP};
	__property System::UnicodeString Country = {read=FCountry, write=FCountry};
	__property System::UnicodeString CountryCode = {read=FCountryCode, write=FCountryCode};
	__property TTMSFNCWhatsAppReceiverContactType AddressType = {read=FAddressType, write=FAddressType, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactAddress() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddress() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactEmail : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FEmail;
	TTMSFNCWhatsAppReceiverContactType FEmailType;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString Email = {read=FEmail, write=FEmail};
	__property TTMSFNCWhatsAppReceiverContactType EmailType = {read=FEmailType, write=FEmailType, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactEmail() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmail() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactPhone : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCWhatsAppReceiverContactPhoneType FPhoneType;
	System::UnicodeString FPhone;
	System::UnicodeString FWaID;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString Phone = {read=FPhone, write=FPhone};
	__property TTMSFNCWhatsAppReceiverContactPhoneType PhoneType = {read=FPhoneType, write=FPhoneType, nodefault};
	__property System::UnicodeString WhatsAppID = {read=FWaID, write=FWaID};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactPhone() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhone() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactURL : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCWhatsAppReceiverContactType FURLType;
	System::UnicodeString FURL;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString URL = {read=FURL, write=FURL};
	__property TTMSFNCWhatsAppReceiverContactType URLType = {read=FURLType, write=FURLType, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactURL() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURL() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactName : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FLastName;
	System::UnicodeString FFormattedName;
	System::UnicodeString FMiddleName;
	System::UnicodeString FFirstName;
	System::UnicodeString FPrefix;
	System::UnicodeString FSuffix;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString FormattedName = {read=FFormattedName, write=FFormattedName};
	__property System::UnicodeString FirstName = {read=FFirstName, write=FFirstName};
	__property System::UnicodeString LastName = {read=FLastName, write=FLastName};
	__property System::UnicodeString MiddleName = {read=FMiddleName, write=FMiddleName};
	__property System::UnicodeString Suffix = {read=FSuffix, write=FSuffix};
	__property System::UnicodeString Prefix = {read=FPrefix, write=FPrefix};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactName() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactName() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactOrganization : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDepartment;
	System::UnicodeString FCompany;
	System::UnicodeString FTitle;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString Company = {read=FCompany, write=FCompany};
	__property System::UnicodeString Department = {read=FDepartment, write=FDepartment};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactOrganization() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactOrganization() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactAddresses : public System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*> inherited;
	
public:
	System::UnicodeString __fastcall ToJSON();
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactAddress*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactAddress*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactAddresses() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactAddress*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactAddress*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactAddress>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactAddresses(TTMSFNCWhatsAppReceiverContactAddress* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactAddress*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactEmails : public System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*> inherited;
	
public:
	System::UnicodeString __fastcall ToJSON();
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactEmail*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactEmail*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactEmails() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactEmail*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactEmail*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactEmail>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactEmails(TTMSFNCWhatsAppReceiverContactEmail* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactEmail*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactPhones : public System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*> inherited;
	
public:
	System::UnicodeString __fastcall ToJSON();
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactPhone*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactPhone*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactPhones() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactPhone*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactPhone*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactPhone>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactPhones(TTMSFNCWhatsAppReceiverContactPhone* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactPhone*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContactURLs : public System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*> inherited;
	
public:
	System::UnicodeString __fastcall ToJSON();
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactURL*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactURL*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContactURLs() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContactURL*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContactURL*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContactURL>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContactURLs(TTMSFNCWhatsAppReceiverContactURL* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContactURL*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContact : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCWhatsAppReceiverContactPhones* FPhones;
	System::TDateTime FBirthday;
	TTMSFNCWhatsAppReceiverContactURLs* FURLs;
	TTMSFNCWhatsAppReceiverContactOrganization* FOrganization;
	TTMSFNCWhatsAppReceiverContactName* FContactName;
	TTMSFNCWhatsAppReceiverContactEmails* FEmails;
	TTMSFNCWhatsAppReceiverContactAddresses* FAddresses;
	
public:
	__fastcall TTMSFNCWhatsAppReceiverContact();
	__fastcall virtual ~TTMSFNCWhatsAppReceiverContact();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property TTMSFNCWhatsAppReceiverContactAddresses* Addresses = {read=FAddresses, write=FAddresses};
	__property System::TDateTime Birthday = {read=FBirthday, write=FBirthday};
	__property TTMSFNCWhatsAppReceiverContactEmails* Emails = {read=FEmails, write=FEmails};
	__property TTMSFNCWhatsAppReceiverContactName* ContactName = {read=FContactName, write=FContactName};
	__property TTMSFNCWhatsAppReceiverContactOrganization* Organization = {read=FOrganization, write=FOrganization};
	__property TTMSFNCWhatsAppReceiverContactPhones* Phones = {read=FPhones, write=FPhones};
	__property TTMSFNCWhatsAppReceiverContactURLs* URLs = {read=FURLs, write=FURLs};
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverContacts : public System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*> inherited;
	
public:
	System::UnicodeString __fastcall ToJSON();
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContact*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContact*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverContacts() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCWhatsAppReceiverContact*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts(System::Generics::Collections::TEnumerable__1<TTMSFNCWhatsAppReceiverContact*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCWhatsAppReceiver_TTMSFNCWhatsAppReceiverContact>.Create */ inline __fastcall TTMSFNCWhatsAppReceiverContacts(TTMSFNCWhatsAppReceiverContact* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCWhatsAppReceiverContact*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverText : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FBody;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property System::UnicodeString Body = {read=FBody, write=FBody};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverText() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverText() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverMedia : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FMimeType;
	System::UnicodeString FFilename;
	System::UnicodeString FID;
	System::UnicodeString FCaption;
	System::UnicodeString FChecksum;
	TTMSFNCWhatsAppReceiverMessageType FFileType;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property TTMSFNCWhatsAppReceiverMessageType FileType = {read=FFileType, write=FFileType, nodefault};
	__property System::UnicodeString MimeType = {read=FMimeType, write=FMimeType};
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString Caption = {read=FCaption, write=FCaption};
	__property System::UnicodeString Filename = {read=FFilename, write=FFilename};
	__property System::UnicodeString Checksum = {read=FChecksum, write=FChecksum};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverMedia() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverMedia() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverLocation : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FLocationName;
	double FLatitude;
	double FLongitude;
	System::UnicodeString FAddress;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property double Longitude = {read=FLongitude, write=FLongitude};
	__property double Latitude = {read=FLatitude, write=FLatitude};
	__property System::UnicodeString LocationName = {read=FLocationName, write=FLocationName};
	__property System::UnicodeString Address = {read=FAddress, write=FAddress};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverLocation() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverLocation() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverMessageFrom : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FID;
	System::UnicodeString FPhone;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString WhatsAppID = {read=FID, write=FID};
	__property System::UnicodeString Phone = {read=FPhone, write=FPhone};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiverMessageFrom() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWhatsAppReceiverMessageFrom() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiverMessage : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCWhatsAppReceiverLocation* FLocation;
	TTMSFNCWhatsAppReceiverMedia* FMedia;
	System::UnicodeString FID;
	System::UnicodeString FTimeStamp;
	TTMSFNCWhatsAppReceiverContacts* FContacts;
	TTMSFNCWhatsAppReceiverText* FText;
	TTMSFNCWhatsAppReceiverMessageType FMessageType;
	bool FIsReply;
	TTMSFNCWhatsAppReceiverMessageFrom* FFrom;
	
public:
	__fastcall TTMSFNCWhatsAppReceiverMessage();
	__fastcall virtual ~TTMSFNCWhatsAppReceiverMessage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::UnicodeString __fastcall ToJSON();
	__property bool IsReply = {read=FIsReply, write=FIsReply, nodefault};
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property TTMSFNCWhatsAppReceiverMessageFrom* From = {read=FFrom, write=FFrom};
	__property System::UnicodeString TimeStamp = {read=FTimeStamp, write=FTimeStamp};
	__property TTMSFNCWhatsAppReceiverContacts* Contacts = {read=FContacts, write=FContacts};
	__property TTMSFNCWhatsAppReceiverLocation* Location = {read=FLocation, write=FLocation};
	__property TTMSFNCWhatsAppReceiverMedia* Media = {read=FMedia, write=FMedia};
	__property TTMSFNCWhatsAppReceiverText* Text = {read=FText, write=FText};
	__property TTMSFNCWhatsAppReceiverMessageType MessageType = {read=FMessageType, write=FMessageType, nodefault};
};


typedef void __fastcall (__closure *TTMSFNCCustomWhatsAppMessageReceivedEvent)(System::TObject* Sender, TTMSFNCWhatsAppReceiverMessage* AMessage);

class PASCALIMPLEMENTATION TTMSFNCCustomWhatsAppReceiver : public Fmx::Tmsfncwebsocketclient::TTMSFNCCustomWebsocketClient
{
	typedef Fmx::Tmsfncwebsocketclient::TTMSFNCCustomWebsocketClient inherited;
	
private:
	TTMSFNCCustomWhatsAppMessageReceivedEvent FOnWhatsAppMessageReceived;
	
protected:
	virtual void __fastcall DoMessageReceived(TTMSFNCWhatsAppReceiverMessage* AMessage);
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual void __fastcall MessageReceived(System::TObject* Sender, Fmx::Tmsfncwebsocketcommon::TTMSFNCWebSocketConnection* AConnection, const System::UnicodeString AMessage);
	__property TTMSFNCCustomWhatsAppMessageReceivedEvent OnWhatsAppMessageReceived = {read=FOnWhatsAppMessageReceived, write=FOnWhatsAppMessageReceived};
	
public:
	__fastcall virtual TTMSFNCCustomWhatsAppReceiver(System::Classes::TComponent* aOwner)/* overload */;
public:
	/* TTMSFNCCustomWebsocketClient.Create */ inline __fastcall virtual TTMSFNCCustomWhatsAppReceiver()/* overload */ : Fmx::Tmsfncwebsocketclient::TTMSFNCCustomWebsocketClient() { }
	/* TTMSFNCCustomWebsocketClient.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWhatsAppReceiver() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppReceiver : public TTMSFNCCustomWhatsAppReceiver
{
	typedef TTMSFNCCustomWhatsAppReceiver inherited;
	
public:
	__property AutoSyncEvents = {default=1};
	__property ConnectTimeout = {default=0};
	__property OnPing;
	__property OnPong;
	
__published:
	__property Active = {default=0};
	__property HostName = {default=0};
	__property Port = {default=8888};
	__property PathName = {default=0};
	__property OnConnect;
	__property OnDisconnect;
	__property OnWhatsAppMessageReceived;
	__property OnMessageReceived;
	__property OnBinaryDataReceived;
public:
	/* TTMSFNCCustomWhatsAppReceiver.Create */ inline __fastcall virtual TTMSFNCWhatsAppReceiver(System::Classes::TComponent* aOwner)/* overload */ : TTMSFNCCustomWhatsAppReceiver(aOwner) { }
	
public:
	/* TTMSFNCCustomWebsocketClient.Create */ inline __fastcall virtual TTMSFNCWhatsAppReceiver()/* overload */ : TTMSFNCCustomWhatsAppReceiver() { }
	/* TTMSFNCCustomWebsocketClient.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppReceiver() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncwhatsappreceiver */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWHATSAPPRECEIVER)
using namespace Fmx::Tmsfncwhatsappreceiver;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwhatsappreceiverHPP
