// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Google_Calendar.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_google_calendarHPP
#define Sgchttp_google_calendarHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcHTTP_Google_Cloud.hpp>
#include <sgcJSON.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_google_calendar
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcGoogleCalendarScopes;
class DELPHICLASS TsgcGoogleCalendarErrorItem;
class DELPHICLASS TsgcGoogleCalendarError;
class DELPHICLASS TsgcGoogleCalendarJSON;
class DELPHICLASS TsgcGoogleCalendarWatch;
class DELPHICLASS TsgcGoogleCalendarResource;
class DELPHICLASS TsgcGoogleCalendarScope;
class DELPHICLASS TsgcGoogleCalendarResource_ACL;
class DELPHICLASS TsgcGoogleCalendarDefaultReminderItem;
class DELPHICLASS TsgcGoogleCalendarDefaultReminders;
class DELPHICLASS TsgcGoogleCalendarNotificationItem;
class DELPHICLASS TsgcGoogleCalendarNotificationSettings;
class DELPHICLASS TsgcGoogleCalendarConferenceProperties;
class DELPHICLASS TsgcGoogleCalendarResource_CalendarList;
class DELPHICLASS TsgcGoogleCalendarResource_Calendar;
class DELPHICLASS TsgcGoogleCalendarPerson;
class DELPHICLASS TsgcGoogleCalendarCreator;
class DELPHICLASS TsgcGoogleCalendarOrganizer;
class DELPHICLASS TsgcGoogleCalendarDateTime;
class DELPHICLASS TsgcGoogleCalendarStart;
class DELPHICLASS TsgcGoogleCalendarEnd;
class DELPHICLASS TsgcGoogleCalendarOriginalStart;
class DELPHICLASS TsgcGoogleCalendarAttendeeItem;
class DELPHICLASS TsgcGoogleCalendarAttendees;
class DELPHICLASS TsgcGoogleCalendarExtendedProperties;
class DELPHICLASS TsgcGoogleCalendarConferenceSolutionKey;
class DELPHICLASS TsgcGoogleCalendarConferenceStatus;
class DELPHICLASS TsgcGoogleCalendarCreateRequest;
class DELPHICLASS TsgcGoogleCalendarConferenceEntryPointItem;
class DELPHICLASS TsgcGoogleCalendarConferenceSolution;
class DELPHICLASS TsgcGoogleCalendarConferenceData;
class DELPHICLASS TsgcGoogleCalendarGadget;
class DELPHICLASS TsgcGoogleCalendarRemindersOverridesItem;
class DELPHICLASS TsgcGoogleCalendarRemindersOverrides;
class DELPHICLASS TsgcGoogleCalendarReminders;
class DELPHICLASS TsgcGoogleCalendarSource;
class DELPHICLASS TsgcGoogleCalendarAttachmentItem;
class DELPHICLASS TsgcGoogleCalendarResource_Event;
class DELPHICLASS TsgcHTTP_Google_Cloud_Calendar_API_Client;
class DELPHICLASS TsgcGoogleCalendarEventItem;
class DELPHICLASS TsgcGoogleCalendarEventList;
class DELPHICLASS TsgcGoogleCalendarItem;
class DELPHICLASS TsgcGoogleCalendarList;
class DELPHICLASS TsgcHTTP_Google_Cloud_Calendar_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcGoogleCalendarRole : unsigned char { gcrNone, gcrFreeBusyReader, gcrReader, gcrWrite, gcrOwner };

enum DECLSPEC_DENUM TsgcGoogleCalendarScopeType : unsigned char { gcstDefault, gcstUser, gcstGroup, gcstDomain };

enum DECLSPEC_DENUM TsgcGoogleCalenarNotificationType : unsigned char { gcntEventCreation, gcntEventChange, gcntEventCancellation, gcntEventResponse, gcntEventAgenda };

enum DECLSPEC_DENUM TsgcGoogleCalendarEventListOrdered : unsigned char { gceoNone, gceoStartTime, gceoUpdated };

enum DECLSPEC_DENUM TsgcGoogleCalendarEventVisibility : unsigned char { gcevDefault, gcevPublic, gcevPrivate, gcevConfidential };

enum DECLSPEC_DENUM TsgcGoogleCalendarEventStatus : unsigned char { gcesConfirmed, gcesTentative, gcesCancelled };

enum DECLSPEC_DENUM TsgcGoogleCalendarReminderMethod : unsigned char { gcrmEmail, gcrmPopup };

typedef void __fastcall (__closure *TsgcGoogleCalendarEvent)(System::TObject* Sender, TsgcGoogleCalendarItem* const aCalendar, bool &Accept);

typedef void __fastcall (__closure *TsgcGoogleCalendarGetEventEvent)(System::TObject* Sender, TsgcGoogleCalendarItem* const aCalendar, TsgcGoogleCalendarEventItem* const aEvent, bool &Accept);

typedef void __fastcall (__closure *TsgcGoogleCalendarErrorEvent)(System::TObject* Sender, TsgcGoogleCalendarError* const aError);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarScopes : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FCalendarAddOn;
	bool FCalendarsReadOnly;
	bool FCalendarsReadWrite;
	bool FEventsReadOnly;
	bool FEventsReadWrite;
	bool FSettings;
	
public:
	__fastcall virtual TsgcGoogleCalendarScopes();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool CalendarAddOn = {read=FCalendarAddOn, write=FCalendarAddOn, nodefault};
	__property bool CalendarsReadOnly = {read=FCalendarsReadOnly, write=FCalendarsReadOnly, nodefault};
	__property bool CalendarsReadWrite = {read=FCalendarsReadWrite, write=FCalendarsReadWrite, nodefault};
	__property bool EventsReadOnly = {read=FEventsReadOnly, write=FEventsReadOnly, nodefault};
	__property bool EventsReadWrite = {read=FEventsReadWrite, write=FEventsReadWrite, nodefault};
	__property bool Settings = {read=FSettings, write=FSettings, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarScopes() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarErrorItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FDomain;
	System::UnicodeString FLocation;
	System::UnicodeString FLocationType;
	System::UnicodeString FReason;
	System::UnicodeString F_Message;
	
public:
	__property System::UnicodeString Domain = {read=FDomain, write=FDomain};
	__property System::UnicodeString Location = {read=FLocation, write=FLocation};
	__property System::UnicodeString LocationType = {read=FLocationType, write=FLocationType};
	__property System::UnicodeString Reason = {read=FReason, write=FReason};
	__property System::UnicodeString _Message = {read=F_Message, write=F_Message};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarErrorItem() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarErrorItem() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<TsgcGoogleCalendarErrorItem*> TsgcGoogleCalendarErrors;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarError : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FCode;
	TsgcGoogleCalendarErrors FErrors;
	System::UnicodeString FDescription;
	
public:
	__property int Code = {read=FCode, write=FCode, nodefault};
	__property TsgcGoogleCalendarErrors Errors = {read=FErrors, write=FErrors};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarError() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarError() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarJSON : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	Sgcjson::_di_IsgcJSON __fastcall GetObjectJSON(Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON());
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON};
	virtual System::UnicodeString __fastcall JSONText();
	virtual System::Variant __fastcall DoReadJSONValue(const System::UnicodeString aField, Sgcjson::_di_IsgcObjectJSON aJSONObject = Sgcjson::_di_IsgcObjectJSON())/* overload */;
	virtual System::Variant __fastcall DoReadJSONValue(int aItem, Sgcjson::TsgcObjectJSON* aJSONObject = (Sgcjson::TsgcObjectJSON*)(0x0))/* overload */;
	virtual void __fastcall DoRead();
	void __fastcall Read(const System::UnicodeString aValue);
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcGoogleCalendarJSON() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarJSON() { }
	
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarWatch : public TsgcGoogleCalendarJSON
{
	typedef TsgcGoogleCalendarJSON inherited;
	
private:
	System::UnicodeString Faddress;
	System::UnicodeString Ftoken;
	System::UnicodeString Fttl;
	System::UnicodeString F_type;
	
protected:
	virtual System::UnicodeString __fastcall JSONText();
	
public:
	__fastcall virtual TsgcGoogleCalendarWatch();
	__property System::UnicodeString address = {read=Faddress, write=Faddress};
	__property System::UnicodeString token = {read=Ftoken, write=Ftoken};
	__property System::UnicodeString ttl = {read=Fttl, write=Fttl};
	__property System::UnicodeString _type = {read=F_type, write=F_type};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarWatch() { }
	
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarResource : public TsgcGoogleCalendarJSON
{
	typedef TsgcGoogleCalendarJSON inherited;
	
private:
	System::UnicodeString FEtag;
	System::UnicodeString FKind;
	
protected:
	virtual void __fastcall DoJSONBody() = 0 ;
	virtual System::UnicodeString __fastcall JSONText();
	virtual void __fastcall DoRead();
	
public:
	__fastcall virtual TsgcGoogleCalendarResource();
	__fastcall virtual ~TsgcGoogleCalendarResource();
	__property System::UnicodeString Etag = {read=FEtag, write=FEtag};
	__property System::UnicodeString Kind = {read=FKind, write=FKind};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarScope : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FValue;
	TsgcGoogleCalendarScopeType F_type;
	
public:
	__property System::UnicodeString Value = {read=FValue, write=FValue};
	__property TsgcGoogleCalendarScopeType _type = {read=F_type, write=F_type, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarScope() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarScope() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarResource_ACL : public TsgcGoogleCalendarResource
{
	typedef TsgcGoogleCalendarResource inherited;
	
private:
	TsgcGoogleCalendarRole FRole;
	TsgcGoogleCalendarScope* FScope;
	TsgcGoogleCalendarScope* __fastcall GetScope();
	
protected:
	virtual void __fastcall DoJSONBody();
	
public:
	__fastcall virtual TsgcGoogleCalendarResource_ACL();
	__fastcall virtual ~TsgcGoogleCalendarResource_ACL();
	__property TsgcGoogleCalendarRole Role = {read=FRole, write=FRole, nodefault};
	__property TsgcGoogleCalendarScope* Scope = {read=GetScope, write=FScope};
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarDefaultReminderItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcGoogleCalendarReminderMethod FMethod;
	int FMinutes;
	
public:
	__property TsgcGoogleCalendarReminderMethod Method = {read=FMethod, write=FMethod, nodefault};
	__property int Minutes = {read=FMinutes, write=FMinutes, nodefault};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcGoogleCalendarDefaultReminderItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarDefaultReminderItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarDefaultReminders : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcGoogleCalendarDefaultReminders() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarDefaultReminders() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarNotificationItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FMethod;
	TsgcGoogleCalenarNotificationType F_type;
	
public:
	__fastcall virtual TsgcGoogleCalendarNotificationItem();
	__property System::UnicodeString Method = {read=FMethod, write=FMethod};
	__property TsgcGoogleCalenarNotificationType _type = {read=F_type, write=F_type, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarNotificationItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarNotificationSettings : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcGoogleCalendarNotificationSettings() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarNotificationSettings() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarConferenceProperties : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* FAllowedConferenceSolutionTypes;
	System::Classes::TStringList* __fastcall GetAllowedConferenceSolutionTypes();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarConferenceProperties();
	__property System::Classes::TStringList* AllowedConferenceSolutionTypes = {read=GetAllowedConferenceSolutionTypes, write=FAllowedConferenceSolutionTypes};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarConferenceProperties() : System::TObject() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarResource_CalendarList : public TsgcGoogleCalendarResource
{
	typedef TsgcGoogleCalendarResource inherited;
	
private:
	TsgcGoogleCalendarRole FAccessRole;
	System::UnicodeString FBackgroundColor;
	System::UnicodeString FColorId;
	TsgcGoogleCalendarConferenceProperties* FConferenceProperties;
	TsgcGoogleCalendarDefaultReminders* FDefaultReminders;
	bool FDeleted;
	System::UnicodeString FDescription;
	System::UnicodeString FForegroundColor;
	bool FHidden;
	System::UnicodeString FLocation;
	TsgcGoogleCalendarNotificationSettings* FNotificationSettings;
	bool FPrimary;
	bool FSelected;
	System::UnicodeString FSummary;
	System::UnicodeString FSummaryOverride;
	System::UnicodeString FTimeZone;
	TsgcGoogleCalendarConferenceProperties* __fastcall GetConferenceProperties();
	TsgcGoogleCalendarDefaultReminders* __fastcall GetDefaultReminders();
	TsgcGoogleCalendarNotificationSettings* __fastcall GetNotificationSettings();
	
protected:
	virtual void __fastcall DoJSONBody();
	virtual void __fastcall DoRead();
	
public:
	__fastcall virtual TsgcGoogleCalendarResource_CalendarList();
	__fastcall virtual ~TsgcGoogleCalendarResource_CalendarList();
	__property TsgcGoogleCalendarRole AccessRole = {read=FAccessRole, write=FAccessRole, nodefault};
	__property System::UnicodeString BackgroundColor = {read=FBackgroundColor, write=FBackgroundColor};
	__property System::UnicodeString ColorId = {read=FColorId, write=FColorId};
	__property TsgcGoogleCalendarConferenceProperties* ConferenceProperties = {read=GetConferenceProperties, write=FConferenceProperties};
	__property TsgcGoogleCalendarDefaultReminders* DefaultReminders = {read=GetDefaultReminders, write=FDefaultReminders};
	__property bool Deleted = {read=FDeleted, write=FDeleted, nodefault};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
	__property System::UnicodeString ForegroundColor = {read=FForegroundColor, write=FForegroundColor};
	__property bool Hidden = {read=FHidden, write=FHidden, nodefault};
	__property System::UnicodeString Location = {read=FLocation, write=FLocation};
	__property TsgcGoogleCalendarNotificationSettings* NotificationSettings = {read=GetNotificationSettings, write=FNotificationSettings};
	__property bool Primary = {read=FPrimary, write=FPrimary, nodefault};
	__property bool Selected = {read=FSelected, write=FSelected, nodefault};
	__property System::UnicodeString Summary = {read=FSummary, write=FSummary};
	__property System::UnicodeString SummaryOverride = {read=FSummaryOverride, write=FSummaryOverride};
	__property System::UnicodeString TimeZone = {read=FTimeZone, write=FTimeZone};
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarResource_Calendar : public TsgcGoogleCalendarResource
{
	typedef TsgcGoogleCalendarResource inherited;
	
private:
	TsgcGoogleCalendarConferenceProperties* FConferenceProperties;
	System::UnicodeString FDescription;
	System::UnicodeString FLocation;
	System::UnicodeString FSummary;
	System::UnicodeString FTimeZone;
	TsgcGoogleCalendarConferenceProperties* __fastcall GetConferenceProperties();
	
protected:
	virtual void __fastcall DoJSONBody();
	virtual void __fastcall DoRead();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarResource_Calendar();
	__property TsgcGoogleCalendarConferenceProperties* ConferenceProperties = {read=GetConferenceProperties, write=FConferenceProperties};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
	__property System::UnicodeString Location = {read=FLocation, write=FLocation};
	__property System::UnicodeString Summary = {read=FSummary, write=FSummary};
	__property System::UnicodeString TimeZone = {read=FTimeZone, write=FTimeZone};
public:
	/* TsgcGoogleCalendarResource.Create */ inline __fastcall virtual TsgcGoogleCalendarResource_Calendar() : TsgcGoogleCalendarResource() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarPerson : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FDisplayName;
	System::UnicodeString FEmail;
	System::UnicodeString Fid;
	bool F_self;
	
public:
	__property System::UnicodeString DisplayName = {read=FDisplayName, write=FDisplayName};
	__property System::UnicodeString Email = {read=FEmail, write=FEmail};
	__property System::UnicodeString id = {read=Fid, write=Fid};
	__property bool _self = {read=F_self, write=F_self, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarPerson() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarPerson() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarCreator : public TsgcGoogleCalendarPerson
{
	typedef TsgcGoogleCalendarPerson inherited;
	
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarCreator() : TsgcGoogleCalendarPerson() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarCreator() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarOrganizer : public TsgcGoogleCalendarPerson
{
	typedef TsgcGoogleCalendarPerson inherited;
	
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarOrganizer() : TsgcGoogleCalendarPerson() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarOrganizer() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarDateTime : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TDateTime FDate;
	System::TDateTime FDateTime;
	System::UnicodeString FTimeZone;
	
public:
	__fastcall virtual TsgcGoogleCalendarDateTime();
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::TDateTime DateTime = {read=FDateTime, write=FDateTime};
	__property System::UnicodeString TimeZone = {read=FTimeZone, write=FTimeZone};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarDateTime() { }
	
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarStart : public TsgcGoogleCalendarDateTime
{
	typedef TsgcGoogleCalendarDateTime inherited;
	
public:
	/* TsgcGoogleCalendarDateTime.Create */ inline __fastcall virtual TsgcGoogleCalendarStart() : TsgcGoogleCalendarDateTime() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarStart() { }
	
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarEnd : public TsgcGoogleCalendarDateTime
{
	typedef TsgcGoogleCalendarDateTime inherited;
	
public:
	/* TsgcGoogleCalendarDateTime.Create */ inline __fastcall virtual TsgcGoogleCalendarEnd() : TsgcGoogleCalendarDateTime() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarEnd() { }
	
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarOriginalStart : public TsgcGoogleCalendarDateTime
{
	typedef TsgcGoogleCalendarDateTime inherited;
	
public:
	/* TsgcGoogleCalendarDateTime.Create */ inline __fastcall virtual TsgcGoogleCalendarOriginalStart() : TsgcGoogleCalendarDateTime() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarOriginalStart() { }
	
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarAttendeeItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	int FAdditionalGuests;
	System::UnicodeString FComment;
	System::UnicodeString FDisplayName;
	System::UnicodeString FEmail;
	bool FOptional;
	bool FOrganizer;
	bool FResource;
	System::UnicodeString FResponseStatus;
	bool F_self;
	
public:
	__property int AdditionalGuests = {read=FAdditionalGuests, write=FAdditionalGuests, nodefault};
	__property System::UnicodeString Comment = {read=FComment, write=FComment};
	__property System::UnicodeString DisplayName = {read=FDisplayName, write=FDisplayName};
	__property System::UnicodeString Email = {read=FEmail, write=FEmail};
	__property bool Optional = {read=FOptional, write=FOptional, nodefault};
	__property bool Organizer = {read=FOrganizer, write=FOrganizer, nodefault};
	__property bool Resource = {read=FResource, write=FResource, nodefault};
	__property System::UnicodeString ResponseStatus = {read=FResponseStatus, write=FResponseStatus};
	__property bool _self = {read=F_self, write=F_self, nodefault};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcGoogleCalendarAttendeeItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarAttendeeItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarAttendees : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcGoogleCalendarAttendees() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarAttendees() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarExtendedProperties : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* F_Private;
	System::Classes::TStringList* F_Shared;
	System::Classes::TStringList* __fastcall Get_Private();
	System::Classes::TStringList* __fastcall Get_Shared();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarExtendedProperties();
	__property System::Classes::TStringList* _Private = {read=Get_Private, write=F_Private};
	__property System::Classes::TStringList* _Shared = {read=Get_Shared, write=F_Shared};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarExtendedProperties() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarConferenceSolutionKey : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString F_type;
	
public:
	__property System::UnicodeString _type = {read=F_type, write=F_type};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarConferenceSolutionKey() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarConferenceSolutionKey() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarConferenceStatus : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FStatusCode;
	
public:
	__property System::UnicodeString StatusCode = {read=FStatusCode, write=FStatusCode};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarConferenceStatus() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarConferenceStatus() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarCreateRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcGoogleCalendarConferenceSolutionKey* FConferenceSolutionKey;
	System::UnicodeString FRequestId;
	TsgcGoogleCalendarConferenceStatus* FStatus;
	TsgcGoogleCalendarConferenceSolutionKey* __fastcall GetConferenceSolutionKey();
	TsgcGoogleCalendarConferenceStatus* __fastcall GetStatus();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarCreateRequest();
	__property TsgcGoogleCalendarConferenceSolutionKey* ConferenceSolutionKey = {read=GetConferenceSolutionKey, write=FConferenceSolutionKey};
	__property System::UnicodeString RequestId = {read=FRequestId, write=FRequestId};
	__property TsgcGoogleCalendarConferenceStatus* Status = {read=GetStatus, write=FStatus};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarCreateRequest() : System::TObject() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarConferenceEntryPointItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FAccessCode;
	System::UnicodeString FEntryPointType;
	System::UnicodeString FMeetingCode;
	System::UnicodeString FPasscode;
	System::UnicodeString FPassword;
	System::UnicodeString FPin;
	System::UnicodeString FUri;
	System::UnicodeString F_Label;
	
public:
	__property System::UnicodeString AccessCode = {read=FAccessCode, write=FAccessCode};
	__property System::UnicodeString EntryPointType = {read=FEntryPointType, write=FEntryPointType};
	__property System::UnicodeString MeetingCode = {read=FMeetingCode, write=FMeetingCode};
	__property System::UnicodeString Passcode = {read=FPasscode, write=FPasscode};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString Pin = {read=FPin, write=FPin};
	__property System::UnicodeString Uri = {read=FUri, write=FUri};
	__property System::UnicodeString _Label = {read=F_Label, write=F_Label};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcGoogleCalendarConferenceEntryPointItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarConferenceEntryPointItem() { }
	
};


typedef Sgcwebsocket_classes_queues::TsgcQueue TsgcGoogleCalendarConferenceEntryPoints;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarConferenceSolution : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FIconURI;
	TsgcGoogleCalendarConferenceSolutionKey* FKey;
	System::UnicodeString F_Name;
	TsgcGoogleCalendarConferenceSolutionKey* __fastcall GetKey();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarConferenceSolution();
	__property System::UnicodeString IconURI = {read=FIconURI, write=FIconURI};
	__property TsgcGoogleCalendarConferenceSolutionKey* Key = {read=GetKey, write=FKey};
	__property System::UnicodeString _Name = {read=F_Name, write=F_Name};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarConferenceSolution() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarConferenceData : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FConferenceId;
	TsgcGoogleCalendarConferenceSolution* FConferenceSolution;
	TsgcGoogleCalendarCreateRequest* FCreateRequest;
	Sgcwebsocket_classes_queues::TsgcQueue* FEntryPoints;
	System::UnicodeString FNotes;
	System::UnicodeString FSignature;
	TsgcGoogleCalendarConferenceSolution* __fastcall GetConferenceSolution();
	TsgcGoogleCalendarCreateRequest* __fastcall GetCreateRequest();
	Sgcwebsocket_classes_queues::TsgcQueue* __fastcall GetEntryPoints();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarConferenceData();
	__property System::UnicodeString ConferenceId = {read=FConferenceId, write=FConferenceId};
	__property TsgcGoogleCalendarConferenceSolution* ConferenceSolution = {read=GetConferenceSolution, write=FConferenceSolution};
	__property TsgcGoogleCalendarCreateRequest* CreateRequest = {read=GetCreateRequest, write=FCreateRequest};
	__property Sgcwebsocket_classes_queues::TsgcQueue* EntryPoints = {read=GetEntryPoints, write=FEntryPoints};
	__property System::UnicodeString Notes = {read=FNotes, write=FNotes};
	__property System::UnicodeString Signature = {read=FSignature, write=FSignature};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarConferenceData() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarGadget : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FDisplay;
	int FHeight;
	System::UnicodeString FIconLink;
	System::UnicodeString FLink;
	System::Classes::TStringList* FPreferences;
	System::UnicodeString FTitle;
	int FWidth;
	System::UnicodeString F_type;
	System::Classes::TStringList* __fastcall GetPreferences();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarGadget();
	__property System::UnicodeString Display = {read=FDisplay, write=FDisplay};
	__property int Height = {read=FHeight, write=FHeight, nodefault};
	__property System::UnicodeString IconLink = {read=FIconLink, write=FIconLink};
	__property System::UnicodeString Link = {read=FLink, write=FLink};
	__property System::Classes::TStringList* Preferences = {read=GetPreferences, write=FPreferences};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
	__property int Width = {read=FWidth, write=FWidth, nodefault};
	__property System::UnicodeString _type = {read=F_type, write=F_type};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarGadget() : System::TObject() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarRemindersOverridesItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcGoogleCalendarReminderMethod FMethod;
	int FMinutes;
	
public:
	__property TsgcGoogleCalendarReminderMethod Method = {read=FMethod, write=FMethod, nodefault};
	__property int Minutes = {read=FMinutes, write=FMinutes, nodefault};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcGoogleCalendarRemindersOverridesItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarRemindersOverridesItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarRemindersOverrides : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcGoogleCalendarRemindersOverrides() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarRemindersOverrides() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarReminders : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcGoogleCalendarRemindersOverrides* FOverrides;
	bool FUseDefault;
	TsgcGoogleCalendarRemindersOverrides* __fastcall GetOverrides();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarReminders();
	__property TsgcGoogleCalendarRemindersOverrides* Overrides = {read=GetOverrides, write=FOverrides};
	__property bool UseDefault = {read=FUseDefault, write=FUseDefault, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarReminders() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarSource : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FTitle;
	System::UnicodeString FURL;
	
public:
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
	__property System::UnicodeString URL = {read=FURL, write=FURL};
public:
	/* TObject.Create */ inline __fastcall TsgcGoogleCalendarSource() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarSource() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarAttachmentItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FFileId;
	System::UnicodeString FFileUrl;
	System::UnicodeString FIconLink;
	System::UnicodeString FMimeType;
	System::UnicodeString FTitle;
	
public:
	__property System::UnicodeString FileId = {read=FFileId, write=FFileId};
	__property System::UnicodeString FileUrl = {read=FFileUrl, write=FFileUrl};
	__property System::UnicodeString IconLink = {read=FIconLink, write=FIconLink};
	__property System::UnicodeString MimeType = {read=FMimeType, write=FMimeType};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcGoogleCalendarAttachmentItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarAttachmentItem() { }
	
};


typedef Sgcwebsocket_classes_queues::TsgcQueue TagcGoogleCalendarAttachments;

class PASCALIMPLEMENTATION TsgcGoogleCalendarResource_Event : public TsgcGoogleCalendarResource
{
	typedef TsgcGoogleCalendarResource inherited;
	
private:
	bool FAnyoneCanAddSelf;
	Sgcwebsocket_classes_queues::TsgcQueue* FAttachments;
	TsgcGoogleCalendarAttendees* FAttendees;
	bool FAttendeesOmitted;
	System::UnicodeString FColorId;
	TsgcGoogleCalendarConferenceData* FConferenceData;
	System::TDateTime FCreated;
	TsgcGoogleCalendarCreator* FCreator;
	System::UnicodeString FDescription;
	bool FEndTimeUnspecified;
	TsgcGoogleCalendarExtendedProperties* FExtendedProperties;
	TsgcGoogleCalendarGadget* FGadget;
	bool FGuestsCanInviteOthers;
	bool FGuestsCanModify;
	bool FGuestsCanSeeOtherGuests;
	System::UnicodeString FHangoutLink;
	System::UnicodeString FHtmllink;
	System::UnicodeString FiCalUID;
	System::UnicodeString FLocation;
	bool FLocked;
	TsgcGoogleCalendarOrganizer* FOrganizer;
	TsgcGoogleCalendarOriginalStart* FOriginalStartTime;
	bool FPrivateCopy;
	System::Classes::TStringList* FRecurrence;
	System::UnicodeString FRecurringEventId;
	TsgcGoogleCalendarReminders* FReminders;
	int FSequence;
	TsgcGoogleCalendarSource* FSource;
	TsgcGoogleCalendarStart* FStart;
	TsgcGoogleCalendarEventStatus FStatus;
	System::UnicodeString FSummary;
	System::UnicodeString FTransparency;
	System::TDateTime FUpdated;
	TsgcGoogleCalendarEventVisibility FVisibility;
	TsgcGoogleCalendarEnd* F_End;
	Sgcwebsocket_classes_queues::TsgcQueue* __fastcall GetAttachments();
	TsgcGoogleCalendarAttendees* __fastcall GetAttendees();
	TsgcGoogleCalendarConferenceData* __fastcall GetConferenceData();
	TsgcGoogleCalendarCreator* __fastcall GetCreator();
	TsgcGoogleCalendarExtendedProperties* __fastcall GetExtendedProperties();
	TsgcGoogleCalendarGadget* __fastcall GetGadget();
	TsgcGoogleCalendarOrganizer* __fastcall GetOrganizer();
	TsgcGoogleCalendarOriginalStart* __fastcall GetOriginalStartTime();
	System::Classes::TStringList* __fastcall GetRecurrence();
	TsgcGoogleCalendarReminders* __fastcall GetReminders();
	TsgcGoogleCalendarSource* __fastcall GetSource();
	TsgcGoogleCalendarStart* __fastcall GetStart();
	TsgcGoogleCalendarEnd* __fastcall Get_End();
	
protected:
	virtual void __fastcall DoJSONBody();
	virtual void __fastcall DoRead();
	
public:
	__fastcall virtual TsgcGoogleCalendarResource_Event();
	__fastcall virtual ~TsgcGoogleCalendarResource_Event();
	__property bool AnyoneCanAddSelf = {read=FAnyoneCanAddSelf, write=FAnyoneCanAddSelf, nodefault};
	__property Sgcwebsocket_classes_queues::TsgcQueue* Attachments = {read=GetAttachments, write=FAttachments};
	__property TsgcGoogleCalendarAttendees* Attendees = {read=GetAttendees, write=FAttendees};
	__property bool AttendeesOmitted = {read=FAttendeesOmitted, write=FAttendeesOmitted, nodefault};
	__property System::UnicodeString ColorId = {read=FColorId, write=FColorId};
	__property TsgcGoogleCalendarConferenceData* ConferenceData = {read=GetConferenceData, write=FConferenceData};
	__property System::TDateTime Created = {read=FCreated, write=FCreated};
	__property TsgcGoogleCalendarCreator* Creator = {read=GetCreator, write=FCreator};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
	__property bool EndTimeUnspecified = {read=FEndTimeUnspecified, write=FEndTimeUnspecified, nodefault};
	__property TsgcGoogleCalendarExtendedProperties* ExtendedProperties = {read=GetExtendedProperties, write=FExtendedProperties};
	__property TsgcGoogleCalendarGadget* Gadget = {read=GetGadget, write=FGadget};
	__property bool GuestsCanInviteOthers = {read=FGuestsCanInviteOthers, write=FGuestsCanInviteOthers, nodefault};
	__property bool GuestsCanModify = {read=FGuestsCanModify, write=FGuestsCanModify, nodefault};
	__property bool GuestsCanSeeOtherGuests = {read=FGuestsCanSeeOtherGuests, write=FGuestsCanSeeOtherGuests, nodefault};
	__property System::UnicodeString HangoutLink = {read=FHangoutLink, write=FHangoutLink};
	__property System::UnicodeString Htmllink = {read=FHtmllink, write=FHtmllink};
	__property System::UnicodeString iCalUID = {read=FiCalUID, write=FiCalUID};
	__property System::UnicodeString Location = {read=FLocation, write=FLocation};
	__property bool Locked = {read=FLocked, write=FLocked, nodefault};
	__property TsgcGoogleCalendarOrganizer* Organizer = {read=GetOrganizer, write=FOrganizer};
	__property TsgcGoogleCalendarOriginalStart* OriginalStartTime = {read=GetOriginalStartTime, write=FOriginalStartTime};
	__property bool PrivateCopy = {read=FPrivateCopy, write=FPrivateCopy, nodefault};
	__property System::Classes::TStringList* Recurrence = {read=GetRecurrence, write=FRecurrence};
	__property System::UnicodeString RecurringEventId = {read=FRecurringEventId, write=FRecurringEventId};
	__property TsgcGoogleCalendarReminders* Reminders = {read=GetReminders, write=FReminders};
	__property int Sequence = {read=FSequence, write=FSequence, nodefault};
	__property TsgcGoogleCalendarSource* Source = {read=GetSource, write=FSource};
	__property TsgcGoogleCalendarStart* Start = {read=GetStart, write=FStart};
	__property TsgcGoogleCalendarEventStatus Status = {read=FStatus, write=FStatus, nodefault};
	__property System::UnicodeString Summary = {read=FSummary, write=FSummary};
	__property System::UnicodeString Transparency = {read=FTransparency, write=FTransparency};
	__property System::TDateTime Updated = {read=FUpdated, write=FUpdated};
	__property TsgcGoogleCalendarEventVisibility Visibility = {read=FVisibility, write=FVisibility, nodefault};
	__property TsgcGoogleCalendarEnd* _End = {read=Get_End, write=F_End};
};


class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_Calendar_API_Client : public Sgchttp_google_cloud::TsgcHTTP_Google_Cloud_Client
{
	typedef Sgchttp_google_cloud::TsgcHTTP_Google_Cloud_Client inherited;
	
protected:
	virtual System::UnicodeString __fastcall DoGetScope();
	System::UnicodeString __fastcall FormatAndEncode(const System::UnicodeString aFormat, const System::TVarRec *Args, const int Args_High);
	
private:
	TsgcGoogleCalendarScopes* FScopes;
	void __fastcall SetScopes(TsgcGoogleCalendarScopes* const Value);
	
public:
	__property TsgcGoogleCalendarScopes* Scopes = {read=FScopes, write=SetScopes};
	__fastcall virtual TsgcHTTP_Google_Cloud_Calendar_API_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_Google_Cloud_Calendar_API_Client();
	System::UnicodeString __fastcall ACL_Delete(const System::UnicodeString aCalendarId, const System::UnicodeString aRuleId);
	System::UnicodeString __fastcall ACL_Get(const System::UnicodeString aCalendarId, const System::UnicodeString aRuleId);
	System::UnicodeString __fastcall ACL_Insert(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_ACL* const aResource, bool aSendNotifications = true);
	System::UnicodeString __fastcall ACL_List(const System::UnicodeString aCalendarId, int aMaxResults = 0x64, System::UnicodeString aPageToken = System::UnicodeString(), bool aShowDeleted = false, System::UnicodeString aSyncToken = System::UnicodeString());
	System::UnicodeString __fastcall ACL_Patch(const System::UnicodeString aCalendarId, const System::UnicodeString aRuleId, TsgcGoogleCalendarResource_ACL* const aResource, bool aSendNotifications = true);
	System::UnicodeString __fastcall ACL_Update(const System::UnicodeString aCalendarId, const System::UnicodeString aRuleId, TsgcGoogleCalendarResource_ACL* const aResource, bool aSendNotifications = true);
	System::UnicodeString __fastcall ACL_Watch(const System::UnicodeString aCalendarId, TsgcGoogleCalendarWatch* const aWatch);
	System::UnicodeString __fastcall CalendarList_Delete(const System::UnicodeString aCalendarId);
	System::UnicodeString __fastcall CalendarList_Get(const System::UnicodeString aCalendarId);
	System::UnicodeString __fastcall CalendarList_Insert(TsgcGoogleCalendarResource_CalendarList* const aResource, bool aColorRgbFormat = false);
	System::UnicodeString __fastcall CalendarList_List(int aMaxResults = 0xfa, System::UnicodeString aMinAccessRole = System::UnicodeString(), System::UnicodeString aPageToken = System::UnicodeString(), bool aShowDeleted = false, bool aShowHidden = false, System::UnicodeString aSyncToken = System::UnicodeString());
	System::UnicodeString __fastcall CalendarList_Patch(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_CalendarList* const aResource, bool aColorRgbFormat = false);
	System::UnicodeString __fastcall CalendarList_Update(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_CalendarList* const aResource, bool aColorRgbFormat = false);
	System::UnicodeString __fastcall CalendarList_Watch(TsgcGoogleCalendarWatch* const aWatch);
	System::UnicodeString __fastcall Calendar_Clear(const System::UnicodeString aCalendarId);
	System::UnicodeString __fastcall Calendar_Delete(const System::UnicodeString aCalendarId);
	System::UnicodeString __fastcall Calendar_Get(const System::UnicodeString aCalendarId);
	System::UnicodeString __fastcall Calendar_Insert(TsgcGoogleCalendarResource_Calendar* const aResource);
	System::UnicodeString __fastcall Calendar_Patch(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_Calendar* const aResource);
	System::UnicodeString __fastcall Calendar_Update(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_Calendar* const aResource);
	System::UnicodeString __fastcall Channel_Stop(const System::UnicodeString aId, const System::UnicodeString aResourceId, const System::UnicodeString aToken = System::UnicodeString());
	System::UnicodeString __fastcall Color_Get();
	System::UnicodeString __fastcall Event_Delete(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId, bool aSendNotifications = false, const System::UnicodeString aSendUpdates = L"all");
	System::UnicodeString __fastcall Event_Get(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId, int aMaxAttendees = 0x0, const System::UnicodeString aTimeZone = System::UnicodeString());
	System::UnicodeString __fastcall Event_Import(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_Event* const aResource, int aConferenceDataVersion = 0x0, bool aSupportsAttachments = false);
	System::UnicodeString __fastcall Event_Insert(const System::UnicodeString aCalendarId, TsgcGoogleCalendarResource_Event* const aResource, int aConferenceDataVersion = 0x0, int aMaxAttendees = 0x0, const System::UnicodeString aSendUpdates = L"all", bool aSupportsAttachments = false);
	System::UnicodeString __fastcall Event_Instances(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId, int aMaxAttendees = 0x0, int aMaxResults = 0xfa, const System::UnicodeString aOriginalStart = System::UnicodeString(), const System::UnicodeString aPageToken = System::UnicodeString(), bool aShowDeleted = false, System::TDateTime aTimeMax = 0.000000E+00, System::TDateTime aTimeMin = 0.000000E+00, const System::UnicodeString aTimeZone = System::UnicodeString());
	System::UnicodeString __fastcall Event_List(const System::UnicodeString aCalendarId, System::UnicodeString aICalUID = System::UnicodeString(), int aMaxAttendees = 0x0, int aMaxResults = 0xfa, const System::UnicodeString aOrderBy = System::UnicodeString(), const System::UnicodeString aPageToken = System::UnicodeString(), const System::UnicodeString aPrivateExtendedProperty = System::UnicodeString(), const System::UnicodeString aQ = System::UnicodeString(), const System::UnicodeString aSharedExtendedProperty = System::UnicodeString(), bool aShowDeleted = false, bool aShowHiddenInvitations = false, bool aSingleEvents = false, System::UnicodeString aSyncToken = System::UnicodeString(), System::TDateTime aTimeMax = 0.000000E+00, System::TDateTime aTimeMin = 0.000000E+00, const System::UnicodeString aTimeZone = System::UnicodeString(),
		System::TDateTime aUpdatedMin = 0.000000E+00);
	System::UnicodeString __fastcall Event_Move(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId, const System::UnicodeString aDestination, const System::UnicodeString aSendUpdates = L"all");
	System::UnicodeString __fastcall Event_Patch(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId, TsgcGoogleCalendarResource_Event* const aResource, int aConferenceDataVersion = 0x0, int aMaxAttendees = 0x0, const System::UnicodeString aSendUpdates = L"all", bool aSupportsAttachments = false);
	System::UnicodeString __fastcall Event_QuickAdd(const System::UnicodeString aCalendarId, const System::UnicodeString aText, const System::UnicodeString aSendUpdates = L"all");
	System::UnicodeString __fastcall Event_Update(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId, TsgcGoogleCalendarResource_Event* const aResource, int aConferenceDataVersion = 0x0, int aMaxAttendees = 0x0, const System::UnicodeString aSendUpdates = L"all", bool aSupportsAttachments = false);
	System::UnicodeString __fastcall Event_Watch(const System::UnicodeString aCalendarId, TsgcGoogleCalendarWatch* const aWatch);
	System::UnicodeString __fastcall Freebusy_Query(System::Classes::TStrings* const aItems, System::TDateTime aTimeMin, System::TDateTime aTimeMax, const System::UnicodeString aTimeZone = System::UnicodeString(), int aGroupExpansionMax = 0x0, int aCalendarExpansionMax = 0x0);
	System::UnicodeString __fastcall Settings_Get(const System::UnicodeString aSetting);
	System::UnicodeString __fastcall Settings_List(int aMaxResults = 0x0, const System::UnicodeString aPageToken = System::UnicodeString(), const System::UnicodeString aSyncToken = System::UnicodeString());
	System::UnicodeString __fastcall Settings_Watch(TsgcGoogleCalendarWatch* const aWatch);
};


class PASCALIMPLEMENTATION TsgcGoogleCalendarEventItem : public TsgcGoogleCalendarResource_Event
{
	typedef TsgcGoogleCalendarResource_Event inherited;
	
public:
	/* TsgcGoogleCalendarResource_Event.Create */ inline __fastcall virtual TsgcGoogleCalendarEventItem() : TsgcGoogleCalendarResource_Event() { }
	/* TsgcGoogleCalendarResource_Event.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarEventItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarEventList : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	TsgcGoogleCalendarEventItem* operator[](int Index) { return this->Event[Index]; }
	
public:
	__fastcall virtual TsgcGoogleCalendarEventList();
	void __fastcall AddEvent(TsgcGoogleCalendarEventItem* const aEvent);
	TsgcGoogleCalendarEventItem* __fastcall GetEvent(const System::UnicodeString aId);
	
private:
	System::UnicodeString FNextSyncToken;
	TsgcGoogleCalendarEventItem* __fastcall GetEventItem(int Index);
	
public:
	__property TsgcGoogleCalendarEventItem* Event[int Index] = {read=GetEventItem/*, default*/};
	__property System::UnicodeString NextSyncToken = {read=FNextSyncToken, write=FNextSyncToken};
public:
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarEventList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcGoogleCalendarItem : public TsgcGoogleCalendarResource_CalendarList
{
	typedef TsgcGoogleCalendarResource_CalendarList inherited;
	
private:
	TsgcGoogleCalendarEventList* FEvents;
	TsgcGoogleCalendarEventList* __fastcall GetEvents();
	
public:
	__fastcall virtual ~TsgcGoogleCalendarItem();
	__property TsgcGoogleCalendarEventList* Events = {read=GetEvents, write=FEvents};
public:
	/* TsgcGoogleCalendarResource_CalendarList.Create */ inline __fastcall virtual TsgcGoogleCalendarItem() : TsgcGoogleCalendarResource_CalendarList() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcGoogleCalendarList : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	TsgcGoogleCalendarItem* operator[](int Index) { return this->Calendar[Index]; }
	
public:
	__fastcall virtual TsgcGoogleCalendarList();
	void __fastcall AddCalendar(TsgcGoogleCalendarItem* const aCalendar);
	TsgcGoogleCalendarItem* __fastcall GetCalendar(const System::UnicodeString aId);
	
private:
	System::UnicodeString FNextSyncToken;
	TsgcGoogleCalendarItem* __fastcall GetCalendarItem(int Index);
	
public:
	__property TsgcGoogleCalendarItem* Calendar[int Index] = {read=GetCalendarItem/*, default*/};
	__property System::UnicodeString NextSyncToken = {read=FNextSyncToken, write=FNextSyncToken};
public:
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcGoogleCalendarList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_Calendar_Client : public TsgcHTTP_Google_Cloud_Calendar_API_Client
{
	typedef TsgcHTTP_Google_Cloud_Calendar_API_Client inherited;
	
protected:
	virtual bool __fastcall DoCheckResponse(const System::UnicodeString aValue);
	bool __fastcall DoLoadCalendars(const System::UnicodeString aValue, bool aClear = true);
	System::UnicodeString __fastcall DoNewCalendar(const System::UnicodeString aValue);
	bool __fastcall DoUpdateCalendar(const System::UnicodeString aValue);
	
public:
	__fastcall virtual ~TsgcHTTP_Google_Cloud_Calendar_Client();
	System::UnicodeString __fastcall NewCalendar(const System::UnicodeString aSummary);
	bool __fastcall LoadCalendars(System::UnicodeString aPageToken = System::UnicodeString(), System::UnicodeString aSyncToken = System::UnicodeString(), bool aClear = true);
	bool __fastcall LoadCalendarsChanged(System::UnicodeString aSyncToken);
	bool __fastcall DeleteCalendar(const System::UnicodeString aId);
	bool __fastcall UpdateCalendar(TsgcGoogleCalendarItem* const aResource);
	
protected:
	bool __fastcall DoLoadEvents(const System::UnicodeString aCalendarId, const System::UnicodeString aValue, bool aClear = true);
	System::UnicodeString __fastcall DoNewEvent(const System::UnicodeString aValue);
	bool __fastcall DoUpdateEvent(const System::UnicodeString aValue);
	bool __fastcall DoDeleteEvent(const System::UnicodeString aValue);
	
public:
	System::UnicodeString __fastcall NewEvent(const System::UnicodeString aCalendarId, TsgcGoogleCalendarEventItem* const aResource);
	bool __fastcall LoadEvents(const System::UnicodeString aCalendarId, TsgcGoogleCalendarEventListOrdered aOrderBy = (TsgcGoogleCalendarEventListOrdered)(0x1))/* overload */;
	bool __fastcall LoadEvents(const System::UnicodeString aCalendarId, System::TDateTime aDateFrom, System::TDateTime aDateTo, TsgcGoogleCalendarEventListOrdered aOrderBy = (TsgcGoogleCalendarEventListOrdered)(0x1), System::UnicodeString aPageToken = System::UnicodeString(), System::UnicodeString aSyncToken = System::UnicodeString(), bool aClear = true)/* overload */;
	bool __fastcall LoadEventsChanged(const System::UnicodeString aCalendarId, System::UnicodeString aSyncToken);
	bool __fastcall UpdateEvent(const System::UnicodeString aCalendarId, TsgcGoogleCalendarEventItem* const aResource);
	bool __fastcall DeleteEvent(const System::UnicodeString aCalendarId, const System::UnicodeString aEventId);
	
private:
	TsgcGoogleCalendarList* FCalendars;
	TsgcGoogleCalendarList* __fastcall GetCalendars();
	
public:
	__property TsgcGoogleCalendarList* Calendars = {read=GetCalendars, write=FCalendars};
	
private:
	TsgcGoogleCalendarEvent FOnGetCalendar;
	TsgcGoogleCalendarGetEventEvent FOnGetCalendarEvent;
	TsgcGoogleCalendarErrorEvent FOnError;
	
protected:
	bool __fastcall DoCalendarEvent(TsgcGoogleCalendarItem* const aCalendar);
	bool __fastcall DoCalendarEventEvent(TsgcGoogleCalendarItem* const aCalendar, TsgcGoogleCalendarEventItem* const aEvent);
	
public:
	__property TsgcGoogleCalendarErrorEvent OnError = {read=FOnError, write=FOnError};
	__property TsgcGoogleCalendarEvent OnGetCalendar = {read=FOnGetCalendar, write=FOnGetCalendar};
	__property TsgcGoogleCalendarGetEventEvent OnGetCalendarEvent = {read=FOnGetCalendarEvent, write=FOnGetCalendarEvent};
public:
	/* TsgcHTTP_Google_Cloud_Calendar_API_Client.Create */ inline __fastcall virtual TsgcHTTP_Google_Cloud_Calendar_Client(System::Classes::TComponent* aOwner) : TsgcHTTP_Google_Cloud_Calendar_API_Client(aOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_google_calendar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_GOOGLE_CALENDAR)
using namespace Sgchttp_google_calendar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_google_calendarHPP
