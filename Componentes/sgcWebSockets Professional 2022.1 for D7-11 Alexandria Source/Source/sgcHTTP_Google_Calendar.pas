{ ***************************************************************************
  sgcHTTP Google component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Google_Calendar;

interface

{$I sgcVer.inc}
{$IFDEF SGC_GOOGLE_CLOUD}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes_Queues,
  sgcHTTP_Google_Cloud, sgcJSON;

type
  TsgcGoogleCalendarItem = class;
  TsgcGoogleCalendarEventItem = class;
  TsgcGoogleCalendarError = class;

  TsgcGoogleCalendarRole = (gcrNone, gcrFreeBusyReader, gcrReader, gcrWrite,
    gcrOwner);
  TsgcGoogleCalendarScopeType = (gcstDefault, gcstUser, gcstGroup, gcstDomain);
  TsgcGoogleCalenarNotificationType = (gcntEventCreation, gcntEventChange,
    gcntEventCancellation, gcntEventResponse, gcntEventAgenda);
  TsgcGoogleCalendarEventListOrdered = (gceoNone, gceoStartTime, gceoUpdated);
  TsgcGoogleCalendarEventVisibility = (gcevDefault, gcevPublic, gcevPrivate,
    gcevConfidential);
  TsgcGoogleCalendarEventStatus = (gcesConfirmed, gcesTentative, gcesCancelled);
  TsgcGoogleCalendarReminderMethod = (gcrmEmail, gcrmPopup);

  TsgcGoogleCalendarEvent = procedure(Sender: TObject; const aCalendar:
      TsgcGoogleCalendarItem; var Accept: Boolean) of object;
  TsgcGoogleCalendarGetEventEvent = procedure(Sender: TObject; const aCalendar:
      TsgcGoogleCalendarItem; const aEvent: TsgcGoogleCalendarEventItem; var
      Accept: Boolean) of object;
  TsgcGoogleCalendarErrorEvent = procedure(Sender: TObject;
    const aError: TsgcGoogleCalendarError) of object;

  TsgcGoogleCalendarScopes = class(TPersistent)
  private
    FCalendarAddOn: Boolean;
    FCalendarsReadOnly: Boolean;
    FCalendarsReadWrite: Boolean;
    FEventsReadOnly: Boolean;
    FEventsReadWrite: Boolean;
    FSettings: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property CalendarAddOn: Boolean read FCalendarAddOn write FCalendarAddOn;
    property CalendarsReadOnly: Boolean read FCalendarsReadOnly
      write FCalendarsReadOnly;
    property CalendarsReadWrite: Boolean read FCalendarsReadWrite
      write FCalendarsReadWrite;
    property EventsReadOnly: Boolean read FEventsReadOnly write FEventsReadOnly;
    property EventsReadWrite: Boolean read FEventsReadWrite
      write FEventsReadWrite;
    property Settings: Boolean read FSettings write FSettings;
  end;

  TsgcGoogleCalendarErrorItem = class
  private
    FDomain: string;
    FLocation: string;
    FLocationType: string;
    FReason: string;
    F_Message: string;
  public
    property Domain: string read FDomain write FDomain;
    property Location: string read FLocation write FLocation;
    property LocationType: string read FLocationType write FLocationType;
    property Reason: string read FReason write FReason;
    property _Message: string read F_Message write F_Message;
  end;

  TsgcGoogleCalendarErrors = Array of TsgcGoogleCalendarErrorItem;

  TsgcGoogleCalendarError = class
  private
    FCode: Integer;
    FErrors: TsgcGoogleCalendarErrors;
    FDescription: string;
  public
    property Code: Integer read FCode write FCode;
    property Errors: TsgcGoogleCalendarErrors read FErrors write FErrors;
    property Description: string read FDescription write FDescription;
  end;

  TsgcGoogleCalendarJSON = class(TsgcQueueItemBase)
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
    function GetObjectJSON(aJSONObject: IsgcObjectJSON = nil): IsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON;
    function JSONText: String; virtual;
  protected
    function DoReadJSONValue(const aField: string;
      aJSONObject: IsgcObjectJSON = nil): Variant; overload; virtual;
    function DoReadJSONValue(aItem: Integer; aJSONObject: TsgcObjectJSON = nil)
      : Variant; overload; virtual;
  protected
    procedure DoRead; virtual;
    procedure Read(const aValue: string);
  end;

  TsgcGoogleCalendarWatch = class(TsgcGoogleCalendarJSON)
  private
    Faddress: string;
    Ftoken: string;
    Fttl: string;
    F_type: string;
  protected
    function JSONText: String; override;
  public
    constructor Create; override;
  public
    property address: string read Faddress write Faddress;
    property token: string read Ftoken write Ftoken;
    property ttl: string read Fttl write Fttl;
    property _type: string read F_type write F_type;
  end;

  TsgcGoogleCalendarResource = class(TsgcGoogleCalendarJSON)
  private
    FEtag: string;
    FKind: String;
  protected
    procedure DoJSONBody; virtual; abstract;
    function JSONText: String; override;
  protected
    procedure DoRead; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Etag: string read FEtag write FEtag;
    property Kind: String read FKind write FKind;
  end;

  TsgcGoogleCalendarScope = class
  private
    FValue: string;
    F_type: TsgcGoogleCalendarScopeType;
  public
    property Value: string read FValue write FValue;
    property _type: TsgcGoogleCalendarScopeType read F_type write F_type;
  end;

  TsgcGoogleCalendarResource_ACL = class(TsgcGoogleCalendarResource)
  private
    FRole: TsgcGoogleCalendarRole;
    FScope: TsgcGoogleCalendarScope;
    function GetScope: TsgcGoogleCalendarScope;
  protected
    procedure DoJSONBody; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Role: TsgcGoogleCalendarRole read FRole write FRole;
    property Scope: TsgcGoogleCalendarScope read GetScope write FScope;
  end;

  TsgcGoogleCalendarDefaultReminderItem = class(TsgcQueueItemBase)
  private
    FMethod: TsgcGoogleCalendarReminderMethod;
    FMinutes: Integer;
  public
    property Method: TsgcGoogleCalendarReminderMethod read FMethod
      write FMethod;
    property Minutes: Integer read FMinutes write FMinutes;
  end;

  TsgcGoogleCalendarDefaultReminders = class(TsgcQueue);

  TsgcGoogleCalendarNotificationItem = class(TsgcQueueItemBase)
  private
    FMethod: string;
    F_type: TsgcGoogleCalenarNotificationType;
  public
    constructor Create; override;
  public
    property Method: string read FMethod write FMethod;
    property _type: TsgcGoogleCalenarNotificationType read F_type write F_type;
  end;

  TsgcGoogleCalendarNotificationSettings = class(TsgcQueue);

  TsgcGoogleCalendarConferenceProperties = class
  private
    FAllowedConferenceSolutionTypes: TStringList;
    function GetAllowedConferenceSolutionTypes: TStringList;
  public
    destructor Destroy; override;
  public
    property AllowedConferenceSolutionTypes: TStringList
      read GetAllowedConferenceSolutionTypes
      write FAllowedConferenceSolutionTypes;
  end;

  TsgcGoogleCalendarResource_CalendarList = class(TsgcGoogleCalendarResource)
  private
    FAccessRole: TsgcGoogleCalendarRole;
    FBackgroundColor: string;
    FColorId: string;
    FConferenceProperties: TsgcGoogleCalendarConferenceProperties;
    FDefaultReminders: TsgcGoogleCalendarDefaultReminders;
    FDeleted: Boolean;
    FDescription: string;
    FForegroundColor: string;
    FHidden: Boolean;
    FLocation: string;
    FNotificationSettings: TsgcGoogleCalendarNotificationSettings;
    FPrimary: Boolean;
    FSelected: Boolean;
    FSummary: string;
    FSummaryOverride: string;
    FTimeZone: string;
    function GetConferenceProperties: TsgcGoogleCalendarConferenceProperties;
    function GetDefaultReminders: TsgcGoogleCalendarDefaultReminders;
    function GetNotificationSettings: TsgcGoogleCalendarNotificationSettings;
  protected
    procedure DoJSONBody; override;
  protected
    procedure DoRead; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property AccessRole: TsgcGoogleCalendarRole read FAccessRole
      write FAccessRole;
    property BackgroundColor: string read FBackgroundColor
      write FBackgroundColor;
    property ColorId: string read FColorId write FColorId;
    property ConferenceProperties: TsgcGoogleCalendarConferenceProperties
      read GetConferenceProperties write FConferenceProperties;
    property DefaultReminders: TsgcGoogleCalendarDefaultReminders
      read GetDefaultReminders write FDefaultReminders;
    property Deleted: Boolean read FDeleted write FDeleted;
    property Description: string read FDescription write FDescription;
    property ForegroundColor: string read FForegroundColor
      write FForegroundColor;
    property Hidden: Boolean read FHidden write FHidden;
    property Location: string read FLocation write FLocation;
    property NotificationSettings: TsgcGoogleCalendarNotificationSettings
      read GetNotificationSettings write FNotificationSettings;
    property Primary: Boolean read FPrimary write FPrimary;
    property Selected: Boolean read FSelected write FSelected;
    property Summary: string read FSummary write FSummary;
    property SummaryOverride: string read FSummaryOverride
      write FSummaryOverride;
    property TimeZone: string read FTimeZone write FTimeZone;
  end;

  TsgcGoogleCalendarResource_Calendar = class(TsgcGoogleCalendarResource)
  private
    FConferenceProperties: TsgcGoogleCalendarConferenceProperties;
    FDescription: string;
    FLocation: string;
    FSummary: string;
    FTimeZone: string;
    function GetConferenceProperties: TsgcGoogleCalendarConferenceProperties;
  protected
    procedure DoJSONBody; override;
    procedure DoRead; override;
  public
    destructor Destroy; override;
  public
    property ConferenceProperties: TsgcGoogleCalendarConferenceProperties
      read GetConferenceProperties write FConferenceProperties;
    property Description: string read FDescription write FDescription;
    property Location: string read FLocation write FLocation;
    property Summary: string read FSummary write FSummary;
    property TimeZone: string read FTimeZone write FTimeZone;
  end;

  TsgcGoogleCalendarPerson = class
  private
    FDisplayName: string;
    FEmail: string;
    Fid: string;
    F_self: Boolean;
  public
    property DisplayName: string read FDisplayName write FDisplayName;
    property Email: string read FEmail write FEmail;
    property id: string read Fid write Fid;
    property _self: Boolean read F_self write F_self;
  end;

  TsgcGoogleCalendarCreator = class(TsgcGoogleCalendarPerson);
  TsgcGoogleCalendarOrganizer = class(TsgcGoogleCalendarPerson);

  TsgcGoogleCalendarDateTime = class
  private
    FDate: TDateTime;
    FDateTime: TDateTime;
    FTimeZone: string;
  public
    constructor Create; virtual;
  public
    property Date: TDateTime read FDate write FDate;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property TimeZone: string read FTimeZone write FTimeZone;
  end;

  TsgcGoogleCalendarStart = class(TsgcGoogleCalendarDateTime);
  TsgcGoogleCalendarEnd = class(TsgcGoogleCalendarDateTime);
  TsgcGoogleCalendarOriginalStart = class(TsgcGoogleCalendarDateTime);

  TsgcGoogleCalendarAttendeeItem = class(TsgcQueueItemBase)
  private
    FAdditionalGuests: Integer;
    FComment: string;
    FDisplayName: string;
    FEmail: string;
    FOptional: Boolean;
    FOrganizer: Boolean;
    FResource: Boolean;
    FResponseStatus: string;
    F_self: Boolean;
  public
    property AdditionalGuests: Integer read FAdditionalGuests
      write FAdditionalGuests;
    property Comment: string read FComment write FComment;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Email: string read FEmail write FEmail;
    property Optional: Boolean read FOptional write FOptional;
    property Organizer: Boolean read FOrganizer write FOrganizer;
    property Resource: Boolean read FResource write FResource;
    property ResponseStatus: string read FResponseStatus write FResponseStatus;
    property _self: Boolean read F_self write F_self;
  end;

  TsgcGoogleCalendarAttendees = class(TsgcQueue);

  TsgcGoogleCalendarExtendedProperties = class
  private
    F_Private: TStringList;
    F_Shared: TStringList;
    function Get_Private: TStringList;
    function Get_Shared: TStringList;
  public
    destructor Destroy; override;
  public
    property _Private: TStringList read Get_Private write F_Private;
    property _Shared: TStringList read Get_Shared write F_Shared;
  end;

  TsgcGoogleCalendarConferenceSolutionKey = class
  private
    F_type: string;
  public
    property _type: string read F_type write F_type;
  end;

  TsgcGoogleCalendarConferenceStatus = class
  private
    FStatusCode: string;
  public
    property StatusCode: string read FStatusCode write FStatusCode;
  end;

  TsgcGoogleCalendarCreateRequest = class
  private
    FConferenceSolutionKey: TsgcGoogleCalendarConferenceSolutionKey;
    FRequestId: string;
    FStatus: TsgcGoogleCalendarConferenceStatus;
    function GetConferenceSolutionKey: TsgcGoogleCalendarConferenceSolutionKey;
    function GetStatus: TsgcGoogleCalendarConferenceStatus;
  public
    destructor Destroy; override;
  public
    property ConferenceSolutionKey: TsgcGoogleCalendarConferenceSolutionKey
      read GetConferenceSolutionKey write FConferenceSolutionKey;
    property RequestId: string read FRequestId write FRequestId;
    property Status: TsgcGoogleCalendarConferenceStatus read GetStatus
      write FStatus;
  end;

  TsgcGoogleCalendarConferenceEntryPointItem = class(TsgcQueueItemBase)
  private
    FAccessCode: string;
    FEntryPointType: string;
    FMeetingCode: string;
    FPasscode: string;
    FPassword: string;
    FPin: string;
    FUri: string;
    F_Label: string;
  public
    property AccessCode: string read FAccessCode write FAccessCode;
    property EntryPointType: string read FEntryPointType write FEntryPointType;
    property MeetingCode: string read FMeetingCode write FMeetingCode;
    property Passcode: string read FPasscode write FPasscode;
    property Password: string read FPassword write FPassword;
    property Pin: string read FPin write FPin;
    property Uri: string read FUri write FUri;
    property _Label: string read F_Label write F_Label;
  end;

  TsgcGoogleCalendarConferenceEntryPoints = TsgcQueue;

  TsgcGoogleCalendarConferenceSolution = class
  private
    FIconURI: string;
    FKey: TsgcGoogleCalendarConferenceSolutionKey;
    F_Name: string;
    function GetKey: TsgcGoogleCalendarConferenceSolutionKey;
  public
    destructor Destroy; override;
  public
    property IconURI: string read FIconURI write FIconURI;
    property Key: TsgcGoogleCalendarConferenceSolutionKey read GetKey
      write FKey;
    property _Name: string read F_Name write F_Name;
  end;

  TsgcGoogleCalendarConferenceData = class
  private
    FConferenceId: string;
    FConferenceSolution: TsgcGoogleCalendarConferenceSolution;
    FCreateRequest: TsgcGoogleCalendarCreateRequest;
    FEntryPoints: TsgcGoogleCalendarConferenceEntryPoints;
    FNotes: string;
    FSignature: string;
    function GetConferenceSolution: TsgcGoogleCalendarConferenceSolution;
    function GetCreateRequest: TsgcGoogleCalendarCreateRequest;
    function GetEntryPoints: TsgcGoogleCalendarConferenceEntryPoints;
  public
    destructor Destroy; override;
  public
    property ConferenceId: string read FConferenceId write FConferenceId;
    property ConferenceSolution: TsgcGoogleCalendarConferenceSolution
      read GetConferenceSolution write FConferenceSolution;
    property CreateRequest: TsgcGoogleCalendarCreateRequest
      read GetCreateRequest write FCreateRequest;
    property EntryPoints: TsgcGoogleCalendarConferenceEntryPoints
      read GetEntryPoints write FEntryPoints;
    property Notes: string read FNotes write FNotes;
    property Signature: string read FSignature write FSignature;
  end;

  TsgcGoogleCalendarGadget = class
  private
    FDisplay: string;
    FHeight: Integer;
    FIconLink: string;
    FLink: string;
    FPreferences: TStringList;
    FTitle: string;
    FWidth: Integer;
    F_type: string;
    function GetPreferences: TStringList;
  public
    destructor Destroy; override;
  public
    property Display: string read FDisplay write FDisplay;
    property Height: Integer read FHeight write FHeight;
    property IconLink: string read FIconLink write FIconLink;
    property Link: string read FLink write FLink;
    property Preferences: TStringList read GetPreferences write FPreferences;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;
    property _type: string read F_type write F_type;
  end;

  TsgcGoogleCalendarRemindersOverridesItem = class(TsgcQueueItemBase)
  private
    FMethod: TsgcGoogleCalendarReminderMethod;
    FMinutes: Integer;
  public
    property Method: TsgcGoogleCalendarReminderMethod read FMethod
      write FMethod;
    property Minutes: Integer read FMinutes write FMinutes;
  end;

  TsgcGoogleCalendarRemindersOverrides = class(TsgcQueue)
  end;

  TsgcGoogleCalendarReminders = class
  private
    FOverrides: TsgcGoogleCalendarRemindersOverrides;
    FUseDefault: Boolean;
    function GetOverrides: TsgcGoogleCalendarRemindersOverrides;
  public
    destructor Destroy; override;
  public
    property Overrides: TsgcGoogleCalendarRemindersOverrides read GetOverrides
      write FOverrides;
    property UseDefault: Boolean read FUseDefault write FUseDefault;
  end;

  TsgcGoogleCalendarSource = class
  private
    FTitle: string;
    FURL: String;
  public
    property Title: string read FTitle write FTitle;
    property URL: String read FURL write FURL;
  end;

  TsgcGoogleCalendarAttachmentItem = class(TsgcQueueItemBase)
  private
    FFileId: string;
    FFileUrl: String;
    FIconLink: string;
    FMimeType: string;
    FTitle: string;
  public
    property FileId: string read FFileId write FFileId;
    property FileUrl: String read FFileUrl write FFileUrl;
    property IconLink: string read FIconLink write FIconLink;
    property MimeType: string read FMimeType write FMimeType;
    property Title: string read FTitle write FTitle;
  end;

  TagcGoogleCalendarAttachments = TsgcQueue;

  TsgcGoogleCalendarResource_Event = class(TsgcGoogleCalendarResource)
  private
    FAnyoneCanAddSelf: Boolean;
    FAttachments: TagcGoogleCalendarAttachments;
    FAttendees: TsgcGoogleCalendarAttendees;
    FAttendeesOmitted: Boolean;
    FColorId: string;
    FConferenceData: TsgcGoogleCalendarConferenceData;
    FCreated: TDateTime;
    FCreator: TsgcGoogleCalendarCreator;
    FDescription: string;
    FEndTimeUnspecified: Boolean;
    FExtendedProperties: TsgcGoogleCalendarExtendedProperties;
    FGadget: TsgcGoogleCalendarGadget;
    FGuestsCanInviteOthers: Boolean;
    FGuestsCanModify: Boolean;
    FGuestsCanSeeOtherGuests: Boolean;
    FHangoutLink: string;
    FHtmllink: string;
    FiCalUID: string;
    FLocation: string;
    FLocked: Boolean;
    FOrganizer: TsgcGoogleCalendarOrganizer;
    FOriginalStartTime: TsgcGoogleCalendarOriginalStart;
    FPrivateCopy: Boolean;
    FRecurrence: TStringList;
    FRecurringEventId: string;
    FReminders: TsgcGoogleCalendarReminders;
    FSequence: Integer;
    FSource: TsgcGoogleCalendarSource;
    FStart: TsgcGoogleCalendarStart;
    FStatus: TsgcGoogleCalendarEventStatus;
    FSummary: string;
    FTransparency: string;
    FUpdated: TDateTime;
    FVisibility: TsgcGoogleCalendarEventVisibility;
    F_End: TsgcGoogleCalendarEnd;
    function GetAttachments: TagcGoogleCalendarAttachments;
    function GetAttendees: TsgcGoogleCalendarAttendees;
    function GetConferenceData: TsgcGoogleCalendarConferenceData;
    function GetCreator: TsgcGoogleCalendarCreator;
    function GetExtendedProperties: TsgcGoogleCalendarExtendedProperties;
    function GetGadget: TsgcGoogleCalendarGadget;
    function GetOrganizer: TsgcGoogleCalendarOrganizer;
    function GetOriginalStartTime: TsgcGoogleCalendarOriginalStart;
    function GetRecurrence: TStringList;
    function GetReminders: TsgcGoogleCalendarReminders;
    function GetSource: TsgcGoogleCalendarSource;
    function GetStart: TsgcGoogleCalendarStart;
    function Get_End: TsgcGoogleCalendarEnd;
  protected
    procedure DoJSONBody; override;
    procedure DoRead; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property AnyoneCanAddSelf: Boolean read FAnyoneCanAddSelf
      write FAnyoneCanAddSelf;
    property Attachments: TagcGoogleCalendarAttachments read GetAttachments
      write FAttachments;
    property Attendees: TsgcGoogleCalendarAttendees read GetAttendees
      write FAttendees;
    property AttendeesOmitted: Boolean read FAttendeesOmitted
      write FAttendeesOmitted;
    property ColorId: string read FColorId write FColorId;
    property ConferenceData: TsgcGoogleCalendarConferenceData
      read GetConferenceData write FConferenceData;
    property Created: TDateTime read FCreated write FCreated;
    property Creator: TsgcGoogleCalendarCreator read GetCreator write FCreator;
    property Description: string read FDescription write FDescription;
    property EndTimeUnspecified: Boolean read FEndTimeUnspecified
      write FEndTimeUnspecified;
    property ExtendedProperties: TsgcGoogleCalendarExtendedProperties
      read GetExtendedProperties write FExtendedProperties;
    property Gadget: TsgcGoogleCalendarGadget read GetGadget write FGadget;
    property GuestsCanInviteOthers: Boolean read FGuestsCanInviteOthers
      write FGuestsCanInviteOthers;
    property GuestsCanModify: Boolean read FGuestsCanModify
      write FGuestsCanModify;
    property GuestsCanSeeOtherGuests: Boolean read FGuestsCanSeeOtherGuests
      write FGuestsCanSeeOtherGuests;
    property HangoutLink: string read FHangoutLink write FHangoutLink;
    property Htmllink: string read FHtmllink write FHtmllink;
    property iCalUID: string read FiCalUID write FiCalUID;
    property Location: string read FLocation write FLocation;
    property Locked: Boolean read FLocked write FLocked;
    property Organizer: TsgcGoogleCalendarOrganizer read GetOrganizer
      write FOrganizer;
    property OriginalStartTime: TsgcGoogleCalendarOriginalStart
      read GetOriginalStartTime write FOriginalStartTime;
    property PrivateCopy: Boolean read FPrivateCopy write FPrivateCopy;
    property Recurrence: TStringList read GetRecurrence write FRecurrence;
    property RecurringEventId: string read FRecurringEventId
      write FRecurringEventId;
    property Reminders: TsgcGoogleCalendarReminders read GetReminders
      write FReminders;
    property Sequence: Integer read FSequence write FSequence;
    property Source: TsgcGoogleCalendarSource read GetSource write FSource;
    property Start: TsgcGoogleCalendarStart read GetStart write FStart;
    property Status: TsgcGoogleCalendarEventStatus read FStatus write FStatus;
    property Summary: string read FSummary write FSummary;
    property Transparency: string read FTransparency write FTransparency;
    property Updated: TDateTime read FUpdated write FUpdated;
    property Visibility: TsgcGoogleCalendarEventVisibility read FVisibility
      write FVisibility;
    property _End: TsgcGoogleCalendarEnd read Get_End write F_End;
  end;

  TsgcHTTP_Google_Cloud_Calendar_API_Client = class
    (TsgcHTTP_Google_Cloud_Client)
    { from TsgcHTTP_Google_Cloud_Client }
  protected
    function DoGetScope: string; override;
    { from TsgcHTTP_Google_Cloud_Client }

    { helpers }
  protected
    function FormatAndEncode(const aFormat: string;
      const Args: array of const): string;
    { helpers }

    { scopes }
  private
    FScopes: TsgcGoogleCalendarScopes;
    procedure SetScopes(const Value: TsgcGoogleCalendarScopes);
  public
    property Scopes: TsgcGoogleCalendarScopes read FScopes write SetScopes;
    { scopes }

    { constructor/destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor/destructor }

    { acl }
  public
    function ACL_Delete(const aCalendarId, aRuleId: String): String;
    function ACL_Get(const aCalendarId, aRuleId: String): String;
    function ACL_Insert(const aCalendarId: String;
      const aResource: TsgcGoogleCalendarResource_ACL;
      aSendNotifications: Boolean = True): String;
    function ACL_List(const aCalendarId: String; aMaxResults: Integer = 100;
      aPageToken: String = ''; aShowDeleted: Boolean = False;
      aSyncToken: String = ''): String;
    function ACL_Patch(const aCalendarId, aRuleId: String;
      const aResource: TsgcGoogleCalendarResource_ACL;
      aSendNotifications: Boolean = True): String;
    function ACL_Update(const aCalendarId, aRuleId: String;
      const aResource: TsgcGoogleCalendarResource_ACL;
      aSendNotifications: Boolean = True): String;
    function ACL_Watch(const aCalendarId: String;
      const aWatch: TsgcGoogleCalendarWatch): String;
    { acl }

    { calendar list }
  public
    function CalendarList_Delete(const aCalendarId: string): string;
    function CalendarList_Get(const aCalendarId: string): string;
    function CalendarList_Insert(const aResource
      : TsgcGoogleCalendarResource_CalendarList;
      aColorRgbFormat: Boolean = False): string;
    function CalendarList_List(aMaxResults: Integer = 250;
      aMinAccessRole: string = ''; aPageToken: string = '';
      aShowDeleted: Boolean = False; aShowHidden: Boolean = False;
      aSyncToken: string = ''): string;
    function CalendarList_Patch(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarResource_CalendarList;
      aColorRgbFormat: Boolean = False): string;
    function CalendarList_Update(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarResource_CalendarList;
      aColorRgbFormat: Boolean = False): string;
    function CalendarList_Watch(const aWatch: TsgcGoogleCalendarWatch): string;
    { calendar list }

    { calendar }
  public
    function Calendar_Clear(const aCalendarId: string): string;
    function Calendar_Delete(const aCalendarId: string): string;
    function Calendar_Get(const aCalendarId: string): string;
    function Calendar_Insert(const aResource
      : TsgcGoogleCalendarResource_Calendar): string;
    function Calendar_Patch(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarResource_Calendar): string;
    function Calendar_Update(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarResource_Calendar): string;
    { calendar }

    { channels }
  public
    function Channel_Stop(const aId, aResourceId: string;
      const aToken: string = ''): string;
    { channels }

    { color }
  public
    function Color_Get: string;
    { color }

    { events }
  public
    function Event_Delete(const aCalendarId, aEventId: string;
      aSendNotifications: Boolean = False;
      const aSendUpdates: string = 'all'): string;
    function Event_Get(const aCalendarId, aEventId: string;
      aMaxAttendees: Integer = 0; const aTimeZone: String = ''): string;
    function Event_Import(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarResource_Event;
      aConferenceDataVersion: Integer = 0;
      aSupportsAttachments: Boolean = False): string;
    function Event_Insert(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarResource_Event;
      aConferenceDataVersion: Integer = 0; aMaxAttendees: Integer = 0;
      const aSendUpdates: string = 'all';
      aSupportsAttachments: Boolean = False): string;
    function Event_Instances(const aCalendarId, aEventId: string;
      aMaxAttendees: Integer = 0; aMaxResults: Integer = 250;
      const aOriginalStart: string = ''; const aPageToken: string = '';
      aShowDeleted: Boolean = False; aTimeMax: TDateTime = 0;
      aTimeMin: TDateTime = 0; const aTimeZone: String = ''): string;
    function Event_List(const aCalendarId: string; aICalUID: string = '';
      aMaxAttendees: Integer = 0; aMaxResults: Integer = 250;
      const aOrderBy: string = ''; const aPageToken: string = '';
      const aPrivateExtendedProperty: string = ''; const aQ: string = '';
      const aSharedExtendedProperty: string = ''; aShowDeleted: Boolean = False;
      aShowHiddenInvitations: Boolean = False; aSingleEvents: Boolean = False;
      aSyncToken: string = ''; aTimeMax: TDateTime = 0; aTimeMin: TDateTime = 0;
      const aTimeZone: String = ''; aUpdatedMin: TDateTime = 0): string;
    function Event_Move(const aCalendarId, aEventId, aDestination: string;
      const aSendUpdates: string = 'all'): string;
    function Event_Patch(const aCalendarId, aEventId: string;
      const aResource: TsgcGoogleCalendarResource_Event;
      aConferenceDataVersion: Integer = 0; aMaxAttendees: Integer = 0;
      const aSendUpdates: string = 'all';
      aSupportsAttachments: Boolean = False): string;
    function Event_QuickAdd(const aCalendarId, aText: string;
      const aSendUpdates: string = 'all'): string;
    function Event_Update(const aCalendarId, aEventId: string;
      const aResource: TsgcGoogleCalendarResource_Event;
      aConferenceDataVersion: Integer = 0; aMaxAttendees: Integer = 0;
      const aSendUpdates: string = 'all';
      aSupportsAttachments: Boolean = False): string;
    function Event_Watch(const aCalendarId: string;
      const aWatch: TsgcGoogleCalendarWatch): string;
    { events }

    { freebusy }
  public
    function Freebusy_Query(const aItems: TStrings;
      aTimeMin, aTimeMax: TDateTime; const aTimeZone: String = '';
      aGroupExpansionMax: Integer = 0;
      aCalendarExpansionMax: Integer = 0): string;
    { freebusy }

    { settings }
  public
    function Settings_Get(const aSetting: string): string;
    function Settings_List(aMaxResults: Integer = 0;
      const aPageToken: string = ''; const aSyncToken: string = ''): string;
    function Settings_Watch(const aWatch: TsgcGoogleCalendarWatch): string;
    { settings }
  end;

  TsgcGoogleCalendarEventItem = class(TsgcGoogleCalendarResource_Event)

  end;

  TsgcGoogleCalendarEventList = class(TsgcQueue)
  public
    constructor Create; override;
  public
    procedure AddEvent(const aEvent: TsgcGoogleCalendarEventItem);
    function GetEvent(const aId: string): TsgcGoogleCalendarEventItem;
  private
    FNextSyncToken: string;
    function GetEventItem(Index: Integer): TsgcGoogleCalendarEventItem;
  public
    property Event[Index: Integer]: TsgcGoogleCalendarEventItem
      read GetEventItem; default;
    property NextSyncToken: string read FNextSyncToken write FNextSyncToken;
  end;

  TsgcGoogleCalendarItem = class(TsgcGoogleCalendarResource_CalendarList)
  private
    FEvents: TsgcGoogleCalendarEventList;
    function GetEvents: TsgcGoogleCalendarEventList;
  public
    destructor Destroy; override;
  public
    property Events: TsgcGoogleCalendarEventList read GetEvents write FEvents;
  end;

  TsgcGoogleCalendarList = class(TsgcQueue)
  public
    constructor Create; override;
  public
    procedure AddCalendar(const aCalendar: TsgcGoogleCalendarItem);
    function GetCalendar(const aId: string): TsgcGoogleCalendarItem;
  private
    FNextSyncToken: String;
    function GetCalendarItem(Index: Integer): TsgcGoogleCalendarItem;
  public
    property Calendar[Index: Integer]: TsgcGoogleCalendarItem
      read GetCalendarItem; default;
    property NextSyncToken: String read FNextSyncToken write FNextSyncToken;
  end;

  TsgcHTTP_Google_Cloud_Calendar_Client = class
    (TsgcHTTP_Google_Cloud_Calendar_API_Client)
    { methods }
  protected
    function DoCheckResponse(const aValue: string): Boolean; virtual;
    { methods }

    { calendar methods }
  protected
    function DoLoadCalendars(const aValue: string;
      aClear: Boolean = True): Boolean;
    function DoNewCalendar(const aValue: string): string;
    function DoUpdateCalendar(const aValue: string): Boolean;
  public
    destructor Destroy; override;
  public
    function NewCalendar(const aSummary: String): string;
    function LoadCalendars(aPageToken: string = ''; aSyncToken: string = '';
        aClear: Boolean = True): Boolean;
    function LoadCalendarsChanged(aSyncToken: string): Boolean;
    function DeleteCalendar(const aId: string): Boolean;
    function UpdateCalendar(const aResource: TsgcGoogleCalendarItem): Boolean;
    { calendar methods }

    { events methods }
  protected
    function DoLoadEvents(const aCalendarId, aValue: string;
      aClear: Boolean = True): Boolean;
    function DoNewEvent(const aValue: string): string;
    function DoUpdateEvent(const aValue: string): Boolean;
    function DoDeleteEvent(const aValue: string): Boolean;
  public
    function NewEvent(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarEventItem): string;
    function LoadEvents(const aCalendarId: string;
      aOrderBy: TsgcGoogleCalendarEventListOrdered = gceoStartTime)
      : Boolean; overload;
    function LoadEvents(const aCalendarId: string;
      aDateFrom, aDateTo: TDateTime;
      aOrderBy: TsgcGoogleCalendarEventListOrdered = gceoStartTime;
      aPageToken: String = ''; aSyncToken: string = ''; aClear: Boolean = True)
      : Boolean; overload;
    function LoadEventsChanged(const aCalendarId: string;
      aSyncToken: string): Boolean;
    function UpdateEvent(const aCalendarId: string;
      const aResource: TsgcGoogleCalendarEventItem): Boolean;
    function DeleteEvent(const aCalendarId: string;
      const aEventId: string): Boolean;
    { events methods }

    { properties }
  private
    FCalendars: TsgcGoogleCalendarList;
    function GetCalendars: TsgcGoogleCalendarList;
  public
    property Calendars: TsgcGoogleCalendarList read GetCalendars
      write FCalendars;
    { properties }

    { events }
  private
    FOnGetCalendar: TsgcGoogleCalendarEvent;
    FOnGetCalendarEvent: TsgcGoogleCalendarGetEventEvent;
    FOnError: TsgcGoogleCalendarErrorEvent;
  protected
    function DoCalendarEvent(const aCalendar: TsgcGoogleCalendarItem): Boolean;
    function DoCalendarEventEvent(const aCalendar: TsgcGoogleCalendarItem; const
        aEvent: TsgcGoogleCalendarEventItem): Boolean;
  public
    property OnError: TsgcGoogleCalendarErrorEvent read FOnError write FOnError;
    property OnGetCalendar: TsgcGoogleCalendarEvent read FOnGetCalendar write
        FOnGetCalendar;
    property OnGetCalendarEvent: TsgcGoogleCalendarGetEventEvent read
        FOnGetCalendarEvent write FOnGetCalendarEvent;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_GOOGLE_CLOUD}

uses
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFNDEF INDY10_2}IdSysWin32, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdGlobalProtocols{$ELSE}IdGlobalProtocols{$ENDIF},
  sgcBase_Helpers;

const
  CS_GOOGLE_CALENDAR_API_URL = 'https://www.googleapis.com/calendar/v3';

function GetStrDateTime(const aValue: TDateTime = 0): string;
var
  vDate: TDateTime;
begin
  vDate := aValue;
  if vDate = 0 then
    vDate := Now;
  vDate := vDate - {$IFNDEF INDY10_2}TIdSysWin32.{$ENDIF}OffsetFromUTC;
  result := FormatDateTime('yyyy-mm-dd', vDate) + 'T' +
    FormatDateTime('hh:nn:ss', vDate) + '.000Z';
end;

function GetVisibility(aValue: string): TsgcGoogleCalendarEventVisibility;
begin
  result := gcevDefault;
  if aValue = 'public' then
    result := gcevPublic
  else if aValue = 'private' then
    result := gcevPrivate
  else if aValue = 'confidential' then
    result := gcevConfidential;
end;

function GetVisibilityString(aValue: TsgcGoogleCalendarEventVisibility): String;
begin
  case aValue of
    gcevDefault:
      result := 'default';
    gcevPublic:
      result := 'public';
    gcevPrivate:
      result := 'private';
    gcevConfidential:
      result := 'confidential';
  else
    result := '';
  end;
end;

function GetReminderMethod(aValue: string): TsgcGoogleCalendarReminderMethod;
begin
  result := gcrmEmail;
  if aValue = 'popup' then
    result := gcrmPopup;
end;

function GetReminderMethodString
  (aValue: TsgcGoogleCalendarReminderMethod): string;
begin
  case aValue of
    gcrmEmail:
      result := 'email';
    gcrmPopup:
      result := 'popup';
  end;
end;

function GetStatus(aValue: string): TsgcGoogleCalendarEventStatus;
begin
  result := gcesConfirmed;
  if aValue = 'tentative' then
    result := gcesTentative
  else if aValue = 'cancelled' then
    result := gcesCancelled;
end;

function GetStatusString(aValue: TsgcGoogleCalendarEventStatus): String;
begin
  case aValue of
    gcesConfirmed:
      result := 'confirmed';
    gcesTentative:
      result := 'tentative';
    gcesCancelled:
      result := 'cancelled';
  else
    result := '';
  end;
end;

function GetRole(aValue: string): TsgcGoogleCalendarRole;
begin
  result := gcrNone;
  if aValue = 'freeBusyReader' then
    result := gcrFreeBusyReader
  else if aValue = 'reader' then
    result := gcrReader
  else if aValue = 'writer' then
    result := gcrWrite
  else if aValue = 'owner' then
    result := gcrOwner;
end;

function GetRoleString(aValue: TsgcGoogleCalendarRole): string;
begin
  case aValue of
    gcrNone:
      result := 'none';
    gcrFreeBusyReader:
      result := 'freeBusyReader';
    gcrReader:
      result := 'reader';
    gcrWrite:
      result := 'writer';
    gcrOwner:
      result := 'owner';
  else
    result := '';
  end;
end;

function GetScopeType(const aValue: string): TsgcGoogleCalendarScopeType;
begin
  result := gcstDefault;
  if aValue = 'user' then
    result := gcstUser
  else if aValue = 'group' then
    result := gcstGroup
  else if aValue = 'domain' then
    result := gcstDomain;
end;

function GetScopeTypeString(aValue: TsgcGoogleCalendarScopeType): string;
begin
  case aValue of
    gcstDefault:
      result := 'default';
    gcstUser:
      result := 'user';
    gcstGroup:
      result := 'group';
    gcstDomain:
      result := 'domain';
  else
    result := '';
  end;
end;

function GetNotificationType(const aValue: string)
  : TsgcGoogleCalenarNotificationType;
begin
  result := gcntEventCreation;
  if aValue = 'eventChange' then
    result := gcntEventChange
  else if aValue = 'eventCancellation' then
    result := gcntEventCancellation
  else if aValue = 'eventResponse' then
    result := gcntEventResponse
  else if aValue = 'agenda' then
    result := gcntEventAgenda;
end;

function GetNotificationTypeString
  (aValue: TsgcGoogleCalenarNotificationType): string;
begin
  case aValue of
    gcntEventCreation:
      result := 'eventCreation';
    gcntEventChange:
      result := 'eventChange';
    gcntEventCancellation:
      result := 'eventCancellation';
    gcntEventResponse:
      result := 'eventResponse';
    gcntEventAgenda:
      result := 'agenda';
  else
    result := '';
  end;
end;

constructor TsgcGoogleCalendarScopes.Create;
begin
  inherited;
  CalendarsReadWrite := True;
  EventsReadWrite := True;
  Settings := False;
  CalendarAddOn := False;
end;

procedure TsgcGoogleCalendarScopes.Assign(aSource: TPersistent);
begin
  if aSource is TsgcGoogleCalendarScopes then
  begin
    CalendarsReadWrite := TsgcGoogleCalendarScopes(aSource).CalendarsReadWrite;
    CalendarsReadOnly := TsgcGoogleCalendarScopes(aSource).CalendarsReadOnly;
    EventsReadWrite := TsgcGoogleCalendarScopes(aSource).EventsReadWrite;
    EventsReadOnly := TsgcGoogleCalendarScopes(aSource).EventsReadOnly;
    Settings := TsgcGoogleCalendarScopes(aSource).Settings;
    CalendarAddOn := TsgcGoogleCalendarScopes(aSource).CalendarAddOn;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTP_Google_Cloud_Calendar_API_Client.Create
  (aOwner: TComponent);
begin
  inherited;
  GoogleCloudOptions.JWT.API_Endpoint := 'https://calendar.googleapis.com/';
  FScopes := TsgcGoogleCalendarScopes.Create;
end;

destructor TsgcHTTP_Google_Cloud_Calendar_API_Client.Destroy;
begin
  sgcFree(FScopes);
  inherited;
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_Delete(const aCalendarId,
  aRuleId: String): String;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s/acl/%s',
    [aCalendarId, aRuleId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_Get(const aCalendarId,
  aRuleId: String): String;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s/acl/%s',
    [aCalendarId, aRuleId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_Insert(const aCalendarId
  : String; const aResource: TsgcGoogleCalendarResource_ACL;
  aSendNotifications: Boolean = True): String;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/acl', [aCalendarId]);
  if not aSendNotifications then
    vURL := AddQueryParameter(vURL, 'SendNotifications', 'false');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_List(const aCalendarId
  : String; aMaxResults: Integer = 100; aPageToken: String = '';
  aShowDeleted: Boolean = False; aSyncToken: String = ''): String;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/acl', [aCalendarId]);
  if aMaxResults <> 100 then
    vURL := AddQueryParameter(vURL, 'MaxResults', aMaxResults);
  if aPageToken <> '' then
    vURL := AddQueryParameter(vURL, 'PageToken', aPageToken);
  if aShowDeleted then
    vURL := AddQueryParameter(vURL, 'ShowDeleted', 'true');
  if aSyncToken <> '' then
    vURL := AddQueryParameter(vURL, 'SyncToken', aSyncToken);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_Patch(const aCalendarId,
  aRuleId: String; const aResource: TsgcGoogleCalendarResource_ACL;
  aSendNotifications: Boolean = True): String;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/acl', [aCalendarId]);
  if not aSendNotifications then
    vURL := AddQueryParameter(vURL, 'SendNotifications', 'false');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PATCH, CS_GOOGLE_CALENDAR_API_URL +
    vURL, aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_Update(const aCalendarId,
  aRuleId: String; const aResource: TsgcGoogleCalendarResource_ACL;
  aSendNotifications: Boolean = True): String;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/acl', [aCalendarId]);
  if not aSendNotifications then
    vURL := AddQueryParameter(vURL, 'SendNotifications', 'false');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.ACL_Watch(const aCalendarId
  : String; const aWatch: TsgcGoogleCalendarWatch): String;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s/acl/watch',
    [aCalendarId]), aWatch.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_Delete
  (const aCalendarId: string): string;
begin

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/users/me/calendarList/%s',
    [aCalendarId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_Get
  (const aCalendarId: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/users/me/calendarList/%s',
    [aCalendarId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_Insert
  (const aResource: TsgcGoogleCalendarResource_CalendarList;
  aColorRgbFormat: Boolean = False): string;
var
  vURL: string;
begin
  vURL := '/users/me/calendarList';
  if aColorRgbFormat then
    vURL := AddQueryParameter(vURL, 'colorRgbFormat', 'true');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_List
  (aMaxResults: Integer = 250; aMinAccessRole: string = '';
  aPageToken: string = ''; aShowDeleted: Boolean = False;
  aShowHidden: Boolean = False; aSyncToken: string = ''): string;
var
  vURL: string;
begin
  vURL := '/users/me/calendarList';
  if aMaxResults <> 250 then
    vURL := AddQueryParameter(vURL, 'MaxResults', aMaxResults);
  if aMinAccessRole <> '' then
    vURL := AddQueryParameter(vURL, 'MinAccessRole', aMinAccessRole);
  if aPageToken <> '' then
    vURL := AddQueryParameter(vURL, 'PageToken', aPageToken);
  if aShowDeleted then
    vURL := AddQueryParameter(vURL, 'ShowDeleted', 'true');
  if aShowHidden then
    vURL := AddQueryParameter(vURL, 'ShowHidden', 'true');
  if aSyncToken <> '' then
    vURL := AddQueryParameter(vURL, 'SyncToken', aSyncToken);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_Patch
  (const aCalendarId: string;
  const aResource: TsgcGoogleCalendarResource_CalendarList;
  aColorRgbFormat: Boolean = False): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/users/me/calendarList/%s', [aCalendarId]);
  if aColorRgbFormat then
    vURL := AddQueryParameter(vURL, 'colorRgbFormat', 'true');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PATCH, CS_GOOGLE_CALENDAR_API_URL +
    vURL, aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_Update
  (const aCalendarId: string;
  const aResource: TsgcGoogleCalendarResource_CalendarList;
  aColorRgbFormat: Boolean = False): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/users/me/calendarList/%s', [aCalendarId]);
  if aColorRgbFormat then
    vURL := AddQueryParameter(vURL, 'colorRgbFormat', 'true');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PUT, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.CalendarList_Watch
  (const aWatch: TsgcGoogleCalendarWatch): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL +
    '/users/me/calendarList/watch', aWatch.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Watch(const aCalendarId
  : string; const aWatch: TsgcGoogleCalendarWatch): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL +
    FormatAndEncode('/calendars/%s/events/watch', [aCalendarId]),
    aWatch.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Calendar_Clear
  (const aCalendarId: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s/clear',
    [aCalendarId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Calendar_Delete
  (const aCalendarId: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s',
    [aCalendarId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Calendar_Get
  (const aCalendarId: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s',
    [aCalendarId]));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Calendar_Insert
  (const aResource: TsgcGoogleCalendarResource_Calendar): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL +
    '/calendars', aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Calendar_Patch
  (const aCalendarId: string;
  const aResource: TsgcGoogleCalendarResource_Calendar): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PATCH,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s', [aCalendarId]
    ), aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Calendar_Update
  (const aCalendarId: string;
  const aResource: TsgcGoogleCalendarResource_Calendar): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PUT,
    FormatAndEncode(CS_GOOGLE_CALENDAR_API_URL + '/calendars/%s', [aCalendarId]
    ), aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Channel_Stop(const aId,
  aResourceId: string; const aToken: string = ''): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('id', aId);
    oJSON.AddPair('resourceId', aResourceId);
    if aToken <> '' then
      oJSON.AddPair('token', aToken);

    result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL +
      '/channels/stop', oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Color_Get: string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL +
    '/colors');
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.DoGetScope: string;
var
  oScopes: TStringList;
begin
  oScopes := TStringList.Create;
  Try
    if Scopes.CalendarsReadWrite then
      oScopes.Add('https://www.googleapis.com/auth/calendar');
    if Scopes.CalendarsReadOnly then
      oScopes.Add('https://www.googleapis.com/auth/calendar.readonly');
    if Scopes.EventsReadWrite then
      oScopes.Add('https://www.googleapis.com/auth/calendar.events');
    if Scopes.EventsReadOnly then
      oScopes.Add('https://www.googleapis.com/auth/calendar.events.readonly');
    if Scopes.Settings then
      oScopes.Add('https://www.googleapis.com/auth/calendar.settings.readonly');
    if Scopes.CalendarAddOn then
      oScopes.Add('https://www.googleapis.com/auth/calendar.addons.execute');

    result := oScopes.Text;
  Finally
    sgcFree(oScopes);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Delete
  (const aCalendarId, aEventId: string; aSendNotifications: Boolean = False;
  const aSendUpdates: string = 'all'): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/%s', [aCalendarId, aEventId]);
  if aSendNotifications then
    vURL := AddQueryParameter(vURL, 'sendNotifications', 'true');
  if aSendUpdates <> '' then
    vURL := AddQueryParameter(vURL, 'sendUpdates', aSendUpdates);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Get(const aCalendarId,
  aEventId: string; aMaxAttendees: Integer = 0;
  const aTimeZone: String = ''): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/%s', [aCalendarId, aEventId]);
  if aMaxAttendees > 0 then
    vURL := AddQueryParameter(vURL, 'maxAttendees', aMaxAttendees);
  if aTimeZone <> '' then
    vURL := AddQueryParameter(vURL, 'timeZone', aTimeZone);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Move(const aCalendarId,
  aEventId, aDestination: string; const aSendUpdates: string = 'all'): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/%s/move',
    [aCalendarId, aEventId]);
  vURL := AddQueryParameter(vURL, 'destination', aDestination);
  if aSendUpdates <> '' then
    vURL := AddQueryParameter(vURL, 'sendUpdates', aSendUpdates);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Import
  (const aCalendarId: string; const aResource: TsgcGoogleCalendarResource_Event;
  aConferenceDataVersion: Integer = 0;
  aSupportsAttachments: Boolean = False): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/import', [aCalendarId]);
  if aConferenceDataVersion > 0 then
    vURL := AddQueryParameter(vURL, 'conferenceDataVersion',
      aConferenceDataVersion);
  if aSupportsAttachments then
    vURL := AddQueryParameter(vURL, 'supportsAttachments', 'true');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Insert
  (const aCalendarId: string; const aResource: TsgcGoogleCalendarResource_Event;
  aConferenceDataVersion: Integer = 0; aMaxAttendees: Integer = 0;
  const aSendUpdates: string = 'all';
  aSupportsAttachments: Boolean = False): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events', [aCalendarId]);
  if aConferenceDataVersion > 0 then
    vURL := AddQueryParameter(vURL, 'conferenceDataVersion',
      aConferenceDataVersion);
  if aMaxAttendees > 0 then
    vURL := AddQueryParameter(vURL, 'maxAttendees', aMaxAttendees);
  if aSendUpdates <> '' then
    vURL := AddQueryParameter(vURL, 'sendUpdates', aSendUpdates);
  if aSupportsAttachments then
    vURL := AddQueryParameter(vURL, 'supportsAttachments', 'true');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Patch
  (const aCalendarId, aEventId: string;
  const aResource: TsgcGoogleCalendarResource_Event;
  aConferenceDataVersion: Integer = 0; aMaxAttendees: Integer = 0;
  const aSendUpdates: string = 'all';
  aSupportsAttachments: Boolean = False): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/%s', [aCalendarId, aEventId]);
  if aConferenceDataVersion > 0 then
    vURL := AddQueryParameter(vURL, 'conferenceDataVersion',
      aConferenceDataVersion);
  if aMaxAttendees > 0 then
    vURL := AddQueryParameter(vURL, 'maxAttendees', aMaxAttendees);
  if aSendUpdates <> '' then
    vURL := AddQueryParameter(vURL, 'sendUpdates', aSendUpdates);
  if aSupportsAttachments then
    vURL := AddQueryParameter(vURL, 'supportsAttachments', 'true');

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PATCH, CS_GOOGLE_CALENDAR_API_URL +
    vURL, aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Instances
  (const aCalendarId, aEventId: string; aMaxAttendees: Integer = 0;
  aMaxResults: Integer = 250; const aOriginalStart: string = '';
  const aPageToken: string = ''; aShowDeleted: Boolean = False;
  aTimeMax: TDateTime = 0; aTimeMin: TDateTime = 0;
  const aTimeZone: String = ''): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/eventId/%s',
    [aCalendarId, aEventId]);
  if aMaxAttendees > 0 then
    vURL := AddQueryParameter(vURL, 'maxAttendees', aMaxAttendees);
  if aMaxResults > 0 then
    vURL := AddQueryParameter(vURL, 'maxResults', aMaxResults);
  if aOriginalStart <> '' then
    vURL := AddQueryParameter(vURL, 'originalStart', aOriginalStart);
  if aPageToken <> '' then
    vURL := AddQueryParameter(vURL, 'pageToken', aPageToken);
  if aShowDeleted then
    vURL := AddQueryParameter(vURL, 'showDeleted', 'true');
  if aTimeMax > 0 then
    vURL := AddQueryParameter(vURL, 'timeMax', GetStrDateTime(aTimeMax));
  if aTimeMin > 0 then
    vURL := AddQueryParameter(vURL, 'timeMin', GetStrDateTime(aTimeMin));
  if aTimeZone <> '' then
    vURL := AddQueryParameter(vURL, 'timeZone', aTimeZone);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_List(const aCalendarId
  : string; aICalUID: string = ''; aMaxAttendees: Integer = 0;
  aMaxResults: Integer = 250; const aOrderBy: string = '';
  const aPageToken: string = ''; const aPrivateExtendedProperty: string = '';
  const aQ: string = ''; const aSharedExtendedProperty: string = '';
  aShowDeleted: Boolean = False; aShowHiddenInvitations: Boolean = False;
  aSingleEvents: Boolean = False; aSyncToken: string = '';
  aTimeMax: TDateTime = 0; aTimeMin: TDateTime = 0;
  const aTimeZone: String = ''; aUpdatedMin: TDateTime = 0): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events', [aCalendarId]);
  if aICalUID <> '' then
    vURL := AddQueryParameter(vURL, 'iCalUID', aICalUID);
  if aMaxAttendees > 0 then
    vURL := AddQueryParameter(vURL, 'maxAttendees', aMaxAttendees);
  if aMaxResults > 0 then
    vURL := AddQueryParameter(vURL, 'maxResults', aMaxResults);
  if aOrderBy <> '' then
    vURL := AddQueryParameter(vURL, 'orderBy', aOrderBy);
  if aPageToken <> '' then
    vURL := AddQueryParameter(vURL, 'pageToken', aPageToken);
  if aPrivateExtendedProperty <> '' then
    vURL := AddQueryParameter(vURL, 'privateExtendedProperty',
      aPrivateExtendedProperty);
  if aQ <> '' then
    vURL := AddQueryParameter(vURL, 'q', aQ);
  if aSharedExtendedProperty <> '' then
    vURL := AddQueryParameter(vURL, 'sharedExtendedProperty',
      aSharedExtendedProperty);
  if aShowDeleted then
    vURL := AddQueryParameter(vURL, 'showDeleted', 'true');
  if aShowHiddenInvitations then
    vURL := AddQueryParameter(vURL, 'showHiddenInvitations', 'true');
  if aSingleEvents then
    vURL := AddQueryParameter(vURL, 'singleEvents', 'true');
  if aSyncToken <> '' then
    vURL := AddQueryParameter(vURL, 'syncToken', aSyncToken);
  if aTimeMin > 0 then
    vURL := AddQueryParameter(vURL, 'timeMin', GetStrDateTime(aTimeMin));
  if aTimeMax > 0 then
    vURL := AddQueryParameter(vURL, 'timeMax', GetStrDateTime(aTimeMax));
  if aTimeZone <> '' then
    vURL := AddQueryParameter(vURL, 'timeZone', aTimeZone);
  if aUpdatedMin > 0 then
    vURL := AddQueryParameter(vURL, 'updatedMin', GetStrDateTime(aUpdatedMin));

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_Update
  (const aCalendarId, aEventId: string;
  const aResource: TsgcGoogleCalendarResource_Event;
  aConferenceDataVersion: Integer = 0; aMaxAttendees: Integer = 0;
  const aSendUpdates: string = 'all';
  aSupportsAttachments: Boolean = False): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/%s', [aCalendarId, aEventId]);
  if aConferenceDataVersion > 0 then
    vURL := AddQueryParameter(vURL, 'conferenceDataVersion',
      aConferenceDataVersion);
  if aMaxAttendees > 0 then
    vURL := AddQueryParameter(vURL, 'maxAttendees', aMaxAttendees);
  if aSendUpdates <> '' then
    vURL := AddQueryParameter(vURL, 'sendUpdates', aSendUpdates);
  if aSupportsAttachments then
    vURL := AddQueryParameter(vURL, 'supportsAttachments', 'true');

  // ... update property
  aResource.Updated := Now;

  HTTPClient.Request.ContentType := 'application/json; charset=utf-8';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PUT, CS_GOOGLE_CALENDAR_API_URL + vURL,
    aResource.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Event_QuickAdd
  (const aCalendarId, aText: string;
  const aSendUpdates: string = 'all'): string;
var
  vURL: string;
begin
  vURL := FormatAndEncode('/calendars/%s/events/quickAdd', [aCalendarId]);
  vURL := AddQueryParameter(vURL, 'text', aText);
  if aSendUpdates <> '' then
    vURL := AddQueryParameter(vURL, 'sendUpdates', aSendUpdates);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.FormatAndEncode
  (const aFormat: string; const Args: array of const): string;
begin
  result := DoPathEncode(Format(aFormat, Args));
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Settings_Watch
  (const aWatch: TsgcGoogleCalendarWatch): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL +
    '/users/me/settings/watch', aWatch.JSONText);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Freebusy_Query
  (const aItems: TStrings; aTimeMin, aTimeMax: TDateTime;
  const aTimeZone: String = ''; aGroupExpansionMax: Integer = 0;
  aCalendarExpansionMax: Integer = 0): string;
var
  i: Integer;
  oJSON: TsgcJSON;
  oJSONObject, oJSONObject2: IsgcObjectJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('timeMin', GetStrDateTime(aTimeMin));
    oJSON.AddPair('timeMax', GetStrDateTime(aTimeMax));
    if aTimeZone <> '' then
      oJSON.AddPair('timeZone', aTimeZone);
    if aGroupExpansionMax > 0 then
      oJSON.AddPair('groupExpansionMax', aGroupExpansionMax);
    if aCalendarExpansionMax > 0 then
      oJSON.AddPair('calendarExpansionMax', aCalendarExpansionMax);
    if aItems.count > 0 then
    begin
      oJSONObject := oJSON.AddObject('items');
      oJSONObject.JSONObject.IsArray := True;
      for i := 0 to aItems.count - 1 do
      begin
        oJSONObject2 := oJSONObject.JSONObject.AddObject(IntToStr(i));
        oJSONObject2.JSONObject.AddPair('id', aItems[i]);
      end;
    end;

    result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST, CS_GOOGLE_CALENDAR_API_URL +
      '/freeBusy', oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcHTTP_Google_Cloud_Calendar_API_Client.SetScopes
  (const Value: TsgcGoogleCalendarScopes);
begin
  if Assigned(FScopes) then
    FScopes.Assign(Value);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Settings_Get
  (const aSetting: string): string;
var
  vURL: string;
begin
  vURL := Format('/users/me/settings/%s', [aSetting]);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);
end;

function TsgcHTTP_Google_Cloud_Calendar_API_Client.Settings_List
  (aMaxResults: Integer = 0; const aPageToken: string = '';
  const aSyncToken: string = ''): string;
var
  vURL: string;
begin
  vURL := '/users/me/settings';
  if aMaxResults > 0 then
    vURL := AddQueryParameter(vURL, 'maxResults', aMaxResults);
  if aPageToken <> '' then
    vURL := AddQueryParameter(vURL, 'pageToken', aPageToken);
  if aSyncToken <> '' then
    vURL := AddQueryParameter(vURL, 'syncToken', aSyncToken);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, CS_GOOGLE_CALENDAR_API_URL + vURL);

end;

constructor TsgcGoogleCalendarResource_ACL.Create;
begin
  inherited;
  Kind := 'calendar#aclRule';
end;

destructor TsgcGoogleCalendarResource_ACL.Destroy;
begin
  sgcFree(FScope);
  inherited;
end;

procedure TsgcGoogleCalendarResource_ACL.DoJSONBody;
var
  oJSONObject: IsgcObjectJSON;
begin
  inherited;
  oJSONObject := JSON.AddObject('scope');
  oJSONObject.JSONObject.AddPair('type', GetScopeTypeString(Scope._type));
  oJSONObject.JSONObject.AddPair('Value', Scope.Value);
  JSON.AddPair('Role', GetRoleString(Role));
end;

function TsgcGoogleCalendarResource_ACL.GetScope: TsgcGoogleCalendarScope;
begin
  if not Assigned(FScope) then
    FScope := TsgcGoogleCalendarScope.Create;
  result := FScope;
end;

constructor TsgcGoogleCalendarResource.Create;
begin
  inherited;
end;

destructor TsgcGoogleCalendarResource.Destroy;
begin
  sgcFree(FJSON);
  inherited;
end;

procedure TsgcGoogleCalendarResource.DoRead;
begin
  inherited;
  Kind := DoReadJSONValue('kind');
  Etag := DoReadJSONValue('etag');
  id := DoReadJSONValue('id');
end;

function TsgcGoogleCalendarResource.JSONText: String;
begin
  result := inherited JSONText;

  JSON.AddPair('kind', Kind);
  JSON.AddPair('etag', sgcStringReplace(Etag, '\"', '"'));
  JSON.AddPair('id', id);
  DoJSONBody;

  result := JSON.Text;
end;

procedure TsgcGoogleCalendarJSON.DoRead;
begin

end;

function TsgcGoogleCalendarJSON.DoReadJSONValue(const aField: string;
  aJSONObject: IsgcObjectJSON = nil): Variant;
begin
  Try
    if GetObjectJSON(aJSONObject).Node[aField] <> nil then
    begin
      if GetObjectJSON(aJSONObject).Node[aField].JSONType = sgcJSONObject then
        result := GetObjectJSON(aJSONObject).Node[aField].JSONObject.Text
      else if GetObjectJSON(aJSONObject).Node[aField].JSONType = sgcJSONList
      then
      begin
        GetObjectJSON(aJSONObject).Node[aField].JSONObject.IsArray := True;
        result := GetObjectJSON(aJSONObject).Node[aField].JSONObject.Text;
      end
      else if GetObjectJSON(aJSONObject).Node[aField].JSONType = sgcJSONString
      then
        result := GetObjectJSON(aJSONObject).Node[aField].Value
      else
        result := GetObjectJSON(aJSONObject).Node[aField].Value;
    end;
  Except
    result := '';
  End;
end;

function TsgcGoogleCalendarJSON.DoReadJSONValue(aItem: Integer;
  aJSONObject: TsgcObjectJSON = nil): Variant;
begin
  Try
    if aItem < JSON.count then
    begin
      if JSON.Item[aItem] <> nil then
      begin
        if JSON.Item[aItem].JSONType = sgcJSONString then
          result := JSON.Item[aItem].Value
        else
          result := JSON.Item[aItem].Value;
      end;
    end;
  Except
    result := '';
  End;
end;

function TsgcGoogleCalendarJSON.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
  begin
    FJSON := TsgcJSON.Create(nil);
    FJSON.EscapeStrings := True;
  end;
  result := FJSON;
end;

function TsgcGoogleCalendarJSON.GetObjectJSON(aJSONObject: IsgcObjectJSON = nil)
  : IsgcJSON;
begin
  if Assigned(aJSONObject) then
    result := aJSONObject.JSONObject
  else
    result := JSON;
end;

function TsgcGoogleCalendarJSON.JSONText: String;
begin
  JSON.Clear;

  result := '';
end;

procedure TsgcGoogleCalendarJSON.Read(const aValue: string);
begin
  JSON.Read(aValue);

  DoRead;
end;

constructor TsgcGoogleCalendarWatch.Create;
begin
  inherited;
  ttl := '604800';
end;

function TsgcGoogleCalendarWatch.JSONText: String;
var
  oJSONObject: IsgcObjectJSON;
begin
  result := inherited JSONText;

  JSON.AddPair('id', id);
  JSON.AddPair('token', token);
  JSON.AddPair('type', _type);
  JSON.AddPair('address', address);
  oJSONObject := JSON.AddObject('params');
  oJSONObject.JSONObject.AddPair('ttl', ttl);
end;

constructor TsgcGoogleCalendarResource_CalendarList.Create;
begin
  inherited;
  Kind := 'calendar#calendarListEntry';
  AccessRole := gcrOwner;
  Deleted := False;
  Primary := False;
  Selected := False;
end;

destructor TsgcGoogleCalendarResource_CalendarList.Destroy;
begin
  sgcFree(FNotificationSettings);
  sgcFree(FDefaultReminders);
  sgcFree(FConferenceProperties);
  inherited;
end;

procedure TsgcGoogleCalendarResource_CalendarList.DoJSONBody;
var
  i: Integer;
  oJSONObject: IsgcObjectJSON;
  oDefaultReminder: TsgcGoogleCalendarDefaultReminderItem;
  oNotification: TsgcGoogleCalendarNotificationItem;
begin
  inherited;
  if Summary <> '' then
    JSON.AddPair('summary', Summary);
  if Description <> '' then
    JSON.AddPair('description', Description);
  if Location <> '' then
    JSON.AddPair('location', Location);
  if TimeZone <> '' then
    JSON.AddPair('timeZone', TimeZone);
  if SummaryOverride <> '' then
    JSON.AddPair('summaryOverride', SummaryOverride);
  if ColorId <> '' then
    JSON.AddPair('colorId', ColorId);
  if BackgroundColor <> '' then
    JSON.AddPair('backgroundColor', BackgroundColor);
  if ForegroundColor <> '' then
    JSON.AddPair('foregroundColor', ForegroundColor);
  JSON.AddPair('hidden', Hidden);
  JSON.AddPair('selected', Selected);
  JSON.AddPair('accessRole', GetRoleString(AccessRole));
  if DefaultReminders.count > 0 then
  begin
    oJSONObject := JSON.AddObject('defaultReminders');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to DefaultReminders.count - 1 do
    begin
      oDefaultReminder := TsgcGoogleCalendarDefaultReminderItem
        (DefaultReminders.Item[i]);
      if Assigned(oDefaultReminder) then
        oJSONObject.JSONObject.AddPair
          (GetReminderMethodString(oDefaultReminder.Method),
          oDefaultReminder.Minutes);
    end;
  end;
  if NotificationSettings.count > 0 then
  begin
    oJSONObject := JSON.AddObject('notificationSettings');
    oJSONObject := oJSONObject.JSONObject.AddObject('notifications');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to NotificationSettings.count - 1 do
    begin
      oNotification := TsgcGoogleCalendarNotificationItem
        (NotificationSettings.Item[i]);
      oJSONObject.JSONObject.AddPair
        (GetNotificationTypeString(oNotification._type), oNotification.Method);
    end;
  end;
  JSON.AddPair('primary', Primary);
  JSON.AddPair('deleted', Deleted);
  if ConferenceProperties.AllowedConferenceSolutionTypes.count > 0 then
  begin
    oJSONObject := JSON.AddObject('conferenceProperties');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to ConferenceProperties.AllowedConferenceSolutionTypes.
      count - 1 do
      oJSONObject.JSONObject.AddPair
        (ConferenceProperties.AllowedConferenceSolutionTypes[i], '');
  end;
end;

procedure TsgcGoogleCalendarResource_CalendarList.DoRead;
var
  i: Integer;
  oReminder: TsgcGoogleCalendarDefaultReminderItem;
  oNotification: TsgcGoogleCalendarNotificationItem;
begin
  inherited;
  AccessRole := GetRole(DoReadJSONValue('accessRole'));
  BackgroundColor := DoReadJSONValue('backgroundColor');
  ColorId := DoReadJSONValue('colorId');
  if JSON.Node['conferenceProperties'] <> nil then
  begin
    if JSON.Node['conferenceProperties'].Node['allowedConferenceSolutionTypes']
      <> nil then
      ConferenceProperties.AllowedConferenceSolutionTypes.Text :=
        JSON.Node['conferenceProperties'].Node
        ['allowedConferenceSolutionTypes'].Value;
  end;
  DefaultReminders.Clear;
  if JSON.Node['defaultReminders'] <> nil then
  begin
    for i := 0 to JSON.Node['defaultReminders'].JSONObject.count - 1 do
    begin
      oReminder := TsgcGoogleCalendarDefaultReminderItem.Create;
      oReminder.Method := GetReminderMethod(JSON.Node['defaultReminders']
        .JSONObject.Item[i].Node['method'].Value);
      oReminder.Minutes := JSON.Node['defaultReminders'].JSONObject.Item[i].Node
        ['minutes'].Value;
      oReminder.id := IntToStr(i);
      DefaultReminders.AddItem(oReminder);
    end;
  end;
  Deleted := DoReadJSONValue('deleted');
  Description := DoReadJSONValue('description');
  ForegroundColor := DoReadJSONValue('foregroundColor');
  Hidden := DoReadJSONValue('hidden');
  Location := DoReadJSONValue('location');
  NotificationSettings.Clear;
  if JSON.Node['notificationSettings'] <> nil then
  begin
    if JSON.Node['notificationSettings'].Node['notifications'] <> nil then
    begin
      for i := 0 to JSON.Node['notificationSettings'].Node['notifications']
        .JSONObject.count - 1 do
      begin
        oNotification := TsgcGoogleCalendarNotificationItem.Create;
        oNotification.Method := JSON.Node['notificationSettings'].Node
          ['notifications'].JSONObject.Item[i].Node['method'].Value;
        oNotification._type := GetNotificationType
          (JSON.Node['notificationSettings'].Node['notifications']
          .JSONObject.Item[i].Node['type'].Value);
        oNotification.id := IntToStr(i);
        NotificationSettings.AddItem(oNotification);
      end;
    end;
  end;
  Primary := DoReadJSONValue('primary');
  Selected := DoReadJSONValue('selected');
  Summary := DoReadJSONValue('summary');
  SummaryOverride := DoReadJSONValue('summaryOverride');
  TimeZone := DoReadJSONValue('timeZone');
end;

function TsgcGoogleCalendarResource_CalendarList.GetConferenceProperties
  : TsgcGoogleCalendarConferenceProperties;
begin
  if not Assigned(FConferenceProperties) then
    FConferenceProperties := TsgcGoogleCalendarConferenceProperties.Create;
  result := FConferenceProperties;
end;

function TsgcGoogleCalendarResource_CalendarList.GetDefaultReminders
  : TsgcGoogleCalendarDefaultReminders;
begin
  if not Assigned(FDefaultReminders) then
  begin
    FDefaultReminders := TsgcGoogleCalendarDefaultReminders.Create;
    FDefaultReminders.OwnObjects := True;
  end;
  result := FDefaultReminders;
end;

function TsgcGoogleCalendarResource_CalendarList.GetNotificationSettings
  : TsgcGoogleCalendarNotificationSettings;
begin
  if not Assigned(FNotificationSettings) then
  begin
    FNotificationSettings := TsgcGoogleCalendarNotificationSettings.Create;
    FNotificationSettings.OwnObjects := True;
  end;
  result := FNotificationSettings;
end;

destructor TsgcGoogleCalendarConferenceProperties.Destroy;
begin
  sgcFree(FAllowedConferenceSolutionTypes);
  inherited;
end;

function TsgcGoogleCalendarConferenceProperties.
  GetAllowedConferenceSolutionTypes: TStringList;
begin
  if not Assigned(FAllowedConferenceSolutionTypes) then
    FAllowedConferenceSolutionTypes := TStringList.Create;
  result := FAllowedConferenceSolutionTypes;
end;

constructor TsgcGoogleCalendarNotificationItem.Create;
begin
  inherited;
  Method := 'email';
end;

destructor TsgcGoogleCalendarResource_Calendar.Destroy;
begin
  sgcFree(FConferenceProperties);
  inherited;
end;

procedure TsgcGoogleCalendarResource_Calendar.DoJSONBody;
var
  i: Integer;
  oJSONObject: IsgcObjectJSON;
begin
  inherited;
  if Summary <> '' then
    JSON.AddPair('summary', Summary);
  if Description <> '' then
    JSON.AddPair('description', Description);
  if Location <> '' then
    JSON.AddPair('location', Location);
  if TimeZone <> '' then
    JSON.AddPair('timeZone', TimeZone);
  if ConferenceProperties.AllowedConferenceSolutionTypes.count > 0 then
  begin
    oJSONObject := JSON.AddObject('conferenceProperties');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to ConferenceProperties.AllowedConferenceSolutionTypes.
      count - 1 do
      oJSONObject.JSONObject.AddPair
        (ConferenceProperties.AllowedConferenceSolutionTypes[i], '');
  end;
end;

procedure TsgcGoogleCalendarResource_Calendar.DoRead;
begin
  inherited;
  // FConferenceProperties: TsgcGoogleCalendarConferenceProperties;
  Description := DoReadJSONValue('description');
  Location := DoReadJSONValue('location');
  Summary := DoReadJSONValue('summary');
  TimeZone := DoReadJSONValue('timeZone');
end;

function TsgcGoogleCalendarResource_Calendar.GetConferenceProperties
  : TsgcGoogleCalendarConferenceProperties;
begin
  if not Assigned(FConferenceProperties) then
    FConferenceProperties := TsgcGoogleCalendarConferenceProperties.Create;
  result := FConferenceProperties;
end;

constructor TsgcGoogleCalendarResource_Event.Create;
begin
  inherited;
  Created := 0;
  Updated := 0;
end;

destructor TsgcGoogleCalendarResource_Event.Destroy;
begin
  sgcFree(FAttachments);
  sgcFree(FAttendees);
  sgcFree(FSource);
  sgcFree(FReminders);
  sgcFree(FGadget);
  sgcFree(FConferenceData);
  sgcFree(FExtendedProperties);
  sgcFree(FOriginalStartTime);
  sgcFree(FRecurrence);
  sgcFree(F_End);
  sgcFree(FStart);
  sgcFree(FOrganizer);
  sgcFree(FCreator);
  inherited;
end;

procedure TsgcGoogleCalendarResource_Event.DoJSONBody;
var
  i: Integer;
  oAttendee: TsgcGoogleCalendarAttendeeItem;
  oAttachment: TsgcGoogleCalendarAttachmentItem;
  oEntryPoint: TsgcGoogleCalendarConferenceEntryPointItem;
  oJSONObject, oJSONObject2, oJSONObject3, oCreateRequest: IsgcObjectJSON;
  oReminder: TsgcGoogleCalendarRemindersOverridesItem;
begin
  inherited;
  if Status <> gcesConfirmed then
    JSON.AddPair('status', GetStatusString(Status));
  if Htmllink <> '' then
    JSON.AddPair('htmlLink', Htmllink);
  if Created > 0 then
    JSON.AddPair('created', GetStrDateTime(Created));
  if Updated > 0 then
    JSON.AddPair('updated', GetStrDateTime(Updated));
  if Summary <> '' then
    JSON.AddPair('summary', Summary);
  if Description <> '' then
    JSON.AddPair('description', Description);
  if Location <> '' then
    JSON.AddPair('location', Location);
  if ColorId <> '' then
    JSON.AddPair('colorId', ColorId);
  // ... creator
  if Assigned(FCreator) then
  begin
    oJSONObject := JSON.AddObject('creator');
    if Creator.id <> '' then
      oJSONObject.JSONObject.AddPair('id', Creator.id);
    if Creator.Email <> '' then
      oJSONObject.JSONObject.AddPair('email', Creator.Email);
    if Creator.DisplayName <> '' then
      oJSONObject.JSONObject.AddPair('displayName', Creator.DisplayName);
    oJSONObject.JSONObject.AddPair('self', Creator._self);
  end;
  // ... organizer
  if Assigned(FOrganizer) then
  begin
    oJSONObject := JSON.AddObject('organizer');
    if Organizer.id <> '' then
      oJSONObject.JSONObject.AddPair('id', Organizer.id);
    if Organizer.Email <> '' then
      oJSONObject.JSONObject.AddPair('email', Organizer.Email);
    if Organizer.DisplayName <> '' then
      oJSONObject.JSONObject.AddPair('displayName', Organizer.DisplayName);
    oJSONObject.JSONObject.AddPair('self', Organizer._self);
  end;
  // ... start
  if Assigned(FStart) then
  begin
    oJSONObject := JSON.AddObject('start');
    if Start.Date > 0 then
      oJSONObject.JSONObject.AddPair('date', FormatDateTime('yyyy-mm-dd',
        Start.Date))
    else if Start.DateTime > 0 then
      oJSONObject.JSONObject.AddPair('dateTime',
        GetStrDateTime(Start.DateTime));
    if Start.TimeZone <> '' then
      oJSONObject.JSONObject.AddPair('timeZone', Start.TimeZone);
  end;
  // ... end
  if Assigned(F_End) then
  begin
    oJSONObject := JSON.AddObject('end');
    if _End.Date > 0 then
      oJSONObject.JSONObject.AddPair('date', FormatDateTime('yyyy-mm-dd',
        _End.DateTime))
    else if _End.DateTime > 0 then
      oJSONObject.JSONObject.AddPair('dateTime', GetStrDateTime(_End.DateTime));
    if _End.TimeZone <> '' then
      oJSONObject.JSONObject.AddPair('timeZone', _End.TimeZone);
  end;
  JSON.AddPair('endTimeUnspecified', EndTimeUnspecified);
  // ... recurrence
  if Recurrence.count > 0 then
  begin
    oJSONObject := JSON.AddObject('recurrence');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to Recurrence.count - 1 do
      oJSONObject.JSONObject.AddArray(Recurrence[i]);
  end;
  if RecurringEventId <> '' then
    JSON.AddPair('recurringEventId', RecurringEventId);
  if Assigned(FOriginalStartTime) then
  begin
    if OriginalStartTime.DateTime > 0 then
    begin
      oJSONObject := JSON.AddObject('originalStartTime');
      oJSONObject.JSONObject.AddPair('date', FormatDateTime('yyyy-mm-dd',
        OriginalStartTime.DateTime));
      oJSONObject.JSONObject.AddPair('dateTime',
        GetStrDateTime(OriginalStartTime.DateTime));
      if _End.TimeZone <> '' then
        oJSONObject.JSONObject.AddPair('timeZone', OriginalStartTime.TimeZone);
    end;
  end;
  if Transparency <> '' then
    JSON.AddPair('transparency', Transparency);
  if Visibility <> gcevDefault then
    JSON.AddPair('visibility', GetVisibilityString(Visibility));
  if iCalUID <> '' then
    JSON.AddPair('iCalUID', iCalUID);
  if Sequence > 0 then
    JSON.AddPair('sequence', Sequence);
  // ... attendees
  if Attendees.count > 0 then
  begin
    oJSONObject := JSON.AddObject('attendees');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to Attendees.count - 1 do
    begin
      oAttendee := TsgcGoogleCalendarAttendeeItem(Attendees.Item[i]);
      oJSONObject2 := oJSONObject.JSONObject.AddObject(IntToStr(i));
      if oAttendee.id <> '' then
        oJSONObject2.JSONObject.AddPair('id', oAttendee.id);
      if oAttendee.Email <> '' then
        oJSONObject2.JSONObject.AddPair('email', oAttendee.Email);
      if oAttendee.DisplayName <> '' then
        oJSONObject2.JSONObject.AddPair('displayName', oAttendee.DisplayName);
      oJSONObject2.JSONObject.AddPair('organizer', oAttendee.Organizer);
      oJSONObject2.JSONObject.AddPair('self', oAttendee._self);
      oJSONObject2.JSONObject.AddPair('resource', oAttendee.Resource);
      oJSONObject2.JSONObject.AddPair('optional', oAttendee.Optional);
      if oAttendee.ResponseStatus <> '' then
        oJSONObject2.JSONObject.AddPair('responseStatus',
          oAttendee.ResponseStatus);
      if oAttendee.Comment <> '' then
        oJSONObject2.JSONObject.AddPair('comment', oAttendee.Comment);
      if oAttendee.AdditionalGuests > 0 then
        oJSONObject2.JSONObject.AddPair('additionalGuests',
          oAttendee.AdditionalGuests);
    end;
  end;
  JSON.AddPair('attendeesOmitted', AttendeesOmitted);
  // ... extended properties
  if Assigned(FExtendedProperties) then
  begin
    oJSONObject := JSON.AddObject('extendedProperties');
    if ExtendedProperties._Private.count > 0 then
    begin
      oJSONObject2 := oJSONObject.JSONObject.AddObject('private');
      for i := 0 to ExtendedProperties._Private.count - 1 do
        oJSONObject2.JSONObject.AddPair(ExtendedProperties._Private.Names[i],
          ExtendedProperties._Private.ValueFromIndex[i]);
    end;
    if ExtendedProperties._Shared.count > 0 then
    begin
      oJSONObject2 := oJSONObject.JSONObject.AddObject('shared');
      for i := 0 to ExtendedProperties._Shared.count - 1 do
        oJSONObject2.JSONObject.AddPair(ExtendedProperties._Shared.Names[i],
          ExtendedProperties._Shared.ValueFromIndex[i]);
    end;
  end;
  if HangoutLink <> '' then
    JSON.AddPair('hangoutLink', HangoutLink);
  // ... conference data
  if Assigned(FConferenceData) then
  begin
    // ... create request
    if Assigned(ConferenceData.FCreateRequest) then
    begin
      oCreateRequest := JSON.AddObject('createRequest');
      if ConferenceData.CreateRequest.RequestId <> '' then
        oCreateRequest.JSONObject.AddPair('requestId',
          ConferenceData.CreateRequest.RequestId);
      if ConferenceData.CreateRequest.ConferenceSolutionKey._type <> '' then
      begin
        oJSONObject2 := oCreateRequest.JSONObject.AddObject
          ('conferenceSolutionKey');
        oJSONObject2.JSONObject.AddPair('type',
          ConferenceData.CreateRequest.ConferenceSolutionKey._type);
      end;
      if ConferenceData.CreateRequest.Status.StatusCode <> '' then
      begin
        oJSONObject2 := oCreateRequest.JSONObject.AddObject('status');
        oJSONObject2.JSONObject.AddPair('statusCode',
          ConferenceData.CreateRequest.Status.StatusCode);
      end;
      // ... entry points
      if ConferenceData.EntryPoints.count > 0 then
      begin
        oJSONObject := oCreateRequest.JSONObject.AddObject('entryPoints');
        oJSONObject.JSONObject.IsArray := True;
        for i := 0 to ConferenceData.EntryPoints.count - 1 do
        begin
          oEntryPoint := TsgcGoogleCalendarConferenceEntryPointItem
            (ConferenceData.EntryPoints.Item[i]);
          if Assigned(oEntryPoint) then
          begin
            oJSONObject2 := oJSONObject.JSONObject.AddObject(IntToStr(i));
            if oEntryPoint.EntryPointType <> '' then
              oJSONObject2.JSONObject.AddPair('entryPointType',
                oEntryPoint.EntryPointType);
            if oEntryPoint.Uri <> '' then
              oJSONObject2.JSONObject.AddPair('uri', oEntryPoint.Uri);
            if oEntryPoint._Label <> '' then
              oJSONObject2.JSONObject.AddPair('label', oEntryPoint._Label);
            if oEntryPoint.Pin <> '' then
              oJSONObject2.JSONObject.AddPair('pin', oEntryPoint.Pin);
            if oEntryPoint.AccessCode <> '' then
              oJSONObject2.JSONObject.AddPair('accessCode',
                oEntryPoint.AccessCode);
            if oEntryPoint.MeetingCode <> '' then
              oJSONObject2.JSONObject.AddPair('meetingCode',
                oEntryPoint.MeetingCode);
            if oEntryPoint.Passcode <> '' then
              oJSONObject2.JSONObject.AddPair('passcode', oEntryPoint.Passcode);
            if oEntryPoint.Password <> '' then
              oJSONObject2.JSONObject.AddPair('password', oEntryPoint.Password);
          end;
        end;
      end;
      // ... conference solution
      if Assigned(ConferenceData.FConferenceSolution) then
      begin
        oJSONObject := oCreateRequest.JSONObject.AddObject
          ('conferenceSolution');
        if ConferenceData.ConferenceSolution.Key._type <> '' then
        begin
          oJSONObject2 := oJSONObject.JSONObject.AddObject('key');
          oJSONObject2.JSONObject.AddPair('type',
            ConferenceData.ConferenceSolution.Key._type);
        end;
        if ConferenceData.ConferenceSolution._Name <> '' then
          oJSONObject.JSONObject.AddPair('name',
            ConferenceData.ConferenceSolution._Name);
        if ConferenceData.ConferenceSolution.IconURI <> '' then
          oJSONObject.JSONObject.AddPair('iconUri',
            ConferenceData.ConferenceSolution.IconURI);
      end;
      // ... other fields
      if ConferenceData.ConferenceId <> '' then
        oCreateRequest.JSONObject.AddPair('conferenceId',
          ConferenceData.ConferenceId);
      if ConferenceData.Signature <> '' then
        oCreateRequest.JSONObject.AddPair('signature',
          ConferenceData.Signature);
      if ConferenceData.Notes <> '' then
        oCreateRequest.JSONObject.AddPair('notes', ConferenceData.Notes);
    end;
  end;
  // ... gadget
  if Assigned(FGadget) then
  begin
    if Gadget._type <> '' then
      JSON.AddPair('type', Gadget._type);
    if Gadget.Title <> '' then
      JSON.AddPair('title', Gadget.Title);
    if Gadget.Link <> '' then
      JSON.AddPair('link', Gadget.Link);
    if Gadget.IconLink <> '' then
      JSON.AddPair('iconLink', Gadget.IconLink);
    if Gadget.Width > 0 then
      JSON.AddPair('width', Gadget.Width);
    if Gadget.Height > 0 then
      JSON.AddPair('height', Gadget.Height);
    if Gadget.Display <> '' then
      JSON.AddPair('display', Gadget.Display);
    if Gadget.Preferences.count > 0 then
    begin
      oJSONObject := JSON.AddObject('preferences');
      for i := 0 to Gadget.Preferences.count - 1 do
        oJSONObject.JSONObject.AddPair(Gadget.Preferences.Names[i],
          Gadget.Preferences.ValueFromIndex[i]);
    end;
  end;
  // ...
  if AnyoneCanAddSelf then
    JSON.AddPair('anyoneCanAddSelf', AnyoneCanAddSelf);
  if GuestsCanInviteOthers then
    JSON.AddPair('guestsCanInviteOthers', GuestsCanInviteOthers);
  if GuestsCanModify then
    JSON.AddPair('guestsCanModify', GuestsCanModify);
  if GuestsCanSeeOtherGuests then
    JSON.AddPair('guestsCanSeeOtherGuests', GuestsCanSeeOtherGuests);
  if PrivateCopy then
    JSON.AddPair('privateCopy', PrivateCopy);
  if Locked then
    JSON.AddPair('locked', Locked);
  // ... reminders
  if Assigned(FReminders) then
  begin
    oJSONObject := JSON.AddObject('reminders');
    oJSONObject.JSONObject.AddPair('useDefault', Reminders.UseDefault);
    if Reminders.Overrides.count > 0 then
    begin
      oJSONObject2 := oJSONObject.JSONObject.AddObject('overrides');
      oJSONObject2.JSONObject.IsArray := True;
      for i := 0 to Reminders.Overrides.count - 1 do
      begin
        oReminder := TsgcGoogleCalendarRemindersOverridesItem
          (Reminders.Overrides.Item[i]);
        if Assigned(oReminder) then
        begin
          oJSONObject3 := oJSONObject2.JSONObject.AddObject(IntToStr(i));
          oJSONObject3.JSONObject.AddPair('method',
            GetReminderMethodString(oReminder.Method));
          oJSONObject3.JSONObject.AddPair('minutes', oReminder.Minutes);
        end;
      end;
    end;
  end;
  // ... source
  if Assigned(FSource) then
  begin
    oJSONObject := JSON.AddObject('source');
    if Source.URL <> '' then
      oJSONObject.JSONObject.AddPair('url', Source.URL);
    if Source.Title <> '' then
      oJSONObject.JSONObject.AddPair('title', Source.Title);
  end;
  // ... attachments
  if Attachments.count > 0 then
  begin
    oJSONObject := JSON.AddObject('attachments');
    oJSONObject.JSONObject.IsArray := True;
    for i := 0 to Attachments.count - 1 do
    begin
      oAttachment := TsgcGoogleCalendarAttachmentItem(Attachments.Item[i]);
      if Assigned(oAttachment) then
      begin
        oJSONObject2 := oJSONObject.JSONObject.AddObject(IntToStr(i));
        if oAttachment.FileUrl <> '' then
          oJSONObject2.JSONObject.AddPair('fileUrl', oAttachment.FileUrl);
        if oAttachment.Title <> '' then
          oJSONObject2.JSONObject.AddPair('title', oAttachment.Title);
        if oAttachment.MimeType <> '' then
          oJSONObject2.JSONObject.AddPair('mimeType', oAttachment.MimeType);
        if oAttachment.IconLink <> '' then
          oJSONObject2.JSONObject.AddPair('iconLink', oAttachment.IconLink);
        if oAttachment.FileId <> '' then
          oJSONObject2.JSONObject.AddPair('fileId', oAttachment.FileId);
      end;
    end;
  end;
end;

procedure TsgcGoogleCalendarResource_Event.DoRead;
var
  i: Integer;
  oAttendee: TsgcGoogleCalendarAttendeeItem;
  oAttachment: TsgcGoogleCalendarAttachmentItem;
  oEntryPoint: TsgcGoogleCalendarConferenceEntryPointItem;
  oJSONObject, oJSONObject2: IsgcObjectJSON;
  oReminder: TsgcGoogleCalendarRemindersOverridesItem;
begin
  inherited;
  AnyoneCanAddSelf := DoReadJSONValue('AnyoneCanAddSelf');
  Attachments.Clear;
  if JSON.Node['attachments'] <> nil then
  begin
    for i := 0 to JSON.Node['attachments'].count - 1 do
    begin
      oAttachment := TsgcGoogleCalendarAttachmentItem.Create;
      oJSONObject := JSON.Node['attachments'].JSONObject.Item[i];
      oAttachment.FileUrl := DoReadJSONValue('fileUrl', oJSONObject);
      oAttachment.Title := DoReadJSONValue('title', oJSONObject);
      oAttachment.MimeType := DoReadJSONValue('mimeType', oJSONObject);
      oAttachment.IconLink := DoReadJSONValue('iconLink', oJSONObject);
      oAttachment.FileId := DoReadJSONValue('fileId', oJSONObject);
      oAttachment.id := oAttachment.FileUrl;

      Attachments.AddItem(oAttachment);
    end;
  end;
  Attendees.Clear;
  if JSON.Node['attendees'] <> nil then
  begin
    for i := 0 to JSON.Node['attendees'].count - 1 do
    begin
      oAttendee := TsgcGoogleCalendarAttendeeItem.Create;
      oJSONObject := JSON.Node['attendees'].JSONObject.Item[i];
      oAttendee.id := DoReadJSONValue('id', oJSONObject);
      oAttendee.Email := DoReadJSONValue('email', oJSONObject);
      oAttendee.DisplayName := DoReadJSONValue('displayName', oJSONObject);
      oAttendee.Organizer := DoReadJSONValue('organizer', oJSONObject);
      oAttendee._self := DoReadJSONValue('self', oJSONObject);
      oAttendee.Resource := DoReadJSONValue('resource', oJSONObject);
      oAttendee.Optional := DoReadJSONValue('optional', oJSONObject);
      oAttendee.ResponseStatus := DoReadJSONValue('responseStatus',
        oJSONObject);
      oAttendee.Comment := DoReadJSONValue('comment', oJSONObject);
      oAttendee.AdditionalGuests := DoReadJSONValue('additionalGuests',
        oJSONObject);

      Attendees.AddItem(oAttendee);
    end;
  end;
  AttendeesOmitted := DoReadJSONValue('attendeesOmitted');
  ColorId := DoReadJSONValue('colorId');
  if Assigned(JSON.Node['conferenceData']) then
  begin
    oJSONObject := JSON.Node['conferenceData'].Node['createRequest'];
    if Assigned(oJSONObject) then
    begin
      ConferenceData.CreateRequest.RequestId := DoReadJSONValue('requestId',
        oJSONObject);
      if Assigned(oJSONObject.Node['conferenceSolutionKey']) then
        ConferenceData.CreateRequest.ConferenceSolutionKey._type :=
          DoReadJSONValue('type', oJSONObject.Node['conferenceSolutionKey']);
      if Assigned(oJSONObject.Node['status']) then
        ConferenceData.CreateRequest.Status.StatusCode :=
          DoReadJSONValue('statusCode', oJSONObject.Node['status']);
    end;
    oJSONObject := JSON.Node['conferenceData'];
    ConferenceData.EntryPoints.Clear;
    if Assigned(oJSONObject.Node['entryPoints']) then
    begin
      for i := 0 to oJSONObject.Node['entryPoints'].count - 1 do
      begin
        oEntryPoint := TsgcGoogleCalendarConferenceEntryPointItem.Create;
        oJSONObject2 := oJSONObject.Node['entryPoints'].JSONObject.Item[i];
        oEntryPoint.EntryPointType := DoReadJSONValue('entryPointType',
          oJSONObject2);
        oEntryPoint.Uri := DoReadJSONValue('uri', oJSONObject2);
        oEntryPoint._Label := DoReadJSONValue('label', oJSONObject2);
        oEntryPoint.Pin := DoReadJSONValue('pin', oJSONObject2);
        oEntryPoint.AccessCode := DoReadJSONValue('accessCode', oJSONObject2);
        oEntryPoint.MeetingCode := DoReadJSONValue('meetingCode', oJSONObject2);
        oEntryPoint.Passcode := DoReadJSONValue('passcode', oJSONObject2);
        oEntryPoint.Password := DoReadJSONValue('password', oJSONObject2);
        oEntryPoint.id := IntToStr(i);

        ConferenceData.EntryPoints.AddItem(oEntryPoint);
      end;
    end;
    oJSONObject := JSON.Node['conferenceData'];
    if Assigned(JSON.Node['conferenceSolution']) then
    begin
      if Assigned(oJSONObject.Node['conferenceSolution'].Node['key']) then
        ConferenceData.ConferenceSolution.Key._type :=
          DoReadJSONValue('type', oJSONObject.Node['conferenceSolution']
          .Node['key']);
      ConferenceData.ConferenceSolution._Name :=
        DoReadJSONValue('name', oJSONObject.Node['conferenceSolution']);
      ConferenceData.ConferenceSolution.IconURI :=
        DoReadJSONValue('name', oJSONObject.Node['iconUri']);
    end;
    ConferenceData.ConferenceId := DoReadJSONValue('conferenceId', oJSONObject);
    ConferenceData.Signature := DoReadJSONValue('signature', oJSONObject);
    ConferenceData.Notes := DoReadJSONValue('notes', oJSONObject);
  end;
  Created := StrInternetToDateTime(DoReadJSONValue('created'));
  oJSONObject := JSON.Node['creator'];
  if Assigned(oJSONObject) then
  begin
    Creator.id := DoReadJSONValue('id', oJSONObject);
    Creator.Email := DoReadJSONValue('email', oJSONObject);
    Creator.DisplayName := DoReadJSONValue('displayName', oJSONObject);
    Creator._self := DoReadJSONValue('self', oJSONObject);
  end;
  Description := DoReadJSONValue('description');
  EndTimeUnspecified := DoReadJSONValue('endTimeUnspecified');
  if Assigned(JSON.Node['extendedProperties']) then
  begin
    ExtendedProperties._Private.Text := DoReadJSONValue('private',
      JSON.Node['extendedProperties']);
    ExtendedProperties._Shared.Text := DoReadJSONValue('shared',
      JSON.Node['extendedProperties']);
  end;
  oJSONObject := JSON.Node['gadget'];
  if Assigned(oJSONObject) then
  begin
    Gadget._type := DoReadJSONValue('type', oJSONObject);
    Gadget.Title := DoReadJSONValue('title', oJSONObject);
    Gadget.Link := DoReadJSONValue('link', oJSONObject);
    Gadget.IconLink := DoReadJSONValue('iconLink', oJSONObject);
    Gadget.Width := DoReadJSONValue('width', oJSONObject);
    Gadget.Height := DoReadJSONValue('height', oJSONObject);
    Gadget.Display := DoReadJSONValue('display', oJSONObject);
    Gadget.Preferences.Text := DoReadJSONValue('preferences', oJSONObject);
  end;
  GuestsCanInviteOthers := DoReadJSONValue('guestsCanInviteOthers');
  GuestsCanModify := DoReadJSONValue('guestsCanModify');
  GuestsCanSeeOtherGuests := DoReadJSONValue('guestsCanSeeOtherGuests');
  HangoutLink := DoReadJSONValue('hangoutLink');
  Htmllink := DoReadJSONValue('htmllink');
  iCalUID := DoReadJSONValue('iCalUID');
  Location := DoReadJSONValue('location');
  Locked := DoReadJSONValue('locked');
  oJSONObject := JSON.Node['organizer'];
  if Assigned(oJSONObject) then
  begin
    Organizer.id := DoReadJSONValue('id', oJSONObject);
    Organizer.Email := DoReadJSONValue('email', oJSONObject);
    Organizer.DisplayName := DoReadJSONValue('displayName', oJSONObject);
    Organizer._self := DoReadJSONValue('self', oJSONObject);
  end;
  OriginalStartTime.Date := StrInternetToDateTime
    (DoReadJSONValue('date', JSON.Node['originalStartTime']));
  OriginalStartTime.DateTime := StrInternetToDateTime
    (DoReadJSONValue('dateTime', JSON.Node['originalStartTime']));
  OriginalStartTime.TimeZone := DoReadJSONValue('timeZone',
    JSON.Node['originalStartTime']);
  PrivateCopy := DoReadJSONValue('privateCopy');
  Recurrence.Text := DoReadJSONValue('recurrence');
  RecurringEventId := DoReadJSONValue('recurringEventId');
  if Assigned(JSON.Node['reminders']) then
  begin
    Reminders.UseDefault := DoReadJSONValue('useDefault',
      JSON.Node['reminders']);
    Reminders.Overrides.Clear;
    if Assigned(JSON.Node['reminders'].Node['overrides']) then
    begin
      oJSONObject := JSON.Node['reminders'].Node['overrides'];
      for i := 0 to oJSONObject.count - 1 do
      begin
        oReminder := TsgcGoogleCalendarRemindersOverridesItem.Create;
        oReminder.Method := GetReminderMethod(DoReadJSONValue('method',
          oJSONObject.Item[i]));
        oReminder.Minutes := DoReadJSONValue('minutes', oJSONObject.Item[i]);
        oReminder.id := IntToStr(i);

        Reminders.Overrides.AddItem(oReminder);
      end;
    end;
  end;
  Sequence := DoReadJSONValue('sequence');
  if Assigned(JSON.Node['source']) then
  begin
    Source.URL := DoReadJSONValue('source', JSON.Node['source']);
    Source.Title := DoReadJSONValue('title', JSON.Node['source']);
  end;
  Start.Date := StrInternetToDateTime(DoReadJSONValue('date',
    JSON.Node['start']));
  Start.DateTime := StrInternetToDateTime(DoReadJSONValue('dateTime',
    JSON.Node['start']));
  Start.TimeZone := DoReadJSONValue('timeZone', JSON.Node['start']);
  Status := GetStatus(DoReadJSONValue('status'));
  Summary := DoReadJSONValue('summary');
  Transparency := DoReadJSONValue('transparency');
  Updated := StrInternetToDateTime(DoReadJSONValue('updated'));
  Visibility := GetVisibility(DoReadJSONValue('visibility'));
  _End.Date := StrInternetToDateTime(DoReadJSONValue('date', JSON.Node['end']));
  _End.DateTime := StrInternetToDateTime(DoReadJSONValue('dateTime',
    JSON.Node['end']));
  _End.TimeZone := DoReadJSONValue('timeZone', JSON.Node['end']);
end;

function TsgcGoogleCalendarResource_Event.GetAttachments
  : TagcGoogleCalendarAttachments;
begin
  if not Assigned(FAttachments) then
  begin
    FAttachments := TagcGoogleCalendarAttachments.Create;
    FAttachments.OwnObjects := True;
  end;
  result := FAttachments;
end;

function TsgcGoogleCalendarResource_Event.GetAttendees
  : TsgcGoogleCalendarAttendees;
begin
  if not Assigned(FAttendees) then
  begin
    FAttendees := TsgcGoogleCalendarAttendees.Create;
    FAttendees.OwnObjects := True;
  end;
  result := FAttendees;
end;

function TsgcGoogleCalendarResource_Event.GetConferenceData
  : TsgcGoogleCalendarConferenceData;
begin
  if not Assigned(FConferenceData) then
    FConferenceData := TsgcGoogleCalendarConferenceData.Create;
  result := FConferenceData;
end;

function TsgcGoogleCalendarResource_Event.GetCreator: TsgcGoogleCalendarCreator;
begin
  if not Assigned(FCreator) then
    FCreator := TsgcGoogleCalendarCreator.Create;
  result := FCreator;
end;

function TsgcGoogleCalendarResource_Event.GetExtendedProperties
  : TsgcGoogleCalendarExtendedProperties;
begin
  if not Assigned(FExtendedProperties) then
    FExtendedProperties := TsgcGoogleCalendarExtendedProperties.Create;
  result := FExtendedProperties;
end;

function TsgcGoogleCalendarResource_Event.GetGadget: TsgcGoogleCalendarGadget;
begin
  if not Assigned(FGadget) then
    FGadget := TsgcGoogleCalendarGadget.Create;
  result := FGadget;
end;

function TsgcGoogleCalendarResource_Event.GetOrganizer
  : TsgcGoogleCalendarOrganizer;
begin
  if not Assigned(FOrganizer) then
    FOrganizer := TsgcGoogleCalendarOrganizer.Create;
  result := FOrganizer;
end;

function TsgcGoogleCalendarResource_Event.GetOriginalStartTime
  : TsgcGoogleCalendarOriginalStart;
begin
  if not Assigned(FOriginalStartTime) then
    FOriginalStartTime := TsgcGoogleCalendarOriginalStart.Create;
  result := FOriginalStartTime;
end;

function TsgcGoogleCalendarResource_Event.GetRecurrence: TStringList;
begin
  if not Assigned(FRecurrence) then
    FRecurrence := TStringList.Create;
  result := FRecurrence;
end;

function TsgcGoogleCalendarResource_Event.GetReminders
  : TsgcGoogleCalendarReminders;
begin
  if not Assigned(FReminders) then
    FReminders := TsgcGoogleCalendarReminders.Create;
  result := FReminders;
end;

function TsgcGoogleCalendarResource_Event.GetSource: TsgcGoogleCalendarSource;
begin
  if not Assigned(FSource) then
    FSource := TsgcGoogleCalendarSource.Create;
  result := FSource;
end;

function TsgcGoogleCalendarResource_Event.GetStart: TsgcGoogleCalendarStart;
begin
  if not Assigned(FStart) then
    FStart := TsgcGoogleCalendarStart.Create;
  result := FStart;
end;

function TsgcGoogleCalendarResource_Event.Get_End: TsgcGoogleCalendarEnd;
begin
  if not Assigned(F_End) then
    F_End := TsgcGoogleCalendarEnd.Create;
  result := F_End;
end;

constructor TsgcGoogleCalendarDateTime.Create;
begin
  inherited;
  DateTime := 0;
end;

destructor TsgcGoogleCalendarExtendedProperties.Destroy;
begin
  sgcFree(F_Private);
  sgcFree(F_Shared);
  inherited;
end;

function TsgcGoogleCalendarExtendedProperties.Get_Private: TStringList;
begin
  if not Assigned(F_Private) then
    F_Private := TStringList.Create;
  result := F_Private;
end;

function TsgcGoogleCalendarExtendedProperties.Get_Shared: TStringList;
begin
  if not Assigned(F_Shared) then
    F_Shared := TStringList.Create;
  result := F_Shared;
end;

destructor TsgcGoogleCalendarCreateRequest.Destroy;
begin
  sgcFree(FStatus);
  sgcFree(FConferenceSolutionKey);
  inherited;
end;

function TsgcGoogleCalendarCreateRequest.GetConferenceSolutionKey
  : TsgcGoogleCalendarConferenceSolutionKey;
begin
  if not Assigned(FConferenceSolutionKey) then
    FConferenceSolutionKey := TsgcGoogleCalendarConferenceSolutionKey.Create;
  result := FConferenceSolutionKey;
end;

function TsgcGoogleCalendarCreateRequest.GetStatus
  : TsgcGoogleCalendarConferenceStatus;
begin
  if not Assigned(FStatus) then
    FStatus := TsgcGoogleCalendarConferenceStatus.Create;
  result := FStatus;
end;

destructor TsgcGoogleCalendarConferenceData.Destroy;
begin
  sgcFree(FEntryPoints);
  sgcFree(FConferenceSolution);
  sgcFree(FCreateRequest);
  inherited;
end;

function TsgcGoogleCalendarConferenceData.GetConferenceSolution
  : TsgcGoogleCalendarConferenceSolution;
begin
  if not Assigned(FConferenceSolution) then
    FConferenceSolution := TsgcGoogleCalendarConferenceSolution.Create;
  result := FConferenceSolution;
end;

function TsgcGoogleCalendarConferenceData.GetCreateRequest
  : TsgcGoogleCalendarCreateRequest;
begin
  if not Assigned(FCreateRequest) then
    FCreateRequest := TsgcGoogleCalendarCreateRequest.Create;
  result := FCreateRequest;
end;

function TsgcGoogleCalendarConferenceData.GetEntryPoints
  : TsgcGoogleCalendarConferenceEntryPoints;
begin
  if not Assigned(FEntryPoints) then
  begin
    FEntryPoints := TsgcGoogleCalendarConferenceEntryPoints.Create;
    FEntryPoints.OwnObjects := True;
  end;
  result := FEntryPoints;
end;

destructor TsgcGoogleCalendarConferenceSolution.Destroy;
begin
  sgcFree(FKey);
  inherited;
end;

function TsgcGoogleCalendarConferenceSolution.GetKey
  : TsgcGoogleCalendarConferenceSolutionKey;
begin
  if not Assigned(FKey) then
    FKey := TsgcGoogleCalendarConferenceSolutionKey.Create;
  result := FKey;
end;

destructor TsgcGoogleCalendarGadget.Destroy;
begin
  sgcFree(FPreferences);
  inherited;
end;

function TsgcGoogleCalendarGadget.GetPreferences: TStringList;
begin
  if not Assigned(FPreferences) then
    FPreferences := TStringList.Create;
  result := FPreferences;
end;

destructor TsgcGoogleCalendarReminders.Destroy;
begin
  sgcFree(FOverrides);
  inherited;
end;

function TsgcGoogleCalendarReminders.GetOverrides
  : TsgcGoogleCalendarRemindersOverrides;
begin
  if not Assigned(FOverrides) then
  begin
    FOverrides := TsgcGoogleCalendarRemindersOverrides.Create;
    FOverrides.OwnObjects := True;
  end;
  result := FOverrides;
end;

destructor TsgcHTTP_Google_Cloud_Calendar_Client.Destroy;
begin
  sgcFree(FCalendars);
  inherited;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DeleteCalendar
  (const aId: string): Boolean;
var
  vResponse: string;
begin
  vResponse := Calendar_Delete(aId);
  result := vResponse = '';
  if not result then
    raise Exception.Create(vResponse);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DeleteEvent(const aCalendarId
  : string; const aEventId: string): Boolean;
begin
  result := DoDeleteEvent(Event_Delete(aCalendarId, aEventId));
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoCalendarEvent(const aCalendar:
    TsgcGoogleCalendarItem): Boolean;
begin
  Result := True;
  if Assigned(FOnGetCalendar) then
    FOnGetCalendar(self, aCalendar, result);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoCalendarEventEvent(const
    aCalendar: TsgcGoogleCalendarItem; const aEvent:
    TsgcGoogleCalendarEventItem): Boolean;
begin
  Result := True;
  if Assigned(FOnGetCalendarEvent) then
    FOnGetCalendarEvent(self, aCalendar, aEvent, result);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoCheckResponse
  (const aValue: string): Boolean;
var
  i: Integer;
  oJSON: TsgcJSON;
  vCode: Integer;
  vMessage: string;
  oError: TsgcGoogleCalendarError;
  oErrorItem: TsgcGoogleCalendarErrorItem;
  oJSONObject: IsgcObjectJSON;
begin
  result := True;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aValue);
    if Assigned(oJSON.Node['error']) then
    begin
      result := False;
      vCode := 0;
      if Assigned(oJSON.Node['error'].Node['code']) then
        vCode := oJSON.Node['error'].Node['code'].Value;
      if Assigned(oJSON.Node['error'].Node['message']) then
        vMessage := oJSON.Node['error'].Node['message'].Value;
      if Assigned(FOnError) then
      begin
        oError := TsgcGoogleCalendarError.Create;
        Try
          oError.Code := vCode;
          oError.Description := vMessage;
          if Assigned(oJSON.Node['error'].Node['errors']) then
          begin
            SetLength(oError.FErrors, oJSON.Node['error'].Node['errors'].count);
            for i := 0 to oJSON.Node['error'].Node['errors'].count - 1 do
            begin
              oJSONObject := oJSON.Node['error'].Node['errors'].Item[i];
              oErrorItem := TsgcGoogleCalendarErrorItem.Create;
              if Assigned(oJSONObject.Node['domain']) then
                oErrorItem.Domain := oJSONObject.Node['domain'].Value;
              if Assigned(oJSONObject.Node['reason']) then
                oErrorItem.Reason := oJSONObject.Node['reason'].Value;
              if Assigned(oJSONObject.Node['message']) then
                oErrorItem._Message := oJSONObject.Node['message'].Value;
              if Assigned(oJSONObject.Node['locationType']) then
                oErrorItem.LocationType := oJSONObject.Node
                  ['locationType'].Value;
              if Assigned(oJSONObject.Node['location']) then
                oErrorItem.Location := oJSONObject.Node['location'].Value;
              oError.Errors[i] := oErrorItem;
            end;
          end;
          FOnError(self, oError);
        Finally
          sgcFree(oError);
        End;
      end
      else
        raise Exception.CreateFmt('Error: %s [%d]', [vMessage, vCode]);
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoDeleteEvent
  (const aValue: string): Boolean;
begin
  result := False;
  if DoCheckResponse(aValue) then
    result := aValue = '';
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoLoadCalendars
  (const aValue: string; aClear: Boolean = True): Boolean;
var
  i: Integer;
  oJSON: TsgcJSON;
  oCalendar: TsgcGoogleCalendarItem;
begin
  result := DoCheckResponse(aValue);
  if not result then
    exit;

  if aClear then
    Calendars.Clear;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aValue);
    Calendars.NextSyncToken := '';
    if Assigned(oJSON.Node['nextSyncToken']) then
      Calendars.NextSyncToken := oJSON.Node['nextSyncToken'].Value;
    if oJSON.Node['items'] <> nil then
    begin
      result := True;
      for i := 0 to oJSON.Node['items'].count - 1 do
      begin
        oCalendar := TsgcGoogleCalendarItem.Create;
        oCalendar.Read(oJSON.Node['items'].Item[i].Value);
        if DoCalendarEvent(oCalendar) then
          Calendars.AddCalendar(oCalendar);
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoLoadEvents(const aCalendarId,
  aValue: string; aClear: Boolean = True): Boolean;
var
  i: Integer;
  oJSON: TsgcJSON;
  oCalendar: TsgcGoogleCalendarItem;
  oEvent: TsgcGoogleCalendarEventItem;
begin
  result := DoCheckResponse(aValue);

  if not result then
    exit;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aValue);

    oCalendar := Calendars.GetCalendar(aCalendarId);
    if Assigned(oCalendar) then
    begin
      if aClear then
        oCalendar.Events.Clear;
      oCalendar.Events.NextSyncToken := '';
      if Assigned(oJSON.Node['nextSyncToken']) then
        oCalendar.Events.NextSyncToken := oJSON.Node['nextSyncToken'].Value;
      if oJSON.Node['items'] <> nil then
      begin
        for i := 0 to oJSON.Node['items'].count - 1 do
        begin
          oEvent := TsgcGoogleCalendarEventItem.Create;
          oEvent.Read(oJSON.Node['items'].Item[i].Value);
          if DoCalendarEventEvent(oCalendar, oEvent) then
            oCalendar.Events.AddEvent(oEvent);
        end;
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoNewCalendar
  (const aValue: string): string;
var
  oJSON: TsgcJSON;
begin
  result := '';

  if not DoCheckResponse(aValue) then
    exit;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aValue);
    if oJSON.Node['id'] <> nil then
      result := oJSON.Node['id'].Value;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoNewEvent
  (const aValue: string): string;
var
  oJSON: TsgcJSON;
begin
  result := '';

  if not DoCheckResponse(aValue) then
    exit;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aValue);
    if oJSON.Node['id'] <> nil then
      result := oJSON.Node['id'].Value;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoUpdateCalendar
  (const aValue: string): Boolean;
begin
  result := DoCheckResponse(aValue);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.DoUpdateEvent
  (const aValue: string): Boolean;
begin
  result := DoCheckResponse(aValue);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.GetCalendars
  : TsgcGoogleCalendarList;
begin
  if not Assigned(FCalendars) then
    FCalendars := TsgcGoogleCalendarList.Create;
  result := FCalendars;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.LoadCalendars(aPageToken: string
    = ''; aSyncToken: string = ''; aClear: Boolean = True): Boolean;
var
  oJSON: TsgcJSON;
  vNextPageToken: string;
  vResponse: String;
begin
  vResponse := CalendarList_List(250, '', aPageToken, False, False, aSyncToken);

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vResponse);
    vNextPageToken := '';
    if Assigned(oJSON.Node['nextPageToken']) then
      vNextPageToken := oJSON.Node['nextPageToken'].Value;
  Finally
    sgcFree(oJSON);
  End;

  result := DoLoadCalendars(vResponse, aClear);

  if result and (vNextPageToken <> '') then
    LoadCalendars(vNextPageToken, aSyncToken, False);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.LoadCalendarsChanged(aSyncToken:
    string): Boolean;
begin
  Result := LoadCalendars('', aSyncToken);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.LoadEvents(const aCalendarId
  : string; aOrderBy: TsgcGoogleCalendarEventListOrdered =
  gceoStartTime): Boolean;
begin
  result := LoadEvents(aCalendarId, 0, 0, aOrderBy);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.LoadEvents(const aCalendarId
  : string; aDateFrom, aDateTo: TDateTime;
  aOrderBy: TsgcGoogleCalendarEventListOrdered = gceoStartTime;
  aPageToken: String = ''; aSyncToken: string = '';
  aClear: Boolean = True): Boolean;
var
  vResponse: String;
  oJSON: TsgcJSON;
  vNextPageToken: string;
begin
  case aOrderBy of
    gceoNone:
      vResponse := Event_List(aCalendarId, '', 0, 2500, '', aPageToken, '', '',
        '', False, False, False, aSyncToken, aDateTo, aDateFrom);
    gceoStartTime:
      vResponse := Event_List(aCalendarId, '', 0, 2500, 'startTime', aPageToken,
        '', '', '', False, False, True, aSyncToken, aDateTo, aDateFrom);
    gceoUpdated:
      vResponse := Event_List(aCalendarId, '', 0, 2500, 'updated', aPageToken,
        '', '', '', False, False, True, aSyncToken, aDateTo, aDateFrom);
  end;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vResponse);
    vNextPageToken := '';
    if Assigned(oJSON.Node['nextPageToken']) then
      vNextPageToken := oJSON.Node['nextPageToken'].Value;
    result := DoLoadEvents(aCalendarId, vResponse, aClear);
  Finally
    sgcFree(oJSON);
  End;

  if result and (vNextPageToken <> '') then
    LoadEvents(aCalendarId, aDateFrom, aDateTo, aOrderBy, vNextPageToken,
      aSyncToken, False);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.LoadEventsChanged
  (const aCalendarId: string; aSyncToken: string): Boolean;
begin
  result := LoadEvents(aCalendarId, 0, 0, gceoNone, '', aSyncToken);
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.NewCalendar(const aSummary
  : String): string;
var
  oResource: TsgcGoogleCalendarResource_Calendar;
begin
  oResource := TsgcGoogleCalendarResource_Calendar.Create;
  Try
    oResource.Summary := aSummary;
    result := DoNewCalendar(Calendar_Insert(oResource));
  Finally
    sgcFree(oResource);
  End;
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.NewEvent(const aCalendarId
  : string; const aResource: TsgcGoogleCalendarEventItem): string;
begin
  result := DoNewEvent(Event_Insert(aCalendarId, aResource));
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.UpdateCalendar(const aResource
  : TsgcGoogleCalendarItem): Boolean;
begin
  result := DoUpdateCalendar(CalendarList_Update(aResource.id,
    aResource, True));
end;

function TsgcHTTP_Google_Cloud_Calendar_Client.UpdateEvent(const aCalendarId
  : string; const aResource: TsgcGoogleCalendarEventItem): Boolean;
begin
  result := DoUpdateEvent(Event_Update(aCalendarId, aResource.id, aResource));
end;

constructor TsgcGoogleCalendarList.Create;
begin
  inherited;
  OwnObjects := True;
end;

procedure TsgcGoogleCalendarList.AddCalendar(const aCalendar
  : TsgcGoogleCalendarItem);
begin
  if GetItem(aCalendar.id) = nil then
    AddItem(aCalendar);
end;

function TsgcGoogleCalendarList.GetCalendar(const aId: string)
  : TsgcGoogleCalendarItem;
begin
  result := TsgcGoogleCalendarItem(GetItem(aId));
end;

function TsgcGoogleCalendarList.GetCalendarItem(Index: Integer)
  : TsgcGoogleCalendarItem;
begin
  result := nil;
  if Index < count then
    result := TsgcGoogleCalendarItem(Item[Index]);
end;

constructor TsgcGoogleCalendarEventList.Create;
begin
  inherited;
  OwnObjects := True;
end;

procedure TsgcGoogleCalendarEventList.AddEvent(const aEvent
  : TsgcGoogleCalendarEventItem);
begin
  if GetItem(aEvent.id) = nil then
    AddItem(aEvent)
  else
  begin
    DeleteItem(aEvent.id);
    AddItem(aEvent);
  end;
end;

function TsgcGoogleCalendarEventList.GetEvent(const aId: string)
  : TsgcGoogleCalendarEventItem;
begin
  result := TsgcGoogleCalendarEventItem(GetItem(aId));
end;

function TsgcGoogleCalendarEventList.GetEventItem(Index: Integer)
  : TsgcGoogleCalendarEventItem;
begin
  result := nil;
  if Index < count then
    result := TsgcGoogleCalendarEventItem(Item[Index]);
end;

destructor TsgcGoogleCalendarItem.Destroy;
begin
  sgcFree(FEvents);
  inherited;
end;

function TsgcGoogleCalendarItem.GetEvents: TsgcGoogleCalendarEventList;
begin
  if not Assigned(FEvents) then
    FEvents := TsgcGoogleCalendarEventList.Create;
  result := FEvents;
end;

{$ENDIF}

end.
