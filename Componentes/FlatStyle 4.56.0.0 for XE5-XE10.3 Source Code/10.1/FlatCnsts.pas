unit FlatCnsts;
//download by http://www.codefans.net
interface

resourcestring
  myFirstHint   = '�ƶ�����ǰһ����¼';
  myPriorHint   = '��ǰ�ƶ�һ����¼';
  myNextHint    = '����ƶ�һ����¼';
  myLastHint    = '�ƶ������һ����¼';
  myNewHint     = '�½�һ����¼';
  myDeleteHint  = 'ɾ��һ����¼';
  myEditHint    = '�༭��ǰ��¼';
  myPostHint    = '���浱ǰ��¼';
  myCancelHint  = 'ȡ���༭����';
  myRefreshHint = '�������ݼ�¼';
  myFirstCap    = '��ǰ';
  myPriorCap    = '��ǰ';
  myNextCap     = '���';
  myLastCap     = '���';
  myNewCap      = '�½�';
  myDeleteCap   = 'ɾ��';
  myEditCap     = '�༭';
  myPostCap     = '����';
  myCancelCap   = 'ȡ��';
  myRefreshCap  = '����';
  myDeleteRecordQuestion          = '����ɾ����¼(Y/N)?';
  myNewRecordQuestion             = '�����½���¼(Y/N)?';
  mySaveRecordQuestion            = '���뱣���¼(Y/N)?';
  myDeleteMultipleRecordsQuestion = '����ɾ��ѡ�������м�¼?';
  SListIndexError      = '�б����������߽�ֵ(%d)';
  SDuplicateString     = '�ִ��б�������!';
  SSortedListError     = '�������б��в��������!';
  sInsertError         = '���ܲ���һ����Ŀ!';
  sInvalidIndex        = '��Ч����!';
  sInvalidLevel        = '��Ч��Ŀ��׼����!';
  sInvalidLevelEx      = '��Ч��׼(%d)Ϊ��Ŀ"%s"!';
  SMultiSelectRequired = 'Ϊ�򿪱����ѡ��ʽ!';
  SOpenFileTitle = '��';
  SAssignError = '���ܷ���һ�� %s ��һ�� %s';
  SFCreateError = '���ܽ����ļ� %s';
  SFOpenError = '���ܴ��ļ� %s';
  SReadError = '����������';
  SWriteError = '����д����';
  SMemoryStreamError = 'չ��������ʱ,�ڴ����';
  SCantWriteResourceStreamError = '����д��ֻ����Դ��';
  SDuplicateReference = 'WriteObject called twice for the same instance';
  SClassNotFound = '�Ҳ����� %s ';
  SInvalidImage = '��Ч����ʽ';
  SResNotFound = '�Ҳ�����Դ %s ';
  SClassMismatch = 'Resource %s is of incorrect class';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SInvalidTabIndex = 'Tab index out of bounds';
  SInvalidTabPosition = 'Tab position incompatible with current tab style';
  SInvalidTabStyle = 'Tab style incompatible with current tab position';
  SDuplicateName = 'A component named %s already exists';
  SInvalidName = '''''%s'''' is not a valid component name';
  SDuplicateClass = 'A class named %s already exists';
  SNoComSupport = '%s has not been registered as a COM class';
  SInvalidInteger = '''''%s'''' is not a valid integer value';
  SLineTooLong = 'Line too long';
  SInvalidPropertyValue = '��Ч����ֵ';
  SInvalidPropertyPath = '��Ч����·��';
  SInvalidPropertyType = '��Ч��������: %s';
  SInvalidPropertyElement = '��Ч����Ԫ��: %s';
  SUnknownProperty = '���Բ�����!';
  SReadOnlyProperty = '������ֻ����!';
  SPropertyException = '��ȡ����: %s%s%s: %s';
  SAncestorNotFound = 'Ancestor for ''%s'' not found';
  SInvalidBitmap = 'Bitmap image is not valid';
  SInvalidIcon = 'Icon image is not valid';
  SInvalidMetafile = 'Metafile is not valid';
  SInvalidPixelFormat = 'Invalid pixel format';
  SBitmapEmpty = 'Bitmap is empty';
  SScanLine = 'Scan line index out of range';
  SChangeIconSize = 'Cannot change the size of an icon';
  SOleGraphic = 'Invalid operation on TOleGraphic';
  SUnknownExtension = 'Unknown picture file extension (.%s)';
  SUnknownClipboardFormat = 'Unsupported clipboard format';
  SOutOfResources = 'Out of system resources';
  SNoCanvasHandle = 'Canvas does not allow drawing';
  SInvalidImageSize = 'Invalid image size';
  STooManyImages = 'Too many images';
  SDimsDoNotMatch = 'Image dimensions do not match image list dimensions';
  SInvalidImageList = 'Invalid ImageList';
  SReplaceImage = 'Unable to Replace Image';
  SImageIndexError = 'Invalid ImageList Index';
  SImageReadFail = 'Failed to read ImageList data from stream';
  SImageWriteFail = 'Failed to write ImageList data to stream';
  SWindowDCError = 'Error creating window device context';
  SClientNotSet = 'Client of TDrag not initialized';
  SWindowClass = 'Error creating window class';
  SWindowCreate = 'Error creating window';
  SCannotFocus = 'Cannot focus a disabled or invisible window';
  SParentRequired = 'Control ''%s'' has no parent window';
  SMDIChildNotVisible = 'Cannot hide an MDI Child Form';
  SVisibleChanged = 'Cannot change Visible in OnShow or OnHide';
  SCannotShowModal = 'Cannot make a visible window modal';
  SScrollBarRange = 'Scrollbar property out of range';
  SPropertyOutOfRange = '%s property out of range';
  SMenuIndexError = 'Menu index out of range';
  SMenuReinserted = 'Menu inserted twice';
  SMenuNotFound = 'Sub-menu is not in menu';
  SNoTimers = 'Not enough timers available';
  SNotPrinting = 'Printer is not currently printing';
  SPrinting = 'Printing in progress';
  SPrinterIndexError = 'Printer index out of range';
  SInvalidPrinter = 'Printer selected is not valid';
  SDeviceOnPort = '%s on %s';
  SGroupIndexTooLow = 'GroupIndex cannot be less than a previous menu item''s GroupIndex';
  STwoMDIForms = 'Cannot have more than one MDI form per application';
  SNoMDIForm = 'Cannot create form. No MDI forms are currently active';
  SRegisterError = 'Invalid component registration';
  SImageCanvasNeedsBitmap = 'Can only modify an image if it contains a bitmap';
  SControlParentSetToSelf = 'A control cannot have itself as its parent';
  SOKButton     = 'ȷ��';
  SCancelButton = 'ȡ��';
  SYesButton    = '��(&Y)';
  SNoButton     = '��(&N)';
  SHelpButton   = '����(&H)';
  SCloseButton  = '�ر�(&C)';
  SIgnoreButton = '����(&I)';
  SRetryButton  = '����(&R)';
  SAbortButton  = '��ֹ(&B)';
  SAllButton    = '����(&A)';

  SCannotDragForm = 'Cannot drag a form';
  SPutObjectError = 'PutObject to undefined item';
  SCardDLLNotLoaded = 'Could not load CARDS.DLL';
  SDuplicateCardId = 'Duplicate CardId found';

  SDdeErr = 'An error returned from DDE  ($0%x)';
  SDdeConvErr = 'DDE Error - conversation not established ($0%x)';
  SDdeMemErr = 'Error occurred when DDE ran out of memory ($0%x)';
  SDdeNoConnect = 'Unable to connect DDE conversation';

  SFB = 'FB';
  SFG = 'FG';
  SBG = 'BG';
  SOldTShape = 'Cannot load older version of TShape';
  SVMetafiles = 'λͼ�ļ�';
  SVEnhMetafiles = '��ǿλͼ�ļ�';
  SVIcons = 'ͼ��';
  SVBitmaps = 'λͼ';
  SGridTooLarge = 'Grid too large for operation';
  STooManyDeleted = 'Too many rows or columns deleted';
  SIndexOutOfRange = 'Grid index out of range';
  SFixedColTooBig = 'Fixed column count must be less than column count';
  SFixedRowTooBig = 'Fixed row count must be less than row count';
  SInvalidStringGridOp = 'Cannot insert or delete rows from grid';
  SParseError = '%s on line %d';
  SIdentifierExpected = 'Identifier expected';
  SStringExpected = 'String expected';
  SNumberExpected = 'Number expected';
  SCharExpected = '''''%s'''' expected';
  SSymbolExpected = '%s expected';
  SInvalidNumber = 'Invalid numeric value';
  SInvalidString = 'Invalid string constant';
  SInvalidProperty = 'Invalid property value';
  SInvalidBinary = 'Invalid binary value';
  SOutlineIndexError = 'Outline index not found';
  SOutlineExpandError = 'Parent must be expanded';
  SInvalidCurrentItem = 'Invalid value for current item';
  SMaskErr = 'Invalid input value';
  SMaskEditErr = 'Invalid input value.  Use escape key to abandon changes';
  SOutlineError = 'Invalid outline index';
  SOutlineBadLevel = 'Incorrect level assignment';
  SOutlineSelection = 'Invalid selection';
  SOutlineFileLoad = 'File load error';
  SOutlineLongLine = 'Line too long';
  SOutlineMaxLevels = 'Maximum outline depth exceeded';

  SMsgDlgWarning = '����';
  SMsgDlgError = '����';
  SMsgDlgInformation = '��Ϣ';
  SMsgDlgConfirm = 'ȷ��';
  SMsgDlgYes = '��(&Y)';
  SMsgDlgNo = '��(&N)';
  SMsgDlgOK = 'ȷ��';
  SMsgDlgCancel = 'ȡ��';
  SMsgDlgHelp = '����(&H)';
  SMsgDlgHelpNone = 'No help available';
  SMsgDlgHelpHelp = '����';
  SMsgDlgAbort = '��ֹ(&A)';
  SMsgDlgRetry = '����(&R)';
  SMsgDlgIgnore = '����(&I)';
  SMsgDlgAll = '����(&A)';
  SMsgDlgNoToAll = 'ȡ������(&O)';
  SMsgDlgYesToAll = 'ȷ������(&A)';

  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  srUnknown = '(δ֪)';
  srNone = '(û��)';
  SOutOfRange = '��ֵ������ %d �� %d ֮��!';
  SCannotCreateName = 'Cannot create a default method name for an unnamed component';

  SDateEncodeError = 'Invalid argument to date encode';
  STimeEncodeError = 'Invalid argument to time encode';
  SInvalidDate = '''''%s'''' ��һ����Ч����!';
  SInvalidTime = '''''%s'''' ��һ����Чʱ��!';
  SInvalidDateTime = '''''%s'''' ��һ����Ч���ں�ʱ��!';
  SInvalidFileName = '��Ч�ļ��� - %s';
  SDefaultFilter = '�����ļ� (*.*)|*.*';
  sAllFilter = 'ȫ��';
  SNoVolumeLabel = ': [ - û�о�� - ]';
  SInsertLineError = '���ܲ���һ��!';

  SConfirmCreateDir = 'ָ��Ŀ¼������.������?';
  SSelectDirCap = 'ѡ��Ŀ¼';
  SCannotCreateDir = '���ܴ���Ŀ¼!';
  SDirNameCap = 'Ŀ¼����::';
  SDrivesCap = '������:';
  SDirsCap = 'Ŀ¼:';
  SFilesCap = '�ļ�: (*.*)';
  SNetworkCap = '����...';

  SColorPrefix = 'Color';               //!! obsolete - delete in 5.0
  SColorTags = 'ABCDEFGHIJKLMNOP';      //!! obsolete - delete in 5.0

  SInvalidClipFmt = 'Invalid clipboard format';
  SIconToClipboard = 'Clipboard does not support Icons';
  SCannotOpenClipboard = 'Cannot open clipboard';

  SDefault = 'Ĭ��';

  SInvalidMemoSize = 'Text exceeds memo capacity';
  SCustomColors = 'Custom Colors';
  SInvalidPrinterOp = 'Operation not supported on selected printer';
  SNoDefaultPrinter = 'There is no default printer currently selected';

  SIniFileWriteError = 'Unable to write to %s';

  SBitsIndexError = 'Bits index out of range';

  SUntitled = '(Untitled)';

  SInvalidRegType = 'Invalid data type for ''%s''';
  SRegCreateFailed = 'Failed to create key %s';
  SRegSetDataFailed = 'Failed to set data for ''%s''';
  SRegGetDataFailed = 'Failed to get data for ''%s''';

  SUnknownConversion = 'Unknown RichEdit conversion file extension (.%s)';
  SDuplicateMenus = 'Menu ''%s'' is already being used by another form';

  SPictureLabel = 'Picture:';
  SPictureDesc = ' (%dx%d)';
  SPreviewLabel = 'Preview';

  SCannotOpenAVI = 'Cannot open AVI';

  SNotOpenErr = 'No MCI device open';
  SMPOpenFilter = 'All files (*.*)|*.*|Wave files (*.wav)|*.wav|Midi files (*.mid)|*.mid|Video for Windows (*.avi)|*.avi';
  SMCINil = '';
  SMCIAVIVideo = 'AVIVideo';
  SMCICDAudio = 'CDAudio';
  SMCIDAT = 'DAT';
  SMCIDigitalVideo = 'DigitalVideo';
  SMCIMMMovie = 'MMMovie';
  SMCIOther = 'Other';
  SMCIOverlay = 'Overlay';
  SMCIScanner = 'Scanner';
  SMCISequencer = 'Sequencer';
  SMCIVCR = 'VCR';
  SMCIVideodisc = 'Videodisc';
  SMCIWaveAudio = 'WaveAudio';
  SMCIUnknownError = 'Unknown error code';

  SBoldItalicFont = 'Bold Italic';
  SBoldFont = 'Bold';
  SItalicFont = 'Italic';
  SRegularFont = 'Regular';

  SPropertiesVerb = 'Properties';

  sWindowsSocketError = 'Windows socket error: %s (%d), on API ''%s''';
  sAsyncSocketError = 'Asynchronous socket error %d';
  sNoAddress = 'No address specified';
  sCannotListenOnOpen = 'Can''t listen on an open socket';
  sCannotCreateSocket = 'Can''t create new socket';
  sSocketAlreadyOpen = 'Socket already open';
  sCantChangeWhileActive = 'Can''t change value while socket is active';
  sSocketMustBeBlocking = 'Socket must be in blocking mode';
  sSocketIOError = '%s error %d, %s';
  sSocketRead = 'Read';
  sSocketWrite = 'Write';

  SServiceFailed = 'Service failed on %s: %s';
  SExecute = 'execute';
  SStart = 'start';
  SStop = 'stop';
  SPause = 'pause';
  SContinue = 'continue';
  SInterrogate = 'interrogate';
  SShutdown = 'shutdown';
  SCustomError = 'Service failed in custom message(%d): %s';
  SServiceInstallOK = 'Service installed successfully';
  SServiceInstallFailed = 'Service "%s" failed to install with error: "%s"';
  SServiceUninstallOK = 'Service uninstalled successfully';
  SServiceUninstallFailed = 'Service "%s" failed to uninstall with error: "%s"';

  SInvalidActionRegistration = 'Invalid action registration';
  SInvalidActionUnregistration = 'Invalid action unregistration';
  SInvalidActionEnumeration = 'Invalid action enumeration';
  SInvalidActionCreation = 'Invalid action creation';
  
  SDockedCtlNeedsName = 'Docked control must have a name';
  SDockTreeRemoveError = 'Error removing control from dock tree';
  SDockZoneNotFound = ' - Dock zone not found';
  SDockZoneHasNoCtl = ' - Dock zone has no control';

  SAllCommands = 'All Commands';

  SDuplicateItem = 'List does not allow duplicates ($0%x)';

  SDuplicatePropertyCategory = 'A property category called %s already exists';
  SUnknownPropertyCategory = 'Property category does not exist (%s)';

  SActionCategoryName = 'Action';
  SActionCategoryDesc = 'Action properties and/or events';
  SDataCategoryName = 'Data';
  SDataCategoryDesc = 'Data properties and/or events';
  SDatabaseCategoryName = 'Database';
  SDatabaseCategoryDesc = 'Database and Data Aware properties and/or events';
  SDragNDropCategoryName = 'Drag, Drop and Docking';
  SDragNDropCategoryDesc = 'Drag, Drop and Docking properties and/or events';
  SHelpCategoryName = 'Help and Hints';
  SHelpCategoryDesc = 'Help and Hint properties and/or events';
  SLayoutCategoryName = 'Layout';
  SLayoutCategoryDesc = 'Layout properties and/or events';
  SLegacyCategoryName = 'Legacy';
  SLegacyCategoryDesc = 'Legacy properties and/or events';
  SLinkageCategoryName = 'Linkage';
  SLinkageCategoryDesc = 'Linkage properties and/or events';
  SLocaleCategoryName = 'Locale';
  SLocaleCategoryDesc = 'Locale properties and/or events';
  SLocalizableCategoryName = 'Localizable';
  SLocalizableCategoryDesc = 'Localizable properties and/or events';
  SMiscellaneousCategoryName = 'Miscellaneous';
  SMiscellaneousCategoryDesc = 'Miscellaneous properties and/or events';
  SVisualCategoryName = 'Visual';
  SVisualCategoryDesc = 'Visual properties and/or events';
  SInputCategoryName = 'Input';
  SInputCategoryDesc = 'Input properties and/or events';

  SInvalidMask = '''%s'' is an invalid mask at (%d)';
  SInvalidFilter = 'Property filters may only be name, class or type based (%d:%d)';
  SInvalidCategory = 'Categories must define their own name and description';

  SOperationNotAllowed = 'Operation not allowed while dispatching application events';
  SParentForm      = 'TFlatTitlebar can only be placed on a descndant of a TCustomForm';
  SNotReplicatable = 'Control cannot be used in a DBCtrlGrid';

  STextFalse = 'False';
  STextTrue = 'True';


implementation

end.
