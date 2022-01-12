unit UIconUtils;
// http://stackoverflow.com/questions/1703186/can-48x48-or-64x64-icons-be-obtained-from-the-vista-shell


interface

uses Graphics, ShellApi, Commctrl, LCLType;


const
  SHIL_LARGE     = $00;  //The image size is normally 32x32 pixels. However, if the Use large icons option is selected from the Effects section of the Appearance tab in Display Properties, the image is 48x48 pixels.
  SHIL_SMALL     = $01;  //These images are the Shell standard small icon size of 16x16, but the size can be customized by the user.
  SHIL_EXTRALARGE= $02;  //These images are the Shell standard extra-large icon size. This is typically 48x48, but the size can be customized by the user.
  SHIL_SYSSMALL  = $03;  //These images are the size specified by GetSystemMetrics called with SM_CXSMICON and GetSystemMetrics called with SM_CYSMICON.
  SHIL_JUMBO     = $04;  //Windows Vista and later. The image is normally 256x256 pixels.
  IID_IImageList: TGUID= '{46EB5926-582E-4017-9FDF-E8998DAA0950}';

function GetImageListSH(SHIL_FLAG:Cardinal): HIMAGELIST;
procedure GetIconFromFile( aFile: string; var aIcon: TIcon;SHIL_FLAG: Cardinal );



implementation
uses Windows,sysutils;



function GetImageListSH(SHIL_FLAG:Cardinal): HIMAGELIST;
type
  _SHGetImageList = function (iImageList: integer; const riid: TGUID; var ppv: Pointer): hResult; stdcall;
var
  Handle        : THandle;
  SHGetImageList: _SHGetImageList;
begin
  Result:= 0;
  Handle:= LoadLibrary('Shell32.dll');
  if Handle<> S_OK then
  try
    SHGetImageList:= GetProcAddress(Handle, PChar(727));
    if Assigned(SHGetImageList) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
      SHGetImageList(SHIL_FLAG, IID_IImageList, Pointer(Result));
  finally
    FreeLibrary(Handle);
  end;
end;


procedure GetIconFromFile( aFile: string; var aIcon: TIcon;SHIL_FLAG: Cardinal );
var
  aImgList: HIMAGELIST;
  SFI: TSHFileInfo;
  aIndex: integer;
begin // Get the index of the imagelist
  SHGetFileInfo( PChar( aFile ), FILE_ATTRIBUTE_NORMAL, SFI, SizeOf( TSHFileInfo ),
    SHGFI_ICON or SHGFI_LARGEICON or SHGFI_SHELLICONSIZE or SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_DISPLAYNAME );
  if not Assigned( aIcon ) then
    aIcon := TIcon.Create;
  // get the imagelist
  aImgList := GetImageListSH( SHIL_FLAG );
  // get index
  //aIndex := Pred( ImageList_GetImageCount( aImgList ) );
  aIndex := SFI.iIcon;
  // extract the icon handle
  aIcon.Handle := ImageList_GetIcon( aImgList, aIndex, ILD_NORMAL );
end;
end.
 
