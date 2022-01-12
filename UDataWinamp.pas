unit UDataWinamp;

{$MODE Delphi}

interface

const
  WinampKey = '$Winamp';
  WinampTitleKey = WinampKey + 'Title';
  WinampChannelsKey = WinampKey + 'Channels';
  WinampKBPSKey = WinampKey + 'KBPS';
  WinampFreqKey = WinampKey + 'Freq';
  WinampStatKey = WinampKey + 'Stat';
  WinampPositionKey = WinampKey + 'Position';
  WinampPoloKey = WinampKey + 'Polo';
  WinampPoshKey = WinampKey + 'Posh';
  WinampposKey = WinampKey + 'pos';
  WinampReloKey = WinampKey + 'Relo';
  WinampReshKey = WinampKey + 'Resh';
  WinampRemKey = WinampKey + 'Rem';
  WinampLengtlKey = WinampKey + 'Lengtl';
  WinampLengtsKey = WinampKey + 'Lengts';
  WinampLengthKey = WinampKey + 'Length';
  WinampTracknrKey = WinampKey + 'Tracknr';
  WinampTotalTracksKey = WinampKey + 'TotalTracks';

type
  TWinampStat = (wsWinampTitle,wsWinampChannels,wsWinampKBPS,wsWinampFreq,
    wsWinampStat,wsWinampPosition,wsWinampPolo,wsWinampPosh,wsWinamppos,
    wsWinampRelo,wsWinampResh,wsWinampRem,wsWinampLengtl,wsWinampLengts,
    wsWinampLength,wsWinampTracknr,wsWinampTotalTracks);

const
  FirstWinampStat = wsWinampTitle;
  LastWinampStat = wsWinampTotalTracks;

  WinampKeys : array[TWinAmpStat] of string =
   (WinampTitleKey,WinampChannelsKey,WinampKBPSKey,WinampFreqKey,
    WinampStatKey,WinampPositionKey,WinampPoloKey,WinampPoshKey,WinampposKey,
    WinampReloKey,WinampReshKey,WinampRemKey,WinampLengtlKey,WinampLengtsKey,
    WinampLengthKey,WinampTracknrKey,WinampTotalTracksKey);

  WinampHints : array[TWinAmpStat] of string = (
    'TrackTitle','Channels (stereo/mono)','kbps','KHz',
    'Current Status','Position(10) (bar)',
    'Time (hrs + min + sec) (long)',
    'Time (hrs + min + sec) (short)',
    'Time (seconds)',
    'Remaining (hrs+min+sec) (long)',
    'Remaining (hrs+min+sec) (short)',
    'Remaining Time (seconds)',
    'Total length (hrs + min + sec)(long)',
    'Total length (hrs + min + sec)(short)',
    'Total length (seconds)',
    'playlist number of current track','total tracks in playlist');

procedure ResolveWinampVariables(var Line: string);

implementation

uses
  SysUtils,UMain,UUtils;

procedure ResolveWinampVariables(var Line: string);
var
  tempstr: String;
  barLength: Cardinal;
  barPosition: Integer;
  trackLength, trackPosition, t: Integer;
  i: Integer;
  m, h, s: Integer;
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
begin
  if pos(WinampKey, line) = 0 then exit;

  trackLength := LCDSmartieDisplayForm.winampctrl1.TrackLength;
  trackPosition := LCDSmartieDisplayForm.winampctrl1.TrackPosition;
  if (trackLength < 0) then trackLength := 0;
  if (trackPosition < 0) then trackPosition := 0;

  if pos(WinampTitleKey, line) <> 0 then
  begin
    tempstr := LCDSmartieDisplayForm.winampctrl1.GetCurrSongTitle;
    i:=1;
    while (i<=length(tempstr)) and (tempstr[i]>='0')
      and (tempstr[i]<='9') do Inc(i);

    if (i<length(tempstr)) and (tempstr[i]='.') and (tempstr[i+1]=' ') then
      tempstr := copy(tempstr, i+2, length(tempstr));
    line := StringReplace(line, WinampTitleKey, Trim(tempstr), [rfReplaceAll]);
  end;
  if pos(WinampChannelsKey, line) <> 0 then
  begin
    if LCDSmartieDisplayForm.winampctrl1.GetSongInfo(2)>1 then tempstr := 'stereo'
    else tempstr := 'mono';
    line := StringReplace(line, WinampChannelsKey, tempstr, [rfReplaceAll]);
  end;
  if pos(WinampKBPSKey, line) <> 0 then
  begin
    line := StringReplace(line, WinampKBPSKey,
      IntToStr(LCDSmartieDisplayForm.winampctrl1.GetSongInfo(1)), [rfReplaceAll]);
  end;
  if pos(WinampFreqKey, line) <> 0 then
  begin
    line := StringReplace(line, WinampFreqKey,
      IntToStr(LCDSmartieDisplayForm.winampctrl1.GetSongInfo(0)), [rfReplaceAll]);
  end;
  if pos(WinampStatKey, line) <> 0 then
  begin
    case LCDSmartieDisplayForm.WinampCtrl1.GetState of
      0: line := StringReplace(line, WinampStatKey, 'stopped',
        [rfReplaceAll]);
      1: line := StringReplace(line, WinampStatKey, 'playing',
        [rfReplaceAll]);
      3: line := StringReplace(line, WinampStatKey, 'paused',
        [rfReplaceAll]);
      else line := StringReplace(line, WinampStatKey, '[unknown]',
          [rfReplaceAll]);
    end;
  end;

  while decodeArgs(line, WinampPositionKey, maxArgs, args, prefix, postfix,
    numargs) do
  begin
    try
      RequiredParameters(numargs, 1, 1);
      barlength := strtoint(args[1]);

      if (trackLength > 0) then barPosition := round(((trackPosition /
        1000)*barLength) /trackLength)
      else barPosition := 0;

      tempstr := '';

      for i := 1 to barPosition-1 do tempstr := tempstr +  '-';
      tempstr := tempstr +  '+';
      for i := barPosition + 1 to barlength do tempstr := tempstr +  '-';
      tempstr := copy(tempstr, 1, barlength);

      line := prefix + tempstr + postfix;
    except
      on E: Exception do line := prefix + '[WinampPosition: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  if pos(WinampPoloKey, line) <> 0 then
  begin
    t := trackPosition;
    if t / 1000 > trackLength then t := trackLength;
    h := t div ticksperhour;
    t := t - h * ticksperhour;
    m := t div ticksperminute;
    t := t - m * ticksperminute;
    s := t div ticksperseconde;
    tempstr := '';
    if h > 0 then
    begin
      tempstr := tempstr + IntToStr(h) +  'hrs ';
      tempstr := tempstr + formatfloat('00', m, localeFormat) +  'min ';
      tempstr := tempstr + formatfloat('00', s, localeFormat) +  'sec';
    end
    else
    begin
      if m > 0 then
      begin
        tempstr := tempstr + IntToStr(m) +  'min ';
        tempstr := tempstr + formatfloat('00', s, localeFormat) +  'sec';
      end
      else
      begin
        tempstr := tempstr + IntToStr(s) +  'sec';
      end;
    end;
    line := StringReplace(line, WinampPoloKey, tempstr, [rfReplaceAll]);
  end;

  if pos(WinampReloKey, line) <> 0 then
  begin
    t := trackLength*1000 - trackPosition;
    if t/1000> trackLength then t := trackLength;
    h := t div ticksperhour;
    t := t -h * ticksperhour;
    m := t div ticksperminute;
    t := t -m * ticksperminute;
    s := t div ticksperseconde;
    tempstr := '';
    if h > 0 then
    begin
      tempstr := tempstr + IntToStr(h) +  'hrs ';
      tempstr := tempstr + formatfloat('00', m, localeFormat) +  'min ';
      tempstr := tempstr + formatfloat('00', s, localeFormat) +  'sec';
    end
    else
    begin
      if m > 0 then
      begin
        tempstr := tempstr + IntToStr(m) +  'min ';
        tempstr := tempstr + formatfloat('00', s, localeFormat) +  'sec';
      end
      else
      begin
        tempstr := tempstr + IntToStr(s) +  'sec';
      end;
    end;
    line := StringReplace(line, WinampReloKey, tempstr, [rfReplaceAll]);
  end;

  if pos(WinampPoshKey, line) <> 0 then
  begin
    t := trackPosition;
    if t/1000> trackLength then t := trackLength;
    h := t div ticksperhour;
    t := t -h * ticksperhour;
    m := t div ticksperminute;
    t := t -m * ticksperminute;
    s := t div ticksperseconde;
    tempstr := '';
    if h > 0 then
    begin
      tempstr := tempstr + IntToStr(h) +  ':';
      tempstr := tempstr + formatfloat('00', m, localeFormat) +  ':';
      tempstr := tempstr + formatfloat('00', s, localeFormat);
    end
    else
    begin
      if m > 0 then
      begin
        tempstr := tempstr + IntToStr(m) +  ':';
        tempstr := tempstr + formatfloat('00', s, localeFormat);
      end
      else
      begin
        tempstr := tempstr + IntToStr(s);
      end;
    end;
    line := StringReplace(line, WinampPoshKey, tempstr, [rfReplaceAll]);
  end;

  if pos(WinampReshKey, line) <> 0 then
  begin
    t := trackLength * 1000 - trackPosition;
    if t / 1000 > trackLength then t := trackLength;
    h := t div ticksperhour;
    t := t - h * ticksperhour;
    m := t div ticksperminute;
    t := t - m * ticksperminute;
    s := t div ticksperseconde;
    tempstr := '';
    if h > 0 then
    begin
      tempstr := tempstr + IntToStr(h) +  ':';
      tempstr := tempstr + formatfloat('00', m, localeFormat) +  ':';
      tempstr := tempstr + formatfloat('00', s, localeFormat);
    end
    else
    begin
      if m > 0 then
      begin
        tempstr := tempstr + IntToStr(m) +  ':';
        tempstr := tempstr + formatfloat('00', s, localeFormat);
      end
      else
      begin
        tempstr := tempstr + IntToStr(s);
      end;
    end;
    line := StringReplace(line, WinampReshKey, tempstr, [rfReplaceAll]);
  end;

  if pos(WinampposKey, line) <> 0 then
  begin
    t := round((trackPosition / 1000));
    if t > trackLength then t := trackLength;
    line := StringReplace(line, WinampposKey, IntToStr(t), [rfReplaceAll]);
  end;
  if pos(WinampRemKey, line) <> 0 then
  begin
    t := round(tracklength-(trackPosition / 1000));
    if t > trackLength then t := trackLength;
    line := StringReplace(line, WinampRemKey, IntToStr(t), [rfReplaceAll]);
  end;

  if pos(WinampLengtlKey, line) <> 0 then
  begin
    t := trackLength * 1000;
    h := t div ticksperhour;
    t := t - h * ticksperhour;
    m := t div ticksperminute;
    t := t - m * ticksperminute;
    s := t div ticksperseconde;
    tempstr := '';
    if h > 0 then
    begin
      tempstr := tempstr + IntToStr(h) +  'hrs ';
      tempstr := tempstr + formatfloat('00', m, localeFormat) +  'min ';
      tempstr := tempstr + formatfloat('00', s, localeFormat) +  'sec';
    end
    else
    begin
      if m > 0 then
      begin
        tempstr := tempstr + IntToStr(m) +  'min ';
        tempstr := tempstr + formatfloat('00', s, localeFormat) +  'sec';
      end
      else
      begin
        tempstr := tempstr + IntToStr(s) +  'sec';
      end;
    end;
    line := StringReplace(line, WinampLengtlKey, tempstr, [rfReplaceAll]);
  end;

  if pos(WinampLengtsKey, line) <> 0 then
  begin
    t := trackLength*1000;
    h := t div ticksperhour;
    t := t - h * ticksperhour;
    m := t div ticksperminute;
    t := t - m * ticksperminute;
    s := t div ticksperseconde;
    tempstr := '';
    if h > 0 then
    begin
      tempstr := tempstr + IntToStr(h) + ':';
      tempstr := tempstr + formatfloat('00', m, localeFormat) + ':';
      tempstr := tempstr + formatfloat('00', s, localeFormat);
    end
    else
    begin
      if m > 0 then
      begin
        tempstr := tempstr + IntToStr(m) + ':';
        tempstr := tempstr + formatfloat('00', s, localeFormat);
      end
      else
      begin
        tempstr := tempstr + IntToStr(s);
      end;
    end;
    line := StringReplace(line, WinampLengtsKey, tempstr, [rfReplaceAll]);
  end;

  if pos(WinampLengthKey, line) <> 0 then
  begin
    line := StringReplace(line, WinampLengthKey, IntToStr(trackLength),
      [rfReplaceAll]);
  end;

  if pos(WinampTracknrKey, line) <> 0 then
  begin
    line := StringReplace(line, WinampTracknrKey,
      IntToStr(LCDSmartieDisplayForm.winampctrl1.GetListPos + 1), [rfReplaceAll]);
  end;
  if pos(WinampTotalTracksKey, line) <> 0 then
  begin
    line := StringReplace(line, WinampTotalTracksKey,
      IntToStr(LCDSmartieDisplayForm.winampctrl1.GetListCount), [rfReplaceAll]);
  end;
end;


end.


