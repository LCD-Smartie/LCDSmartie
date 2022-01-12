unit uJSON;

interface

uses
  SysUtils;

type
  myJDType = (dtValue, dtObject, dtArray, dtUnset);
  myJVType = (vtText, vtNumber, vtBoolean, vtNull);

  myJSONItem = class
  private
    // item name
    fKey: string;
    // item type (value/object/array)
    fType: myJDType;
    // if item type is value, additionaly specify value type (text/number/boolean)
    fValType: myJVType;
    // value (if item type is value)
    fValue: string;
    // child nodes (if item type is object/array)
    fChild: array of myJSONItem;

    // getters
    function getItem(key: string): myJSONItem;
    function getCode: string;
    function getKey(index: integer): string;
    function getElem(index: integer): myJSONItem;  
    function hasKey(key: string): boolean;

    // parse code
    procedure setCode(aCode: string);

    // utility
    procedure clear_child;

    // this metod do actual parsing, returns the rest (unparsed) of the code
    function parse(aCode: string): string;
    // those methods read values and return the rest of the code
    function readValue(aCode: string): string;
    function readObject(aCode: string): string;
    function readArray(aCode: string): string;
    function readNumber(aCode: string): string;
    function readBoolean(aCode: string): string;
    function readNull(aCode: string): string;
  public
    // Item's name (key)
    property Name: string read fKey;
    // Returns child node by it's key
    property Item[key: string]: myJSONItem read getItem; default;
    // Own code setter/getter (assign opne item's code to another to clone it)
    property Code: string read getCode write setCode;
    // Returns child node key by it's index
    property Key[index: integer]: string read getKey;
    // Returns child node by it's index (for arrays, but will also work for objects)
    property Value[index: integer]: myJSONItem read getElem;
    // Returns if the node has some key (better than get value and check if it's equal to default)
    property Has[key: string]: boolean read hasKey;

    constructor Create;
    destructor Destroy; override;

    // Clear
    procedure Clear;
    // Child nodes count
    function Count: integer;
    // Remove n-th node
    procedure Remove(n: integer);
    // Load/Save own code
    procedure LoadFromFile(filename: string; utf: boolean = true);
    procedure SaveToFile(filename: string);

    // terminal node's (leafs) methods
    //  setters
    procedure setInt(value: integer);
    procedure setNum(value: double);
    procedure setStr(value: string);
    procedure setBool(value: boolean);
    procedure setType(aType: myJDType);

    //   getters
    function getInt(default: integer = 0): integer;
    function getNum(default: double = 0): double;
    function getStr(default: string = ''): string;
    function getBool(default: boolean = false): boolean;
    function getType(default: myJDType = dtUnset): myJDType;
    function getJSON: string;

    // NULL-handling
    procedure setNull;
    function isNull: boolean;

    // Array helper
    procedure setArray(newLength: integer);
  end;

implementation

const
  WHITESPACE: set of char =  [#9, #10, #13, #32];
  DIGITS: set of char =  ['0'..'9'];
  SIGNS: set of char = ['+', '-'];


// removes all the whitespaces from the begining of the line
function wsTrim(src: string): string;
var
  n, l: integer;
begin
  n := 1;
  l := Length(src);

  while (src[n] in WHITESPACE) do begin
    n := n + 1;
    if n >= l
      then Break;
  end;

  result := Copy(src, n, l - n + 1);
end;

{ myJSONItem }

procedure myJSONItem.Clear;
begin
  clear_child;
end;

procedure myJSONItem.clear_child;
var
  i: integer;
begin
  // recursively removes all childs
  for i := 0 to high(fChild) do
    fChild[i].Free;
  SetLength(fChild, 0);
end;

function myJSONItem.Count: integer;
begin
  result := Length(fChild);
end;

constructor myJSONItem.Create;
begin
  // Should've set initial values
  // But root node don't need them and child node will get them on parsing/from setter
  // or when they are called for the first time
end;

destructor myJSONItem.Destroy;
begin
  // mem leaks prevention
  clear_child;

  inherited;
end;

function myJSONItem.getBool(default: boolean): boolean;
var
  f: double;
begin
  // defaulting
  result := default;

  if self = nil
    then Exit;

  // type checking
  if fType <> dtValue
    then Exit;

  // checking alphabetic values
  if fValue = ''
    then Exit; // unset - defaulting

  if LowerCase(fValue) = 'true' then begin
    result := true;
    Exit;
  end;

  if LowerCase(fValue) = 'false' then begin
    result := false;
    Exit;
  end;

  // checking numeral values
  if default
    then f := 1
    else f := 0;
  f := StrToFloatDef(fValue, f);

  if f > 0
    then result := true
    else result := false;
end;

function myJSONItem.getCode: string;
var
  i: integer;
begin
  result := '""';
  if self = nil
    then Exit;
  case self.fType of
    dtObject: begin
      result := '{';
      // adding childrens code
      for i := 0 to high(fChild) do begin
        result := result + fChild[i].Code;
        if i < high(fChild)
          then result := result + ',';
      end;
      result := result + '}';
    end;  
    dtArray: begin
      result := '[';
      // children's code
      for i := 0 to high(fChild) do begin
        result := result + fChild[i].Code;
        if i < high(fChild)
          then result := result + ',';
      end;
      result := result + ']';
    end;
    dtValue: begin
      // only text values should be enquoted
      if fValType <> vtText
        then result := fValue
        else result := '"' + fValue + '"';
    end;
  end;

  // if we have a key - add it before our value
  if fKey <> ''
    then result := '"' + fKey + '":' + result;
end;

function myJSONItem.getElem(index: integer): myJSONItem;
var
  i: integer;
begin
  // Item type is not changed, unlike getItem
  result := nil;
  // type compatibility check
  if (fType <> dtObject) and (fType <> dtArray)
    then Exit;
  // range check
  if index < 0
    then Exit;

  // здесь небольшая вилка
  if fType = dtObject then begin
    // объект не может вернуть элемент с индексом выше максимального
    if index <= high(fChild)
      then result := fChild[index];
    Exit;
  end;

  // а массив вполне себе может, при этом он заполнит все недостающие элементы пустыми значениями
  if fType = dtArray then begin
    // проверяем длину
    if high(fChild) < index
      then SetLength(fChild, index + 1);
    // добавляем элементы, начиная с конца (в начале они могут уже быть, а если нет, то не похуй ли)
    for i := high(fChild) downto 0 do
      if fChild[i] = nil
        then fChild[i] := myJSONItem.Create
        else Break;
    result := fChild[index];
  end;
end;

function myJSONItem.getInt(default: integer): integer;
var
  f: double;
begin
  // возвращает значение как целое число
  result := default;   

  if self = nil
    then Exit;

  // проверяем на соотвествие типу
  if fType <> dtValue
    then Exit;

  // проверяем, а не boolean ли у нас в значении
  if fValue = 'true' then begin
    result := 1;
    Exit;
  end;
  if fValue = 'false' then begin
    result := 0;
    Exit;
  end;

  // преобразуем как дробное (функция и целое схавает)
  f := StrToFloatDef(fValue, default);
  
  // и выделяем целую часть
  result := Trunc(f);
end;

function myJSONItem.getItem(key: string): myJSONItem;
var
  i, n: integer;
begin
  result := nil;
  if self = nil
    then Exit;
  // возвращает дочерний элемент
  // если элемент отсутствует - создаёт его

  // при этом тип элемента меняется на dtObject
  if fType <> dtObject
    then setType(dtObject);

  // ищем элемент
  key := LowerCase(key);
  n := -1;
  for i := 0 to high(fChild) do
    if LowerCase(fChild[i].fKey) = key then begin
      n := i;
      Break;
    end;

  // если не нашёлся - создаём
  if n < 0 then begin
    SetLength(fChild, Length(fChild) + 1);
    n := high(fChild);
    fChild[n] := myJSONItem.Create;
    fChild[n].fKey := key;
  end;

  // возврращаем результат
  result := fChild[n];
end;

function myJSONItem.getJSON: string;
var
  i: integer;
begin
  // FIXME: this is the same code as in getCode (except we don't add "key":), gotta do something about this
  result := '""';
  if self = nil
    then Exit;

  case self.fType of
    dtObject: begin
      result := '{';
      // adding childrens code
      for i := 0 to high(fChild) do begin
        result := result + fChild[i].Code;
        if i < high(fChild)
          then result := result + ',';
      end;
      result := result + '}';
    end;  
    dtArray: begin
      result := '[';
      // children's code
      for i := 0 to high(fChild) do begin
        result := result + fChild[i].Code;
        if i < high(fChild)
          then result := result + ',';
      end;
      result := result + ']';
    end;
    dtValue: begin
      // only text values should be enquoted
      if fValType <> vtText
        then result := fValue
        else result := '"' + fValue + '"';
    end;
  end;
end;

function myJSONItem.getKey(index: integer): string;
var
  n: myJSONItem;
begin
  // возврращаем ключ N-го потомка
  // работает по прринципу getElem и с его помощью
  result := '';
  n := getElem(index);
  if n <> nil
    then result := n.fKey;
end;

function myJSONItem.getNum(default: double): double;
begin
  // возвращает значение как дробное число
  result := default;  

  if self = nil
    then Exit;

  // проверяем на соотвествие типу
  if fType <> dtValue
    then Exit;

  // проверяем, а не boolean ли у нас в значении
  if fValue = 'true' then begin
    result := 1;
    Exit;
  end;
  if fValue = 'false' then begin
    result := 0;
    Exit;
  end;
  
  // преобразуем значение
  result := StrToFloatDef(fValue, default);
end;

function myJSONItem.getStr(default: string): string;
begin
  // тут нам насррать что как - всё равно всё хранится в строке
  result := default;   

  if self = nil
    then Exit;

  // проверяем только на соотвествие типу
  if fType <> dtValue
    then Exit;
  result := fValue;
end;

function myJSONItem.getType(default: myJDType): myJDType;
begin
  result := default;

  if self = nil
    then Exit;
    
  result := fType;
end;

function myJSONItem.hasKey(key: string): boolean;
var
  i: integer;
begin
  key := LowerCase(key);
  result := true;
  for i := 0 to high(fChild) do
    if fChild[i].fKey = key
      then Exit;
  result := false;
end;

function myJSONItem.isNull: boolean;
begin
  // or should it be true?
  result := false;

  if Self = nil
    then Exit;

  result := fValType = vtNull;
end;

procedure myJSONItem.LoadFromFile(filename: string; utf: boolean);
var
  f: Text;
  s, b: string;
begin
  clear_child;
  AssignFile(f, filename);
  {$I-}
  Reset(f);
  if IOResult <> 0
    then raise Exception.CreateFmt('JSON: Failed to load %s', [filename]);
  {$I+}
  while not EOF(f) do begin
    Readln(f, b);
    b := Trim(b);
    s := s + b;
  end;
  if utf
    then Code := utf8decode(s)
    else Code := s;
  CloseFile(f);
end;

function myJSONItem.parse(aCode: string): string;
var
  trail: char;
begin
  // 1. чистим WS
  aCode := wsTrim(aCode);
  // теперь в первом символе наша открывающая скобка
  case aCode[1] of
    // generic value (text)
    '"': result := readValue(aCode);
    // number
    '0'..'9', '+', '-': result := readNumber(aCode);
    // boolean
    't', 'T', 'f', 'F': result := readBoolean(aCode);
    // null
    'n', 'N': result := readNull(aCode);
    // Object
    '{': result := readObject(aCode);
    // Array
    '[': result := readArray(aCode);
    // Shit =/
    else begin
      // Shouldda raise an exception here
      raise Exception.CreateFmt('JSON parsing error "%s" is a bullshit', [aCode]);
      result := aCode;
    end;
  end;
end;

function myJSONItem.readArray(aCode: string): string;
var
  n, idx: integer;
  val: myJSONItem;
begin
  // we get here because first symbol was '['
  Delete(aCode, 1, 1);
  aCode := wsTrim(aCode);
  idx := 0;

  Self.setType(dtArray);
  // reading values until we reach a ']'
  while aCode[1] <> ']' do begin
    // Creating a new value
    val := Self.Value[idx];
    aCode := val.parse(aCode); // "foo"\n,  ,  "bar"] -> \n,  ,  "bar"]
    aCode := wsTrim(aCode); // \n,  ,  "bar'] -> ,  ,  "bar"]
    while aCode[1] = ',' do begin
      aCode := wsTrim(
        Copy(
          aCode,
          2,
          Length(aCode)
        )
      ); // ,  ,  "bar"] -> ,  "bar"] -> "bar"]
      idx := idx + 1;
    end;
  end;

  result := Copy(aCode, 2, Length(aCode)); // ]... -> ...
end;

function myJSONItem.readBoolean(aCode: string): string;
var
  sample: string;
begin
  sample := Copy(aCode, 1, 4);
  if LowerCase(sample) = 'true' then begin
    Self.setType(dtValue); 
    Self.fValue := 'true';
    Self.fValType := vtBoolean;
    result := copy(aCode, 5, Length(aCode));
    exit;
  end;

  
  sample := Copy(aCode, 1, 5);
  if LowerCase(sample) = 'false' then begin
    Self.setType(dtValue);
    Self.fValue := 'false';
    Self.fValType := vtBoolean;
    result := copy(aCode, 6, Length(aCode));
    exit;
  end;
end;

function myJSONItem.readNull(aCode: string): string;
var
  sample: string;
begin
  sample := Copy(aCode, 1, 4);
  if LowerCase(sample) = 'null' then begin
    Self.setType(dtValue);
    Self.fValue := 'null';
    Self.fValType := vtNull;
    result := copy(aCode, 5, Length(aCode));
    exit;
  end;
end;

function myJSONItem.readNumber(aCode: string): string;
var
  n, l: integer;
begin
  // нужно добавить поддержку чисел вида -1.23E4.5

  l := Length(aCode);
  n := 1;
  // 1. sign (optional)
  if aCode[n] in SIGNS
    then n := n + 1;

  // 2. some digits
  while (aCode[n] in DIGITS) do
    n := n + 1;

  // 3. decimal dot (optional)
  if aCode[n] = '.'
    then n := n + 1;

  // 4. fractional digits (optional)
  while (aCode[n] in DIGITS) do
    n := n + 1;

  // TODO: Add E+/-1.23 part

  // result
  Self.setType(dtValue);
  Self.fValType := vtNumber;
  Self.fValue := Copy(aCode, 1, n - 1);

  result := Copy(aCode, n, l);
end;

function myJSONItem.readObject(aCode: string): string;
var
  n: integer;
  val: myJSONItem;
begin
  // just like an array, but we read pairs of key:value instead
  Delete(aCode, 1, 1);
  aCode := wsTrim(aCode);

  Self.setType(dtObject);

  // reading values until we reach a '}'
  while aCode[1] <> '}' do begin
    // Reading a key
    n := 1;
    while aCode[n] <> ':' do
      n := n + 1;

    // Creating a new value
    // keys are supposted to be quoted
    val := Self[Copy(aCode, 2, n - 3)]; // -quote -quote -colon
    aCode := Copy(aCode, n + 1, Length(aCode));

    // parsing value
    aCode := val.parse(aCode);
    // trimming leftovers
    aCode := wsTrim(aCode);

    // skipping to the next pair
    while aCode[1] = ',' do begin
      aCode := wsTrim(
        Copy(
          aCode,
          2,
          Length(aCode)
        )
      ); // ,  ,  "bar"] -> ,  "bar"] -> "bar"]
    end;
  end;

  result := Copy(aCode, 2, Length(aCode)); // ]... -> ...
end;

function myJSONItem.readValue(aCode: string): string;
var
  n: integer;
begin
  // we get here because our first symbol is '"'
  n := 2;

  while aCode[n] <> '"' do begin
    case aCode[n] of
      '\': begin
        // TODO: escapes and stuff
        n := n + 2;
      end;

      else
        n := n + 1;
    end;
  end;

  Self.setType(dtValue);
  Self.fValType := vtText;
  Self.fValue := Copy(aCode, 2, n - 2);

  result := Copy(aCode, n + 1, Length(aCode));
end;

procedure myJSONItem.Remove(n: integer);
var
  i: integer;
begin
  // удаляет N-ого потомка
  if (n < 0) or (n > high(fChild)) or (length(fChild) < 1)
    then Exit;

  fChild[n].Free;
  for i := n to high(fChild) - 1 do
    fChild[i] := fChild[i + 1];

  setLength(fChild, high(fChild));
end;

procedure myJSONItem.SaveToFile(filename: string);
var
  f: TextFile;
begin
  AssignFile(f, filename);
  Rewrite(f);
  Write(f, Code);
  if IOResult <> 0
    then raise Exception.CreateFmt('JSON: Failed to save %s', [filename]);
  CloseFile(f);
end;

procedure myJSONItem.setArray(newLength: integer);
begin
  // using existing methods
  self.setType(dtArray);
  // arrays are zero based
  // trying to get the last element
  // this will resize array and initialize its elements
  self.getElem(newLength - 1);
end;

procedure myJSONItem.setBool(value: boolean);
begin
  if self = nil
    then Exit;

  // значения могут иметь только dtValue, массивы и объекты будут редуцированы
  setType(dtValue);
  Self.fValType := vtBoolean;
  //fValType := vtBoolean;
  if value
    then fValue := 'true'
    else fValue := 'false';
end;

procedure myJSONItem.setCode(aCode: string);
begin
  clear_child;
  Self.parse(aCode); // =/
end;

procedure myJSONItem.setInt(value: integer);
begin
  if self = nil
    then Exit;

  // значения могут иметь только dtValue, массивы и объекты будут редуцированы
  setType(dtValue);
  fValType := vtNumber;
  fValue := IntToStr(value);
end;

procedure myJSONItem.setNull;
begin
  if self = nil
    then Exit;

  // значения могут иметь только dtValue, массивы и объекты будут редуцированы
  setType(dtValue);
  fValType := vtNull;
  fValue := 'null';
end;

procedure myJSONItem.setNum(value: double);
begin             
  if self = nil
    then Exit;

  // значения могут иметь только dtValue, массивы и объекты будут редуцированы
  setType(dtValue);
  fValType := vtNumber;
  fValue := FloatToStr(value);
  fValue := StringReplace(fValue, ',', '.', [rfReplaceAll]);
end;

procedure myJSONItem.setStr(value: string);
begin      
  if self = nil
    then Exit;

  // значения могут иметь только dtValue, массивы и объекты будут редуцированы
  setType(dtValue);
  fValType := vtText;
  fValue := value;
end;

procedure myJSONItem.setType(aType: myJDType);
var
  i: integer;
begin    
  if self = nil
    then Exit;
    
  // если массив или объект преобразуются в число, то нужно изничтожить всех потомков
  if (aType = dtValue) and (fType <> dtValue) then begin
    clear_child;
    // по умолчанию тип значения - текст
    // gfhcth b ctnnths 'nj bcghfdkz.n? tckb ye;yj
    fValType := vtText;
  end;

  // если число преобразуется в объект или массив, то нужно отнять у него значение
  if (aType <> dtValue) and (fType = dtValue) then begin
    fValue := '';
  end;

  // если массив преобразуется в объект, то нужно всем его элементам назначить ключи
  if (aType = dtObject) and (fType = dtArray) then begin
    for i := 0 to high(fChild) do
      fChild[i].fKey := IntToStr(i);
  end;

  // если объект преобрразуется в массив, то нужно поубиррать ключи у его потомков
  if (aType = dtArray) and (fType = dtObject) then begin
    for i := 0 to high(fChild) do
      fChild[i].fKey := '';
  end;

  fType := aType;
end;

end.