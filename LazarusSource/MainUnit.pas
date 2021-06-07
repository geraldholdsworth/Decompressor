unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 ZStream, Spark;

type

 { TMainForm }

 TMainForm = class(TForm)
  Label1: TLabel;
  Memo1: TMemo;
  Memo2: TMemo;
  Panel1: TPanel;
  procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  function RemoveTopBit(input: String):String;
  function Inflate(filename: String): TDynByteArray;
 private

 public

 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
var
 i,j         : Cardinal;
 buffer      : TDynByteArray;
 F           : TFileStream;
 fn          : String;
 gzip,
 zip,
 pack        : Boolean;
 SparkFile   : TSpark;
begin
 Memo1.Visible:=False;
 buffer:=nil;
 for i:=0 to Length(FileNames)-1 do
 begin
  //Reset the flags
  gzip:=False;
  zip :=False;
  pack:=False;
  //See if it is a GZ file or ZIP file
  F:=TFileStream.Create(FileNames[i],fmOpenRead or fmShareDenyNone);
  SetLength(buffer,5);
  F.Read(buffer[0],5);
  F.Free;
  if(buffer[0]=$1F)and(buffer[1]=$8B)and(buffer[2]=$08)then gzip:=True;
  if(buffer[0]=$50)and(buffer[1]=$4B)and(buffer[2]=$03)and(buffer[3]=$04)then zip:=True;
  if(buffer[0]=$50)and(buffer[1]=$41)and(buffer[2]=$43)and(buffer[3]=$4B)and(buffer[4]=$00)then pack:=True;
  if gzip then //GZip
  begin
   //Inflate the file into buffer
   buffer:=Inflate(FileNames[i]);
   fn:=ExtractFileExt(FileNames[i]);
   fn:=LeftStr(FileNames[i],Length(FileNames[i])-Length(fn))+'-inflated'+fn;
   F:=TFileStream.Create(fn,fmCreate);
   F.Write(buffer[0],Length(buffer));
   F.Free;
  end;
  if(zip)or(pack)then //Zip
  begin
   SparkFile:=TSpark.Create(FileNames[i]);
   Memo1.Clear;
   Memo1.Visible:=True;
   Memo1.Lines.BeginUpdate;
   Memo1.Lines.Add(FileNames[i]);
   if SparkFile.IsSpark then
   begin
    Memo1.Lines.Add('Number of entries: '+IntToStr(Length(SparkFile.FileList)));
    Memo1.Lines.Add('Total uncompressed size: '+IntToStr(SparkFile.UncompressedSize));
    Memo1.Lines.Add('Parent | Filename | Load Address | Execution Address | File Length | Attributes | Extra information');
    for j:=0 to Length(SparkFile.FileList)-1 do
    begin
     fn:='';
     if SparkFile.FileList[j].Directory then fn:='D';
     if SparkFile.FileList[j].Attributes AND $08=$08 then fn:=fn+'L';
     if SparkFile.FileList[j].Attributes AND $02=$02 then fn:=fn+'W';
     if SparkFile.FileList[j].Attributes AND $01=$01 then fn:=fn+'R';
     if SparkFile.FileList[j].Attributes AND $04=$04 then fn:=fn+'E';
     fn:=fn+'/';
     if SparkFile.FileList[j].Attributes AND $80=$80 then fn:=fn+'l';
     if SparkFile.FileList[j].Attributes AND $20=$20 then fn:=fn+'w';
     if SparkFile.FileList[j].Attributes AND $10=$10 then fn:=fn+'r';
     if SparkFile.FileList[j].Attributes AND $40=$40 then fn:=fn+'e';
     fn:=RemoveTopBit(SparkFile.FileList[j].Parent)+' '+
                     RemoveTopBit(SparkFile.FileList[j].Filename)+' '+
                     IntToHex(SparkFile.FileList[j].LoadAddr,8)+' '+
                     IntToHex(SparkFile.FileList[j].ExecAddr,8)+' '+
                     IntToHex(SparkFile.FileList[j].Length  ,8)+' '+
                     fn;
     if SparkFile.FileList[j].Directory then //Display the number of entries
      fn:=fn+' '+IntToStr(SparkFile.FileList[j].NumEntries)+' entries';
     if not SparkFile.FileList[j].Directory then //Otherwise display where the data is
     begin
      fn:=fn+' '+IntToHex(SparkFile.FileList[j].DataOffset,8);
      //And if it is uncompressed
      if SparkFile.FileList[j].Length=SparkFile.FileList[j].Size then
       fn:=fn+' uncompressed';
     end;
     Memo1.Lines.Add(fn);
     //Uncomment to extract each file - this will increase the reading time.
     {if not SparkFile.FileList[j].Directory then
     begin
      buffer:=SparkFile.ExtractFileData(j);
      F:=TFileStream.Create(Filenames[i]+'-'+SparkFile.FileList[j].Filename,fmCreate);
      F.Write(buffer[0],Length(buffer));
      F.Free;
     end;}
    end;
   end;
   fn:='ADFS Old Directory (S/M/L)';
   if SparkFile.MaxDirEnt>47 then fn:='ADFS New Directory (D/E/F)';
   if SparkFile.MaxDirEnt>77 then fn:='ADFS Big Directory (E+/F+)';
   Memo1.Lines.Add('Minimum recommended FS to use: '+fn);
   Memo1.Lines.EndUpdate;
   SparkFile.Free;
  end;
 end;
end;

function TMainForm.RemoveTopBit(Input: String):String;
var
 c,i: Integer;
begin
 Result:='';
 for i:=1 to Length(Input) do
 begin
  c:=ord(Input[i])AND$7F;
  if c<31 then c:=ord('_');
  Result:=Result+chr(c);
 end;
end;

function TMainForm.Inflate(filename: String): TDynByteArray;
 function L_Inflate(Source: String): TDynByteArray;
 var
  GZ     : TGZFileStream;
  chunk  : TDynByteArray;
  cnt,
  i,
  buflen : Integer;
 const
   ChunkSize=4096; //4K chunks
 begin
  //Initialise the variables
  Result:=nil;
  chunk:=nil;
  //Open the stream
  try
   GZ:=TGZFileStream.Create(Source,gzOpenRead);
   //This is our length counter
   buflen:=0;
   //We'll be reading it in chunks
   SetLength(chunk,ChunkSize);
   repeat
    //Read in the next chunk
    cnt:=GZ.Read(chunk[0],ChunkSize);
    //Extend the buffer accordingly
    SetLength(Result,buflen+cnt);
    //Copy the chunk into the buffer
    for i:=0 to cnt-1 do Result[buflen+i]:=chunk[i];
    //Increase the buffer length counter
    inc(buflen,cnt);
    //Until we are done
   until cnt<ChunkSize;
   //Free up the stream
  except
  end;
  GZ.Free;
 end;
var
 F        : TFileStream;
 buffer,
 inflated : TDynByteArray;
 ptr,i,old: Cardinal;
 blockptrs: array of Cardinal;
 fn       : String;
begin
 buffer   :=nil;
 blockptrs:=nil;
 inflated :=nil;
 Result   :=nil;
 //Read in the entire file
 try
  F:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
  SetLength(buffer,F.Size);
  F.Read(buffer[0],F.Size);
 except
 end;
 F.Free;
 //First, is it actually a GZip file?
 if(buffer[$00]=$1F)and(buffer[$01]=$8B)and(buffer[$02]=$08)then
 begin
  //Count how many blocks and make note of their positions
  for ptr:=0 to Length(buffer)-10 do
   if(buffer[ptr]=$1F)and(buffer[ptr+1]=$8B)and(buffer[ptr+2]=$08)then
   begin
    //Make a note of the position
    SetLength(blockptrs,Length(blockptrs)+1);
    blockptrs[Length(blockptrs)-1]:=ptr;
   end;
 end;
 //Separate each block, if more than one
 if Length(blockptrs)>1 then
 begin
  //Add the file end to the end of the block pointers
  SetLength(blockptrs,Length(blockptrs)+1);
  blockptrs[Length(blockptrs)-1]:=Length(buffer);
  //Set up the container for the inflated file
  SetLength(Result,0);
  //Get a temporary filename
  fn:=GetTempDir+ExtractFileName(filename);
  //Iterate through the pointers
  for i:=0 to Length(blockptrs)-2 do
  begin
   //Create the temporary file and write the block to it
   try
    F:=TFileStream.Create(fn,fmCreate);
    F.Write(buffer[blockptrs[i]],blockptrs[i+1]-blockptrs[i]);
   except
   end;
   F.Free;
   //Inflate the block
   inflated:=L_Inflate(fn);
   old:=Length(Result); //Previous length of the inflated file
   //Increase the inflated file buffer to accomodate
   SetLength(Result,Length(Result)+Length(inflated));
   //Move the inflated data across
   for ptr:=0 to Length(inflated)-1 do Result[old+ptr]:=inflated[ptr];
  end;
  //Delete the temporary file
  if FileExists(fn) then DeleteFile(fn);
 end;
 //If just the one block, then don't bother splitting
 if Length(blockptrs)=1 then Result:=L_Inflate(filename);
 //If there are no blocks, then just return the entire file
 if Length(blockptrs)=0 then Result:=buffer;
end;

end.
