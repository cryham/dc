{
   Double Commander
   -------------------------------------------------------------------------
   Generic file view containing default panels (header, footer, etc.)

   Copyright (C) 2012-2018  Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFileViewWithPanels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Graphics, math,
  uFileView, uFileViewHeader, uFileSource, uTypes, uFileFunctions, LCLType;

type
  TFileViewWithPanels = class;

  TStatusPanel = class(TCustomControl)
  public
    FTFileView: TFileViewWithPanels;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  end;

  { TFileViewWithPanels }

  TFileViewWithPanels = class(TFileView)
  protected
    FSelectedCount: Integer;
    FInfo: string;

    pnlFooter: TStatusPanel;
    pnlHeader: TFileViewHeader;

    FFilesInDir, FFilesSelected, FFolderInDir, FFolderSelected: Integer;
    FSizeInDir, FSizeSelected: Int64;

    procedure AfterChangePath; override;
    procedure CreateDefault(AOwner: TWinControl); override;
    procedure DisplayFileListChanged; override;
    procedure DoActiveChanged; override;
    procedure DoSelectionChanged; override;
    procedure ShowPathEdit;
    procedure DoUpdateView; override;
    procedure UpdateFlatFileName; virtual;
    procedure UpdateInfoPanel; virtual;
    procedure UpdateFooterDetails;
  public
    property Header:TFileViewHeader read pnlHeader;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;
    procedure RemoveCurrentFileSource; override;

  published
    procedure cm_EditPath(const {%H-}Params: array of string);
  end;

implementation

uses
  DCStrUtils, uFile, uGlobs, uLng, uFileProperty, uFileViewWorker, uDCUtils;

type
  TColorSize = record
    b,g,r: byte;  size: Int64;
  end;

const
  sClrSizes: array[0..13] of TColorSize=(
    (b:$20;g:$20;r:$20; size:  0),  // -
    (b:$08;g:$C0;r:$80; size:  1),  // B
    (b:$10;g:$B0;r:$10; size: 10),
    (b:$80;g:$A0;r:$10; size:100),
    (b:$FF;g:$88;r:$28; size:  1 *1000),  // K
    (b:$FF;g:$C0;r:$40; size: 10 *1000),
    (b:$F0;g:$F0;r:$20; size:100 *1000),
    (b:$FF;g:$A0;r:$A0; size:  1 *1000000),  // M
    (b:$F0;g:$98;r:$C0; size: 10 *1000000),
    (b:$FF;g:$80;r:$D8; size:100 *1000000),
    (b:$80;g:$F8;r:$F8; size:  1 *1000000000),  // G
    (b:$40;g:$80;r:$F0; size: 10 *1000000000),
    (b:$50;g:$50;r:$FF; size:100 *1000000000),
    (b:$90;g:$40;r:$E8; size:  1 *1000000000000)  // T
  );
  cSizeColors: array[0..12] of TColor=(
    $08C080,  //1  // B,G,R
    $10C010,  //10
    $80A010,  //100
    $FF8828,  //1 k
    $FFC040,  //10 k
    $F0F020,  //100 k
    $FFC0C0,  //1 m
    $F098C0,  //10 m
    $FF80D8,  //100 m
    $80F8F8,  //1 g
    $4080F0,  //10 g
    $5050FF,  //100 g
    $9040E8); //1 t

{ TFileViewWithPanels }

procedure TFileViewWithPanels.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);
  pnlHeader.UpdateAddressLabel;
end;

procedure TFileViewWithPanels.AfterChangePath;
begin
  inherited AfterChangePath;
  if FileSourcesCount > 0 then
    pnlHeader.UpdatePathLabel;
  UpdateFooterDetails;
end;

procedure TFileViewWithPanels.cm_EditPath(const Params: array of string);
begin
  ShowPathEdit;
end;

procedure TFileViewWithPanels.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);

  pnlHeader := TFileViewHeader.Create(Self, Self);

  pnlFooter            := TStatusPanel.Create(Self);
  pnlFooter.FTFileView := Self;
  pnlFooter.Parent     := Self;
  pnlFooter.Align      := alBottom;
  pnlFooter.AutoSize   := False;
  pnlFooter.Font.Style := [fsBold];
  pnlFooter.DoubleBuffered := True;

  {$IF DEFINED(LCLGTK2)}
  // Workaround: "Layout and line"
  // http://doublecmd.sourceforge.net/mantisbt/view.php?id=573
  pnlFooter.Visible := False;
  {$ELSE}
  pnlFooter.Height := 2 * pnlFooter.Canvas.TextHeight('Wg');
  {$ENDIF}

  {$IFDEF LCLCARBON}
  // Under Carbon AutoSize don't work without it
  pnlHeader.ClientHeight:= 0;
  pnlFooter.ClientHeight:= 0;
  {$ENDIF}
end;

procedure TFileViewWithPanels.DisplayFileListChanged;
begin
  inherited DisplayFileListChanged;
  UpdateInfoPanel;
end;

procedure TFileViewWithPanels.DoActiveChanged;
begin
  inherited DoActiveChanged;
  pnlHeader.SetActive(Active);
  UpdateFooterDetails;
end;

procedure TFileViewWithPanels.DoSelectionChanged;
begin
  inherited DoSelectionChanged;
  UpdateInfoPanel;
end;

procedure TFileViewWithPanels.DoUpdateView;
begin
  inherited DoUpdateView;
  pnlHeader.Visible := gCurDir;     // Current directory
  pnlFooter.Visible := gStatusBar;  // Status bar
  pnlHeader.UpdateFont;
  pnlHeader.UpdateAddressLabel;
  pnlHeader.UpdatePathLabel;
  UpdateFooterDetails;
end;

procedure TFileViewWithPanels.RemoveCurrentFileSource;
begin
  inherited RemoveCurrentFileSource;
  if FileSourcesCount > 0 then
    pnlHeader.UpdateAddressLabel;
end;

procedure TFileViewWithPanels.ShowPathEdit;
begin
  pnlHeader.ShowPathEdit;
end;

procedure TFileViewWithPanels.UpdateFlatFileName;
begin
  UpdateFooterDetails;
end;

procedure TFileViewWithPanels.UpdateInfoPanel;
var
  i: Integer;
  FilesInDir, FilesSelected, FolderInDir, FolderSelected: Integer;
  SizeInDir, SizeSelected: Int64;
  SizeProperty: TFileSizeProperty;
begin
  FSelectedCount := 0;
  if GetCurrentWorkType = fvwtCreate then
  begin
    FInfo := rsMsgLoadingFileList;
  end
  else if not Assigned(FAllDisplayFiles) or (FAllDisplayFiles.Count = 0) then
  begin
    FInfo := rsMsgNoFiles;
  end
  else if Assigned(FileSource) then
  begin
    FilesInDir  := 0;  FilesSelected  := 0;
    SizeInDir   := 0;  SizeSelected   := 0;
    FolderInDir := 0;  FolderSelected := 0;

    for i := 0 to FFiles.Count - 1 do
    begin
      with FFiles[i] do
      begin
        if FSFile.Name = '..' then Continue;
        if FSFile.IsDirectory then
          inc(FolderInDir)
        else
          inc(FilesInDir);
        if Selected then
        begin
          if FSFile.IsDirectory then
            inc(FolderSelected)
          else
            inc(FilesSelected);
        end;

        // Count size if Size property exists.
        if fpSize in FSFile.AssignedProperties then
        begin
          SizeProperty := FSFile.SizeProperty;

          if Selected then
            SizeSelected := SizeSelected + SizeProperty.Value;

          SizeInDir := SizeInDir + SizeProperty.Value;
        end;
      end;
    end;
    FSelectedCount := FilesSelected + FolderSelected;

    FFilesInDir:= FilesInDir;   FFilesSelected:= FilesSelected;
    FFolderInDir:=FolderInDir;  FFolderSelected:=FolderSelected;
    FSizeInDir:=  SizeInDir;    FSizeSelected:=  SizeSelected;
    FInfo := '';
  end
  else if not (csDestroying in ComponentState) then
    FInfo := '';

  UpdateFooterDetails;
end;

procedure TFileViewWithPanels.UpdateFooterDetails;
begin
  pnlFooter.Paint;
end;


function BlendColor(R, G, B: Byte; R1, G1, B1: Byte; APercent: Byte): TColor;
var
  dR,dG,dB, iR,iG,iB: Integer;
begin
  dR:= R1 - R;
  dG:= G1 - G;
  dB:= B1 - B;
  iR:= R + dR * APercent div 100;
  iG:= G + dG * APercent div 100;
  iB:= B + dB * APercent div 100;
  Result:= RGBToColor(iR, iG, iB);
end;

procedure TStatusPanel.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TStatusPanel.Paint;

  function GetSizeColor(s: Int64): TColor;
  var
    r,g,b,i: Integer;
  begin
    // linear
    i:= 0;
    while (i < high(sClrSizes)) and (s >= sClrSizes[i+1].size) do  Inc(i);
    Result:= BlendColor(
      sClrSizes[i  ].r, sClrSizes[i  ].g, sClrSizes[i  ].b,
      sClrSizes[i+1].r, sClrSizes[i+1].g, sClrSizes[i+1].b,
      100 * (s - sClrSizes[i].size) div (sClrSizes[i+1].size - sClrSizes[i].size) );
    //  old, step
    //i:= Min(High(cSizeColors), trunc(log10(Size+1)));
    //Result:= cSizeColors[i];
  end;

var
  AFile: TFile;
  AFileName,s: String;
  x0,x1,x2,x3, x,w,h: Integer;
  Bitmap: TBitmap;
begin
  if FTFileView = nil then Exit;

  //  draw on Bitmap
  Bitmap := TBitmap.Create;
  Bitmap.Height := Height;
  Bitmap.Width := Width;

  with FTFileView do
  with Bitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Name := gFonts[dcfMain].Name;
    Font.Size := gFonts[dcfMain].Size + 1;
    Font.Style := gFonts[dcfMain].Style;
    h:= -Font.Height + 3;  // par_
    x0:= 2;  x2:= (ClientWidth * 74) div 100;  x1:= x2 - 12;  x3:= x2 + 44;

    if FInfo <> '' then
    begin
      //  background  ------
      GradientFill(ClientRect, $030306, $181830, gdHorizontal);
      Font.Color := $B0C8D0;
      TextOut(2, h, FInfo);
    end
    else if (FSelectedCount > 0) then
    begin
      //  background  ------
      GradientFill(ClientRect, $010408, $102441, gdHorizontal);

      //  size  1 float
      Font.Color := GetSizeColor(FSizeSelected +1);
      s:= cnvFormatFileSize(FSizeSelected, fsfFloat, 1);
      x:= x1 - GetTextWidth(s);
      TextOut(x, 1, s);
      //  size  2 fixed
      s:= cnvFormatFileSize(FSizeSelected, fsfByte, 0)+' B';
      x:= x1 - GetTextWidth(s);
      TextOut(x, h, s);

      //  files
      s:= 'Files:   ';
      x:= GetTextWidth(s);
      Font.Color := $C0A080;
      TextOut(x0, 1, s);  s:= '';

      if FFilesSelected > 0 then
        if FFilesSelected = FFilesInDir then  s:= 'All'  else
          s:= Format('%.1f%%', [100.0 * real(FFilesSelected) / real(FFilesInDir)]);
      Font.Color := $E0C0A0;
      TextOut(x, 1, Format('%d / %d   ', [FFilesSelected, FFilesInDir]) + s);
      //  dirs
      Font.Color := $80C080;
      TextOut(x0, h, 'Dirs:');  s:= '';

      if FFolderSelected > 0 then
        if FFolderSelected = FFolderInDir then  s:= 'All'  else
          s:= Format('%.1f%%', [100.0 * real(FFolderSelected) / real(FFolderInDir)]);
      Font.Color := $A0E0A0;
      TextOut(x, h, Format('%d / %d   ', [FFolderSelected, FFolderInDir]) + s);

      //  sel
      Font.Color := $20A0FF;
      s:= 'Selected';
      x:= ClientWidth-6 - GetTextWidth(s);
      TextOut(x, 1, s);

    end else begin

      AFile:= CloneActiveFile;
      if Assigned(AFile) then
      try
        //  background  ------
        GradientFill(ClientRect, $040202, $2C2618, gdHorizontal);

        //  size  1 float
        Font.Color := GetSizeColor(AFile.Size +1);
        s:= cnvFormatFileSize(AFile.Size, fsfFloat, 1);
        x:= x1 - GetTextWidth(s);
        TextOut(x, 1, s);  w:= x-30;
        //  size  2 fixed
        s:= cnvFormatFileSize(AFile.Size, fsfByte, 0)+' B';
        x:= x1 - GetTextWidth(s);
        TextOut(x, h, s);

        //  name
        Font.Color := gColorExt.GetColorBy(AFile);
        TextOut(x0, 1, MinimizeFilePath(FormatFileFunction('DC().GETFILENAMENOEXT{}', AFile, FileSource),
                   Canvas, w));
        //  ext
        TextOut(x0, h,  FormatFileFunction('DC().GETFILEEXT{}', AFile, FileSource));
        //if AFile.IsDirectory then TextOut(40, 10, 'DIR');  AFile.IsHidden;

        //  attr
        if AFile.IsDirectory then    Font.Color := $90D090
        else if AFile.IsHidden then  Font.Color := $C0B0A0
        else                         Font.Color := $D0B090;
        TextOut(x3, 1, FormatFileFunction('DC().GETFILEATTR{}', AFile, FileSource));

        //  time, date
        Font.Color := $E0C8B0;
        TextOut(x2, h, FormatDateTime('hh:nn', AFile.ModificationTime));
        Font.Color := $E8E0D8;  // dd.mm.yy
        TextOut(x3, h, FormatDateTime('dd mm`yy', AFile.ModificationTime));

      finally
        AFile.Free;
      end;
    end;
  end;

  Canvas.Draw(0, 0, Bitmap);
  Bitmap.Free;
end;

end.

