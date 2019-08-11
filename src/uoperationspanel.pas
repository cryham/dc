{
    Double Commander
    -------------------------------------------------------------------------
    Panel displaying file operations.

    Copyright (C) 2012  Przemysław Nagay (cobines@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uOperationsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LCLVersion,
  fFileOpDlg,
  uFileSourceOperation, uOperationsManager, uFileSourceOperationUI,
  uFileSourceOperationTypes;

type

  { TOperationsPanel }

  TOperationsPanel = class(TScrollBox)
  private
    FMaximumItemWidth: Integer;
    FUserInterface: TFileSourceOperationUI;
    FOperations, FQueues: TFPList;
    FParentWidth: Integer;
    procedure ClearItems;
    procedure DeleteItem(List: TFPList; Index: Integer);
    procedure GetStateColor(State: TFileSourceOperationState; ID: TFileSourceOperationType;
                            out ColorFrom, ColorTo: TColor);
    procedure OperationsManagerEvent(Item: TOperationsManagerItem; Event: TOperationManagerEvent);
    procedure ProgressWindowEvent(OperationHandle: TOperationHandle;
                                  Event: TOperationProgressWindowEvent);
    procedure UpdateItems;
    procedure UpdateVisibility;
{$if lcl_fullversion >= 1070000}
  protected
    procedure SetParent(NewParent: TWinControl); override;
{$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ParentResized(Sender: TObject);
    procedure UpdateView;
  end;

implementation

uses
  LCLIntf, LCLType, Math,
  fViewOperations, uDCUtils,
  uFileSourceCopyOperation, uFileSourceMoveOperation, uFileSourceDeleteOperation,
  uFileSourceOperationMisc, uFileSourceOperationMessageBoxesUI;

const
  MinimumHeight = 10;
  MaximumItemWidth = 200;
  LeftRightTextMargin = 4;
  TopBottomTextMargin = 1;
  HorizontalSpaceBetween = 1;
  PanelBorderWidth = 1;

type
  TOperationPanelItem = record
    Width: Integer;
    OperationHandle: TOperationHandle;
    QueueId: TOperationsManagerQueueIdentifier;
  end;
  POperationPanelItem = ^TOperationPanelItem;

{ TOperationsPanel }

procedure TOperationsPanel.ParentResized(Sender: TObject);
begin
  FParentWidth := (Sender as TControl).Width;
  UpdateItems;
end;

procedure TOperationsPanel.ClearItems;
var
  p: Pointer;
begin
  for p in FOperations do
    Dispose(POperationPanelItem(p));
  for p in FQueues do
    Dispose(POperationPanelItem(p));
  FOperations.Clear;
  FQueues.Clear;
end;

procedure TOperationsPanel.DeleteItem(List: TFPList; Index: Integer);
begin
  Dispose(POperationPanelItem(List[Index]));
  List.Delete(Index);
end;

procedure TOperationsPanel.GetStateColor(State: TFileSourceOperationState; ID: TFileSourceOperationType;
                                         out ColorFrom, ColorTo: TColor);
begin
    case ID of
      fsoCopy, fsoCopyIn, fsoCopyOut:  // copy
        begin  ColorTo  := RGB( 42, 93,153);
               ColorFrom:= RGB(  0, 31, 51);  end;
      fsoMove:             // Move/rename
        begin  ColorTo  := RGB( 22,143,143);
               ColorFrom:= RGB(  0, 15, 31);  end;
      fsoDelete, fsoWipe:  // del
        begin  ColorTo  := RGB(152,103, 13);
               ColorFrom:= RGB( 36,  8,  0);  end;
      fsoCalcChecksum, fsoCalcStatistics:  // Ctrl-Q
        begin  ColorTo  := RGB( 72, 52,102);
               ColorFrom:= RGB( 18, 18,  0);  end;
      else
        begin  ColorTo  := RGB( 52, 53,143);
               ColorFrom:= RGB(  8,  8, 42);  end;
      end;
end;

procedure TOperationsPanel.OperationsManagerEvent(Item: TOperationsManagerItem; Event: TOperationManagerEvent);
begin
  UpdateItems;
  UpdateView;
  if Event = omevOperationAdded then
    Item.Operation.AddUserInterface(FUserInterface)
  else if Event = omevOperationRemoved then begin
    Item.Operation.RemoveUserInterface(FUserInterface);
  end;
end;

procedure TOperationsPanel.ProgressWindowEvent(OperationHandle: TOperationHandle; Event: TOperationProgressWindowEvent);
begin
  UpdateVisibility;
end;

procedure TOperationsPanel.UpdateItems;
var
  OpManItem: TOperationsManagerItem;
  QueueIndex, OperIndex: Integer;
  OutString: String;
  ItemRect: TRect;
  Queue: TOperationsManagerQueue;
  OperationItem: POperationPanelItem;
  OverallHeight: Integer = MinimumHeight;
  OverallWidth: Integer = 0;
  Visibility: Boolean = False;

  procedure SetSize;
  begin
    ItemRect := Rect(0, 0, 0, 0);
    DrawText(Canvas.Handle, PChar(OutString), Length(OutString), ItemRect,
      DT_NOPREFIX or DT_CALCRECT);

    OperationItem^.Width  := FMaximumItemWidth;  // const
      //Min(ItemRect.Right  + (LeftRightTextMargin + PanelBorderWidth) * 2, FMaximumItemWidth);
    OverallHeight :=
      Max(ItemRect.Bottom + (TopBottomTextMargin + PanelBorderWidth) * 2, OverallHeight);
    OverallWidth := OverallWidth + OperationItem^.Width + HorizontalSpaceBetween;
  end;
begin
  ClearItems;
  for QueueIndex := 0 to OperationsManager.QueuesCount - 1 do
  begin
    Queue := OperationsManager.QueueByIndex[QueueIndex];
    if Queue.Count > 0 then
    begin
      if Queue.Identifier = FreeOperationsQueueId then
      begin
        for OperIndex := 0 to Queue.Count - 1 do
        begin
          OpManItem := Queue.Items[OperIndex];
          if Assigned(OpManItem) then
          begin
            New(OperationItem);
            FOperations.Add(OperationItem);
            OperationItem^.QueueId := Queue.Identifier;
            OperationItem^.OperationHandle := OpManItem.Handle;

            OutString := //IntToStr(OpManItem.Handle) + ': ' +
              OpManItem.Operation.GetDescription(fsoddJob) + ' - ' + GetProgressString(100);
            SetSize;

            if not TfrmFileOp.IsOpenedFor(OpManItem.Handle) and
               not (OpManItem.Operation.State in [fsosStopping, fsosStopped]) then
              Visibility := True;
          end;
        end;
      end
      else
      begin
        New(OperationItem);
        FQueues.Add(OperationItem);
        OperationItem^.QueueId := Queue.Identifier;
        OperationItem^.OperationHandle := InvalidOperationHandle;

        OutString := Queue.GetDescription(True) + LineEnding +
                     Queue.Items[0].Operation.GetDescription(fsoddJob) + ' - ' +
                     GetProgressString(100);
        SetSize;

        if not TfrmFileOp.IsOpenedFor(Queue.Identifier) then
          Visibility := True;
      end;
    end;
  end;

  ClientHeight := OverallHeight + 2;
  ClientWidth := Max(OverallWidth - HorizontalSpaceBetween, FParentWidth);
  Visible := Visibility;
end;

procedure TOperationsPanel.UpdateVisibility;
var
  OpManItem: TOperationsManagerItem;
  QueueIndex, OperIndex: Integer;
  Queue: TOperationsManagerQueue;
  Visibility: Boolean = False;
begin
  for QueueIndex := 0 to OperationsManager.QueuesCount - 1 do
  begin
    Queue := OperationsManager.QueueByIndex[QueueIndex];
    if Queue.Count > 0 then
    begin
      if Queue.Identifier = FreeOperationsQueueId then
      begin
        for OperIndex := 0 to Queue.Count - 1 do
        begin
          OpManItem := Queue.Items[OperIndex];
          if Assigned(OpManItem) then
          begin
            if not TfrmFileOp.IsOpenedFor(OpManItem.Handle) and
               not (OpManItem.Operation.State in [fsosStopping, fsosStopped]) then
              Visibility := True;
          end;
        end;
      end
      else
      begin
        if not TfrmFileOp.IsOpenedFor(Queue.Identifier) then
          Visibility := True;
      end;
    end;
  end;
  Visible := Visibility;
end;

{$if lcl_fullversion >= 1070000}
procedure TOperationsPanel.SetParent(NewParent: TWinControl);
var
  AForm: TCustomForm;
begin
  inherited SetParent(NewParent);
  AForm := GetParentForm(NewParent);
  if Assigned(AForm) then begin
    FMaximumItemWidth := ScaleX(MaximumItemWidth, AForm.DesignTimePPI);
  end;
end;
{$endif}

constructor TOperationsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperations := TFPList.Create;
  FQueues := TFPList.Create;
  FMaximumItemWidth := MaximumItemWidth;
  FUserInterface := TFileSourceOperationMessageBoxesUI.Create;

  OperationsManager.AddEventsListener(
    [omevOperationAdded, omevOperationRemoved, omevOperationMoved],
    @OperationsManagerEvent);
  TfrmFileOp.AddEventsListener([opwevOpened, opwevClosed],
    @ProgressWindowEvent);
end;

destructor TOperationsPanel.Destroy;
begin
  OperationsManager.RemoveEventsListener(
    [omevOperationAdded, omevOperationRemoved, omevOperationMoved],
    @OperationsManagerEvent);
  TfrmFileOp.RemoveEventsListener([opwevOpened, opwevClosed],
    @ProgressWindowEvent);

  inherited Destroy;
  FUserInterface.Free;
  FOperations.Free;
  FQueues.Free;
end;

procedure TOperationsPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickPos: TPoint;
  OpManItem: TOperationsManagerItem;

  procedure HandleItem(Item: POperationPanelItem);
  var
    Queue: TOperationsManagerQueue;
  begin
    Queue := OperationsManager.QueueByIdentifier[Item^.QueueId];
    if Assigned(Queue) and (Queue.Count > 0) then
    begin
      if Item^.OperationHandle = InvalidOperationHandle then
      begin
        case Button of
          mbLeft:
            TfrmFileOp.ShowFor(Item^.QueueId, [opwoIfExistsBringToFront]);
          mbMiddle:
            Queue.TogglePause;
          mbRight:
            ShowOperationsViewer(Item^.QueueId);
        end;
      end
      else
      begin
        OpManItem := Queue.ItemByHandle[Item^.OperationHandle];
        if Assigned(OpManItem) then
        begin
          case Button of
            mbLeft:
              TfrmFileOp.ShowFor(OpManItem.Handle, [opwoIfExistsBringToFront]);
            mbMiddle:
              OpManItem.Operation.TogglePause;
            mbRight:
              ShowOperationsViewer(OpManItem.Handle);
          end;
        end;
      end;
    end;
  end;
var
  ItemRect: TRect;
  Item: POperationPanelItem;
begin
  inherited MouseDown(Button, Shift, X, Y);

  ClickPos := Point(X, Y);

  ItemRect := ClientRect;
  InflateRect(ItemRect, -PanelBorderWidth, -PanelBorderWidth);
  ItemRect.Right := ItemRect.Left - HorizontalSpaceBetween;

  for Item in FQueues do
  begin
    ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
    ItemRect.Right := ItemRect.Left + Item^.Width;
    if PtInRect(ItemRect, ClickPos) then
    begin
      HandleItem(Item);
      Exit;
    end;
  end;

  for Item in FOperations do
  begin
    ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
    ItemRect.Right := ItemRect.Left + Item^.Width;
    if PtInRect(ItemRect, ClickPos) then
    begin
      HandleItem(Item);
      Exit;
    end;
  end;
end;


procedure TOperationsPanel.Paint;
var
  OpManItem: TOperationsManagerItem;
  ARect, ItemRect: TRect;
  ColorFrom, ColorTo: TColor;
  Queue: TOperationsManagerQueue;
  Item: POperationPanelItem;
  i: Integer;
  AProgress: Double;

  function StrTime(dt: TDateTime): string;
  Var
    H,M,S,MS : Word;
  begin
    DecodeTime(dt,H,M,S,MS);
    if H > 0 then
      Result := Format('%d:%2d:%2d h',[h,m,s])
    else if M > 0 then
      Result := Format('%d:%2d m',[m,s])
    else
      Result := Format('%d s',[s]);
  end;

  function GetSpeedText(OpManItem: TOperationsManagerItem) :string;
  var
    CopyOperation: TFileSourceCopyOperation;  CopyStat: TFileSourceCopyOperationStatistics;
    MoveOperation: TFileSourceMoveOperation;  MoveStat: TFileSourceMoveOperationStatistics;
    DeleteOperation: TFileSourceDeleteOperation;  DeleteStat: TFileSourceDeleteOperationStatistics;
  begin
    case OpManItem.Operation.ID of
      fsoCopy, fsoCopyIn, fsoCopyOut:
        begin
          CopyOperation := OpManItem.Operation as TFileSourceCopyOperation;
          CopyStat := CopyOperation.RetrieveStatistics;

          Result := cnvFormatFileSize(CopyStat.BytesPerSecond, uoscOperation) + '/s  ' +
                    StrTime(CopyStat.RemainingTime);
        end;
      fsoMove:
        begin
          MoveOperation := OpManItem.Operation as TFileSourceMoveOperation;
          MoveStat := MoveOperation.RetrieveStatistics;

          Result := cnvFormatFileSize(MoveStat.BytesPerSecond, uoscOperation) + '/s  ' +
                    StrTime(MoveStat.RemainingTime);
        end;
      fsoDelete:
        begin
          DeleteOperation := OpManItem.Operation as TFileSourceDeleteOperation;
          DeleteStat := DeleteOperation.RetrieveStatistics;

          Result := IntToStr(DeleteStat.FilesPerSecond) + '/s  ' +
                    StrTime(DeleteStat.RemainingTime);
        end;
      //fsoCalcChecksum:
        //InitializeCalcChecksumOperation(OpManItem);
      else
        Result := '';
    end;
  end;

  procedure DrawString(s: String);
  begin
    // Draw output string
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color:= clWhite;
    Canvas.Font.Color:= clWhite;
    ARect := ItemRect;
    InflateRect(ARect, -4, -2);
    DrawText(Canvas.Handle, PChar(s), Length(s), ARect, DT_LEFT or DT_VCENTER or DT_NOPREFIX);
  end;

  procedure DrawProgress(State: TFileSourceOperationState; ID: TFileSourceOperationType; Progress: Double);
  var
    x: integer;
  begin
    // Draw background
    GetStateColor(State, ID, ColorFrom, ColorTo);
    ARect := ItemRect;
    InflateRect(ARect, -1, -1);
    Canvas.GradientFill(ARect, ColorFrom, ColorTo, gdVertical);

    // Draw progress line
    x := ARect.Left + Round(ARect.Width * Min(1.0, Progress));
    Canvas.GradientFill(Rect(x-1, ARect.Top, x+1, ARect.Bottom), $008080, $F0FFFF, gdHorizontal);

    // Special indication
    if State in [fsosWaitingForFeedback] then
    begin
      ARect.Bottom := ARect.Top + 1;
      Canvas.Pen.Color := $30C0FF;
      Canvas.Rectangle(ARect);  // line top
    end else
    if State in [fsosPaused, fsosStopped] then
    begin
      Canvas.Pen.Color := clWhite;
      ARect.Top := ARect.Bottom - 1;
      Canvas.Rectangle(ARect);  // line bottom
    end;
  end;

begin
  inherited Paint;

  ItemRect := ClientRect;

  Canvas.Pen.Color:= cl3DDkShadow;
  Canvas.Rectangle(ItemRect);

  InflateRect(ItemRect, -PanelBorderWidth, -PanelBorderWidth);
  Canvas.GradientFill(ItemRect, $100808, $302020, gdVertical);

  ItemRect.Right := ItemRect.Left - HorizontalSpaceBetween;

  i := 0;
  while i < FQueues.Count do
  begin
    Item := FQueues[i];
    Queue := OperationsManager.QueueByIdentifier[Item^.QueueId];
    if Assigned(Queue) and (Queue.Count > 0) then
    begin
      OpManItem := Queue.Items[0];
      if Assigned(OpManItem) then
      begin
        ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
        ItemRect.Right := ItemRect.Left + Item^.Width;

        // Draw border
        Canvas.Pen.Color := LightColor(cl3DDkShadow, 25);
        Canvas.Pen.Style := psSolid;
        Canvas.Rectangle(ItemRect);

        AProgress := OpManItem.Operation.Progress;
        DrawProgress(OpManItem.Operation.State, OpManItem.Operation.ID, AProgress);
        DrawString(Queue.GetDescription(True) + LineEnding +
                   OpManItem.Operation.GetDescription(fsoddJob) + ' - ' +
                   GetProgressString(AProgress));
        Inc(i);
      end
      else
        DeleteItem(FQueues, i);
    end
    else
      DeleteItem(FQueues, i);
  end;

  i := 0;
  while i < FOperations.Count do
  begin
    Item := FOperations[i];
    Queue := OperationsManager.QueueByIdentifier[Item^.QueueId];
    if Assigned(Queue) and (Queue.Count > 0) then
    begin
      OpManItem := Queue.ItemByHandle[Item^.OperationHandle];
      if Assigned(OpManItem) then
      begin
        ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
        ItemRect.Right := ItemRect.Left + Item^.Width;

        if TfrmFileOp.IsOpenedFor(OpManItem.Handle) then
          Canvas.Pen.Color := clMenuHighlight
        else
          Canvas.Pen.Color := LightColor(cl3DDkShadow, 40);

        // Draw border
        Canvas.Pen.Style := psSolid;
        Canvas.Rectangle(ItemRect);

        AProgress := OpManItem.Operation.Progress;
        DrawProgress(OpManItem.Operation.State, OpManItem.Operation.ID, AProgress);

        DrawString(//IntToStr(OpManItem.Handle) + ': ' +
                   OpManItem.Operation.GetDescription(fsoddJob) + ' ' +
                   GetProgressString(AProgress) + '  ' + GetSpeedText(OpManItem));
        Inc(i);
      end
      else
        DeleteItem(FOperations, i);
    end
    else
      DeleteItem(FOperations, i);
  end;
end;

procedure TOperationsPanel.UpdateView;
begin
  Invalidate;
end;

end.

