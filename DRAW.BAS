OPTION BASE 0
DEFINT A-Z

TYPE MazeAreasStr
 MonstersStartX1 AS INTEGER
 MonstersStartY1 AS INTEGER
 MonstersStartX2 AS INTEGER
 MonstersStartY2 AS INTEGER
 PlayerStartX AS INTEGER
 PlayerStartY AS INTEGER
END TYPE

DECLARE FUNCTION BaseFileName$ (File$)
DECLARE FUNCTION Choose$ (x, y, Text$, Choices$)
DECLARE FUNCTION FileExists (File$)
DECLARE FUNCTION GetMazeCount ()
DECLARE FUNCTION GetInput$ (x, y, Prompt$, MaxLength, Default$)
DECLARE FUNCTION ReservedArea$ (x%, y%)
DECLARE FUNCTION SaveMaze$ (MazeFile$, MazeName$, MazeCount, Maze$(), MazeAreas AS MazeAreasStr)
DECLARE FUNCTION SelectMaze$ (MazeFile$, MazeCount, Maze$(), MazeAreas AS MazeAreasStr)
DECLARE FUNCTION Trim$ (Text$)
DECLARE SUB AddMazeToList (MazeFile$, MazeName$, MazeCount)
DECLARE SUB Center (x, Text$)
DECLARE SUB DeleteMaze (SelectedMaze, MazeFileList$())
DECLARE SUB DisplayHelp (TopRow, HelpText$())
DECLARE SUB DisplayMaze (Maze$())
DECLARE SUB DisplayMazeList (MazeNameList$(), SelectedMaze, TopMaze)
DECLARE SUB DisplayNoDraw (GameCharacter$)
DECLARE SUB DisplayStartingAreas (MazeAreas AS MazeAreasStr)
DECLARE SUB DisplayStatus (CursorX, CursorY, EditMode$, MazeName$)
DECLARE SUB DrawBox (x, y, w, h, Shadow)
DECLARE SUB Initialize ()
DECLARE SUB LoadHelp (HelpFile$)
DECLARE SUB LoadMaze (Maze$(), MazeFile$, MazeAreas AS MazeAreasStr)
DECLARE SUB Main ()
DECLARE SUB Quit ()

ON ERROR GOTO ErrorTrap
DIM SHARED ErrorOccurred
DIM SHARED FileName$

Initialize
CALL Main

ErrorTrap:
DIM Choice$
DIM ErrorCode

ErrorOccurred = -1
IF NOT FileName$ = "" THEN MID$(FileName$, 1, 1) = UCASE$(LEFT$(FileName$, 1))
PCOPY 0, 3
DO
 COLOR 12, 4
 DrawBox 19, 24, 30, 3, 0
 LOCATE 20
 ErrorCode = ERR
 SELECT CASE ErrorCode
  CASE 53
   Center 0, "Cannot find: " + CHR$(34) + FileName$ + CHR$(34) + "."
  CASE 55, 64
   Center 0, "Cannot open:"
   Center 0, CHR$(34) + FileName$ + CHR$(34) + "."
  CASE 61
   Center 0, "Not enough diskspace."
  CASE 62
   Center 0, "Error while reading:"
   Center 0, CHR$(34) + FileName$ + CHR$(34) + "."
  CASE 70
   Center 0, "Disk or file is "
   Center 0, "write protected."
  CASE 71
   Center 0, "Disk is not in drive."
  CASE 72
   Center 0, "The disk is damaged."
  CASE ELSE
   Center 0, "Unexpected error."
 END SELECT
 Center 0, "Error code:" + STR$(ErrorCode)
 COLOR 0, 7: Center 24, " 1 = Retry  2 = Ignore  3 = Quit "
 DO
  Choice$ = INKEY$
 LOOP WHILE Choice$ = ""
 PCOPY 3, 0
 SELECT CASE Choice$
  CASE "1": RESUME
  CASE "2": RESUME NEXT
  CASE "3": Quit
 END SELECT
LOOP

SUB AddMazeToList (MazeFile$, MazeName$, MazeCount)
DIM FileH
DIM Key$

 IF MazeCount = 250 THEN
  PCOPY 0, 1
  COLOR 0, 7
  DrawBox 19, 19, 40, 2, 1
  Center 20, "No more mazes can be added."
  Center 0, "A maximum of 250 mazes can be stored."
  Key$ = INPUT$(1)
  PCOPY 1, 0
 ELSE
  MazeCount = MazeCount + 1
  FileH = FREEFILE
  FileName$ = "Mazes.lst"
  OPEN "Mazes.lst" FOR APPEND LOCK READ WRITE AS FileH
  IF LOF(FileH) = 0 THEN PRINT #FileH, CHR$(0);
 
   PRINT #FileH, CHR$(LEN(MazeFile$)); MazeFile$;
   PRINT #FileH, CHR$(LEN(MazeName$)); MazeName$;
 
   SEEK #FileH, 1
   PRINT #FileH, CHR$(MazeCount);
  CLOSE FileH
 END IF
END SUB

FUNCTION BaseFileName$ (File$)
DIM BaseName$
DIM Position

 BaseName$ = File$
 Position = INSTR(BaseName$, ".")
 IF Position > 0 THEN BaseName$ = LEFT$(BaseName$, Position - 1)
 BaseFileName$ = BaseName$
END FUNCTION

SUB Center (x, Text$)
 IF x > 0 THEN LOCATE x
 LOCATE , INT(40 - (LEN(Text$) / 2))
 PRINT Text$
END SUB

FUNCTION Choose$ (x, y, Text$, Choices$)
DIM Choice$

 LOCATE x, y: PRINT Text$
 DO
  Choice$ = UCASE$(INPUT$(1))
  IF INSTR(Choices$, Choice$) THEN EXIT DO
 LOOP
 LOCATE x, y: PRINT SPACE$(LEN(Text$))

 Choose$ = Choice$
END FUNCTION

SUB DeleteMaze (SelectedMaze, MazeFileList$())
DIM FileH1
DIM FileH2
DIM Length
DIM MazeCount
DIM MazeFile$
DIM MazeIndex
DIM MazeName$

 ErrorOccurred = 0
 FileName$ = MazeFileList$(SelectedMaze) + ".maz"
 KILL MazeFileList$(SelectedMaze) + ".maz"

 IF NOT ErrorOccurred THEN
  FileH1 = FREEFILE
  FileName$ = "Mazes.lst"
  FileH1 = FREEFILE
  OPEN "Mazes.lst" FOR BINARY LOCK READ WRITE AS FileH1
   MazeCount = ASC(INPUT$(1, FileH1))
   IF MazeCount > 0 THEN MazeCount = MazeCount - 1
   FileH2 = FREEFILE
   OPEN "Mazes.tmp" FOR OUTPUT LOCK READ WRITE AS FileH2
    PRINT #FileH2, CHR$(MazeCount);
    FOR MazeIndex = LBOUND(MazeFileList$) TO MazeCount - 1
     IF MazeIndex = UBOUND(MazeFileList$) OR LOC(FileH1) = LOF(FileH1) THEN
      EXIT FOR
     ELSE
      Length = ASC(INPUT$(1, FileH1))
      MazeFile$ = INPUT$(Length, FileH1)
      Length = ASC(INPUT$(1, FileH1))
      MazeName$ = INPUT$(Length, FileH1)
      IF NOT MazeIndex = SelectedMaze THEN
       PRINT #FileH2, CHR$(LEN(MazeFile$)); MazeFile$;
       PRINT #FileH2, CHR$(LEN(MazeName$)); MazeName$;
      END IF
     END IF
    NEXT MazeIndex
   CLOSE FileH2
  CLOSE FileH1
  KILL "Mazes.lst"
  NAME "Mazes.tmp" AS "Mazes.lst"
 END IF
END SUB

SUB DisplayHelp (TopRow, HelpText$())
DIM HelpLine
DIM Row

 DrawBox 10, 20, 38, 20, 1
 Center 10, "´HelpÃ"
 Row = 11
 FOR HelpLine = TopRow TO TopRow + 19
  LOCATE Row, 21
  PRINT HelpText$(HelpLine)
  Row = Row + 1
 NEXT HelpLine
END SUB

SUB DisplayMaze (Maze$())
DIM Column
DIM Row

 COLOR 2, 0
 
 FOR Row = LBOUND(Maze$, 1) TO UBOUND(Maze$, 1)
  FOR Column = LBOUND(Maze$, 2) TO UBOUND(Maze$, 2)
   LOCATE Row + 2, Column + 1
   IF Maze$(Row, Column) = "" THEN
    PRINT " ";
   ELSE
    PRINT Maze$(Row, Column);
   END IF
  NEXT Column
 NEXT Row
END SUB

SUB DisplayMazeList (MazeNameList$(), SelectedMaze, TopMaze)
DIM MazeIndex
DIM Row

 Row = 11
 FOR MazeIndex = TopMaze TO TopMaze + 29
  IF MazeIndex > UBOUND(MazeNameList$) THEN EXIT FOR
  IF MazeIndex = SelectedMaze THEN COLOR 0, 2 ELSE COLOR 0, 7
  LOCATE Row, 27
  PRINT MazeNameList$(MazeIndex); SPACE$(26 - LEN(MazeNameList$(MazeIndex)))
  Row = Row + 1
 NEXT MazeIndex
END SUB

SUB DisplayStartingAreas (MazeAreas AS MazeAreasStr)
DIM Row
DIM x2
DIM y2

 LOCATE MazeAreas.PlayerStartX + 2, MazeAreas.PlayerStartY + 1
 COLOR 15, 0
 PRINT "S";

 IF MazeAreas.MonstersStartX1 >= 0 AND MazeAreas.MonstersStartY1 >= 0 THEN
  COLOR 12, 0
  IF MazeAreas.MonstersStartX2 >= 0 THEN
   x2 = MazeAreas.MonstersStartX2
  ELSE
   x2 = MazeAreas.MonstersStartX1
  END IF
  IF MazeAreas.MonstersStartY2 >= 0 THEN
   y2 = MazeAreas.MonstersStartY2
  ELSE
   y2 = MazeAreas.MonstersStartY1
  END IF
  FOR Row = MazeAreas.MonstersStartX1 TO x2
   LOCATE Row + 2, MazeAreas.MonstersStartY1 + 1
   PRINT STRING$((y2 - MazeAreas.MonstersStartY1) + 1, "M");
  NEXT Row
 END IF
END SUB

SUB DisplayStatus (CursorX, CursorY, EditMode$, MazeName$)
 COLOR 15, 1
 LOCATE 1, 1
 PRINT " Draw maze for Monsters, By: Peter Swinkels, ***1998***, version 1.01" + SPACE$(11)
 LOCATE 48, 1
 PRINT USING " F1 = Help  X: ## Y: ## "; CursorX; CursorY;
 PRINT USING " Edit mode: \" + SPACE$(8) + "\ Name: \" + SPACE$(24) + "\ "; EditMode$; MazeName$
END SUB

SUB DrawBox (x, y, w, h, Shadow)
DIM BoxX

 LOCATE x, y: PRINT "Ú"; STRING$(w, "Ä"); "¿"
 FOR BoxX = 1 TO h
  LOCATE , y: PRINT "³"; SPACE$(w); "³";
   IF Shadow THEN PRINT "°" ELSE PRINT
 NEXT BoxX
 LOCATE , y: PRINT "À"; STRING$(w, "Ä"); "Ù";
 IF Shadow THEN PRINT "°": LOCATE , y + 1: PRINT STRING$(w + 2, "°") ELSE PRINT
END SUB

FUNCTION FileExists (File$)
DIM Exists
DIM FileH

 Exists = 0
 FileH = FREEFILE
 FileName$ = File$
 OPEN File$ FOR BINARY LOCK READ WRITE AS FileH
  Exists = (LOF(FileH) > 0)
 CLOSE FileH
 IF NOT Exists THEN KILL File$

FileExists = Exists
END FUNCTION

FUNCTION GetInput$ (x, y, Prompt$, MaxLength, Default$)
DIM Key$
DIM Length
DIM Text$

 Text$ = Default$

 DO
  COLOR 7, 0
  LOCATE x, y: PRINT Prompt$; Text$; : COLOR 23: PRINT "_ "
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  Length = LEN(Text$)
  SELECT CASE ASC(Key$)
   CASE 8
    IF Length > 0 THEN Text$ = LEFT$(Text$, Length - 1)
   CASE 13
    EXIT DO
   CASE 27
    Text$ = "": EXIT DO
   CASE IS > 31
    IF Length < MaxLength THEN Text$ = Text$ + Key$
  END SELECT
 LOOP
 COLOR 7
 LOCATE x, y: PRINT SPACE$(LEN(Prompt$) + Length + 2)

 GetInput$ = Text$
END FUNCTION

FUNCTION GetMazeCount
DIM FileH
DIM MazeCount

 MazeCount = 0
 FileName$ = "Mazes.lst"
 IF FileExists("Mazes.lst") THEN
   FileH = FREEFILE
   OPEN "Mazes.lst" FOR BINARY LOCK READ WRITE AS FileH
    MazeCount = ASC(INPUT$(1, FileH))
   CLOSE FileH
 END IF

 GetMazeCount = MazeCount
END FUNCTION

SUB Initialize
 SCREEN 0
 WIDTH 80, 50
 COLOR 7, 0
 CLS
END SUB

SUB LoadHelp (HelpFile$)
DIM Character$
DIM FileH
DIM Formatting$
DIM HelpLine
DIM HelpText$(0 TO 99)
DIM Key$
DIM LastLine
DIM TopRow

 PCOPY 0, 1
 LastLine = 0
 TopRow = 0
 IF FileExists(HelpFile$) THEN
  FileH = FREEFILE
  FileName$ = HelpFile$
  OPEN HelpFile$ FOR BINARY LOCK READ WRITE AS FileH
   HelpLine = 0
   DO UNTIL LOC(FileH) = LOF(FileH)
    Formatting$ = INPUT$(1, FileH)
    DO
     Character$ = INPUT$(1, FileH)
     IF Character$ = CHR$(10) OR LOC(FileH) = LOF(FileH) THEN EXIT DO
     HelpText$(HelpLine) = HelpText$(HelpLine) + Character$
    LOOP
    SELECT CASE Formatting$
     CASE "-"
      HelpText$(HelpLine) = STRING$(38, 196)
     CASE "="
      HelpText$(HelpLine) = STRING$(38, 205)
     CASE ">"
      HelpText$(HelpLine) = SPACE$(19 - (LEN(HelpText$(HelpLine)) \ 2)) + HelpText$(HelpLine)
    END SELECT
    HelpLine = HelpLine + 1
   LOOP
  CLOSE FileH
  LastLine = HelpLine
 END IF

 COLOR 0, 7
 DO
  DisplayHelp TopRow, HelpText$()
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  SELECT CASE Key$
   CASE CHR$(0) + "H"
    IF TopRow > LBOUND(HelpText$) THEN TopRow = TopRow - 1
   CASE CHR$(0) + "P"
    IF TopRow < LastLine THEN TopRow = TopRow + 1
   CASE CHR$(27)
    PCOPY 1, 0: EXIT DO
  END SELECT
 LOOP
END SUB

SUB LoadMaze (Maze$(), MazeFile$, MazeAreas AS MazeAreasStr)
DIM Column
DIM FileH
DIM MazeRow$
DIM Row

 FileH = FREEFILE
 FileName$ = MazeFile$
 OPEN MazeFile$ + ".maz" FOR BINARY LOCK READ WRITE AS FileH
  MazeAreas.PlayerStartX = ASC(INPUT$(1, FileH))
  MazeAreas.PlayerStartY = ASC(INPUT$(1, FileH))
  MazeAreas.MonstersStartX1 = ASC(INPUT$(1, FileH))
  MazeAreas.MonstersStartY1 = ASC(INPUT$(1, FileH))
  MazeAreas.MonstersStartX2 = ASC(INPUT$(1, FileH))
  MazeAreas.MonstersStartY2 = ASC(INPUT$(1, FileH))
  FOR Row = LBOUND(Maze$, 1) TO UBOUND(Maze$, 1)
   MazeRow$ = INPUT$(80, FileH)
   FOR Column = LBOUND(Maze$, 2) TO UBOUND(Maze$, 2)
    Maze$(Row, Column) = MID$(MazeRow$, Column + 1, 1)
   NEXT Column
  NEXT Row
 CLOSE FileH
END SUB

SUB Main
DIM BlockAuto
DIM CursorX
DIM CursorY
DIM EditMode$
DIM Key$
DIM Maze$(0 TO 45, 0 TO 79)
DIM MazeAreas AS MazeAreasStr
DIM MazeCount
DIM MazeFile$
DIM MazeName$
DIM NewMazeFile$
DIM NewMazeName$

 BlockAuto = 0
 CursorX = 0
 CursorY = 0
 EditMode$ = "default"
 MazeAreas.MonstersStartX1 = 20
 MazeAreas.MonstersStartY1 = 36
 MazeAreas.MonstersStartX2 = 25
 MazeAreas.MonstersStartY2 = 43
 MazeAreas.PlayerStartX = 1
 MazeAreas.PlayerStartY = 1
 MazeCount = GetMazeCount
 MazeFile$ = ""
 MazeName$ = ""

 DisplayMaze Maze$()
 
 DO
  DisplayStartingAreas MazeAreas
  DisplayStatus CursorX, CursorY, EditMode$, MazeName$

  IF Maze$(CursorX, CursorY) = "Û" THEN
   COLOR 23, 2
  ELSE
   COLOR 23, 0
  END IF
  LOCATE CursorX + 2, CursorY + 1
  PRINT "Û";

  DO
   Key$ = UCASE$(INKEY$)
  LOOP WHILE Key$ = ""

  COLOR 2, 0
  LOCATE CursorX + 2, CursorY + 1

  IF Maze$(CursorX, CursorY) = "Û" THEN
   PRINT "Û"
  ELSE
   PRINT " ";
  END IF
 
  SELECT CASE Key$
   CASE CHR$(0) + "H", CHR$(0) + "P", CHR$(0) + "K", CHR$(0) + "M"
    IF BlockAuto THEN
     BlockAuto = 0
    ELSE
     IF EditMode$ = "autodraw" THEN
      Maze$(CursorX, CursorY) = "Û"
     ELSEIF EditMode$ = "autodelete" THEN
      Maze$(CursorX, CursorY) = " "
     END IF
     COLOR 2, 0
     LOCATE CursorX + 2, CursorY + 1
     PRINT Maze$(CursorX, CursorY);
    END IF
  END SELECT

  SELECT CASE Key$
   CASE CHR$(0) + "H"
    IF CursorX = LBOUND(Maze$, 1) THEN
     CursorX = UBOUND(Maze$, 1)
    ELSE
     CursorX = CursorX - 1
    END IF
   CASE CHR$(0) + "P"
    IF CursorX = UBOUND(Maze$, 1) THEN
     CursorX = LBOUND(Maze$, 1)
    ELSE
     CursorX = CursorX + 1
    END IF
   CASE CHR$(0) + "K"
    IF CursorY = LBOUND(Maze$, 2) THEN
     CursorY = UBOUND(Maze$, 2)
    ELSE
     CursorY = CursorY - 1
    END IF
   CASE CHR$(0) + "M"
    IF CursorY = UBOUND(Maze$, 2) THEN
     CursorY = LBOUND(Maze$, 2)
    ELSE
     CursorY = CursorY + 1
    END IF
   CASE CHR$(8)
    IF EditMode$ = "autodelete" THEN
     EditMode$ = "default"
    ELSE
     EditMode$ = "autodelete"
    END IF
   CASE CHR$(9)
    IF EditMode$ = "autodraw" THEN
     EditMode$ = "default"
    ELSE
     EditMode$ = "autodraw"
    END IF
   CASE CHR$(13)
    BlockAuto = -1
    Maze$(CursorX, CursorY) = "Û"
   CASE CHR$(14)
    COLOR 7, 0
    IF Choose$(48, 1, " Do you want to start drawing a new maze y/n? ", "YN") = "Y" THEN RUN "Draw"
   CASE CHR$(27)
    COLOR 7, 0: LOCATE 48, 1: PRINT SPACE$(80)
    IF Choose$(48, 3, " Do you want to quit y/n? ", "YN") = "Y" THEN Quit
   CASE CHR$(0) + ";"
    LoadHelp "Draw.hlp"
   CASE CHR$(0) + "S"
    BlockAuto = -1
    Maze$(CursorX, CursorY) = " "
   CASE CHR$(0) + "<"
    MazeName$ = SaveMaze$(MazeFile$, MazeName$, MazeCount, Maze$(), MazeAreas)
   CASE CHR$(0) + "="
    NewMazeName$ = SelectMaze$(NewMazeFile$, MazeCount, Maze$(), MazeAreas)
    IF NOT NewMazeName$ = "" THEN
     BlockAuto = 0
     CursorX = 0
     CursorY = 0
     EditMode$ = "default"
     MazeAreas.PlayerStartX = 1
     MazeAreas.PlayerStartY = 1
     MazeFile$ = NewMazeFile$
     MazeName$ = NewMazeName$
    END IF
   CASE "M"
    IF MazeAreas.MonstersStartX1 >= 0 AND MazeAreas.MonstersStartY1 >= 0 AND MazeAreas.MonstersStartX2 >= 0 AND MazeAreas.MonstersStartY2 >= 0 THEN
     MazeAreas.MonstersStartX1 = -1
     MazeAreas.MonstersStartY1 = -1
     MazeAreas.MonstersStartX2 = -1
     MazeAreas.MonstersStartY2 = -1
    ELSE
     IF MazeAreas.MonstersStartX1 >= 0 AND MazeAreas.MonstersStartY1 >= 0 THEN
      MazeAreas.MonstersStartX2 = CursorX
      MazeAreas.MonstersStartY2 = CursorY

      IF MazeAreas.MonstersStartX2 < MazeAreas.MonstersStartX1 THEN
       SWAP MazeAreas.MonstersStartX1, MazeAreas.MonstersStartX2
      END IF
      IF MazeAreas.MonstersStartY2 < MazeAreas.MonstersStartY1 THEN
       SWAP MazeAreas.MonstersStartY1, MazeAreas.MonstersStartY2
      END IF
     ELSE
      MazeAreas.MonstersStartX1 = CursorX
      MazeAreas.MonstersStartY1 = CursorY
     END IF
    END IF
    
    DisplayMaze Maze$()
    DisplayStartingAreas MazeAreas
   CASE "P"
    MazeAreas.PlayerStartX = CursorX
    MazeAreas.PlayerStartY = CursorY

    DisplayMaze Maze$()
    DisplayStartingAreas MazeAreas
  END SELECT
 LOOP
END SUB

SUB Quit
 WIDTH 80, 25
 COLOR 7, 0
 CLS
 SYSTEM
END SUB

FUNCTION SaveMaze$ (MazeFile$, MazeName$, MazeCount, Maze$(), MazeAreas AS MazeAreasStr)
DIM Column
DIM FileH
DIM Key$
DIM NewMazeFile$
DIM NewMazeName$
DIM Row

 COLOR 15, 0: LOCATE 48, 1: PRINT SPACE$(80)

 NewMazeFile$ = BaseFileName$(MazeFile$)
 NewMazeFile$ = Trim$(GetInput$(48, 3, " Enter file name for maze: ", 8, NewMazeFile$))
 IF NewMazeFile$ = "" THEN EXIT FUNCTION
 MazeFile$ = NewMazeFile$

 NewMazeName$ = BaseFileName$(MazeFile$)
 NewMazeName$ = Trim$(GetInput$(48, 3, " Enter name for maze: ", 25, NewMazeName$))
 IF NewMazeName$ = "" THEN EXIT FUNCTION
 MazeName$ = NewMazeName$

 IF FileExists(MazeFile$ + ".maz") THEN
  IF Choose$(48, 3, "Maze already exists, do you want to replace the maze y/n?", "YN") = "N" THEN
   EXIT FUNCTION
  END IF
 ELSE
  AddMazeToList MazeFile$, MazeName$, MazeCount
 END IF

 FileH = FREEFILE
 FileName$ = MazeFile$ + ".maz"
 OPEN MazeFile$ + ".maz" FOR OUTPUT LOCK READ WRITE AS FileH
  PRINT #FileH, CHR$(MazeAreas.PlayerStartX);
  PRINT #FileH, CHR$(MazeAreas.PlayerStartY);
  IF MazeAreas.MonstersStartX1 >= 0 AND MazeAreas.MonstersStartY1 >= 0 AND MazeAreas.MonstersStartX2 >= 0 AND MazeAreas.MonstersStartY2 >= 0 THEN
   PRINT #FileH, CHR$(MazeAreas.MonstersStartX1);
   PRINT #FileH, CHR$(MazeAreas.MonstersStartY1);
   PRINT #FileH, CHR$(MazeAreas.MonstersStartX2);
   PRINT #FileH, CHR$(MazeAreas.MonstersStartY2);
  ELSE
   PRINT #FileH, STRING$(4, CHR$(0));
  END IF

  FOR Row = LBOUND(Maze$, 1) TO UBOUND(Maze$, 1)
   FOR Column = LBOUND(Maze$, 2) TO UBOUND(Maze$, 2)
    IF Maze$(Row, Column) = "" THEN Maze$(Row, Column) = " "
    PRINT #FileH, Maze$(Row, Column);
   NEXT Column
  NEXT Row
 CLOSE FileH

 SaveMaze$ = MazeName$
END FUNCTION

FUNCTION SelectMaze$ (MazeFile$, MazeCount, Maze$(), MazeAreas AS MazeAreasStr)
DIM CursorX
DIM FileH
DIM Key$
DIM Length
DIM MazeFileList$(0 TO 249)
DIM MazeIndex
DIM MazeName$
DIM MazeNameList$(0 TO 249)
DIM SelectedMaze
DIM TopMaze

 MazeName$ = ""
 PCOPY 0, 1
 
 IF MazeCount = 0 THEN
  COLOR 0, 7
  DrawBox 19, 25, 28, 1, 1
  Center 20, "There are no saved mazes."
  Key$ = INPUT$(1)
  PCOPY 1, 0
 ELSE
  CursorX = 1
  SelectedMaze = LBOUND(MazeFileList$)
  TopMaze = SelectedMaze
  COLOR 0, 7
  DrawBox 10, 26, 26, 30, 1
  Center 10, "´Select maze to load.Ã"
  FileH = FREEFILE
  OPEN "Mazes.lst" FOR BINARY LOCK READ WRITE AS FileH
   MazeCount = ASC(INPUT$(1, FileH))
   FOR MazeIndex = LBOUND(MazeFileList$) TO MazeCount - 1
    IF LOC(FileH) = LOF(FileH) THEN EXIT FOR
    Length = ASC(INPUT$(1, FileH))
    MazeFileList$(MazeIndex) = INPUT$(Length, FileH)
    Length = ASC(INPUT$(1, FileH))
    MazeNameList$(MazeIndex) = INPUT$(Length, FileH)
   NEXT MazeIndex
  CLOSE FileH
  DO
   DisplayMazeList MazeNameList$(), SelectedMaze, TopMaze
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
   SELECT CASE Key$
    CASE CHR$(0) + "H"
     IF SelectedMaze > 0 THEN
      SelectedMaze = SelectedMaze - 1
      IF CursorX > 1 THEN
       CursorX = CursorX - 1
      ELSE
       IF TopMaze > LBOUND(MazeFileList$) THEN TopMaze = TopMaze - 1
      END IF
     END IF
    CASE CHR$(0) + "P"
     IF SelectedMaze < UBOUND(MazeFileList$) THEN
      SelectedMaze = SelectedMaze + 1
      IF CursorX < 30 THEN
       CursorX = CursorX + 1
      ELSE
       TopMaze = TopMaze + 1
      END IF
     END IF
    CASE CHR$(13)
     IF NOT MazeFileList$(SelectedMaze) = "" THEN
      MazeFile$ = MazeFileList$(SelectedMaze)
      MazeName$ = MazeNameList$(SelectedMaze)
      PCOPY 1, 0
      LoadMaze Maze$(), MazeFile$, MazeAreas
      DisplayMaze Maze$()
      DisplayStartingAreas MazeAreas
      EXIT DO
     END IF
    CASE CHR$(27)
     PCOPY 1, 0
     EXIT DO
    CASE CHR$(0) + "S"
     PCOPY 0, 2
     COLOR 7, 4
     DrawBox 10, 27, 24, 1, 1
     IF Choose$(11, 29, "Delete this maze y/n?", "YN") = "Y" THEN
      DeleteMaze SelectedMaze, MazeFileList$()
      PCOPY 1, 0
      EXIT DO
     END IF
     PCOPY 2, 0
   END SELECT
  LOOP
 END IF

 SelectMaze$ = MazeName$
END FUNCTION

FUNCTION Trim$ (Text$)
 Trim$ = LTRIM$(RTRIM$(Text$))
END FUNCTION

