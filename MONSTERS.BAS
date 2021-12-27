OPTION BASE 0
DEFINT A-Z

TYPE MazeStr
 Count AS INTEGER
 DotsLeft AS INTEGER
 File AS STRING * 255
 MonstersStartX1 AS INTEGER
 MonstersStartY1 AS INTEGER
 MonstersStartX2 AS INTEGER
 MonstersStartY2 AS INTEGER
 NameV AS STRING * 25
 PlayerStartX AS INTEGER
 PlayerStartY AS INTEGER
 TotalDots AS INTEGER
END TYPE

TYPE MonstersStr
 ColorV AS INTEGER
 Direction AS INTEGER
 x AS INTEGER
 y AS INTEGER
END TYPE

TYPE PlayerStr
 Delay AS SINGLE
 Direction AS INTEGER
 LifeCount AS INTEGER
 NameV AS STRING * 10
 NextDirection AS INTEGER
 x AS INTEGER
 y AS INTEGER
END TYPE

TYPE ScoreStr
 DateTime AS STRING * 19
 Maze AS STRING * 25
 NameV AS STRING * 10
 Score AS STRING * 3
END TYPE

TYPE SettingsStr
 JoystickCenterX AS INTEGER
 JoystickCenterY AS INTEGER
 JoystickOn AS INTEGER
 JoystickSensitivity AS INTEGER
 LifeCount AS INTEGER
 MonsterCount AS INTEGER
 MonsterDelay AS SINGLE
 SoundOn AS INTEGER
END TYPE

DECLARE FUNCTION BaseFileName$ (File$)
DECLARE FUNCTION CanMove (x, y, Direction%, MazeV$())
DECLARE FUNCTION Choose$ (x, y, Text$, Choices$)
DECLARE FUNCTION FileExists (File$)
DECLARE FUNCTION GetInput$ (x, y, Prompt$, MaxLength)
DECLARE FUNCTION GetPercentage (Value, percentage)
DECLARE FUNCTION MonsterPresent (x, y, Direction, Monsters() AS MonstersStr, Settings AS SettingsStr)
DECLARE FUNCTION NewMonsterDirection (Monsters() AS MonstersStr, MazeV$())
DECLARE FUNCTION ProcessNewScore (Maze AS MazeStr, Player AS PlayerStr)
DECLARE SUB Center (x, Text$)
DECLARE SUB DecreasePlayerLife (Maze AS MazeStr, MazeV$(), Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB Delay (Seconds AS SINGLE)
DECLARE SUB DisplayGraphics (y, GraphicsFile$)
DECLARE SUB DisplayHelp (TopRow, HelpText$())
DECLARE SUB DisplayList (MazeNameList$(), SelectedMaze, TopMaze)
DECLARE SUB DisplayMenu (Maze AS MazeStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB DisplayScores (Maze AS MazeStr, HighlightRank)
DECLARE SUB DisplayStatus (Maze AS MazeStr, Player AS PlayerStr)
DECLARE SUB DisplayTitle ()
DECLARE SUB DrawBar (x, ColorV)
DECLARE SUB DrawBox (x, y, w, h, Shadow)
DECLARE SUB Game (Maze AS MazeStr, Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB GameOver (Maze AS MazeStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB GetJoystickStatus (MazeV$(), Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB Initialize (Maze AS MazeStr, Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB InitializeMonsters (Monsters() AS MonstersStr, Maze AS MazeStr)
DECLARE SUB LoadHelp (HelpFile$)
DECLARE SUB LoadMaze (MazeV$(), Maze AS MazeStr, Player AS PlayerStr)
DECLARE SUB MoveMonsters (Naze AS MazeStr, MazeV$(), Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB MovePlayer (MazeV$(), Maze AS MazeStr, Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB Quit ()
DECLARE SUB ReadScores (Scores() AS ScoreStr, ScoreFile$)
DECLARE SUB RedrawMaze (MazeV$(), Maze AS MazeStr, Player AS PlayerStr)
DECLARE SUB SaveSettings (Player AS PlayerStr, Settings AS SettingsStr)
DECLARE SUB SelectMaze (Maze AS MazeStr)
DECLARE SUB WriteScores (Scores() AS ScoreStr, ScoreFile$)

ON ERROR GOTO ErrorTrap
DIM Maze AS MazeStr
DIM Monsters(0 TO 9)  AS MonstersStr
DIM Player AS PlayerStr
DIM Settings AS SettingsStr
DIM SHARED FileName$

Initialize Maze, Monsters(), Player, Settings
DisplayMenu Maze, Player, Settings
Game Maze, Monsters(), Player, Settings
SaveSettings Player, Settings
RUN "Monsters"

ErrorTrap:
DIM Choice$
DIM ErrorCode

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

FUNCTION BaseFileName$ (File$)
DIM BaseName$
DIM Position

 BaseName$ = File$
 Position = INSTR(BaseName$, ".")
 IF Position > 0 THEN BaseName$ = LEFT$(BaseName$, Position - 1)
 BaseFileName$ = BaseName$
END FUNCTION

FUNCTION CanMove (x, y, Direction, MazeV$())
DIM Result

 Result = 0

 SELECT CASE Direction
  CASE 1
   Result = NOT (MazeV$(x - 1, y) = "Û")
  CASE 2
   Result = NOT (MazeV$(x + 1, y) = "Û")
  CASE 3
   Result = NOT (MazeV$(x, y - 1) = "Û")
  CASE 4
   Result = NOT (MazeV$(x, y + 1) = "Û")
 END SELECT

 CanMove = Result
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

SUB DecreasePlayerLife (Maze AS MazeStr, MazeV$(), Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM Key$

 IF Player.LifeCount = 0 THEN
  SaveSettings Player, Settings
  GameOver Maze, Player, Settings
  RUN "Monsters"
 ELSE
  Player.LifeCount = Player.LifeCount - 1
  IF Settings.SoundOn THEN SOUND 37, 1: SOUND 100, 1
  DisplayStatus Maze, Player
  COLOR 0, 7: DrawBox 10, 24, 30, 2, 0
  Center 11, "You got caught!"
  Center 0, "Press Enter to continue."
  DO
   Key$ = INPUT$(1)
  LOOP UNTIL Key$ = CHR$(13)
  COLOR , 0
  RedrawMaze MazeV$(), Maze, Player
  Player.Direction = 0
  Player.NextDirection = 0
  Player.x = 1
  Player.y = 1
  InitializeMonsters Monsters(), Maze
 END IF
END SUB

SUB Delay (Seconds AS SINGLE)
DIM DelayStart AS SINGLE

 DelayStart = TIMER
 DO UNTIL TIMER > DelayStart OR TIMER < 3
 LOOP
END SUB

SUB DisplayGraphics (y, GraphicsFile$)
DIM FileH
DIM Row$

 FileH = FREEFILE
 FileName$ = GraphicsFile$
 OPEN GraphicsFile$ FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH)
   LINE INPUT #FileH, Row$
   IF y = 0 THEN
    Center 0, Row$
   ELSE
    LOCATE , y
    PRINT Row$
   END IF
  LOOP
 CLOSE FileH
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

SUB DisplayList (MazeNameList$(), SelectedMaze, TopMaze)
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

SUB DisplayMenu (Maze AS MazeStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM CursorX
DIM MenuHelp$(0 TO 9)
DIM Key$
DIM MenuOption$(0 TO 9)
DIM OptionIndex
DIM State$(-1 TO 0)
DIM Text$

 CursorX = 0
 State$(-1) = "on"
 State$(0) = "off"
 MenuOption$(0) = "Begin game"
 MenuOption$(1) = "Joystick on/off"
 MenuOption$(2) = "Sound on/off"
 MenuOption$(3) = "Monsters' speed"
 MenuOption$(4) = "Player's speed"
 MenuOption$(5) = "Number of lives"
 MenuOption$(6) = "Number of monsters"
 MenuOption$(7) = "Help"
 MenuOption$(8) = "View score list"
 MenuOption$(9) = "Quit"
 MenuHelp$(0) = "Press Enter to play."
 MenuHelp$(1) = "The joystick is turned " + State$(Settings.JoystickOn) + "."
 MenuHelp$(2) = "The sound is turned " + State$(Settings.SoundOn) + "."
 MenuHelp$(3) = "The speed is set at:" + STR$(CINT(100 - (Settings.MonsterDelay * 100))) + ", use +/- to change."
 MenuHelp$(4) = "The speed is set at:" + STR$(CINT(100 - (Player.Delay * 100))) + ", use +/- to change."
 MenuHelp$(5) = "Lives:" + STR$(Settings.LifeCount + 1) + ", use +/- to change."
 MenuHelp$(6) = "Number of monsters:" + STR$(Settings.MonsterCount) + ", use +/- to change."
 MenuHelp$(7) = "Help."
 MenuHelp$(8) = "View the score list."
 MenuHelp$(9) = "Exit the program."

 COLOR 15, 0: DisplayGraphics 1, "Title.grp"
 Center 47, "Monsters, By: Peter Swinkels, ***1998***, version 1.19"
 DrawBar 7, 7
 DrawBar 46, 7

 COLOR 3
 FOR OptionIndex = LBOUND(MenuOption$) TO UBOUND(MenuOption$)
  LOCATE (OptionIndex + 6) * 2, 6
  PRINT MenuOption$(OptionIndex)
 NEXT OptionIndex
 COLOR 2: Center 45, "Use " + CHR$(24) + "/" + CHR$(25) + " to move cursor."

 DO
  COLOR 3, 1
  LOCATE (CursorX + 6) * 2, 5
  PRINT " "; MenuOption$(CursorX); SPACE$(20 - LEN(MenuOption$(CursorX)))
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  COLOR 3, 0
  LOCATE (CursorX + 6) * 2, 5
  PRINT " "; MenuOption$(CursorX); SPACE$(20 - LEN(MenuOption$(CursorX)))
  LOCATE 45, 1: PRINT SPACE$(80)

  SELECT CASE Key$
   CASE CHR$(0) + "H"
    IF CursorX = LBOUND(MenuOption$) THEN CursorX = UBOUND(MenuOption$) ELSE CursorX = CursorX - 1
   CASE CHR$(0) + "P"
    IF CursorX = UBOUND(MenuOption$) THEN CursorX = LBOUND(MenuOption$) ELSE CursorX = CursorX + 1
   CASE CHR$(13)
    COLOR , 0
    SELECT CASE CursorX
     CASE 0
      CLS
      Text$ = GetInput$(5, 6, "Enter your name: ", 10)
      IF Text$ = "-" OR Text$ = "" THEN
       SaveSettings Player, Settings
       RUN "Monsters"
      END IF
      SWAP Player.NameV, Text$
      IF Settings.JoystickOn THEN
       COLOR 7
       LOCATE 5, 6: PRINT "Please enter joystick sensetivity (1-9):"
       DO
        Settings.JoystickSensitivity = VAL(INPUT$(1))
       LOOP UNTIL Settings.JoystickSensitivity > 0 AND Settings.JoystickSensitivity < 10
       Settings.JoystickSensitivity = Settings.JoystickSensitivity * 3
      END IF
      SelectMaze Maze
      EXIT DO
     CASE 1
      Settings.JoystickOn = NOT Settings.JoystickOn
      MenuHelp$(1) = "The joystick is turned " + State$(Settings.JoystickOn) + "."
     CASE 2
      Settings.SoundOn = NOT Settings.SoundOn
      MenuHelp$(2) = "The sound is turned " + State$(Settings.SoundOn) + "."
     CASE 7
      SaveSettings Player, Settings
      LoadHelp "Monsters.hlp"
      RUN "Monsters"
     CASE 8
      SaveSettings Player, Settings
      DisplayScores Maze, -1
      RUN "Monsters"
     CASE 9
      SaveSettings Player, Settings
      Quit
    END SELECT
   CASE "-"
    SELECT CASE CursorX
     CASE 3
      IF CINT(Settings.MonsterDelay * 100) >= 100 THEN Settings.MonsterDelay = 0 ELSE Settings.MonsterDelay = Settings.MonsterDelay + .01
      MenuHelp$(3) = "The speed is set at:" + STR$(CINT(100 - (Settings.MonsterDelay * 100))) + ", use +/- to change."
     CASE 4
      IF CINT(Player.Delay * 100) >= 100 THEN Player.Delay = 0 ELSE Player.Delay = Player.Delay + .01
      MenuHelp$(4) = "The speed is set at:" + STR$(CINT(100 - (Player.Delay * 100))) + ", use +/- to change."
     CASE 5
      IF Settings.LifeCount = 0 THEN Settings.LifeCount = 4 ELSE Settings.LifeCount = Settings.LifeCount - 1
      MenuHelp$(5) = "Lives:" + STR$(Settings.LifeCount + 1) + ", use +/- to change."
     CASE 6
      IF Settings.MonsterCount = 3 THEN Settings.MonsterCount = 10 ELSE Settings.MonsterCount = Settings.MonsterCount - 1
      MenuHelp$(6) = "Number of monsters:" + STR$(Settings.MonsterCount) + ", use +/- to change."
    END SELECT
   CASE "+"
    SELECT CASE CursorX
     CASE 3
      IF CINT(Settings.MonsterDelay * 100) <= 0 THEN Settings.MonsterDelay = 1 ELSE Settings.MonsterDelay = Settings.MonsterDelay - .01
      MenuHelp$(3) = "The speed is set at:" + STR$(CINT(100 - (Settings.MonsterDelay * 100))) + ", use +/- to change."
     CASE 4
      IF CINT(Player.Delay * 100) <= 0 THEN Player.Delay = 1 ELSE Player.Delay = Player.Delay - .01
      MenuHelp$(4) = "The speed is set at:" + STR$(CINT(100 - (Player.Delay * 100))) + ", use +/- to change."
     CASE 5
      IF Settings.LifeCount = 4 THEN Settings.LifeCount = 0 ELSE Settings.LifeCount = Settings.LifeCount + 1
      MenuHelp$(5) = "Lives:" + STR$(Settings.LifeCount + 1) + ", use +/- to change."
     CASE 6
      IF Settings.MonsterCount = 10 THEN Settings.MonsterCount = 3 ELSE Settings.MonsterCount = Settings.MonsterCount + 1
      MenuHelp$(6) = "Number of monsters:" + STR$(Settings.MonsterCount) + ", use +/- to change."
    END SELECT
  END SELECT

  COLOR 2: Center 45, MenuHelp$(CursorX)
 LOOP
END SUB

SUB DisplayScores (Maze AS MazeStr, HighlightRank)
DIM Key$
DIM ScoreIndex
DIM Scores(0 TO 24) AS ScoreStr

 COLOR 2, 0: CLS
 DisplayGraphics 0, "Scorelst.grp"
 DrawBar 0, 1
 COLOR 14
 PRINT SPACE$(4);
 PRINT "Name:"; SPACE$(6);
 PRINT "Score: ";
 PRINT "Date:"; SPACE$(15);
 PRINT "Maze:"
 DrawBar 0, 1
 COLOR 15: Center 47, "Press any key to return to the menu."
 DrawBar 46, 1

 IF LEFT$(Maze.NameV, 1) = CHR$(0) THEN SelectMaze Maze
 IF NOT LEFT$(Maze.NameV, 1) = CHR$(0) THEN
  ReadScores Scores(), BaseFileName$(RTRIM$(Maze.File)) + ".scr"
  LOCATE 9
  FOR ScoreIndex = LBOUND(Scores) TO UBOUND(Scores)
   IF NOT LEFT$(Scores(ScoreIndex).NameV, 1) = CHR$(0) THEN
    IF ScoreIndex = HighlightRank THEN COLOR 31, 0 ELSE COLOR 15, 0
    LOCATE , 1
    PRINT USING " ## "; ScoreIndex + 1;
    PRINT USING "\        \"; Scores(ScoreIndex).NameV;
    PRINT USING "   ###% "; VAL(Scores(ScoreIndex).Score);
    PRINT USING "\                  \"; Scores(ScoreIndex).DateTime;
    PRINT Scores(ScoreIndex).Maze
   END IF
  NEXT ScoreIndex

  Key$ = INPUT$(1)
 END IF
END SUB

SUB DisplayStatus (Maze AS MazeStr, Player AS PlayerStr)
 COLOR 12
 LOCATE 47, 1
 PRINT USING "Lives: \   \   Name: \        \   Completed: ###%"; STRING$(Player.LifeCount + 1, 1); Player.NameV; GetPercentage(Maze.TotalDots, Maze.TotalDots - Maze.DotsLeft);
END SUB

SUB DrawBar (x, ColorV)
 COLOR ColorV
 IF x = 0 THEN LOCATE , 1 ELSE LOCATE x, 1
 PRINT STRING$(80, "Ä");
END SUB

SUB DrawBox (x, y, w, h, Shadow)
DIM BoxX

 LOCATE x, y: PRINT "Ú"; STRING$(w, "Ä"); "¿"
 FOR BoxX = 1 TO h
  LOCATE , y: PRINT "³"; SPC(w); "³";
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

SUB Game (Maze AS MazeStr, Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM Key$
DIM MazeV$(0 TO 45, 0 TO 79)

 IF Settings.JoystickOn THEN
  COLOR , 0: CLS
  COLOR 12: LOCATE 3, 5: PRINT "Make sure the joystick is centered."
  LOCATE , 6: PRINT "Press a key "
  Key$ = INPUT$(1)
  Settings.JoystickCenterX = STICK(0)
  Settings.JoystickCenterY = STICK(1)
 END IF

 LoadMaze MazeV$(), Maze, Player

 InitializeMonsters Monsters(), Maze

 Player.LifeCount = Settings.LifeCount
 Player.NextDirection = 0
 Player.x = Maze.PlayerStartX
 Player.y = Maze.PlayerStartY

 DisplayStatus Maze, Player
 DO
  DO
   IF Settings.JoystickOn THEN GetJoystickStatus MazeV$(), Player, Settings
   MovePlayer MazeV$(), Maze, Monsters(), Player, Settings
   MoveMonsters Maze, MazeV$(), Monsters(), Player, Settings
   Key$ = UCASE$(INKEY$)
  LOOP WHILE Key$ = ""
  IF NOT Settings.JoystickOn THEN
   SELECT CASE Key$
    CASE CHR$(0) + "H"
     IF CanMove(Player.x, Player.y, 1, MazeV$()) THEN
      Player.Direction = 1
      Player.NextDirection = 0
     ELSE
      Player.NextDirection = 1
     END IF
    CASE CHR$(0) + "P"
     IF CanMove(Player.x, Player.y, 2, MazeV$()) THEN
      Player.Direction = 2
      Player.NextDirection = 0
     ELSE
      Player.NextDirection = 2
     END IF
    CASE CHR$(0) + "K"
     IF CanMove(Player.x, Player.y, 3, MazeV$()) THEN
      Player.Direction = 3
      Player.NextDirection = 0
     ELSE
      Player.NextDirection = 3
     END IF
    CASE CHR$(0) + "M"
     IF CanMove(Player.x, Player.y, 4, MazeV$()) THEN
      Player.Direction = 4
      Player.NextDirection = 0
     ELSE
      Player.NextDirection = 4
     END IF
    CASE CHR$(0) + "O"
     Player.Direction = 0:
     Player.NextDirection = 0
   END SELECT
  END IF
  SELECT CASE Key$
   CASE "J"
    IF Settings.JoystickOn THEN
     Settings.JoystickCenterX = STICK(0)
     Settings.JoystickCenterY = STICK(1)
    END IF
   CASE "P"
    COLOR 0, 7: DrawBox 10, 24, 30, 2, 0
    Center 11, "Pause."
    Center 0, "Press any key to continue."
    DO
    LOOP WHILE INKEY$ = ""
    COLOR , 0
    RedrawMaze MazeV$(), Maze, Player
  END SELECT
 LOOP UNTIL Key$ = CHR$(27)
END SUB

SUB GameOver (Maze AS MazeStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM Answer$
DIM DelayStart AS SINGLE
DIM FrameIndex
DIM Frames$

 Frames$ = " *-Ü"
 FrameIndex = 3
 COLOR 15
 DelayStart = TIMER
 DO
  IF TIMER > DelayStart + .3 THEN
   IF Settings.SoundOn THEN SOUND (FrameIndex * 30) + 37, 3 ELSE Delay .5
   LOCATE Player.x + 1, Player.y + 1: PRINT MID$(Frames$, FrameIndex + 1, 1)
   IF FrameIndex = 0 THEN EXIT DO ELSE FrameIndex = FrameIndex - 1
   DelayStart = TIMER
  ELSE
   IF TIMER < 3 THEN DelayStart = TIMER
  END IF
 LOOP

 COLOR 0, 7
 DrawBox 20, 22, 34, 2, 0
 Center 21, "Game over!"
 Answer$ = Choose$(22, 24, "Do you want to play again y/n?", "YN")
 DisplayScores Maze, ProcessNewScore(Maze, Player)
 IF Answer$ = "N" THEN
  SaveSettings Player, Settings
  Quit
 END IF
END SUB

FUNCTION GetInput$ (x, y, Prompt$, MaxLength)
DIM Key$
DIM Length
DIM Text$

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

SUB GetJoystickStatus (MazeV$(), Player AS PlayerStr, Settings AS SettingsStr)
DIM x
DIM y

 x = STICK(0)
 y = STICK(1)
 IF x < Settings.JoystickCenterX - Settings.JoystickSensitivity AND CanMove(Player.x, Player.y, 3, MazeV$()) THEN Player.Direction = 3
 IF x > Settings.JoystickCenterX + Settings.JoystickSensitivity AND CanMove(Player.x, Player.y, 4, MazeV$()) THEN Player.Direction = 4
 IF y < Settings.JoystickCenterY - Settings.JoystickSensitivity AND CanMove(Player.x, Player.y, 1, MazeV$()) THEN Player.Direction = 1
 IF y > Settings.JoystickCenterY + Settings.JoystickSensitivity AND CanMove(Player.x, Player.y, 2, MazeV$()) THEN Player.Direction = 2
END SUB

FUNCTION GetPercentage (Value1, Value2)
 GetPercentage = (Value2 / Value1) * 100
END FUNCTION

SUB Initialize (Maze AS MazeStr, Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM FileH
DIM Key$

 SCREEN 0
 WIDTH 80, 50
 COLOR 7, 0
 CLS
 RANDOMIZE TIMER

 Maze.Count = 0
 Player.Delay = .01
 Settings.JoystickOn = 0
 Settings.LifeCount = 2
 Settings.MonsterCount = 10
 Settings.MonsterDelay = .02
 Settings.SoundOn = -1

 IF FileExists("Mazes.lst") THEN
  FileH = FREEFILE
  FileName$ = "Mazes.lst"
  OPEN "Mazes.lst" FOR BINARY LOCK READ WRITE AS FileH
   Maze.Count = ASC(INPUT$(1, FileH))
  CLOSE FileH
 END IF

 IF Maze.Count = 0 THEN
  COLOR 0, 7
  DrawBox 19, 25, 28, 1, 1
  Center 20, "There are no saved mazes."
  Key$ = INPUT$(1)
  Quit
 END IF

 IF FileExists("Monsters.set") THEN
  FileH = FREEFILE
  FileName$ = "Monsters.set"
  OPEN "Monsters.set" FOR BINARY LOCK READ WRITE AS FileH
   Player.Delay = (100 - ASC(INPUT$(1, FileH))) / 100
   Settings.JoystickOn = -ASC(INPUT$(1, FileH))
   Settings.LifeCount = ASC(INPUT$(1, FileH))
   Settings.MonsterDelay = (100 - ASC(INPUT$(1, FileH))) / 100
   Settings.MonsterCount = ASC(INPUT$(1, FileH))
   Settings.SoundOn = -ASC(INPUT$(1, FileH))
  CLOSE FileH
 END IF
END SUB

SUB InitializeMonsters (Monsters() AS MonstersStr, Maze AS MazeStr)
DIM Available
DIM MonsterIndex
DIM NewColor
DIM OtherMonsterIndex
DIM x1
DIM y1
DIM x2
DIM y2

 x1 = Maze.MonstersStartX1
 y1 = Maze.MonstersStartY1
 x2 = Maze.MonstersStartX2
 y2 = Maze.MonstersStartY2

 FOR MonsterIndex = LBOUND(Monsters) TO UBOUND(Monsters)
  DO
   NewColor = CINT(RND * 13) + 1
   Available = -1
   FOR OtherMonsterIndex = LBOUND(Monsters) TO UBOUND(Monsters)
    IF NewColor = Monsters(OtherMonsterIndex).ColorV THEN
     Available = 0
     EXIT FOR
    END IF
   NEXT OtherMonsterIndex
  LOOP UNTIL Available

  Monsters(MonsterIndex).ColorV = NewColor
  Monsters(MonsterIndex).Direction = CINT(RND * 3) + 1
  Monsters(MonsterIndex).x = CINT(RND * (x2 - x1)) + x1
  Monsters(MonsterIndex).y = CINT(RND * (y2 - y1)) + y1
 NEXT MonsterIndex
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

SUB LoadMaze (MazeV$(), Maze AS MazeStr, Player AS PlayerStr)
DIM Column
DIM FileH
DIM MazeRow$
DIM Row
DIM x
DIM y

 COLOR 2, 0: CLS

 FileH = FREEFILE
 FileName$ = RTRIM$(Maze.File) + ".maz"
 OPEN RTRIM$(Maze.File) + ".maz" FOR BINARY LOCK READ WRITE AS FileH
  Maze.PlayerStartX = ASC(INPUT$(1, FileH))
  Maze.PlayerStartY = ASC(INPUT$(1, FileH))
  Maze.MonstersStartX1 = ASC(INPUT$(1, FileH))
  Maze.MonstersStartY1 = ASC(INPUT$(1, FileH))
  Maze.MonstersStartX2 = ASC(INPUT$(1, FileH))
  Maze.MonstersStartY2 = ASC(INPUT$(1, FileH))
  FOR Row = LBOUND(MazeV$, 1) TO UBOUND(MazeV$, 1)
   MazeRow$ = INPUT$(80, FileH)
   LOCATE Row + 1, 1: PRINT MazeRow$
   FOR Column = LBOUND(MazeV$, 2) TO UBOUND(MazeV$, 2)
    MazeV$(Row, Column) = MID$(MazeRow$, Column + 1, 1)
   NEXT Column
  NEXT Row
 CLOSE FileH

 COLOR 14
 Maze.TotalDots = 0
 FOR x = LBOUND(MazeV$, 1) TO UBOUND(MazeV$, 1)
  FOR y = LBOUND(MazeV$, 2) TO UBOUND(MazeV$, 2)
   IF MazeV$(x, y) = " " THEN
    MazeV$(x, y) = "."
    LOCATE x + 1, y + 1: PRINT MazeV$(x, y)
    Maze.TotalDots = Maze.TotalDots + 1
   END IF
  NEXT y
 NEXT x
 Maze.DotsLeft = Maze.TotalDots
END SUB

FUNCTION MonsterPresent (x, y, Direction, Monsters() AS MonstersStr, Settings AS SettingsStr)
DIM CheckX
DIM CheckY
DIM MonsterIndex
DIM Present

 Present = 0

 SELECT CASE Direction
  CASE 1
   CheckX = x - 1
   CheckY = y
  CASE 2
   CheckX = x + 1
   CheckY = y
  CASE 3
   CheckX = x
   CheckY = y - 1
  CASE 4
   CheckY = x
   CheckY = y + 1
  CASE ELSE
   CheckX = x
   CheckY = y
 END SELECT


 FOR MonsterIndex = 0 TO Settings.MonsterCount - 1
  IF Monsters(MonsterIndex).x = CheckX AND Monsters(MonsterIndex).y = CheckY THEN
   Present = -1
   EXIT FOR
  END IF
 NEXT MonsterIndex

 MonsterPresent = Present
END FUNCTION

SUB MoveMonsters (Maze AS MazeStr, MazeV$(), Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM MonsterIndex
STATIC DelayStart AS SINGLE

 IF TIMER >= DelayStart + Settings.MonsterDelay THEN
  FOR MonsterIndex = 0 TO Settings.MonsterCount - 1
   COLOR 14
   LOCATE Monsters(MonsterIndex).x + 1, Monsters(MonsterIndex).y + 1

   IF MazeV$(Monsters(MonsterIndex).x, Monsters(MonsterIndex).y) = "." THEN
    PRINT ".";
   ELSE
    PRINT " ";
   END IF

   IF CanMove(Monsters(MonsterIndex).x, Monsters(MonsterIndex).y, Monsters(MonsterIndex).Direction, MazeV$()) THEN
    IF NOT MonsterPresent(Monsters(MonsterIndex).x, Monsters(MonsterIndex).y, Monsters(MonsterIndex).Direction, Monsters(), Settings) THEN
     SELECT CASE Monsters(MonsterIndex).Direction
      CASE 1
       Monsters(MonsterIndex).x = Monsters(MonsterIndex).x - 1
      CASE 2
       Monsters(MonsterIndex).x = Monsters(MonsterIndex).x + 1
      CASE 3
       Monsters(MonsterIndex).y = Monsters(MonsterIndex).y - 1
      CASE 4
       Monsters(MonsterIndex).y = Monsters(MonsterIndex).y + 1
     END SELECT
    END IF
   END IF

   IF CINT(RND * 10) > 9 THEN Monsters(MonsterIndex).Direction = NewMonsterDirection(Monsters(), MazeV$())

   COLOR Monsters(MonsterIndex).ColorV
   LOCATE Monsters(MonsterIndex).x + 1, Monsters(MonsterIndex).y + 1
   PRINT CHR$(2);
  NEXT MonsterIndex
 
  IF MonsterPresent(Player.x, Player.y, 0, Monsters(), Settings) THEN
   DecreasePlayerLife Maze, MazeV$(), Monsters(), Player, Settings
  END IF
 
  DelayStart = TIMER
 ELSE
  IF TIMER < 3 THEN DelayStart = TIMER
 END IF
END SUB

SUB MovePlayer (MazeV$(), Maze AS MazeStr, Monsters() AS MonstersStr, Player AS PlayerStr, Settings AS SettingsStr)
DIM Key$
DIM MonsterIndex
DIM x
DIM y
STATIC DelayStart AS SINGLE
STATIC PlayDotSound
 
 IF NOT Player.NextDirection = 0 THEN
  IF CanMove(Player.x, Player.y, Player.NextDirection, MazeV$()) THEN
   Player.Direction = Player.NextDirection
   IF Player.Direction = Player.NextDirection THEN
    Player.NextDirection = 0
   END IF
  END IF
 END IF

 IF MonsterPresent(Player.x, Player.y, 0, Monsters(), Settings) THEN
  DecreasePlayerLife Maze, MazeV$(), Monsters(), Player, Settings
 END IF

 IF TIMER > DelayStart + Player.Delay THEN
  IF Maze.DotsLeft = 0 THEN
   COLOR 0, 7: DrawBox 10, 24, 30, 2, 0
   COLOR 16: Center 11, "You won!"
   COLOR 0: Center 0, "Press a key to continue."
   Key$ = INPUT$(1)
   SaveSettings Player, Settings
   DisplayScores Maze, ProcessNewScore(Maze, Player)
   RUN "Monsters"
  ELSE
   IF MazeV$(Player.x, Player.y) = "." THEN
    MazeV$(Player.x, Player.y) = " "
    LOCATE Player.x + 1, Player.y + 1: PRINT " "
    IF Settings.SoundOn AND PlayDotSound THEN SOUND 37, .03
    Maze.DotsLeft = Maze.DotsLeft - 1
    DisplayStatus Maze, Player
    PlayDotSound = NOT PlayDotSound
   END IF
  END IF
  IF CanMove(Player.x, Player.y, Player.Direction, MazeV$()) THEN
   LOCATE Player.x + 1, Player.y + 1: PRINT " ";
   SELECT CASE Player.Direction
    CASE 1
     Player.x = Player.x - 1
    CASE 2
     Player.x = Player.x + 1
    CASE 3
     Player.y = Player.y - 1
    CASE 4
     Player.y = Player.y + 1
   END SELECT
  END IF
  LOCATE Player.x + 1, Player.y + 1
  COLOR 15
  PRINT CHR$(1);
  DelayStart = TIMER
 ELSE
  IF TIMER < 3 THEN DelayStart = TIMER
 END IF
END SUB

FUNCTION NewMonsterDirection (Monsters() AS MonstersStr, MazeV$())
DIM MonsterIndex
DIM NewDirection

 DO
  DO
   NewDirection = CINT(RND * 3) + 1
  LOOP WHILE NewDirection = Monsters(MonsterIndex).Direction
 LOOP UNTIL CanMove(Monsters(MonsterIndex).x, Monsters(MonsterIndex).y, NewDirection, MazeV$())

 NewMonsterDirection = NewDirection
END FUNCTION

FUNCTION ProcessNewScore (Maze AS MazeStr, Player AS PlayerStr)
DIM OtherScoreIndex
DIM Rank
DIM ScoreFile$
DIM ScoreIndex
DIM Scores(0 TO 25) AS ScoreStr

 ScoreFile$ = BaseFileName$(RTRIM$(Maze.File)) + ".scr"
 ReadScores Scores(), ScoreFile$

 Rank = UBOUND(Scores)
 Scores(Rank).DateTime = DATE$ + " " + TIME$
 Scores(Rank).Maze = Maze.NameV
 Scores(Rank).NameV = Player.NameV
 Scores(Rank).Score = STR$(GetPercentage(Maze.DotsLeft, Maze.TotalDots - Maze.DotsLeft))

 FOR ScoreIndex = LBOUND(Scores) TO UBOUND(Scores)
  FOR OtherScoreIndex = LBOUND(Scores) TO UBOUND(Scores)
   IF NOT ScoreIndex = OtherScoreIndex THEN
    IF VAL(Scores(ScoreIndex).Score) > VAL(Scores(OtherScoreIndex).Score) THEN
     SWAP Scores(ScoreIndex), Scores(OtherScoreIndex)
     IF ScoreIndex = Rank THEN Rank = OtherScoreIndex
    END IF
   END IF
  NEXT OtherScoreIndex
 NEXT ScoreIndex

 WriteScores Scores(), ScoreFile$

 ProcessNewScore = Rank
END FUNCTION

SUB Quit
 WIDTH 80, 25
 COLOR 7, 0
 CLS
 SYSTEM
END SUB

SUB ReadScores (Scores() AS ScoreStr, ScoreFile$)
DIM FileH
DIM Length
DIM ScoreIndex

 FileH = FREEFILE
 FileName$ = ScoreFile$
 IF FileExists(ScoreFile$) THEN
  OPEN ScoreFile$ FOR BINARY LOCK READ WRITE AS FileH
   FOR ScoreIndex = LBOUND(Scores) TO UBOUND(Scores)
    IF LOC(FileH) >= LOF(FileH) THEN
     Scores(ScoreIndex).NameV = ""
    ELSE
     Length = ASC(INPUT$(1, FileH))
     Scores(ScoreIndex).NameV = INPUT$(Length, FileH)
     Length = ASC(INPUT$(1, FileH))
     Scores(ScoreIndex).Score = INPUT$(Length, FileH)
     Length = ASC(INPUT$(1, FileH))
     Scores(ScoreIndex).Maze = INPUT$(Length, FileH)
     Length = ASC(INPUT$(1, FileH))
     Scores(ScoreIndex).DateTime = INPUT$(Length, FileH)
    END IF
   NEXT ScoreIndex
  CLOSE FileH
 END IF
END SUB

SUB RedrawMaze (MazeV$(), Maze AS MazeStr, Player AS PlayerStr)
DIM x
DIM y

 DisplayStatus Maze, Player

 FOR x = LBOUND(MazeV$, 1) TO UBOUND(MazeV$, 1)
  FOR y = LBOUND(MazeV$, 2) TO UBOUND(MazeV$, 2)
   IF MazeV$(x, y) = "." THEN COLOR 14 ELSE COLOR 2
   LOCATE x + 1, y + 1: PRINT MazeV$(x, y)
  NEXT y
 NEXT x
END SUB

SUB SaveSettings (Player AS PlayerStr, Settings AS SettingsStr)
DIM FileH

 FileH = FREEFILE
 FileName$ = "Monsters.set"
 OPEN "Monsters.set" FOR OUTPUT LOCK READ WRITE AS FileH
  PRINT #FileH, CHR$(CINT(100 - Player.Delay * 100));
  PRINT #FileH, CHR$(-Settings.JoystickOn);
  PRINT #FileH, CHR$(Settings.LifeCount);
  PRINT #FileH, CHR$(CINT(100 - Settings.MonsterDelay * 100));
  PRINT #FileH, CHR$(Settings.MonsterCount);
  PRINT #FileH, CHR$(-Settings.SoundOn);
 CLOSE FileH
END SUB

SUB SelectMaze (Maze AS MazeStr)
DIM CursorX
DIM FileH
DIM Key$
DIM Length
DIM MazeFileList$(0 TO 249)
DIM MazeIndex
DIM MazeNameList$(0 TO 249)
DIM SelectedMaze
DIM TopMaze

 CursorX = 1
 SelectedMaze = LBOUND(MazeFileList$)
 TopMaze = SelectedMaze
 PCOPY 0, 1
 COLOR 0, 7: DrawBox 10, 26, 26, 30, 1
 Center 10, "´Select a maze.Ã"
 FileH = FREEFILE
 FileName$ = "Mazes.lst"
 OPEN "Mazes.lst" FOR INPUT LOCK READ WRITE AS FileH: CLOSE FileH
 FileH = FREEFILE
 OPEN "Mazes.lst" FOR BINARY LOCK READ WRITE AS FileH
  Maze.Count = ASC(INPUT$(1, FileH))
  FOR MazeIndex = LBOUND(MazeFileList$) TO Maze.Count
   IF LOC(FileH) = LOF(FileH) THEN Maze.Count = MazeIndex: EXIT FOR
   Length = ASC(INPUT$(1, FileH))
   MazeFileList$(MazeIndex) = INPUT$(Length, FileH)
   Length = ASC(INPUT$(1, FileH))
   MazeNameList$(MazeIndex) = INPUT$(Length, FileH)
  NEXT MazeIndex
 CLOSE FileH

 DO
  DisplayList MazeNameList$(), SelectedMaze, TopMaze
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  IF Key$ = CHR$(0) + "H" THEN
   IF SelectedMaze > 1 THEN
    SelectedMaze = SelectedMaze - 1
    IF CursorX > 1 THEN
     CursorX = CursorX - 1
    ELSE
     TopMaze = TopMaze - 1
    END IF
   END IF
  ELSEIF Key$ = CHR$(0) + "P" THEN
   IF SelectedMaze < 100 THEN
    SelectedMaze = SelectedMaze + 1
    IF CursorX < 30 THEN
     CursorX = CursorX + 1
    ELSE
     TopMaze = TopMaze + 1
    END IF
   END IF
  ELSEIF Key$ = CHR$(13) AND NOT MazeFileList$(SelectedMaze) = "" THEN
   Maze.File = MazeFileList$(SelectedMaze)
   Maze.NameV = MazeNameList$(SelectedMaze)
   PCOPY 1, 0
   EXIT DO
  ELSEIF Key$ = CHR$(27) THEN
   PCOPY 1, 0: RUN "Monsters"
  END IF
 LOOP
END SUB

SUB WriteScores (Scores() AS ScoreStr, ScoreFile$)
DIM FileH
DIM ScoreIndex

 FileH = FREEFILE
 OPEN ScoreFile$ FOR OUTPUT LOCK READ WRITE AS FileH
  FOR ScoreIndex = LBOUND(Scores) TO UBOUND(Scores)
   PRINT #FileH, CHR$(LEN(Scores(ScoreIndex).NameV)); Scores(ScoreIndex).NameV;
   PRINT #FileH, CHR$(LEN(Scores(ScoreIndex).Score)); Scores(ScoreIndex).Score;
   PRINT #FileH, CHR$(LEN(Scores(ScoreIndex).Maze)); Scores(ScoreIndex).Maze;
   PRINT #FileH, CHR$(LEN(Scores(ScoreIndex).DateTime)); Scores(ScoreIndex).DateTime;
  NEXT ScoreIndex
 CLOSE FileH
END SUB

