--
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.strings.Unbounded; use Ada.strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;

with SDL.Timers;
use type SDL.Timers.Milliseconds;
--with SDL.Video.Pixel_Formats;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

with SDL.RWops;

with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Palettes;
--with SDL.Video.Rectangles;
use type SDL.Video.Renderers.Renderer_Flags;
with SDL.Events.Events; 
use type SDL.Events.Event_Types;
with SDL.Events.Keyboards; 

with SDL.TTFs;
with SDL.TTFs.Makers;
use type SDL.TTFs.Font_Styles;


with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
--use type Interfaces.C.size_t;

with Game;
with TetrisShape; use TetrisShape;

-- gprbuild -p -gnat2012 -XSDL_MODE=debug -XSDL_PLATFORM=windows -Psdl_tetris.gpr -cargs `sdl2-config --cflags` -largs `sdl2-config --libs` -lSDL2_ttf -lSDL2_image
-- gprbuild -p -gnat2012 -gnatwa -gnatW8 -XSDL_MODE=debug -XSDL_PLATFORM=windows -Psdl_tetris.gpr -cargs `sdl2-config --cflags` -largs `sdl2-config --libs` -lSDL2_ttf -lSDL2_image

procedure sdl_tetris is

   type GameMode_Type is (STAND_BY,PLAY,GAME_OVER,HIGH_SCORES);
   curGameMode : GameMode_Type := STAND_BY;

   VeloH                      : Integer := 0;
   fDrop                      : Boolean := False;
   fFastDown                  : Boolean := False;
   fEscapePlayMode            : Boolean := False;
   nbCompletedLines           : Integer := 0;
   horizontalMove             : Integer := 0;
   horizontalMoveStartColumn  : Integer := 0;

   type HightScore is record
      name    : String(1..8);
      score   : Integer; 
   end record;
   curScore        : Integer := 0;
   playerName      : Unbounded_String := To_Unbounded_String("");
   idHighScore     : Integer := 0;
   iHighScoreColor : Integer := 0;

   subtype Index_highScores is Integer range 1 .. 10;
   type highScores_t is array (Index_highScores) of HightScore;
   hightScores : highScores_t := (others => ("XXXXXXXX",0));

   startHTicks    : SDL.Timers.Milliseconds := 0;
   startVTicks    : SDL.Timers.Milliseconds := 0;
   startRTicks    : SDL.Timers.Milliseconds := 0;
   currentTicks   : SDL.Timers.Milliseconds;

   backupX        : Integer;

   --  package US renames Ada.Strings.Unbounded;
   --  function "+" (S : String) return US.Unbounded_String
   --     renames US.To_Unbounded_String;
   --  function "+" (S : US.Unbounded_String) return String
   --     renames US.To_String;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;


   fContinue   : Boolean;

   fPause      : Boolean := False;

   type ProcessEvent_Type is access procedure(event : in out SDL.Events.Events.Events);
   processEvent : ProcessEvent_Type; 


   function SDL_GetTicks return SDL.Timers.Milliseconds with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetTicks";


   MIX_DEFAULT_FREQUENCY : constant C.int := 44100;
   MIX_DEFAULT_CHANNELS  : constant C.int := 2;
   MIX_DEFAULT_FORMAT    : constant C.unsigned := 16#8010#;

   type Mix_Music is null record;
   type Mix_Music_Pointer is access all Mix_Music with
     Convention => C;     

   procedure Mix_OpenAudio( frequency : C.int; format : C.unsigned; channels : C.int; chunksize : C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_OpenAudio";

   function Mix_LoadMUS( FilePath : in String) return Mix_Music_Pointer is
      function CMix_LoadMUS( FilePath : in C.char_array) return Mix_Music_Pointer with
         Import        => True,
         Convention    => C,
         External_Name => "Mix_LoadMUS";
   begin
      return CMix_LoadMUS(C.To_C(FilePath));
   end Mix_LoadMUS;

   procedure Mix_FreeMusic( Music_Pointer : Mix_Music_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeMusic";

   procedure Mix_PlayMusic( Music_Pointer : Mix_Music_Pointer; loops : C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayMusic";

   procedure Mix_VolumeMusic( volume : C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_VolumeMusic";

   procedure Mix_CloseAudio with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_CloseAudio";

   procedure Mix_PauseMusic with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PauseMusic";

   procedure Mix_ResumeMusic with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_ResumeMusic";

   type Mix_Chunk is null record;
   type Mix_Chunk_Pointer is access all Mix_Chunk with
     Convention => C;

   function Mix_LoadWAV( FilePath : in String) return Mix_Chunk_Pointer is
      function CMix_LoadWAV( FilePath : in C.char_array) return Mix_Chunk_Pointer with
         Import        => True,
         Convention    => C,
         External_Name => "Mix_LoadWAV";
   begin
      return CMix_LoadWAV( C.To_C(FilePath));
   end Mix_LoadWAV;

   function Mix_LoadWAV_RW( RWops : in SDL.RWops.RWops; freesrc : C.int) return Mix_Chunk_Pointer with
      Import        => True,
      Convention    => C,
      External_Name => "Mix_LoadWAV_RW";

   procedure Mix_FreeChunk( Music_Chunk : Mix_Chunk_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeChunk";

   procedure Mix_VolumeChunk( Music_Chunk : Mix_Chunk_Pointer; volume : C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_VolumeChunk";

   --  procedure Mix_PlayChannel( channel : C.int; Music_Chunk : Mix_Chunk_Pointer; loops : C.int) with
   --       Import        => True,
   --       Convention    => C,
   --       External_Name => "Mix_PlayChannel";

   procedure Mix_PlayChannelTimed( channel : C.int; Music_Chunk : Mix_Chunk_Pointer; loops : C.int; ticks : C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayChannelTimed";


   Music_Pointer : Mix_Music_Pointer := null;
   Success_Sound : Mix_Chunk_Pointer := null;

   -- type Int_X is array (Integer range <>) of Integer;
   -- tblInt : Int_X(1..5);

    package Character_Ordered_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
        (Key_Type        => Integer,
        Element_Type    => Character);
    --use Character_Ordered_Maps;
    KeyMap : Character_Ordered_Maps.Map;


   curTetromino         : Tetromino;
   nextTetromino        : Tetromino;
   
   board                : Game.arrBoard := (others => 0);

   subtype discreteRange_t is Integer range 0..13;
   package RandomInt is new Ada.Numerics.Discrete_Random(discreteRange_t);
   use RandomInt;
   Gen : RandomInt.Generator;
   type tetrisBag_t is array (discreteRange_t) of Integer;
   tetrisBag : tetrisBag_t := (1,2,3,4,5,6,7,1,2,3,4,5,6,7);
   iTetrisBag : Integer := 14;


   FontMedium           : SDL.TTFs.Fonts;
   FontBig              : SDL.TTFs.Fonts;

   type isOutLimit_t is access function(tetro : in out Tetromino) return Boolean;
   isOutLimit : isOutLimit_t; 

   procedure drawBoard(renderer : in out SDL.Video.Renderers.Renderer) is
      t      : Integer;
      a      : constant Integer := Game.CELL_SIZE - 2;
      lastColIndex : constant Integer := Game.NB_COLUMNS-1;
      vx,vy     : Integer;
   begin

      -- Draw Background
      renderer.Set_Draw_Colour (Colour => (10, 10, 100, 255));
      renderer.Fill(Rectangle => (SDL.Coordinate(Game.LEFT),SDL.Coordinate(Game.TOP),
            SDL.Dimension(Game.NB_COLUMNS*Game.CELL_SIZE),SDL.Dimension(Game.NB_ROWS*Game.CELL_SIZE)));

      -- Draw tetrominos remains
      for r in 0..(Game.NB_ROWS-1) loop
         
         for c in 0..lastColIndex loop
            t := board(r*Game.NB_COLUMNS+c);
            if t /=0 then
               vx := Game.LEFT + c*Game.CELL_SIZE;
               vy := Game.TOP + r*Game.CELL_SIZE;
               renderer.Set_Draw_Colour (Colour => Game.tetrisColors(t));
               renderer.Fill(Rectangle => (SDL.Coordinate(vx+1),SDL.Coordinate(vy+1),SDL.Dimension(a),SDL.Dimension(a)));
            end if;

         end loop;

      end loop;


   end drawBoard;

   function TetrisRandomizer return Integer is
      ityp : Integer;
      iSrc : Integer;
   begin
      if iTetrisBag <= discreteRange_t'Last then
         ityp := tetrisBag(iTetrisBag);
         iTetrisBag := iTetrisBag + 1;
      else
         -- Shuttle Bag
         for i in 1..24 loop
               iSrc := Random(Gen);
               ityp := tetrisBag(iSrc);
               tetrisBag(iSrc) := tetrisBag(discreteRange_t'First);
               tetrisBag(discreteRange_t'First) := ityp;
         end loop;

         --  for it of tetrisBag loop
         --        Put(it'Image);
         --        Put(" ");
         --  end loop;
         --  New_Line;

         ityp := tetrisBag(discreteRange_t'First);
         iTetrisBag := 1;
      end if;
      return ityp;
   end TetrisRandomizer;

   procedure newTetromino is 
   begin
      curTetromino.ityp := nextTetromino.ityp;
      curTetromino.Init(nextTetromino.ityp,6*Game.CELL_SIZE,0);
      curTetromino.y := -(curTetromino.maxY*Game.CELL_SIZE);
      nextTetromino.Init(TetrisRandomizer,(Game.NB_COLUMNS+3)*Game.CELL_SIZE,10*Game.CELL_SIZE);
   end newTetromino;

   function computeCompletedLines return Integer is
      nbLines : Integer := 0;
      fCompleted : Boolean;
   begin
      for r in 0..(Game.NB_ROWS-1) loop
         fCompleted := True;
         for c in 0..(Game.NB_COLUMNS-1) loop
               if board(r*Game.NB_COLUMNS+c)=0 then
                  fCompleted := False;
                  exit;
               end if;
         end loop;
         if fCompleted then
               nbLines := nbLines + 1;
         end if;
      end loop;
      return nbLines;
   end;

   function computeScore(nbLines : Integer) return Integer is
   begin
      return (case nbLines is
         when 0 => 0,
         when 1 => 40,
         when 2 => 100,
         when 3 => 300,
         when 4 => 1200,
         when others => 2000);
   end;

   procedure FreezeCurTetromino is
      ix,iy : Integer;
      x,y   : Integer;
   begin
      if curTetromino.ityp /=0 then
         ix := Integer((curTetromino.x + 1)/Game.CELL_SIZE);
         iy := Integer((curTetromino.y + 1)/Game.CELL_SIZE);
         for p of curTetromino.v loop
               x := ix + Integer(p.x);
               y := iy + Integer(p.y);
               if x>=0 and x<Game.NB_COLUMNS and y>=0 and y<Game.NB_ROWS then
                  board(x + y*Game.NB_COLUMNS) := curTetromino.ityp;
               end if;
         end loop;
      end if;
      --
      nbCompletedLines := computeCompletedLines;
      if nbCompletedLines>0 then
         curScore := curScore + computeScore(nbCompletedLines);
         --Text.setString (textScore, "SCORE : " & Tail(curScore'Image(curScore'Img'First + 1 .. curScore'Img'Last),6,'0'));
      end if;

   end FreezeCurTetromino;

   function isHighScore(score : Integer) return Integer is
   begin
      for i in Index_highScores loop
         if score>=hightScores(i).score then
               return i;
         end if;
      end loop;
      return 0;
   end isHighScore;

   procedure insertHighScore( id:Integer; name:String; score:Integer) is
      i : Integer;
   begin
      -- Shift down highScores array
      i := Index_highScores'Last;
      while i>id loop
         --
         hightScores(i).name := hightScores(i-1).name;
         hightScores(i).score := hightScores(i-1).score;
         --
         i := i - 1;
      end loop;
      hightScores(id) := (name,score);

   end insertHighScore;

   procedure saveHighScores(fileName : String) is
      fich : File_Type;
   begin
      Create(fich,Out_File,fileName);
      for i in Index_highScores loop
         Put_Line(fich, hightScores(i).name & ";" & Ada.Strings.Fixed.Trim(hightScores(i).score'Image,Both) & ";");
      end loop;
      Close(fich);

   end saveHighScores;

   function parseWord(ic : in out Integer; strLine : in Unbounded_String; sep : Character) return Unbounded_String is
      l : Integer;
      c : Character;
      strWord : Unbounded_String;
   begin
      l := Ada.strings.Unbounded.Length(strLine);
      while ic<=l loop
         c := Ada.strings.Unbounded.Element(strLine,ic);
         if c=sep then
               ic := ic + 1;
               return strWord;
         else
               Ada.Strings.Unbounded.Append(strWord,c);
         end if;
         ic := ic + 1;
      end loop;
      return strWord;
   end parseWord;

   function parseWord(ic : in out Integer; strLine : in String; sep : Character) return String is
      i,l        : Integer;
      c        : Character;
      strWord  : String(1..8) := ("        ");
   begin
      i := 1;
      l := strLine'Length;
      while ic<=l loop
         c := strLine(ic);
         if c=sep then
               ic := ic + 1;
               exit;
         else
            strWord(i) := c;
            if (i>strWord'Last) then
               exit;
            end if;
            i := i + 1;
         end if;
         ic := ic + 1;
      end loop;

      while i<=strWord'Last loop
         strWord(i) := ' ';
         i := i + 1;
      end loop;

      return strWord;
   end parseWord;

   procedure loadHighScores(fileName : String) is
      i,ic        : Integer; 
      fich        : File_Type;
      strWord     : String(1..8);
   begin
      begin
         i := Index_highScores'First;
         Open(fich,In_File,fileName);
         while not End_Of_File(fich) and (i<=Index_highScores'Last) loop
            declare
               strLine  : constant String := Get_Line(fich);
            begin
               ic := 1;
               strWord := parseWord(ic, strLine , ';');
               --hightScores(i).name := strWord;
               hightScores(i).name := strWord;
               strWord := parseWord(ic, strLine , ';');
               -- Ada.Strings.Unbounded.Trim(strWord, Both);
               hightScores(i).score := Integer'Value(strWord);
               -- Put_Line(">>Score : " & hightScores(i).score'Image);
               i := i + 1;
            end; 
         end loop;
         Close(fich);
      exception
         when Name_Error => null;
      end;

   end loadHighScores;

   procedure eraseFirstCompletedLine is
      fCompleted : Boolean;
   begin
      for r in 0..(Game.NB_ROWS-1) loop
         fCompleted := True;
         for c in 0..(Game.NB_COLUMNS-1) loop
               if board(r*Game.NB_COLUMNS+c)=0 then
                  fCompleted := False;
                  exit;
               end if;
         end loop;
         if fCompleted then
               -- Shift down Board content
               for r1 in reverse 1..r loop
                  for c1 in 0..(Game.NB_COLUMNS-1) loop
                     board(r1*Game.NB_COLUMNS+c1) := board((r1-1)*Game.NB_COLUMNS+c1);
                  end loop;
               end loop;
               return;
         end if;
      end loop;

   end eraseFirstCompletedLine;


   function isGameOver return Boolean is
   begin
      for c in 0..(Game.NB_COLUMNS-1) loop
         if board(c) /=0 then
               return True;
         end if;
      end loop;
      return False;
   end isGameOver;


   procedure initGame is 
   begin
      curTetromino.ityp := 0;
      nextTetromino.Init(TetrisRandomizer,(Game.NB_COLUMNS+3)*Game.CELL_SIZE,10*Game.CELL_SIZE);
      board := (others => 0);
      curScore := 0;
   end initGame;


   procedure processPlayEvent (event : in out SDL.Events.Events.Events) is
   begin

      if event.Common.Event_Type = SDL.Events.Quit then
         fContinue := False;
         return;

      elsif  event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then

         case event.Keyboard.Key_Sym.Key_Code is
            when  SDL.Events.Keyboards.Code_Escape =>
               fEscapePlayMode := True;
               return;
            when SDL.Events.Keyboards.Code_Space =>
               fDrop := True;
               return;
            when SDL.Events.Keyboards.Code_Pause =>
               if fPause then
                  fPause := False;
                  Mix_ResumeMusic;
               else
                  fPause := True;
                  Mix_PauseMusic;
               end if;
               
            when SDL.Events.Keyboards.Code_Left =>
               VeloH := -1;
               isOutLimit := TetrisShape.isOutLeftLimit'Access;
            when SDL.Events.Keyboards.Code_Right =>
               VeloH := 1;
               isOutLimit := TetrisShape.isOutRightLimit'Access;
            when SDL.Events.Keyboards.Code_Up =>
               if Integer(event.Keyboard.Repeat)=0 then
                  curTetromino.rotateLeft;
                  if curTetromino.hitGround(board) then
                        -- Undo Rotate
                        curTetromino.rotateRight;
                  elsif curTetromino.isOutRightLimit then
                        backupX := curTetromino.x;
                        -- Move curTetromino inside board
                        loop
                           curTetromino.x := curTetromino.x - 1;
                           if not curTetromino.isOutRightLimit then
                              exit;
                           end if;
                        end loop;
                        if curTetromino.hitGround( board) then
                           -- Undo Move
                           curTetromino.x := backupX;
                           -- Undo Rotate
                           curTetromino.rotateRight;
                        end if;
                  elsif curTetromino.isOutLeftLimit then
                        backupX := curTetromino.x;
                        -- Move curTetromino inside board
                        loop
                           curTetromino.x := curTetromino.x + 1;
                           if not curTetromino.isOutLeftLimit
                           then
                              exit;
                           end if;
                        end loop;
                        if curTetromino.hitGround( board) then
                           -- Undo Move
                           curTetromino.x := backupX;
                           -- Undo Rotate
                           curTetromino.rotateRight;
                        end if;
                  end if;
               end if;
            when SDL.Events.Keyboards.Code_Down =>
               if Integer(event.Keyboard.Repeat)=0 then
                  fFastDown := True;
               end if;
            when others => null;
         end case;

      elsif  event.Common.Event_Type = SDL.Events.Keyboards.Key_Up then

         case event.Keyboard.Key_Sym.Key_Code is
            when SDL.Events.Keyboards.Code_Left =>
               VeloH := 0;
            when SDL.Events.Keyboards.Code_Right =>
               VeloH := 0;
            when SDL.Events.Keyboards.Code_Down =>
               fFastDown := False;
            when others => null;
         end case;

      end if;

   end processPlayEvent;

   procedure processStandByEvent (event : in out SDL.Events.Events.Events) is
   begin

      if event.Common.Event_Type = SDL.Events.Quit then
         fContinue := False;
         return;

      elsif  event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then
      
         case event.Keyboard.Key_Sym.Key_Code is
            when  SDL.Events.Keyboards.Code_Escape =>
               fContinue := False;
               return;
            when SDL.Events.Keyboards.Code_Space =>
               curGameMode := PLAY;
               processEvent := processPlayEvent'Access;
               newTetromino;
            when others => null;
         end case;

      end if;

   end processStandByEvent;


   procedure processGameOverEvent (event : in out SDL.Events.Events.Events) is
   begin

      if event.Common.Event_Type = SDL.Events.Quit then
         fContinue := False;
         return;

      elsif  event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then
      
         case event.Keyboard.Key_Sym.Key_Code is
            when  SDL.Events.Keyboards.Code_Escape =>
               fContinue := False;
               return;
            when SDL.Events.Keyboards.Code_Space =>
               curGameMode := PLAY;
               processEvent := processStandByEvent'Access;
               newTetromino;
            when others => null;
         end case;

      end if;

   end processGameOverEvent;

   function ParsePlayerName(playerName : Unbounded_String) return String is
      src : constant String := To_String(playerName);
      res : String(1..8) := ("        ");
   begin
      for i in 1..src'Last loop
         res(i) := src(i); 
      end loop;
      return res;
   end ParsePlayerName;

   procedure processHightScoreEvent (event : in out SDL.Events.Events.Events) is
      ik       : Integer;
      newName  : Unbounded_String;
      l        : Natural;
   begin

      if event.Common.Event_Type = SDL.Events.Quit then
         fContinue := False;
         return;

      elsif  event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then
         
         ik := Integer(event.Keyboard.Key_Sym.Key_Code);
         case event.Keyboard.Key_Sym.Key_Code is
            when  SDL.Events.Keyboards.Code_Escape =>
               curGameMode := STAND_BY;
               processEvent := processStandByEvent'Access;
            when SDL.Events.Keyboards.Code_Return =>
               saveHighScores("HighScores.txt");
               curGameMode := STAND_BY;
               processEvent := processStandByEvent'Access;
            when SDL.Events.Keyboards.Code_Backspace =>
               -- BackSpace
               l := Ada.Strings.Unbounded.Length(playerName);
               if l>0 then
                  newName := Ada.Strings.Unbounded.Delete(playerName,l,l);
                  -- hightScores(idHighScore).name := newName;
                  playerName := newName;
                  Put_Line("High Scores Name >>" & To_String(playerName));
                  hightScores(idHighScore).name := ParsePlayerName(playerName);
               end if;
            when others =>
               if KeyMap.Contains(ik) then
                  if  Ada.Strings.Unbounded.Length(playerName) <= 8 then
                     Ada.Strings.Unbounded.Append(playerName,KeyMap(ik));
                     --hightScores(idHighScore).name := playerName;
                     Put_Line("High Scores Name >>" & To_String(playerName));
                     hightScores(idHighScore).name := ParsePlayerName(playerName);
                  end if;
               end if; 
         end case;

      end if;

   end processHightScoreEvent;


   procedure drawScore(renderer : in out SDL.Video.Renderers.Renderer) is
      Text_Surface   : SDL.Video.Surfaces.Surface;
      Text_Texture   : SDL.Video.Textures.Texture;
      txtsize        : SDL.Sizes;

   begin
      FontBig.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Italic);
      Text_Surface := FontBig.Render_UTF_8_Blended (
            Text   => "SCORE : " & Tail(curScore'Image(curScore'Img'First + 1 .. curScore'Img'Last),6,'0'),
            Colour => SDL.Video.Palettes.Colour'(Red  => 255,
                                                Green => 255,
                                                Blue  => 0,
                                                Alpha => 255));
      SDL.Video.Textures.Makers.Create (Text_Texture, Renderer, Text_Surface);
      txtsize := Text_Texture.Get_Size;
      renderer.Copy(Text_Texture,
         (SDL.Coordinate(Game.LEFT), SDL.Coordinate(Game.TOP+Game.CELL_SIZE*(Game.NB_ROWS)+4), txtsize.Width, txtsize.Height));
      
      Text_Texture.Destroy;


   end drawScore;


   procedure drawHCenteredText(renderer : in out SDL.Video.Renderers.Renderer; font : in SDL.TTFs.Fonts; x : Integer; y : Integer; text : String) is
      Text_Surface   : SDL.Video.Surfaces.Surface;
      Text_Texture   : SDL.Video.Textures.Texture;
      txtSize        : SDL.Sizes;
   begin
      Text_Surface := font.Render_UTF_8_Blended (
            Text   => text,
            Colour => SDL.Video.Palettes.Colour'(Red  => 255,
                                                Green => 255,
                                                Blue  => 0,
                                                Alpha => 255));
      SDL.Video.Textures.Makers.Create (Text_Texture, Renderer, Text_Surface);
      txtsize := Text_Texture.Get_Size;
      renderer.Copy(Text_Texture, (SDL.Coordinate(x-Integer(txtSize.Width)/2), SDL.Coordinate(y), txtsize.Width, txtsize.Height));

   end drawHCenteredText;


   procedure drawLeftAlignText(renderer : in out SDL.Video.Renderers.Renderer; 
         font : in SDL.TTFs.Fonts; color : SDL.Video.Palettes.Colour; x : Integer; y : Integer; textVal : String) is
      Text_Surface   : SDL.Video.Surfaces.Surface;
      Text_Texture   : SDL.Video.Textures.Texture;
      txtSize        : SDL.Sizes;
   begin
      Text_Surface := font.Render_UTF_8_Blended (Text => textVal, Colour => color);
      SDL.Video.Textures.Makers.Create (Text_Texture, Renderer, Text_Surface);
      txtsize := Text_Texture.Get_Size;
      renderer.Copy(Text_Texture, (SDL.Coordinate(x), SDL.Coordinate(y), txtsize.Width, txtsize.Height));
      Text_Texture.Destroy;

   end drawLeftAlignText;


   procedure drawStandBy(renderer : in out SDL.Video.Renderers.Renderer) is
      x,y            : Integer;
   begin

      x := Game.LEFT + Game.NB_COLUMNS*Game.CELL_SIZE/2;
      y := Game.TOP + 3*Game.CELL_SIZE;
      FontBig.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Normal);
      drawHCenteredText(renderer, FontBig, x, y, "ADA Tetris using SDL");

      y := y + 2*Game.CELL_SIZE;
      FontMedium.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Italic);
      drawHCenteredText(renderer, FontMedium, x, y, "R. NGUYEN THANH");

      y := y + 2*Game.CELL_SIZE;
      FontMedium.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Italic);
      drawHCenteredText(renderer, FontMedium, x, y, "Press SPACE to Play");

   end drawStandBy;


   procedure drawGameOver(renderer : in out SDL.Video.Renderers.Renderer) is
      x,y            : Integer;

   begin

      x := Game.LEFT + Game.NB_COLUMNS*Game.CELL_SIZE/2;
      y := Game.TOP + 4*Game.CELL_SIZE;
      FontBig.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Normal);
      drawHCenteredText(renderer, FontBig, x, y, "GAME OVER");

      y := y + 3*Game.CELL_SIZE;
      FontMedium.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Italic);
      drawHCenteredText(renderer, FontMedium, x, y, "Press SPACE to Continue");

   end drawGameOver;


   procedure drawHightScores(renderer : in out SDL.Video.Renderers.Renderer) is
      x,y          : Integer;
      color          : SDL.Video.Palettes.Colour;
   
   begin
      
      x := Game.LEFT + Game.NB_COLUMNS*Game.CELL_SIZE/2;
      y := Game.TOP + 2*Game.CELL_SIZE;
      FontBig.Set_Style(SDL.TTFs.Style_Bold+SDL.TTFs.Style_Normal);
      drawHCenteredText(renderer, FontBig, x, y, "HIGH SCORE");

      y := y + 2*Game.CELL_SIZE;

      for i in 1..10 loop

            -- Flashing current High Score
            if (i=idHighScore) and ((iHighScoreColor mod 2)/=0) then
               color := SDL.Video.Palettes.Colour'(Red  => 180,
                                                Green => 180,
                                                Blue  => 0,
                                                Alpha => 255);
            else
               color := SDL.Video.Palettes.Colour'(Red  => 255,
                                                Green => 255,
                                                Blue  => 0,
                                                Alpha => 255);
            end if;

            x := Game.LEFT  + Game.CELL_SIZE;
            drawLeftAlignText(renderer, FontMedium, color, x, y, hightScores(i).name);

            x := Game.LEFT + (Game.NB_COLUMNS/2+2)*Game.CELL_SIZE;
            drawLeftAlignText(renderer, FontMedium, color, x, y,
               Tail(hightScores(i).score'Image(hightScores(i).score'Img'First + 1 .. hightScores(i).score'Img'Last),6,'0'));

            y := y + Game.CELL_SIZE + 2;


      end loop; 


   end drawHightScores;


begin

   --
   Reset (Gen);

   if (not SDL.Initialise) or (not SDL.TTFs.Initialise) then
      raise Program_Error;
   end if;

   -- Fill KeyMap for Player Name Input in HighScores Mode
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_A),'A');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_B),'B');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_C),'C');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_D),'D');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_E),'E');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_F),'F');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_G),'G');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_H),'H');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_I),'I');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_J),'J');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_K),'K');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_L),'L');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_M),'M');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_N),'N');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_O),'O');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_P),'P');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_Q),'Q');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_R),'R');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_S),'S');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_T),'T');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_U),'U');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_V),'V');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_W),'W');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_X),'X');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_Y),'Y');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_Z),'Z');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_0),'0');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_1),'1');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_2),'2');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_3),'3');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_4),'4');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_5),'5');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_6),'6');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_7),'7');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_8),'8');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_9),'9');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_0),'0');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_1),'1');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_2),'2');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_3),'3');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_4),'4');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_5),'5');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_6),'6');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_7),'7');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_8),'8');
   KeyMap.Include(Integer(SDL.Events.Keyboards.Code_KP_9),'9');

   SDL.Video.Windows.Makers.Create (Win      => Window,
                                    Title    => "SDL Tetris in ADA",
                                    X        => SDL.Video.Windows.Centered_Window_Position,
                                    Y        => SDL.Video.Windows.Centered_Window_Position,
                                    Width    => Game.WIN_WIDTH,
                                    Height   => Game.WIN_HEIGHT,
                                    Flags    => 0);

   Mix_OpenAudio(MIX_DEFAULT_FREQUENCY,MIX_DEFAULT_FORMAT,MIX_DEFAULT_CHANNELS,1024);
   Music_Pointer := Mix_LoadMUS("Tetris.wav");

   if Music_Pointer/=null then
      Mix_PlayMusic( Music_Pointer, C.int(-1));
      Mix_VolumeMusic(20);
   end if;

   --  Success_Sound := Mix_LoadWAV("109662__grunz__success.wav");
   --  if Success_Sound/=null then
   --     Mix_VolumeChunk(Success_Sound,15);
   --  end if;

   Success_Sound := Mix_LoadWAV_RW( 
      SDL.RWops.From_File("109662__grunz__success.wav", SDL.RWops.Read_Binary),1);
   if Success_Sound/=null then
      Mix_VolumeChunk(Success_Sound,15);
   end if;


   SDL.Video.Renderers.Makers.Create ( 
      Renderer,
      Window,
      SDL.Video.Renderers.Accelerated+SDL.Video.Renderers.Present_V_Sync);


   SDL.TTFs.Makers.Create (FontBig, "sansation.ttf", 18);
   SDL.TTFs.Makers.Create (FontMedium, "sansation.ttf", 15);


   --  VeloH := Ada.Strings.Unbounded.Length(playerName);
   --  Ada.Strings.Unbounded.Append(playerName,"dd");
   --  VeloH := Ada.Strings.Unbounded.Length(playerName);
   --  Put_Line(">>> " & To_String(playerName));

   fContinue := True;
   
   loadHighScores("HighScores.txt");

   startHTicks := SDL.Timers.Ticks;
   startVTicks := startHTicks;
   startRTicks := startHTicks;

   --
   initGame;
   curGameMode := STAND_BY;
   processEvent:= processStandByEvent'Access;

   while fContinue loop

      while SDL.Events.Events.Poll (Event) loop

         processEvent(Event);
         
         -- curScore := 500;

         if fEscapePlayMode then
               fEscapePlayMode := False;
               -- Check High Score
               idHighScore := isHighScore(curScore);
               if idHighScore>0 and curScore/=0 then
                  declare
                     tmpName : constant String:=ParsePlayerName(playerName);
                  begin
                     insertHighScore(idHighScore,tmpName,curScore);
                     curGameMode := HIGH_SCORES;
                     processEvent := processHightScoreEvent'Access;
                     initGame;
                  end;
               else
                  curGameMode := STAND_BY;
                  processEvent := processStandByEvent'Access;
                  initGame;
               end if;
         end if;

      end loop;

      --  curTicks := SDL_GetTicks;
      --  Put_Line(curTicks'Image);
      --  curTicks := SDL.Timers.Ticks;
      --  Put_Line(">>>" & curTicks'Image);

      currentTicks := SDL.Timers.Ticks;
      if curGameMode=PLAY and (not fPause) then

         if nbCompletedLines>0 then
            --
            if (currentTicks-startVTicks)>250 then
               startVTicks := currentTicks;
               nbCompletedLines := nbCompletedLines - 1;
               eraseFirstCompletedLine;
               if Success_Sound/=null then
                  --Mix_PlayChannel(C.int(-1),Success_Sound, 0);
                  Mix_PlayChannelTimed( C.int(-1), Success_Sound, 0, C.int(-1));
               end if;
            end if;

         elsif horizontalMove/=0 then
            --
            if (currentTicks-startHTicks)>20 then
               startHTicks := currentTicks;
               for i in 1..4 loop
                  backupX := curTetromino.x;
                  curTetromino.x := curTetromino.x + horizontalMove;
                  if isOutLimit(curTetromino) then
                     curTetromino.x := backupX;
                     horizontalMove := 0;
                     exit;
                  else
                     if curTetromino.hitGround(board) then
                        curTetromino.x := backupX;
                        horizontalMove := 0;
                        exit;
                     end if;
                  end if;
                  if horizontalMove /=0 then
                     if horizontalMoveStartColumn/=curTetromino.Column then
                        curTetromino.x := backupX;
                        horizontalMove := 0;
                        startHTicks := currentTicks;
                        exit;
                     end if;
                  end if;
               end loop;
            end if;

         elsif fDrop then   
            -- 
            if (currentTicks-startVTicks)>10 then
               startVTicks := currentTicks;
               for i in 1..6 loop
                  -- Move Down
                  curTetromino.y := curTetromino.y + 1;
                  if curTetromino.hitGround( board) then
                        curTetromino.y := curTetromino.y - 1;
                        FreezeCurTetromino;
                        NewTetromino;
                        fDrop := False;                    
                  elsif curTetromino.isOutBottomLimit then
                        curTetromino.y := curTetromino.y - 1;
                        FreezeCurTetromino;
                        NewTetromino;
                        fDrop := False;                                        
                  end if;
                  if fDrop then
                     if veloH /=0 then
                        if (currentTicks-startHTicks)>20 then
                           startHTicks := currentTicks;
                           backupX := curTetromino.x;
                           curTetromino.x := curTetromino.x + veloH;

                           if isOutLimit(curTetromino) then
                              curTetromino.x := backupX;
                           else
                              startHTicks := SDL.Timers.Ticks;
                              horizontalMove := veloH;
                              horizontalMoveStartColumn := curTetromino.Column;
                              exit;
                           end if;
                        end if;
                     end if;
                  end if;
               end loop;
            end if;

         else
            declare
               fMove       : Boolean;
               limitElapse : SDL.Timers.Milliseconds := (if fFastDown then 15 else 31);
            begin
               if (currentTicks-startVTicks)>limitElapse then
                  startVTicks := currentTicks;
                  for i in 1..3 loop
                     curTetromino.y := curTetromino.y + 1;
                     fMove := True;
                     if curTetromino.hitGround(board) then
                        curTetromino.y := curTetromino.y - 1;
                        FreezeCurTetromino;
                        NewTetromino;
                        fMove := False;
                     elsif curTetromino.isOutBottomLimit then
                        curTetromino.y := curTetromino.y - 1;
                        FreezeCurTetromino;
                        NewTetromino;
                        fMove := False;
                     end if;
                        
                     if fMove then
                        if VeloH /= 0 then
                           if (currentTicks-startHTicks)>12 then
                              startHTicks := currentTicks;
                              backupX := curTetromino.x;
                              curTetromino.x := curTetromino.x + VeloH;
                              if isOutLimit(curTetromino) then
                                 curTetromino.x := backupX;
                              else
                                 if curTetromino.hitGround(board) then
                                    curTetromino.x := backupX;
                                 else
                                    startHTicks := currentTicks;
                                    horizontalMove := VeloH;
                                    horizontalMoveStartColumn := curTetromino.Column;
                                    exit;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end loop;
               end if;

            end;
         end if;


         -- Check Game Over
         if isGameOver then
               -- Check High Score
               idHighScore := isHighScore(curScore);
               if idHighScore>0 and curScore/=0 then
                  declare
                        tmpName : String(1..8);
                  begin
                     tmpName := ParsePlayerName(playerName);
                     insertHighScore(idHighScore,tmpName,curScore);
                     curGameMode := HIGH_SCORES;
                     processEvent := processHightScoreEvent'Access;
                     initGame;
                  end;
               else
                  curGameMode := GAME_OVER;
                  processEvent := processGameOverEvent'Access;
                  initGame;
               end if;
         end if;

         --
         if (currentTicks-startRTicks)>800 then
            startRTicks := currentTicks;
            if nextTetromino.ityp/=0 then
               nextTetromino.rotateRight;
            end if;
         end if;

      end if;

      -- Clear Window
      Renderer.Set_Draw_Colour ((48, 48, 255, 255));
      Renderer.Clear;

      -- 
      drawBoard(Renderer);

      case curGameMode is
         when STAND_BY =>
               drawStandBy(Renderer);
         when HIGH_SCORES => 
               drawHightScores(Renderer);
               currentTicks := SDL.Timers.Ticks;
               if (currentTicks - startVTicks)>500 then
                  startVTicks := currentTicks;
                  iHighScoreColor := iHighScoreColor + 1;
               end if;
         when GAME_OVER => 
               drawGameOver(Renderer);
         when others => null;
      end case;

      --
      if curTetromino.ityp /= 0 then
         curTetromino.draw(Renderer);
      end if;

      --
      if nextTetromino.ityp /= 0 then
         nextTetromino.draw(Renderer);
      end if;

      --
      drawScore(Renderer);

      --
      Renderer.Present;

      --
      -- SDL.Timers.Wait_Delay (100);

   end loop;

   --
   if Music_Pointer/=null then
      Mix_FreeMusic(Music_Pointer);
   end if;

   --
   if Success_Sound/=null then
      Mix_FreeChunk(Success_Sound);
   end if;

   --
   Renderer.Finalize;
   Window.Finalize;
   FontBig.Finalize;
   FontMedium.Finalize;
   SDL.TTFs.Finalise;
   Mix_CloseAudio;
   SDL.Finalise;


end sdl_tetris;
