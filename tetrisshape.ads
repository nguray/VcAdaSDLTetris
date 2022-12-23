with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;
with Interfaces.C;
use type Interfaces.C.size_t;

with SDL.Video.Palettes;
use type SDL.Video.Palettes.Colour;
with SDL.Video.Renderers;
with SDL.Video.Rectangles;

with Game; use Game;

package TetrisShape is

    type Vector2i is record
        x : Integer;
        y : Integer; 
    end record;
    
    subtype Index_arrV is Integer range 0 .. 3;
    type arrV is array (Index_arrV) of Vector2i;

    type Tetromino is tagged 
    record
        x       : Integer := 0;
        y       : Integer := 0;
        ityp    : Integer := 0;
        v       : arrV;
    end record;


    procedure Init(Self : in out Tetromino; typ : Integer; x : Integer; y : Integer);
    procedure draw(Self : in out Tetromino; renderer : in out SDL.Video.Renderers.Renderer);
    procedure rotateLeft(Self : in out Tetromino);
    procedure rotateRight(Self : in out Tetromino);
    function minX(Self : in out Tetromino) return Integer;
    function maxX(Self : in out Tetromino) return Integer;
    function maxY(Self : in out Tetromino) return Integer;
    function Column(Self : in Tetromino) return Integer;
    function isOutLRLimit( Self : in out Tetromino; veloH : Integer) return Boolean;
    function isOutLeftLimit( Self : in out Tetromino) return Boolean;
    function isOutRightLimit(Self : in out Tetromino) return Boolean;
    function isOutBottomLimit(tetro : in out Tetromino) return Boolean;
    function hitGround(tetro : in out Tetromino; board : in arrBoard) return Boolean;

end TetrisShape;