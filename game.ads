with SDL.Video.Palettes;

package Game is

    TOP   : constant := 10;
    LEFT  : constant := 10;
    WIDTH : constant := 480;
    WIN_WIDTH   : constant := 480;
    WIN_HEIGHT  : constant := 560;
    NB_COLUMNS  : constant := 12;
    NB_ROWS     : constant := 20;
    CELL_SIZE   : constant := Integer(WIN_WIDTH / (NB_COLUMNS + 7));

    type arrBoard is array (0..NB_COLUMNS*NB_ROWS) of Integer;

    type arrtetrisColors is array (0..7) of SDL.Video.Palettes.Colour;
    tetrisColors : arrtetrisColors := (
        (16#00#, 16#00#, 16#00#, 16#FF#),
        (16#FF#, 16#60#, 16#60#, 16#FF#),
        (16#60#, 16#FF#, 16#60#, 16#FF#),
        (16#60#, 16#60#, 16#FF#, 16#FF#),
        (16#CC#, 16#CC#, 16#60#, 16#FF#),
        (16#CC#, 16#60#, 16#CC#, 16#FF#),
        (16#60#, 16#CC#, 16#CC#, 16#FF#),
        (16#DA#, 16#AA#, 16#00#, 16#FF#)
    );


end Game;