
package body TetrisShape is


    procedure Init(Self : in out Tetromino; typ : Integer; x : Integer; y : Integer) is
    begin

        Self.ityp := typ;
        Self.x := x;
        Self.y := y;
        Self.v := (case typ is
            when 1 =>
                ((0,-1),(0,0),(-1,0),(-1,1)),
            when 2 =>
                ((0,-1),(0,0),(1,0),(1,1)),
            when 3 =>
                ((0,-1),(0,0),(0,1),(0,2)),
            when 4 =>
                ((-1,0),(0,0),(1,0),(0,1)),
            when 5 =>
                ((0,0),(1,0),(0,1),(1,1)),
            when 6 =>
                ((-1,-1),(0,-1),(0,0),(0,1)),
            when 7 =>
                ((1,-1),(0,-1),(0,0),(0,1)),
            when others =>
                ((0,0),(0,0),(0,0),(0,0)));

    end Init;

    procedure draw(Self : in out Tetromino; renderer : in out SDL.Video.Renderers.Renderer) is

        vx,vy   :   Integer;
        ox,oy   :   Integer;
        rects   :   SDL.Video.Rectangles.Rectangle_Arrays(0..3);
        d       :   SDL.Dimension;

    begin
        --
        -- Put_Line(">>> TetrisShape draw");Interfaces.C.size_t(i)

        ox := LEFT + Self.x;
        oy := TOP + Self.y;
        d := SDL.Dimension(CELL_SIZE - 2);
        for i in Self.v'Range loop
            vx := ox + (Self.v(i).x*CELL_SIZE) + 2;
            vy := oy + (Self.v(i).y*CELL_SIZE) + 2;
            rects(Interfaces.C.size_t(i)) := (X => SDL.Coordinate(vx), Y => SDL.Coordinate(vy), Width => d, Height => d);
        end loop;

        --
        renderer.Set_Draw_Colour (Colour => Game.tetrisColors(Self.ityp));
        renderer.Fill(Rectangles => rects);

    end draw;

    procedure rotateLeft(Self : in out Tetromino) is
        x,y : Integer;
    begin
        if Self.ityp/= 5 then
            for i in Index_arrV loop
                x := Self.v(i).y;
                y := -Self.v(i).x;
                Self.v(i).x := x;
                Self.v(i).y := y;

            end loop;
        end if;
    end rotateLeft;

    procedure rotateRight(Self : in out Tetromino) is
        x,y : Integer;
    begin
        if Self.ityp/= 5 then
            for i in Index_arrV loop
                x := -Self.v(i).y;
                y := Self.v(i).x;
                Self.v(i).x := x;
                Self.v(i).y := y;

            end loop;
        end if;
    end rotateRight;

    function minX(Self : in out Tetromino) return Integer is
        x       : Integer;
        mini    : Integer;
    begin
        mini := Self.v(Index_arrV'First).x;
        for i in (Index_arrV'First+1)..Index_arrV'Last loop
            x := Self.v(i).x;
            if (x<mini) then
                mini := x;
            end if;
        end loop;
        return mini;
    end minX;

    function maxX(Self : in out Tetromino) return Integer is
        x       : Integer;
        maxi    : Integer;
    begin
        maxi := Self.v(Index_arrV'First).x;
        for i in (Index_arrV'First+1)..Index_arrV'Last loop
            x := Self.v(i).x;
            if (x>maxi) then
                maxi := x;
            end if;
        end loop;
        return maxi;
    end maxX;

    function maxY(Self : in out Tetromino) return Integer is
        y       : Integer;
        maxi    : Integer;
    begin
        maxi := Self.v(Index_arrV'First).y;
        for i in (Index_arrV'First+1)..Index_arrV'Last loop
            y := Self.v(i).y;
            if (y>maxi) then
                maxi := y;
            end if;
        end loop;
        return maxi;
    end maxY;

    function Column(Self : in Tetromino) return Integer is
    begin
        return Integer(Self.x/CELL_SIZE);
    end Column;

    function isOutLeftLimit( Self : in out Tetromino) return Boolean is
        l : Integer;
    begin
        l := Self.minX*CELL_SIZE + Self.x;
        return l < 0;
    end isOutLeftLimit;

    function isOutRightLimit(Self : in out Tetromino) return Boolean is
        r : Integer;
    begin
        r := Self.maxX*CELL_SIZE + CELL_SIZE + Self.x;
        return r > (NB_COLUMNS*CELL_SIZE);
    end isOutRightLimit;

    function isOutBottomLimit(tetro : in out Tetromino) return Boolean is
        b : Integer;
    begin
        b := tetro.maxY*CELL_SIZE + CELL_SIZE + tetro.y;
        return b > (NB_ROWS*CELL_SIZE);
    end isOutBottomLimit;

    function hitGround(tetro : in out Tetromino; board : in arrBoard) return Boolean is
        sx,sy     : Integer;

        function hit(x : Integer; y : Integer) return Boolean is
            ix,iy : Integer;
        begin
            ix := x/CELL_SIZE;
            iy := y/CELL_SIZE;
            if (ix>=0) and (ix<NB_COLUMNS) and (iy>=0) and (iy<NB_ROWS) then
                if (board(iy*NB_COLUMNS+ix)/=0) then 
                    return True;
                end if;
            end if;
            return False;
        end hit;
        pragma Inline (hit);

    begin

        for p of tetro.v loop

            sx := Integer(p.x*CELL_SIZE + tetro.x + 1);
            sy := Integer(p.y*CELL_SIZE + tetro.y + 1);
            if hit(sx, sy) then
                return True;
            end if;

            sx := Integer(p.x*CELL_SIZE + CELL_SIZE -1 + tetro.x);
            sy := Integer(p.y*CELL_SIZE + tetro.y + 1);
            if hit(sx, sy) then
                return True;
            end if;

            sx := Integer(p.x*CELL_SIZE + CELL_SIZE - 1 + tetro.x);
            sy := Integer(p.y*CELL_SIZE + CELL_SIZE - 1 + tetro.y);
            if hit(sx, sy) then
                return True;
            end if;

            sx := Integer(p.x*CELL_SIZE + tetro.x + 1);
            sy := Integer(p.y*CELL_SIZE + CELL_SIZE - 1 + tetro.y);
            if hit(sx, sy) then
                return True;
            end if;

        end loop;

        return False;
    end hitGround;

    function isOutLRLimit( Self : in out Tetromino; veloH : Integer) return Boolean is
    begin
        if veloH<0 then
            return Self.isOutLeftLimit;
        elsif veloH>0 then
            return Self.isOutRightLimit;
        end if;
        return True;
    end isOutLRLimit;

end TetrisShape;

