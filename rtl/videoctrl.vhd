----------------------------------------------------------------------------------
-- Engineer: Jozsef Laszlo ( rbendr AT gmail DOT com )
-- 
-- Create Date:    08:47:30 03/05/2017 
-- Design Name: 	 TVC Video controller
-- Module Name:    videoctrl - Behavioral 
-- Project Name:   TVC Home computer VHDL version
-- Description: 
-- No clock tricks, fpga uses dual port ram, so no time stretch is required to 
-- access video data. Also means it's not cycle compatible with the original
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: All rights reserved
-- Status: works
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity videoctrl is
    Port (
            reset : in  STD_LOGIC;
           clk50m : in  STD_LOGIC;
        clken12_5 : in  STD_LOGIC;
        clken6_25 : in  STD_LOGIC;
       clken3_125 : in  STD_LOGIC;
            clken : in  STD_LOGIC;
               ma : in  STD_LOGIC_VECTOR (13 downto 0);
               ra : in  STD_LOGIC_VECTOR (4 downto 0);
               de : in  STD_LOGIC;				  
                -- cpu
               cs : in  STD_LOGIC;
               wr : in  STD_LOGIC;
              iow : in  STD_LOGIC;
                a : in  STD_LOGIC_VECTOR (13 downto 0);
              din : in  std_logic_vector(7 downto 0);
             dout : out std_logic_vector(7 downto 0);
             rgbi : out std_logic_vector(3 downto 0)
    );
end videoctrl;

architecture Behavioral of videoctrl is

type  
  vramarray is array(0 to 16383) of std_logic_vector(7 downto 0);

type 
  palette is array(0 to 3) of std_logic_vector(3 downto 0);

signal vram : vramarray;

-- IGRB
signal  pal : palette;

signal pxclk_en : std_logic;
signal vdata : std_logic_vector(7 downto 0);
signal pxpos : std_logic_vector(2 downto 0);
signal pxrgb : std_logic_vector(3 downto 0);
signal border : std_logic_vector(3 downto 0) := "0011"; -- IGRB
signal vm : std_logic_vector(1 downto 0) := "11" ; -- 2 4 16 colors 512 256 128

signal dereg : std_logic;
signal vload : std_logic;
signal sr : std_logic_vector(7 downto 0);
signal ps2,ps4 : std_logic_vector(1 downto 0);

begin

process(clk50m)
begin
  if rising_edge(clk50m) then
    vdata <= vram( conv_integer( ma(11 downto 6) & ra(1 downto 0) & ma(5 downto 0) ) );
    if wr='0' and cs='0' and clken='1' then
        vram( conv_integer(a) ) <= din;
    end if;
    dout <= vram( conv_integer(a) );
  end if;
end process;

process(reset,clk50m)
begin
  if reset='0' then
    border <= "0011";
    pal(0)<="0000";
    pal(1)<="0001";
    pal(2)<="0010";
    pal(3)<="0100";
  elsif rising_edge(clk50m) then
    -- i/o write
    if iow='0' then
        if a(7 downto 0)=x"00" then
            -- Border bits IGRB 7 5 3 1
            border <= din(7) & din (5) & din(3) & din(1);
        end if;
        if a(7 downto 0)=x"06" then
            -- video mod on port $06		
            vm <= din(1 downto 0);
        end if;
        if a(7 downto 4)=x"6" then
            -- Palette bits IGRB 6 4 2 0
            pal(conv_integer(a(1 downto 0))) <= din(6) & din (4) & din(2) & din(0);
        end if;		
	 end if;
  end if;
end process;

with vm select pxclk_en <=
  clken12_5 when "00",  
  clken6_25 when "01",
  clken3_125 when others;

process(clk50m)
begin
  if rising_edge(clk50m) then
    if pxclk_en = '1' then
        dereg<=de;
    end if;
  end if;
end process;
  
vload <= '1' when (vm="00" and pxpos="111") or (vm="01" and pxpos(1 downto 0)="11") or (vm(1)='1' and pxpos(0)='1') else '0';

process(clk50m)
begin
  if rising_edge(clk50m) then
    if pxclk_en = '1' then
        if de='1' then

            if vload='1' then
                sr <= vdata;
            else
                sr <= sr(6 downto 0) & '0';
            end if;

            pxpos<=pxpos+1;
        else  
            pxpos <= "111";
            sr <= vdata;
        end if;
    end if;
  end if;
end process;  

ps2 <= sr(3) & sr(7) when vm(0)='1' else '0' & sr(7);
 
pxrgb <= pal( conv_integer(ps2) ) when vm(1)='0' else sr(7)&sr(5)&sr(3)&sr(1);

rgbi <= pxrgb when dereg='1' else border;

end Behavioral;

 