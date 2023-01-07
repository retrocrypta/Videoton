----------------------------------------------------------------------------------
-- Engineer: Jozsef Laszlo ( rbendr AT gmail DOT com )
-- 
-- Create Date:    08:47:30 03/05/2017 
-- Design Name: 	 TVC Keyboard interface
-- Module Name:    pager - Behavioral 
-- Project Name:   TVC Home computer VHDL version
-- Description: 
--		PS2 codes mapped to TVC key codes
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: All rights reserved
-- Status: Works. See TVC documentation for details
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

	
entity ps2kbd is
    Port ( 
	   RESET : in  STD_LOGIC;
 	   KBCLK : in  STD_LOGIC;
	   KBDAT : in  STD_LOGIC;
		SWRES : out STD_LOGIC;
		 JOY0 : in  STD_LOGIC_VECTOR(7 downto 0);
		 JOY1 : in  STD_LOGIC_VECTOR(7 downto 0);
	     CLK : in  STD_LOGIC;
	  KEYROW : in  STD_LOGIC_VECTOR(3 downto 0);
	  KEYCOL : out STD_LOGIC_VECTOR(7 downto 0)		 
		  );
			  
end ps2kbd;

architecture Behavioral of ps2kbd is

type keys is array(0 to 9) of std_logic_vector(7 downto 0);
 
signal keypress : std_logic:='0';
signal extkey   : std_logic:='0';

signal hasRead  : std_logic;
signal kbdsreg  : std_logic;

signal keybits : keys;
signal keyregs : keys;
signal lastkc : std_logic;

signal kbdsign : std_logic;
signal kbddata : std_logic_vector(7 downto 0);
signal swreset : std_logic := '1';

signal  YZSWAP : std_logic := '1';
signal JOYSWAP : std_logic := '0';


begin

  ps2rd : entity work.ps2reader
  port map (
	 mclk => CLK,
	 PS2C => KBCLK, 
	 PS2D => KBDAT,
	 rst => not RESET,
	 Ps2Dout => kbddata,
	 fRd => kbdsign
  );
    
  process(CLK,RESET,kbdsign,kbddata)
  variable kk : std_logic_vector(6 downto 0);
  variable ix : integer;
  begin
   if RESET='0' then
	   keypress <= '0';
		keybits(0) <= "00000000";
		keybits(1) <= "00000000";
		keybits(2) <= "00000000";
		keybits(3) <= "00000000";
		keybits(4) <= "00000000";
		keybits(5) <= "00000000";
		keybits(6) <= "00000000";
		keybits(7) <= "00000000";
		keybits(8) <= "00000000";
		keybits(9) <= "00000000";
		
		extkey <= '0';
		swreset <= '1';
		YZSWAP <= '1';
		JOYSWAP <= '0';
	else
	
	  if rising_edge(kbdsign) then
	     
		  --kbdsreg<=kbdsign;
	  	  
	     --if kbdsreg='0' and kbdsign='1' then
			if kbddata=x"F0" then
			  keypress <= '0'; -- released  
			  --if shifrpress='1' then
			--		keybits(7)(0)<='0';
			--		shifrpress<='0';
			  --end if;
			elsif kbddata=x"E0" then
			  extkey<='1';
			else
			  keypress <= '1'; -- pressed
			  
			  -- this is for ps2 read. we convert 0x83 to 0x02 (keyboard F2)
			  kk:= kbddata(6 downto 0);
			  if kbddata=x"83" then 
			    kk:="0000010"; -- keyboard F7 code 0x83 converted to 0x02
			  end if;			

				
			  
			  case '0' & kk is
			  						
				when x"03"=> -- F5
								if keypress='1' then YZSWAP <= not YZSWAP; end if;
				when x"0b"=> -- F6
								if keypress='1' then JOYSWAP <= not JOYSWAP; end if;
								
				when x"78"=> swreset <=	not keypress; -- F11
								
				-- \| U"
				when x"5d"=> keybits(5)(5)<=keypress;
				-- A
				when x"1c"=> keybits(4)(6)<=keypress;
				-- B
				when x"32"=> keybits(6)(0)<=keypress;
				-- C
				when x"21"=> keybits(6)(1)<=keypress;
				-- D
				when x"23"=> keybits(4)(1)<=keypress;
				-- E
				when x"24"=> keybits(2)(1)<=keypress;
				-- F
				when x"2b"=> keybits(4)(7)<=keypress;
				-- G
				when x"34"=> keybits(4)(0)<=keypress;

				-- H
				when x"33"=> keybits(4)(4)<=keypress;
				-- I
				when x"43"=> keybits(3)(1)<=keypress;
				-- J
				when x"3B"=> keybits(5)(7)<=keypress;
				-- K
				when x"42"=> keybits(5)(1)<=keypress;
				-- L
				when x"4B"=> keybits(5)(2)<=keypress;
				-- M
				when x"3A"=> keybits(7)(7)<=keypress;
				-- N
				when x"31"=> keybits(6)(4)<=keypress;
				-- O
				when x"44"=> keybits(3)(2)<=keypress;

				-- P
				when x"4D"=> keybits(3)(6)<=keypress;
				-- Q
				when x"15"=> keybits(2)(6)<=keypress;
				-- R
				when x"2D"=> keybits(2)(7)<=keypress;
				-- S
				when x"1B"=> keybits(4)(2)<=keypress;
				-- T
				when x"2C"=> keybits(2)(0)<=keypress;
				-- U
				when x"3C"=> keybits(3)(7)<=keypress;
				-- V
				when x"2A"=> keybits(6)(7)<=keypress;
				-- W
				when x"1D"=> keybits(2)(2)<=keypress;

				-- X
				when x"22"=> keybits(6)(2)<=keypress;	
				
				-- Y
				when x"35"=> --keybits(6)(6)<=keypress;
					if YZSWAP='1' then
						keybits(2)(4)<=keypress;
					else
					   keybits(6)(6)<=keypress;
					end if;				
				-- Z
				when x"1A"=> --keybits(2)(4)<=keypress;
					if YZSWAP='0' then
						keybits(2)(4)<=keypress;
					else
					   keybits(6)(6)<=keypress;
					end if;				
				
				-- F2
				when x"06"=> keybits(3)(4)<=keypress;
				-- F3
				when x"04"=> keybits(3)(0)<=keypress;
				-- F4
				when x"0C"=> keybits(2)(5)<=keypress;
				-- F1 TVC ; $
				when x"05"=> keybits(2)(3)<=keypress;

				-- -_  TVC U:
				when x"4e"=> keybits(1)(3)<=keypress;
				-- =+  TVC O'
				when x"55"=> keybits(1)(5)<=keypress;
				-- 0 TVC O:
				when x"45"=> keybits(1)(6)<=keypress;
				-- 1
				when x"16"=> keybits(0)(6)<=keypress;
				-- 2
				when x"1E"=> keybits(0)(2)<=keypress;
				-- 3
				when x"26"=> keybits(0)(1)<=keypress;
				-- 4
				when x"25"=> keybits(0)(7)<=keypress;
				-- 5
				when x"2E"|x"73" => keybits(0)(0)<=keypress;
				-- 6
				when x"36"=> keybits(0)(4)<=keypress;
				-- 7
				when x"3D"=> keybits(1)(7)<=keypress;

				-- 8
				when x"3E"=> keybits(1)(1)<=keypress;
				-- 9
				when x"46"=> keybits(1)(2)<=keypress;

				-- [ O"
				when x"54"=> keybits(3)(3)<=keypress;
				-- ] U'
				when x"5b"=> keybits(3)(5)<=keypress;
				-- ' A'
				when x"52"=> keybits(5)(3)<=keypress;

				-- ENTER
				when x"5A"=> keybits(5)(4)<=keypress;
				
				-- HOME TVC: ~^
				when x"6C"=> 
						if extkey='1' then
							keybits(1)(0)<=keypress;
						else
							keybits(1)(7)<=keypress;
						end if;
				-- END TVC: \|
				when x"69"=> 
						if extkey='1' then	
							keybits(4)(3)<=keypress;
						else
							keybits(0)(6)<=keypress;
						end if;				
						
				-- PGUP TVC: <>
				when x"7d"=> 
						if extkey='1' then	
							keybits(4)(5)<=keypress;
						else
							keybits(1)(2)<=keypress;
						end if;				

				-- PGDN TVC: <>
				when x"7a"=> 
						if extkey='1' then	
							keybits(4)(5)<=keypress;
							keybits(6)(3)<=keypress;
						else
							keybits(0)(1)<=keypress;							
						end if;				

				-- ESC
				when x"76"=> keybits(7)(3)<=keypress;
				-- up-arrow
				when x"75"=> --keybits(8)(1)<=keypress;
					if extkey='1' then
						if JOYSWAP='0' then
							keybits(8)(1)<=keypress;
						else
							keybits(9)(1)<=keypress;
						end if;
					else
						keybits(1)(1)<=keypress;
					end if;					
				-- dn-arrow
				when x"72"=> --keybits(8)(2)<=keypress;
					if extkey='1' then
						if JOYSWAP='0' then
							keybits(8)(2)<=keypress;
						else
							keybits(9)(2)<=keypress;
						end if;				
					else
						keybits(0)(2)<=keypress;
					end if;
				
				-- backspace
				when x"66"=> keybits(5)(0)<=keypress;
				
				-- lf-arrow 
				when x"6B"=> --keybits(8)(6)<=keypress;
					if extkey='1' then
						if JOYSWAP='0' then
							keybits(8)(6)<=keypress;
						else
							keybits(9)(6)<=keypress;
						end if;
					else
						keybits(0)(7)<=keypress;
					end if;
						
				-- rg-arrow
				when x"74"=> --keybits(8)(5)<=keypress;
					if extkey='1' then
						if JOYSWAP='0' then
							keybits(8)(5)<=keypress;
						else
							keybits(9)(5)<=keypress;
						end if;				
					else
						keybits(0)(4)<=keypress;
					end if;
				
				-- SPA
				when x"29"=> keybits(7)(5)<=keypress;

				-- TAB => joy fire
				when x"0d"=> --keybits(8)(3)<=keypress;
					if JOYSWAP='0' then
						keybits(8)(3)<=keypress;
					else
						keybits(9)(3)<=keypress;
					end if;

				-- L-SHIFT R-SHIFT
				when x"12"|x"59"=> keybits(6)(3)<=keypress;
				
				-- CTRL
				when x"14"=> keybits(7)(4)<=keypress;
				-- ALT
				when x"11"=> keybits(7)(0)<=keypress;
				-- CAPS LOCK
				when x"58"=> keybits(6)(5)<=keypress;
				-- INS
				when x"70"=> 
					if extkey='1' then
						keybits(8)(0)<=keypress; -- INS
					else
						keybits(0)(3)<=keypress; -- 0
					end if;						
				-- DEL
				when x"71"=> 
					if extkey='1' then
						keybits(5)(0)<=keypress; -- DEL
					else
						keybits(7)(2)<=keypress; -- .
					end if;
				
				-- `~ => TVC 0 &
				when x"0e"=> keybits(0)(3)<=keypress;
				-- ,<
				when x"41"=> keybits(7)(1)<=keypress;
				-- .>
				when x"49"=> keybits(7)(2)<=keypress;
				-- /?
				when x"4a"=> 
					if extkey='1' then
					   -- numpad /
						keybits(0)(4)<=keypress;
						keybits(6)(3)<=keypress;
					else
						keybits(7)(6)<=keypress;
					end if;
				-- *
				when x"7c"=> keybits(1)(4)<=keypress; 							
				-- ;: => TVC E
				when x"4c"=> keybits(5)(6)<=keypress; 
				
				-- Hungarian i'
				when x"61"=> keybits(0)(5)<=keypress; 
				
				-- numpad +
				when x"79"=> keybits(0)(1)<=keypress; 
								 keybits(6)(3)<=keypress;
				-- numpad -
				when x"7b"=> keybits(7)(6)<=keypress; 							 
				
			   when others =>
				  null;
			  end case;
			  extkey<='0';
			end if; 
		  --end if;	
     end if;	  
	end if;
  end process;
  
  keyregs(0) <= keybits(0);
  keyregs(1) <= keybits(1);
  keyregs(2) <= keybits(2);
  keyregs(3) <= keybits(3);
  
  keyregs(4) <= keybits(4);
  keyregs(5) <= keybits(5);
  keyregs(6) <= keybits(6);
  keyregs(7) <= keybits(7);

  keyregs(8)(0) <= keybits(8)(0);
  keyregs(8)(1) <= keybits(8)(1) or JOY1(3);
  keyregs(8)(2) <= keybits(8)(2) or JOY1(2);
  keyregs(8)(3) <= keybits(8)(3) or JOY1(4);
  keyregs(8)(5) <= keybits(8)(5) or JOY1(0);
  keyregs(8)(6) <= keybits(8)(6) or JOY1(1);
  
  keyregs(9)(1) <=  JOY0(3);
  keyregs(9)(2) <=  JOY0(2);
  keyregs(9)(3) <=  JOY0(4);
  keyregs(9)(5) <=  JOY0(0);
  keyregs(9)(6) <=  JOY0(1);

  
  SWRES <= swreset;
  KEYCOL <= not keyregs( conv_integer(KEYROW) );
  
end Behavioral;
