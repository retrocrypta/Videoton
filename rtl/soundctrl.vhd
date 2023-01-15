----------------------------------------------------------------------------------
-- Engineer: Jozsef Laszlo ( rbendr AT gmail DOT com )
-- 
-- Create Date:    08:47:30 03/05/2017 
-- Design Name: 	 TVC Sound module 
-- Module Name:    soundctrl - Behavioral 
-- Project Name:   TVC Home computer VHDL version
-- Description: 
--						 TVC Sound generator 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: All rights reserved
-- Status: works
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;  

entity soundctrl is
    Port ( 
	      reset : in  STD_LOGIC;	        
			  clk : in  STD_LOGIC;
			clken : in  STD_LOGIC;
		  sndclk_en : in  STD_LOGIC;	   -- 3.125M
           din : in  STD_LOGIC_VECTOR (7 downto 0);
           iow : in  STD_LOGIC;
           ior : in  STD_LOGIC;
             a : in  STD_LOGIC_VECTOR (7 downto 0);
        sndint : out STD_LOGIC;
          aout : out STD_LOGIC_VECTOR (7 downto 0));
end soundctrl;

architecture Behavioral of soundctrl is

signal  vol : std_logic_vector( 3 downto 0); -- 4bits volume
signal freg : std_logic_vector(11 downto 0); -- 12bit freq registe
signal fcnt : std_logic_vector(11 downto 0); -- 12bit counter
signal fdiv : std_logic_vector( 3 downto 0); -- constant divider (div by 16)

signal snd_ena : std_logic := '0';
signal  snd_ei : std_logic := '0';
signal instart : std_logic;

begin

process(reset,clk)
begin
  if reset='0' then -- reset, active low resets all registers
    vol <= "0000";
    freg <= "000000000000";
    snd_ena <= '0';
    snd_ei  <= '0';
  elsif rising_edge(clk) then
    -- I/O write, sound ports
    if iow='0' and clken='1' then
        case a is
            when x"04" => freg(7 downto 0)<=din; 				   -- freq low
            when x"05" => freg(11 downto 8)<=din(3 downto 0); -- freq high
                          snd_ena <= din(4); -- sound enable
                          snd_ei  <= din(5); -- sound interrupt enable
            when x"06" => vol<=din(5 downto 2); -- volume 		 
            when others=>null;
        end case;
    end if;		
  end if;
end process;

process(clk)
begin
  if rising_edge(clk) then
    if sndclk_en = '1' then
        -- overflow, active low reset or instart active high reloads the counter
        if fcnt(11 downto 0)=x"FFF" or reset='0' or instart='1' then
          fcnt <= freg;
          fdiv <= fdiv + 1;
        else
          fcnt <= fcnt + 1;
        end if;
    end if;
    if reset='0' or instart='1' then
        fdiv <= "0000";
    end if;
  end if;
end process;

-- reload signal for the counter, cpu instruction: in a,($5b)
instart <= '1' when ior='0' and a(7 downto 3)="01011" and a(1 downto 0)="11" and clken='1' else '0';

sndint <= snd_ei and fdiv(3); -- active high, 1 if int req.
aout <= "00000000" when snd_ena='1' and fdiv(3)='1' else vol & "0000"; -- sound output

end Behavioral;


