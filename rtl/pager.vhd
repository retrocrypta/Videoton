----------------------------------------------------------------------------------
-- Engineer: Jozsef Laszlo ( rbendr AT gmail DOT com )
-- 
-- Create Date:    08:47:30 03/05/2017 
-- Design Name: 	 TVC Paging module 
-- Module Name:    pager - Behavioral 
-- Project Name:   TVC Home computer VHDL version
-- Description: 
--						 RAM/ROM paging for the TVC
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: All rights reserved
-- Status: Works. See TVC documentation for paging details
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;  

entity pager is
    Port (   clk : in  STD_LOGIC;
	        clken : in  STD_LOGIC;
			  reset : in  STD_LOGIC;
               a : in  STD_LOGIC_VECTOR (7 downto 0);
             din : in  STD_LOGIC_VECTOR (7 downto 0);
             iow : in  STD_LOGIC;
              ah : in  STD_LOGIC_VECTOR (1 downto 0);
			    a13 : in  STD_LOGIC;
            mreq : in  STD_LOGIC;
              np : out  STD_LOGIC_VECTOR(3 downto 0);
            nrom : out  STD_LOGIC;
           nvram : out  STD_LOGIC;
           ncart : out  STD_LOGIC;
           nrom5 : out  STD_LOGIC;
            nexp : out  STD_LOGIC_VECTOR(1 to 4)
		);
end pager;

architecture Behavioral of pager is

signal iomempg : std_logic_vector(1 downto 0);

signal page0,page1,page2,page3 : std_logic; 
												         -- 00   01   10    11
signal psel0 : std_logic_vector(1 downto 0); -- SYS  CART U0    U3
signal psel1 : std_logic; 						   -- U1   VID
signal psel2 : std_logic;						   -- VID  U2
signal psel3 : std_logic_vector(1 downto 0); -- CART SYS  U3    EXT

signal iomsel : std_logic;	

begin

  -- writing the ports 02 and 03 
  process(reset,clk) 
  begin
    if reset='0' then
        psel0<="00";
        psel1<='0';
        psel2<='0';
        psel3<="00";
        iomempg<="00";
    elsif rising_edge(clk) then
        if iow='0' and clken='1' then
            case a is
                when x"02" => 
                    psel0 <= din(4 downto 3);
                    psel1 <= din(2);
                    psel2 <= din(5);
                    psel3 <= din(7 downto 6);
                when x"03" => iomempg <= din(7 downto 6);
                when others => null;
            end case;
		 end if;
    end if;
  end process;

  page0 <= mreq or (ah(1) or ah(0));        -- 00 0 if  a15=0 and a14=0
  page1 <= mreq or (ah(1) or (not ah(0)));  -- 01
  page2 <= mreq or ((not ah(1)) or ah(0));  -- 10
  page3 <= mreq or (not (ah(1) and ah(0))); -- 11

  nrom <= '0' when (psel0="00" and page0='0') or (psel3="01" and page3='0') else '1';
  np(0) <= '0' when psel0="10" and page0='0' else '1';
  np(1) <= '0' when psel1='0' and page1='0' else '1';
  np(2) <= '0' when psel2='1' and page2='0' else '1';
  np(3) <= '0' when (psel0="11" and page0='0') or (psel3="10" and page3='0') else '1';
  nvram <= '0' when (psel1='1' and page1='0') or (psel2='0' and page2='0') else '1';
  ncart <= '0' when (psel0="01" and page0='0') or (psel3="00" and page3='0') else '1';
  nrom5 <= '0' when psel3="11" and page3='0' and a13='1' else '1'; -- rom externsion 8k  
  iomsel <= '0' when psel3="11" and page3='0' and a13='0' else '1';
  nexp(1) <= '0' when iomsel='0' and iomempg="00" else '1';
  nexp(2) <= '0' when iomsel='0' and iomempg="01" else '1';
  nexp(3) <= '0' when iomsel='0' and iomempg="10" else '1';
  nexp(4) <= '0' when iomsel='0' and iomempg="11" else '1';
  
end Behavioral;
