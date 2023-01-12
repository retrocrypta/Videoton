----------------------------------------------------------------------------------
-- Engineer: Gyorgy Szombathelyi
-- 
-- Create Date:    01/12/2023
-- Design Name:    TVC Floppy Controller module
-- Module Name:    hbf - Behavioral
-- Project Name:   TVC Home computer VHDL version
-- Description: 
--                 TVC Floppy Controller
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

entity hbf is
    Port ( 
         reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
     clken12_5 : in STD_LOGIC;
           din : in  STD_LOGIC_VECTOR (7 downto 0);
          dout : out STD_LOGIC_VECTOR(7 downto 0);
          iorq : in STD_LOGIC;
         memrq : in STD_LOGIC;
           rd  : in STD_LOGIC;
           wr  : in STD_LOGIC;
          addr : in STD_LOGIC_VECTOR(12 downto 0);
         memrd : out STD_LOGIC;
         memwr : out STD_LOGIC;
          mema : out STD_LOGIC_VECTOR(2 downto 0); -- addr(14 downto 12)

   img_mounted : in  std_logic_vector( 1 downto 0);
        img_wp : in  std_logic_vector( 1 downto 0);
      img_size : in  std_logic_vector(31 downto 0); -- in bytes

        sd_lba : out std_logic_vector(31 downto 0);
         sd_rd : out std_logic_vector( 1 downto 0);
         sd_wr : out std_logic_vector( 1 downto 0);
        sd_ack : in  std_logic;
  sd_buff_addr : in  std_logic_vector( 8 downto 0);
       sd_dout : in  std_logic_vector( 7 downto 0);
        sd_din : out std_logic_vector( 7 downto 0);
sd_dout_strobe : in  std_logic
    );
end hbf;

architecture Behavioral of hbf is
    component fdc1772 is
        generic (
            MODEL            : integer := 3;
            CLK_EN           : integer := 12500;
            INVERT_HEAD_RA   : boolean := true;
            EXT_MOTOR        : boolean := true
        );
        port (
            clkcpu           : in  std_logic;
            clk8m_en         : in  std_logic;
    
            floppy_drive     : in  std_logic_vector( 3 downto 0);
            floppy_side      : in  std_logic;
            floppy_reset     : in  std_logic;
            floppy_motor     : in  std_logic;
    
            irq              : out std_logic;
            drq              : out std_logic;
    
            cpu_addr         : in  std_logic_vector( 1 downto 0);
            cpu_sel          : in  std_logic;
            cpu_rw           : in  std_logic;
            cpu_din          : in  std_logic_vector( 7 downto 0);
            cpu_dout         : out std_logic_vector( 7 downto 0);
    
            img_type         : in  std_logic_vector( 2 downto 0);
            img_mounted      : in  std_logic_vector( 1 downto 0);
            img_wp           : in  std_logic_vector( 1 downto 0);
            img_ds           : in  std_logic;
            img_size         : in  std_logic_vector(31 downto 0); -- in bytes
    
            sd_lba           : out std_logic_vector(31 downto 0);
            sd_rd            : out std_logic_vector( 1 downto 0);
            sd_wr            : out std_logic_vector( 1 downto 0);
            sd_ack           : in  std_logic;
            sd_buff_addr     : in  std_logic_vector( 8 downto 0);
            sd_dout          : in  std_logic_vector( 7 downto 0);
            sd_din           : out std_logic_vector( 7 downto 0);
            sd_dout_strobe   : in  std_logic
    );
    end component;

signal fdc_sel   : std_logic;
signal fdd_sel   : std_logic;
signal ctrl_sel  : std_logic;
signal rom_a1312 : std_logic_vector(1 downto 0);
signal fdc_reset : std_logic := '0';
signal fdc_dout  : std_logic_vector(7 downto 0) := x"00";
signal fdc_irq   : std_logic;
signal fdc_drq   : std_logic;
signal ds0       : std_logic;
signal ds1       : std_logic;
signal motor_on  : std_logic;
signal sidesel   : std_logic;

begin

fdc_sel  <= '1' when iorq = '0' and addr(3 downto 2) = "00" else '0';
fdd_sel  <= '1' when iorq = '0' and addr(3 downto 2) = "01" else '0';
ctrl_sel <= '1' when iorq = '0' and addr(3 downto 2) = "10" else '0';

memrd <= memrq or rd;
memwr <= memrq or wr or not addr(12);
mema <= '0' & rom_a1312 when addr(12) = '0' else "100"; -- addr(12) selects ROM or RAM

dout <= fdc_dout when fdc_sel = '1' else
        fdc_drq&"000000"&fdc_irq when fdd_sel = '1' else
        x"FF";

process(reset,clk)
begin
  if reset='0' then -- reset, active low resets all registers
	fdc_reset <= '0';
	rom_a1312 <= "00";
	ds0 <= '0';
	ds1 <= '0';
	motor_on <= '0';
	sidesel <= '0';
  elsif rising_edge(clk) then
    if wr='0' then
        if ctrl_sel = '1' then
            fdc_reset <= '1';
            rom_a1312 <= din(5 downto 4);
        end if;
        if fdd_sel = '1' then
            ds0 <= din(0);
            ds1 <= din(1);
            --ds2 <= din(2);
            --ds3 <= din(3);
            --hld <= din(4);
            --dden <= din(5);
            motor_on <= din(6);
            sidesel <= din(7);
        end if;
    end if;
  end if;
end process;

fdc : fdc1772
port map
(
    clkcpu  => clk,
    clk8m_en => clken12_5,

    floppy_drive => "11"&not ds1&not ds0,
    floppy_side => not sidesel,
    floppy_reset => fdc_reset,
    floppy_motor => motor_on,

    irq => fdc_irq,
    drq => fdc_drq,

    cpu_addr => addr(1 downto 0),
    cpu_sel => fdc_sel,
    cpu_rw => wr,
    cpu_din => din,
    cpu_dout => fdc_dout,

    -- The following signals are all passed in from the Top module
    img_mounted => img_mounted,
    img_wp => img_wp,
    img_size => img_size,
    img_ds => '1',
    img_type => "001",

    sd_lba => sd_lba,
    sd_rd => sd_rd,
    sd_wr => sd_wr,
    sd_ack => sd_ack,
    sd_buff_addr => sd_buff_addr,
    sd_dout => sd_dout,
    sd_din => sd_din,
    sd_dout_strobe => sd_dout_strobe
);

end Behavioral;

