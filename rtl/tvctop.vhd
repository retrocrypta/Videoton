----------------------------------------------------------------------------------
-- Engineer: Jozsef Laszlo ( rbendr AT gmail DOT com )
-- 
-- Create Date:    08:47:30 03/05/2017 
-- Design Name: 	 TVC Top level
-- Module Name:    soundctrl - Behavioral 
-- Project Name:   TVC Home computer VHDL version
-- Description: 
--						 
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
use IEEE.NUMERIC_STD.ALL;

use work.mist.all;

entity tvctop is
  Port (  CLK75M : in STD_LOGIC;
          CLK50M : in STD_LOGIC;
       PLLLOCKED : in STD_LOGIC;
          RESET  : in STD_LOGIC;
             LED : out STD_LOGIC;
		SPEEDSEL : in STD_LOGIC_VECTOR(1 downto 0) := "00";

        -- ROM/CAS download
           DN_GO : in STD_LOGIC;
           DN_WR : in STD_LOGIC;
         DN_ADDR : in STD_LOGIC_VECTOR(24 downto 0);
         DN_DATA : in STD_LOGIC_VECTOR(7 downto 0);
          DN_IDX : in STD_LOGIC_VECTOR(5 downto 0);
       DN_CLKREF : out STD_LOGIC; -- DN_WR must be valid after the next cycle of DN_CLKREF
     CART_UNLOAD : in STD_LOGIC;

               R : out STD_LOGIC_VECTOR(5 downto 0);
               G : out STD_LOGIC_VECTOR(5 downto 0);
               B : out STD_LOGIC_VECTOR(5 downto 0);
              HS : out STD_LOGIC;
              VS : out STD_LOGIC;
          HBLANK : out STD_LOGIC;
          VBLANK : out STD_LOGIC;

            AOUT : out STD_LOGIC_VECTOR(7 downto 0);

          PS2CLK : in STD_LOGIC;
          PS2DAT : in STD_LOGIC;
            JOY0 : in STD_LOGIC_VECTOR(7 downto 0);
            JOY1 : in STD_LOGIC_VECTOR(7 downto 0);

     IMG_MOUNTED : in  std_logic_vector( 1 downto 0);
          IMG_WP : in  std_logic_vector( 1 downto 0);
        IMG_SIZE : in  std_logic_vector(31 downto 0); -- in bytes

          SD_LBA : out std_logic_vector(31 downto 0);
           SD_RD : out std_logic_vector( 1 downto 0);
           SD_WR : out std_logic_vector( 1 downto 0);
          SD_ACK : in  std_logic;
    SD_BUFF_ADDR : in  std_logic_vector( 8 downto 0);
         SD_DOUT : in  std_logic_vector( 7 downto 0);
          SD_DIN : out std_logic_vector( 7 downto 0);
  SD_DOUT_STROBE : in  std_logic;

        -- SDRAM
       SDRAM_nCS : out std_logic;                     -- Chip Select
        SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
         SDRAM_A : out std_logic_vector(12 downto 0);  -- SDRAM Address bus 13 Bits
      SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
      SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
       SDRAM_nWE : out std_logic;  -- SDRAM Write Enable
      SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
      SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
        SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
       SDRAM_CLK : out std_logic; -- SDRAM Clock
       SDRAM_CKE : out std_logic  -- SDRAM Clock Enable
       ); 
end tvctop;

architecture Behavioral of tvctop is

component sdram is
      port( sd_data : inout std_logic_vector(15 downto 0);
            sd_addr : out std_logic_vector(12 downto 0);
             sd_dqm : out std_logic_vector(1 downto 0);
              sd_ba : out std_logic_vector(1 downto 0);
              sd_cs : out std_logic;
              sd_we : out std_logic;
             sd_ras : out std_logic;
             sd_cas : out std_logic;
               init : in std_logic;
                clk : in std_logic;
             clkref : in std_logic;
                din : in std_logic_vector(7 downto 0);
               dout : out std_logic_vector(7 downto 0);
               addr : in std_logic_vector(24 downto 0);
                 oe : in std_logic;
                 we : in std_logic
      );
end component;

signal swres     : std_logic;
signal masterres : std_logic;

-- 0  1    2    3     4
-- 25 12.5 6.25 3.125 1.5625
signal clkdiv    : std_logic_vector(4 downto 0);

signal cpua     : std_logic_vector(15 downto 0); 
signal cpudo    : std_logic_vector(7 downto 0);
signal cpudi    : std_logic_vector(7 downto 0);
signal cpuwr,cpurd,cpumreq,cpuiorq,cpunmi,cpuint,cpum1,cpurfsh,clken,cpuclk_en : std_logic;
signal iorq,memrq : std_logic;

signal clken1_56, clken3_125, clken6_25, clken12_5 : std_logic;

signal nrom,nvram,ncart,nrom5 : std_logic;
signal np : std_logic_vector(3 downto 0);
signal nexp : std_logic_vector(1 to 4);
signal rambanksel : std_logic_vector(1 downto 0);

signal rgbi : std_logic_vector(3 downto 0);
signal bank : std_logic_vector(1 downto 0);
signal hspal : std_logic;
signal romdo,ramdo,extromdo : std_logic_vector(7 downto 0);
signal ioaddr : std_logic_vector(7 downto 0);
signal ior,iow,memr,memw : std_logic;
signal romsel,ramsel,vramsel : std_logic;

signal crtc_hs, crtc_vs : std_logic;
signal crtcde,crtcses,crtccur,crtcsel : std_logic;
signal crtcma : std_logic_vector(13 downto 0);
signal crtcra : std_logic_vector(4 downto 0);
signal crtcdo : std_logic_vector(7 downto 0);

signal vramdo : std_logic_vector(7 downto 0);
signal  vrgbi : std_logic_vector(3 downto 0);

signal hsreg,vsreg : std_logic;
signal hscnt : std_logic_vector(5 downto 0);
signal hblankctr : std_logic_vector(7 downto 0);
signal vblankctr : std_logic_vector(3 downto 0);

signal indata0 : std_logic_vector(7 downto 0); -- 58 59 5a 5b ... 5F

signal intreq : std_logic:='1';
signal sndint : std_logic;

signal keyrow : std_logic_vector(3 downto 0);
signal keycol : std_logic_vector(7 downto 0);

signal ledctr : std_logic_vector(23 downto 0); 

signal rgb : std_logic_vector(17 downto 0);

signal sdram_dqm  : std_logic_vector(1 downto 0);
signal ram_addr : std_logic_vector(24 downto 0);
signal  ram_din : STD_LOGIC_VECTOR(7 downto 0);
signal ram_dout : STD_LOGIC_VECTOR(7 downto 0);
signal ram_we: std_logic;
signal ram_oe: std_logic;

signal  dn_rom : std_logic;

signal   dn_wr_r : std_logic;
signal dn_addr_r : std_logic_vector(24 downto 0);
signal dn_data_r : std_logic_vector(7 downto 0); 
signal     dn_sr : std_logic_vector(3 downto 0) := "0000"; 

signal   romram : std_logic :='0';

signal cpu_ram_wr : std_logic;
signal cpu_ram_rd : std_logic;

signal cart_loaded : std_logic := '0';
signal rambsel2 : std_logic_vector(5 downto 0);
signal ledr : std_logic := '1';

signal hbfdo : std_logic_vector(7 downto 0);
signal hbf_iosel : std_logic;
signal hbf_oe : std_logic;
signal hbf_memrd : std_logic;
signal hbf_memwr : std_logic;
signal hbf_mema : std_logic_vector(2 downto 0);

begin

  masterres <= swres and pllLocked and (not RESET) and dn_rom and (not CART_UNLOAD);  -- 0 when reset (L active)

  SDRAM_CLK <= clk75m;

  process(clk50m)
  begin
    if rising_edge(clk50m) then
	   clkdiv <= clkdiv - 1;
	 end if;
  end process;

  -- sdram interface
  SDRAM_CKE <= '1';
  SDRAM_DQMH <= sdram_dqm(1);
  SDRAM_DQML <= sdram_dqm(0);   

  iorq <= cpuiorq or not cpum1;
  memrq <= cpumreq or not cpurfsh;

  ior <= cpurd or iorq;
  iow <= cpuwr or iorq;
  memr <= cpurd or memrq;
  memw <= cpuwr or memrq;
  bank <= cpua(15 downto 14);
  ioaddr <= cpua(7 downto 0);

  clken1_56  <= '1' when clkdiv(4 downto 0) = "00000" else '0';
  clken3_125 <= '1' when clkdiv(3 downto 0) = "0000" else '0';
  clken6_25  <= '1' when clkdiv(2 downto 0) = "000" else '0';
  clken12_5  <= '1' when clkdiv(1 downto 0) = "00" else '0';
  cpuclk_en <= '1' when
    (speedsel = "00"   and clken3_125 = '1') or -- 3.125M
    (speedsel = "01"   and clken6_25 = '1') or  -- 6.25M
    (speedsel(1) = '1' and clken12_5 = '1')     -- 12.5M
  else '0';

  crtcsel <= '1' when (ior='0' or iow='0') and cpua(7 downto 4)="0111" and clken='1' else '0'; -- in/out 70h/71h
  
  clken <= not dn_go;
  
  cpu : entity work.T80se
   port map (  
	   RESET_n => masterres,
		CLK_n   => clk50m,
		CLKEN   => cpuclk_en and clken,
		WAIT_n  => '1',
		INT_n   => intreq,
		NMI_n   => '1',
		BUSRQ_n => '1',
		M1_n    => cpum1,
		MREQ_n  => cpumreq,
		IORQ_n  => cpuiorq,
		RD_n    => cpurd,
		WR_n    => cpuwr,
		RFSH_n  => cpurfsh,
		HALT_n  => open,
		BUSAK_n => open,
		A       => cpua, 
		DI      => cpudi,
		DO      => cpudo
  );

  paging : entity work.pager
    port map (
		clk => clk50m,
	 clken => clken,
	 reset => masterres,	 
		  a => cpua(7 downto 0),  
		din => cpudo,	 
		iow => iow,	 
		 ah => cpua(15 downto 14),	 
		a13 => cpua(13),
	  mreq => cpumreq,	 
		 np => np,	 
	  nrom => nrom,
	 nvram => nvram,	 
	 ncart => ncart, 
	 nrom5 => nrom5,	 
	  nexp => nexp		
	 );
	 
  crtc : entity work.mc6845
  port map (
	CLOCK		=> clk50m,
	CLKEN		=> clken1_56,
	nRESET	=> masterres,

	-- Bus interface
	ENABLE	=> crtcsel,
	R_nW		=> cpuwr,
	RS			=> cpua(0),
	DI			=> cpudo,
	DO			=> crtcdo,

	-- Display interface
	VSYNC		=> crtc_vs,
	HSYNC		=> crtc_hs,
	DE			=> crtcde,
	CURSOR	=> crtccur,
	LPSTB		=> '0',

	-- Memory interface
	MA			=> crtcma,
	RA			=> crtcra
	);

  video: entity work.videoctrl 
    port map (
	  reset =>  masterres,
	  clk50m => clk50m,
	clken12_5 => clken12_5,
	clken6_25 => clken6_25,
  clken3_125 => clken3_125,
     clken => clken,
		  ma => crtcma,
		  ra => crtcra,
		  de => crtcde,
		  -- cpu
		  cs => nvram,--not vramsel,
		  wr => cpuwr,
		 iow => iow,
			a => cpua(13 downto 0),
		 din => cpudo,
		dout => vramdo,
		rgbi => vrgbi
		  );

--  rom : entity work.rom16k 
--    port map (
--   	 CLK => clkdiv(0),
--         A => cpua(13 downto 0),
--      DOUT => romdo
--		);
--		
--  extrom : entity work.extrom8k 
--    port map (
--   	 CLK => clkdiv(0),
--         A => cpua(12 downto 0),
--      DOUT => extromdo
--		);  
  
  kbd : entity work.ps2kbd 
    port map (
		RESET => pllLocked, 
		  CLK => clk50m,	 
		KBCLK => ps2clk,
		KBDAT => ps2dat,
		SWRES => swres,	 
		 JOY0 => joy0(7 downto 0),
		 JOY1 => joy1(7 downto 0),
	  KEYROW => keyrow,
	  KEYCOL => keycol
	 );
  
  -- sound generator
  sound : entity work.soundctrl 
    port map( 
		reset => masterres,	 
		  clk => clk50m,
		clken => clken,
	  sndclk_en => clken3_125, -- 3.125M
		  din => cpudo,
		  iow => iow,
		  ior => ior,
			 a => ioaddr,
	  sndint => sndint,
		 aout => aout
	);

  hbf_iosel <= '0' when cpua(7 downto 4) = x"2" else '1';

  -- Floppy controller
  hbf : entity work.hbf
    port map(
          reset => masterres,
            clk => clk50m,
      clken12_5 => clken12_5,
            din => cpudo,
           dout => hbfdo,
           iorq => iorq or hbf_iosel,
          memrq => memrq or nexp(2),
            rd  => cpurd,
            wr  => cpuwr,
           addr => cpua(12 downto 0),
          memrd => hbf_memrd,
          memwr => hbf_memwr,
           mema => hbf_mema,

     img_mounted => IMG_MOUNTED,
          img_wp => IMG_WP,
        img_size => IMG_SIZE,

          sd_lba => SD_LBA,
           sd_rd => SD_RD,
           sd_wr => SD_WR,
          sd_ack => SD_ACK,
    sd_buff_addr => SD_BUFF_ADDR,
         sd_dout => SD_DOUT,
          sd_din => SD_DIN,
  sd_dout_strobe => SD_DOUT_STROBE
    );

  -- I/O read
  process(clk50m)
  begin
    if rising_edge(clk50m) then
        if ior='0' and clken='1' then
            if ioaddr(7 downto 3)="01011" then -- 58..5F
                case ioaddr(1 downto 0) is
                    when   "00" => -- read key cols
                        indata0 <= keycol; --x"ff"; -- no key pressed
                    when   "01" => -- 7=printer,6=bw,5=tape,int requests (---43210)
                        indata0 <= "010" & intreq & "1111"; -- cursor/sound irq implemented only
                    when   "10" => -- expansion cards id byte
                        indata0 <= x"ff"; -- all slots unused
                    when others => 
                        indata0 <= x"ff";
                end case;
            end if;
        end if;
    end if;
  end process;

  -- I/O writes
  process(clk50m,masterres)
  begin
    if masterres='0' then
	   ledr <= '1';
    elsif rising_edge(clk50m) then
        if iow='0' and clken='1' then
            case ioaddr is
                when x"a0" => ledr <= not cpudo(7);
                when x"03" => keyrow <= cpudo(3 downto 0);
                when others=>null;
            end case;
        end if;
	 end if;
  end process;

  process(clk50m,masterres)
  begin
    if masterres='0' then
        intreq <= '1';
    elsif rising_edge(clk50m) then
      if iow='0' and ioaddr=x"07" then
        intreq<='1';
      elsif (crtccur='1' or sndint='1') and clken='1' then -- cursor or sound interrupt
        intreq<='0'; 
      end if;
    end if;
  end process;

  -- horizontal blanking
  process(clk50m)
  begin
    if rising_edge( clk50m ) then
      if clken12_5 = '1' then
        hsreg<=crtc_hs;

        if hsreg='1' and crtc_hs='0' then
          hscnt <= "110010"; -- 50 cycles / 4 us hsync time (LS123 with R=10k C=1n)
          hspal <= '1';
        elsif hscnt /= 0 then
          hscnt <= hscnt - 1;
        else
          hspal <= '0';
        end if;

        if hsreg='0' and crtc_hs='1' then
          hblankctr <= "00000000";
          hblank<='1';
        end if;

        if hblank='1' then
          hblankctr <= hblankctr + 1;
          if hblankctr=150 then --  300T@12.5MHz 12us blanking time
            hblank<='0';
          end if;
        end if;
      end if;    
    end if;
  end process;
  
  -- vertical blanking
  process(crtc_hs)
  begin
    if rising_edge(crtc_hs) then
      vsreg<=crtc_vs;
		if vsreg='0' and crtc_vs='1' then
		  vblankctr <= "0000";
		  VBLANK<='1';
		end if;		
		if vblank='1' then
		  vblankctr <= vblankctr + 1;
		  if vblankctr=5 then
		    VBLANK<='0';
		  end if;		
		end if;
	 end if;
  end process;

  HS <= hspal;
  VS <= VBLANK;

  cpudi <= --romdo when nrom='0' else
           --extromdo when nrom5='0' else

--			  ramdo when nrom='0' else
--			  ramdo when nrom5='0' else
--			  vramdo when nvram='0' else
--			  ramdo when np/="1111" else
--			  indata0 when ior='0' else
--			  x"ff";

			  vramdo when nvram='0' else
			  crtcdo when crtcsel='1' else
			  hbfdo when ior='0' and hbf_iosel = '0' else
			  indata0 when ior='0' else
			  ramdo when np/="1111" or nrom='0' or nrom5='0' or (ncart='0' and cart_loaded='1') or hbf_memrd='0' else
			  ramdo when np/="1111" or nrom='0' or nrom5='0' or (ncart='0' and cart_loaded='1') else
			  x"ff";

  -- IGRB
  R <= "111000" when vrgbi(3)='1' and vrgbi(1)='1' else
       "011100" when vrgbi(3)='0' and vrgbi(1)='1' else
       "000000";
  G <= "111000" when vrgbi(3)='1' and vrgbi(2)='1' else
       "011100" when vrgbi(3)='0' and vrgbi(2)='1' else
       "000000";
  B <= "111000" when vrgbi(3)='1' and vrgbi(0)='1' else
       "011100" when vrgbi(3)='0' and vrgbi(0)='1' else
       "000000";

  with np select rambanksel <=
   "00" when "1110", 
	"01" when "1101",
	"10" when "1011",
	"11" when others;
  
  ramsel <= '0' when np/="1111" else '1';
  
  -- sdram clocked @ 75MHz
  sdram_inst : sdram
    port map( sd_data => SDRAM_DQ,
              sd_addr => SDRAM_A,
               sd_dqm => sdram_dqm,
                sd_cs => SDRAM_nCS,
                sd_ba => SDRAM_BA,
                sd_we => SDRAM_nWE,
               sd_ras => SDRAM_nRAS,
               sd_cas => SDRAM_nCAS,
                  clk => clk75m,
               clkref => clkdiv(3),
                 init => not pllLocked,
                  din => ram_din,
                 addr => ram_addr,
                   we => ram_we,
                   oe => ram_oe,
                 dout => ram_dout
    );

  ramdo <= ram_dout;

  -- nrom    (16k)
  -- nrom5    (8k) - duplicated
  -- hbf rom (16k)
  -- hbf ram (16k)
  -- ncart   (16k)
  romram <= not (nrom and nrom5); -- romram=1 if nrom or nrom5 selected (0)

  rambsel2 <= "0100"&cpua(13 downto 12) when nrom='0' else
              "01010"&cpua(12) when nrom5='0' else
              "011"&hbf_mema when hbf_memrd='0' or hbf_memwr='0' else
              "1000"&cpua(13 downto 12) when ncart='0' else
              "00"&rambanksel&cpua(13 downto 12);

  ram_addr <= "0000000" & rambsel2 & cpua(11 downto 0) when dn_go='0' else dn_addr_r;
  ram_din <= cpudo when dn_go='0' else dn_data_r;

  cpu_ram_wr <= not ((ramsel and hbf_memwr) or cpuwr);
  cpu_ram_rd <= not ((ramsel and nrom and nrom5 and ncart and hbf_memrd) or cpurd);

  ram_we <= cpu_ram_wr when dn_go='0' else dn_wr_r;
  ram_oe <= cpu_ram_rd when dn_go='0' else '0';

  dn_rom <= '0' when dn_go='1' and (dn_idx=0 or dn_idx=2) else '1';
  DN_CLKREF <= clken12_5;

  LED <= ledr when dn_go='0' else '0'; 	 

  process(CLK50M)
  begin
    if rising_edge(CLK50M) then
      if DN_GO='1' and DN_IDX=2 then
        cart_loaded <= '1';
      end if;
      if CART_UNLOAD = '1' then
        cart_loaded <= '0';
      end if;
    end if;
  end process;

  process(dn_idx, dn_addr, dn_wr, dn_data)
  begin
    dn_wr_r <= '0';
    dn_data_r <= dn_data;
    dn_addr_r <= dn_addr + x"10000";
    if ((dn_idx=1 and dn_addr>143) or (dn_idx=0) or (dn_idx=2)) then
      dn_wr_r <= dn_wr;
      if dn_idx=1 then
        dn_addr_r <= dn_addr+6495; -- cas
      elsif dn_idx=2 then
        dn_addr_r <= dn_addr + x"20000"; --cart
      end if;
    end if;
  end process;

end Behavioral;
