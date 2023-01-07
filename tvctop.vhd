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
 

entity tvctop is
    Port ( CLK27M : in  STD_LOGIC;
              RGB : out  STD_LOGIC_VECTOR (17 downto 0);
            HSYNC : out  STD_LOGIC;
            VSYNC : out  STD_LOGIC;
              LED : out  STD_LOGIC;
			  AUDIOL : out  STD_LOGIC;
			  AUDIOR : out  STD_LOGIC;
			  
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
		  SDRAM_CKE : out std_logic; -- SDRAM Clock Enable					  
			  
		-- SPI interface to arm io controller
			SPI_DO	: out std_logic;
			SPI_DI	: in  std_logic;
			SPI_SCK	: in  std_logic;
			SPI_SS2	: in  std_logic;
			SPI_SS3	: in  std_logic;
			SPI_SS4	: in  std_logic;
		CONF_DATA0  : in  std_logic			  
           ); 
end tvctop;

architecture Behavioral of tvctop is

component data_io
  port ( sck, ss, sdi 	:	in std_logic;

			-- download info
			downloading  	:  out std_logic;
			--size				:  out std_logic_vector(24 downto 0);
			index				:  out std_logic_vector(4 downto 0);
  
			-- external ram interface
			clk				:	in std_logic;
			wr					:  out std_logic;
			addr   			:  out std_logic_vector(24 downto 0);
			data				:  out std_logic_vector(7 downto 0)
);
end component data_io;

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

component osd
  generic ( OSD_COLOR : integer );
  port ( pclk 			: in std_logic;
		sck, sdi, ss 	: in std_logic;

		-- VGA signals coming from core
      red_in 			: in std_logic_vector(5 downto 0);
      green_in 		: in std_logic_vector(5 downto 0);
      blue_in 			: in std_logic_vector(5 downto 0);
      hs_in 			: in std_logic;
      vs_in 			: in std_logic;

      -- VGA signals going to video connector
      red_out		 	: out std_logic_vector(5 downto 0);
      green_out 		: out std_logic_vector(5 downto 0);
      blue_out 		: out std_logic_vector(5 downto 0);
      hs_out 			: out std_logic;
      vs_out 			: out std_logic
	);
end component osd;
 
component user_io
      generic ( STRLEN : integer := 0 );
      port ( 			
			-- ps2 interface		 
			SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
         SPI_MISO : out std_logic;
         conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
         joystick_0 : out std_logic_vector(7 downto 0);
         joystick_1 : out std_logic_vector(7 downto 0);
			status: out std_logic_vector(7 downto 0);			  
			ps2_clk        : in std_LOGIC;
			ps2_kbd_clk    : out std_logic;
			ps2_kbd_data   : out std_logic;
			ps2_mouse_clk  : out std_logic;
			ps2_mouse_data : out std_logic
      );
		end component user_io;
  
constant CONF_STR : string := "TVC;CAS;O1,Scanlines,On,Off;T2,Reset";
  
  function to_slv(s: string) return std_logic_vector is
    constant ss: string(1 to s'length) := s;
    variable rval: std_logic_vector(1 to 8 * s'length);
    variable p: integer;
    variable c: integer;
  
  begin
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8));
    end loop;
    return rval;

  end function;   

signal clk100m   : std_logic;
signal clk50m    : std_logic;
signal pllLocked : std_logic;
signal swres     : std_logic;
signal masterres : std_logic;

-- 0  1    2    3     4
-- 25 12.5 6.25 3.125 1.5625
signal clkdiv    : std_logic_vector(23 downto 0);

signal cpua     : std_logic_vector(15 downto 0); 
signal cpudo    : std_logic_vector(7 downto 0);
signal cpudi    : std_logic_vector(7 downto 0);
signal cpuwr,cpurd,cpumreq,cpuiorq,cpunmi,cpuint,cpum1,cpuclk,clken : std_logic;

signal nrom,nvram,ncart,nrom5 : std_logic;
signal np : std_logic_vector(3 downto 0);
signal nexp : std_logic_vector(1 to 4);
signal rambanksel : std_logic_vector(1 downto 0);

signal rgbi : std_logic_vector(3 downto 0);
signal bank : std_logic_vector(1 downto 0);
signal hs,vs,hspal : std_logic;
signal romdo,ramdo,extromdo : std_logic_vector(7 downto 0);
signal ioaddr : std_logic_vector(7 downto 0);
signal ior,iow,memr,memw : std_logic;
signal romsel,ramsel,vramsel : std_logic;

signal chrclk,crtcde,crtcses,crtccur,crtcsel : std_logic;
signal crtcma : std_logic_vector(13 downto 0);
signal crtcra : std_logic_vector(4 downto 0);
signal crtcdo : std_logic_vector(7 downto 0);

signal vramdo : std_logic_vector(7 downto 0);
signal  vrgbi : std_logic_vector(3 downto 0);

signal hsreg,vsreg,hblank,vblank : std_logic;
signal hblankctr : std_logic_vector(7 downto 0);
signal vblankctr : std_logic_vector(3 downto 0);
signal red,green,blue : std_logic_vector(5 downto 0);

signal indata0 : std_logic_vector(7 downto 0); -- 58 59 5a 5b ... 5F

signal intreq : std_logic:='1';
signal sndint : std_logic;
signal dacout : std_logic;
signal aout : std_logic_vector(7 downto 0);

signal keyrow : std_logic_vector(3 downto 0);
signal keycol : std_logic_vector(7 downto 0);

signal ps2clkout : std_logic;

signal PS2CLK : std_logic;
signal PS2DAT : std_logic;

signal MPS2CLK : std_logic;
signal MPS2DAT : std_logic;

signal joy0 : std_logic_vector(7 downto 0);
signal joy1 : std_logic_vector(7 downto 0);

signal status: std_logic_vector(7 downto 0);			  

signal ledctr : std_logic_vector(23 downto 0); 

signal pxclk : std_logic;
signal tvc_r,tvc_g,tvc_b : std_logic_vector(5 downto 0);
signal tvc_hs,tvc_vs : std_logic;
signal out_rgb : std_logic_vector(17 downto 0);

signal sdram_dqm  : std_logic_vector(1 downto 0);
signal ram_addr : std_logic_vector(24 downto 0);
signal  ram_din : STD_LOGIC_VECTOR(7 downto 0);
signal ram_dout : STD_LOGIC_VECTOR(7 downto 0);
signal ram_we: std_logic;
signal ram_oe: std_logic;
 

signal  dn_clk : std_logic;
signal   dn_go : std_logic;
signal   dn_wr : std_logic;
signal dn_addr : std_logic_vector(24 downto 0);
signal dn_data : std_logic_vector(7 downto 0);
signal  dn_idx : std_logic_vector(4 downto 0);
signal  dn_rom : std_logic;

signal   dn_wr_r : std_logic;
signal dn_addr_r : std_logic_vector(24 downto 0);
signal dn_data_r : std_logic_vector(7 downto 0); 
signal     dn_sr : std_logic_vector(3 downto 0) := "0000"; 

signal speedsel : std_logic_vector(1 downto 0) := "00";
signal   romram : std_logic :='0';

signal cpu_ram_wr : std_logic;
signal cpu_ram_rd : std_logic;

signal   rambsel2 : std_logic_vector(1 downto 0);
signal ledr : std_logic := '1';

begin

  masterres <= swres and pllLocked and (not status(0)) and (not status(2)) and dn_rom;  -- 0 when reset (L active)
  
  clocks : entity work.pll
   port map (
		inclk0 => clk27m,
 		c0 	 => clk100m,
		c1 	 => SDRAM_CLK, --clk50m2,
		locked => pllLocked
	);

  --SDRAM_CLK <= clk50m2;

  process(clk100m)
  begin
    if rising_edge(clk100m) then
	   clk50m <= not clk50m;
	 end if;
  end process;
  
  process(clk50m)
  begin
    if rising_edge(clk50m) then
	   clkdiv <= clkdiv - 1;
	 end if;
  end process;
  ps2clkout <= clkdiv(11);

  -- sdram interface
  SDRAM_CKE <= '1';
  SDRAM_DQMH <= sdram_dqm(1);
  SDRAM_DQML <= sdram_dqm(0);   
  
  ior <= cpurd or cpuiorq or (not cpum1);
  iow <= cpuwr or cpuiorq;
  memr <= cpurd or cpumreq;
  memw <= cpuwr or cpumreq;
  bank <= cpua(15 downto 14);
  ioaddr <= cpua(7 downto 0);

  --cpuclk <= clkdiv(3);
  with speedsel select cpuclk <=
    clkdiv(3) when "00",   -- 3.125 M
	 clkdiv(2) when "01",   -- 6.25 M
	 clkdiv(1) when others; -- 12.5 M

  
  
  crtcsel <= '1' when iow='0' and cpua(7 downto 4)="0111" and clken='1' else '0'; -- out 70h/71h
  
  clken <= not dn_go;
  
  cpu : entity work.T80se
   port map (  
	   RESET_n => masterres,
		CLK_n   => cpuclk,
		CLKEN   => clken,
		WAIT_n  => '1',
		INT_n   => intreq,
		NMI_n   => '1',
		BUSRQ_n => '1',
		M1_n    => cpum1,
		MREQ_n  => cpumreq,
		IORQ_n  => cpuiorq,
		RD_n    => cpurd,
		WR_n    => cpuwr,
		RFSH_n  => open,
		HALT_n  => open,
		BUSAK_n => open,
		A       => cpua, 
		DI      => cpudi,
		DO      => cpudo
  );

  paging : entity work.pager
    port map (
		clk => clkdiv(0), --25M	 
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
	 
  chrclk <= clkdiv(4);

  crtc : entity work.mc6845
  port map (
	CLOCK		=> chrclk,
	CLKEN		=> '1',
	nRESET	=> masterres,

	-- Bus interface
	CPUCLOCK	=> cpuclk,
	ENABLE	=> crtcsel,
	R_nW		=> cpuwr,
	RS			=> cpua(0),
	DI			=> cpudo,
	DO			=> crtcdo,

	-- Display interface
	VSYNC		=> vs,
	HSYNC		=> hs,
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
	  clk25 => clkdiv(0),
	clk12_5 => clkdiv(1),
	clk6_25 => clkdiv(2),
  clk3_125 => clkdiv(3),
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
		  CLK => clkdiv(2),	 
		KBCLK => ps2clk,
		KBDAT => ps2dat,
		SWRES => swres,	 
		 JOY0 => joy0,
		 JOY1 => joy1,
	  KEYROW => keyrow,
	  KEYCOL => keycol
	 );
  
  -- sound generator
  sound : entity work.soundctrl 
    port map( 
		reset => masterres,	 
		  clk => clkdiv(0), -- 25M
		clken => clken,
	  sndclk => cpuclk, -- 3.125M
		  din => cpudo,
		  iow => iow,
		  ior => ior,
			 a => ioaddr,
	  sndint => sndint,
		 aout => aout
	);

 -- Delta-Sigma DAC for audio (one channel, mono in this implementation)
 audiodac : entity work.dac
    port map ( 
      clk_i   => clkdiv(2), -- 6.25 M
      res_n_i => masterres,
      dac_i   => aout,
      dac_o   => dacout
    ); 
 audiol <= dacout;
 audior <= dacout;   
  
  -- I/O read
  process(clkdiv(0),ior)
  begin
	 if rising_edge(clkdiv(0)) and ior='0' and clken='1' then
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
  end process;

  -- I/O writes
  process(clkdiv(0),iow,masterres)
  begin
    if masterres='0' then
	   ledr <= '1';
		speedsel <= "00";
    else	 
		 if rising_edge(clkdiv(0)) and iow='0' and clken='1' then
			case ioaddr is
			  when x"a0" => ledr <= not cpudo(7);
								 speedsel <= cpudo(1 downto 0);
			  when x"03" => keyrow <= cpudo(3 downto 0);
			  when others=>null;
			end case;	   
		 end if;
	 end if;
  end process;

  process(iow,ioaddr,crtccur,clkdiv(0),sndint)
  begin
    if iow='0' and ioaddr=x"07" then
	   intreq<='1';
	 else 
	   if rising_edge(clkdiv(0)) and (crtccur='1' or sndint='1') and clken='1' then -- cursor or sound interrupt
		  intreq<='0'; 
		end if;
	 end if;
  end process;
  
  -- horizontal blanking
  process(clkdiv(1)) -- 12.5MHz
  begin
    if rising_edge( clkdiv(1) ) then
	   hsreg<=hs;
				
		if hsreg='0' and hs='1' then
        hblankctr <= "00000000";		  
		  hblank<='1';
		  hspal<='0';
		end if;
				
		if hblank='1' then
		  hblankctr <= hblankctr + 1;
		  if hblankctr=150 then --  300T@12.5MHz 12us blanking time
		    hblank<='0';
		  end if;
		  if hblankctr=50 then -- 4 us hsync time
		    hspal<='1'; 
		  end if;
		end if;
				  
	 end if;
  end process;
  
  -- vertical blanking
  process(hs)
  begin
    if rising_edge(hs) then
      vsreg<=vs;
		if vsreg='0' and vs='1' then
		  vblankctr <= "0000";
		  vblank<='1';
		end if;		
		if vblank='1' then
		  vblankctr <= vblankctr + 1;
		  if vblankctr=5 then
		    vblank<='0';
		  end if;		
		end if;
	 end if;
  end process;
  
  cpudi <= --romdo when nrom='0' else
           --extromdo when nrom5='0' else

--			  ramdo when nrom='0' else
--			  ramdo when nrom5='0' else
--			  vramdo when nvram='0' else
--			  ramdo when np/="1111" else
--			  indata0 when ior='0' else
--			  x"ff";

			  vramdo when nvram='0' else
			  indata0 when ior='0' else
			  ramdo when np/="1111" or nrom='0' or nrom5='0' else
			  x"ff";
			  

  -- IGRB
  blue <= "111000" when vrgbi(3)='1' and vrgbi(1)='1' else 
         "011100" when vrgbi(3)='0' and vrgbi(1)='1' else 
			"000000";
  green <= "111000" when vrgbi(3)='1' and vrgbi(2)='1' else 
         "011100" when vrgbi(3)='0' and vrgbi(2)='1' else 
			"000000";
  red <= "111000" when vrgbi(3)='1' and vrgbi(0)='1' else 
         "011100" when vrgbi(3)='0' and vrgbi(0)='1' else 
			"000000";
			
  rgb <= 
	"000000000000000000" when vblank='1' or hblank='1' else out_rgb;

osd_d : osd
	generic map (OSD_COLOR => 6)
	port map (
		pclk => clkdiv(1), -- 12.5 MHz
      sck => SPI_SCK,
      ss => SPI_SS3,
      sdi => SPI_DI,

      red_in => red,
      green_in => green,
      blue_in => blue,
      hs_in => hs,
      vs_in => vs,

      red_out => out_rgb(17 downto 12),
      green_out => out_rgb(11 downto 6),
      blue_out => out_rgb(5 downto 0),
		hs_out => open, --HSYNC,
		vs_out => open --VSYNC
);


  --  hsync <= (not hs) xor (vs);
  hsync <= hspal xor (vs);
  vsync <= '1';
    
  with np select rambanksel <=
   "00" when "1110", 
	"01" when "1101",
	"10" when "1011",
	"11" when others;
  
  ramsel <= '0' when np/="1111" else '1';
  
  -- sdram clocked @ 50MHz
  sdram_inst : sdram
    port map( sd_data => SDRAM_DQ,
              sd_addr => SDRAM_A,
               sd_dqm => sdram_dqm,
                sd_cs => SDRAM_nCS,
                sd_ba => SDRAM_BA,
                sd_we => SDRAM_nWE,
               sd_ras => SDRAM_nRAS,
               sd_cas => SDRAM_nCAS,
                  clk => clk100m,
               clkref => cpuClk,
                 init => not pllLocked,
                  din => ram_din,
                 addr => ram_addr,
                   we => ram_we,
                   oe => ram_oe,
                 dout => ram_dout
    );
     
  ramdo <= ram_dout;
  
  romram <= not (nrom and nrom5); -- romram=1 if nrom or nrom5 selected (0)
  
  rambsel2 <= "00" when nrom='0' else
				  "01" when nrom5='0' else
				  rambanksel;
  
  ram_addr <= "00000000" & romram & rambsel2 & cpua(13 downto 0) when dn_go='0' else dn_addr_r;
  ram_din <= cpudo when dn_go='0' else dn_data_r;
  
  cpu_ram_wr <= not (ramsel or cpuwr);
  cpu_ram_rd <= not ((ramsel and nrom and nrom5) or cpurd);
  
--  ram_we <= not (ramsel or cpuwr) when dn_go='0' else dn_wr_r;
--  ram_oe <= not (ramsel or cpurd) when dn_go='0' else '0';
  ram_we <= cpu_ram_wr when dn_go='0' else dn_wr_r;
  ram_oe <= cpu_ram_rd when dn_go='0' else '0';

  dn_clk <= clkdiv(2);
  dn_rom <= '0' when dn_go='1' and dn_idx=0 else '1';
  
  dataio : data_io
    port map (
	   sck 	=> 	SPI_SCK,
		ss    =>  	SPI_SS2,
		sdi	=>		SPI_DI,

		downloading => dn_go,
		--size        => ioctl_size,
		index       => dn_idx,

		-- ram interface
		clk 	=> 	dn_clk, -- ??? 
		wr    =>    dn_wr,
		addr  =>		dn_addr,
		data  =>		dn_data	 
	 );

  led <= ledr when dn_go='0' else '0'; 	 
	 
  process(dn_clk)
  begin
    if rising_edge(dn_clk) then
	 
--	   if dn_wr='1' and dn_addr>143 then
--		  dn_sr <= "1100";
--		  dn_data_r <= dn_data;
--		  dn_addr_r <= dn_addr+6495;
--		else
--		  dn_sr <= dn_sr(2 downto 0) & "0";
--		end if;
		
	   if dn_wr='1' and ((dn_idx/=0 and dn_addr>143) or (dn_idx=0)) then
		  dn_sr <= "1100";
		  dn_data_r <= dn_data;
		  if dn_idx=0 then
		     dn_addr_r <= dn_addr;
		  else
			  dn_addr_r <= dn_addr+6495;
		  end if;
		else
		  dn_sr <= dn_sr(2 downto 0) & "0";
		end if;
		
	 end if;
  end process; 
  dn_wr_r <= dn_sr(3);
  
  userio: user_io
   generic map (STRLEN => CONF_STR'length)
   port map (
	
	   conf_str => to_slv(CONF_STR),
	
		SPI_CLK   => SPI_SCK    ,
      SPI_SS_IO => CONF_DATA0 ,
      SPI_MISO  => SPI_DO     ,
      SPI_MOSI  => SPI_DI     ,

		status     => status     ,
		 
		-- ps2 interface
		ps2_clk        => ps2clkout,
		ps2_kbd_clk    => ps2CLK,
		ps2_kbd_data   => ps2DAT,
		ps2_mouse_clk  => mps2CLK,
		ps2_mouse_data => mps2DAT,
		 
		joystick_0 => joy0,
      joystick_1 => joy1	  
	); 
  
end Behavioral;
