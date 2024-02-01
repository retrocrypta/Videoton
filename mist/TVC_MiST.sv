module guest_top(
	input         CLOCK_27,
`ifdef USE_CLOCK_50
	input         CLOCK_50,
`endif

	output        LED,
	output [VGA_BITS-1:0] VGA_R,
	output [VGA_BITS-1:0] VGA_G,
	output [VGA_BITS-1:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,

`ifdef USE_HDMI
	output        HDMI_RST,
	output  [7:0] HDMI_R,
	output  [7:0] HDMI_G,
	output  [7:0] HDMI_B,
	output        HDMI_HS,
	output        HDMI_VS,
	output        HDMI_PCLK,
	output        HDMI_DE,
	inout         HDMI_SDA,
	inout         HDMI_SCL,
	input         HDMI_INT,
`endif

	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,    // data_io
	input         SPI_SS3,    // OSD
	input         CONF_DATA0, // SPI_SS for user_io

`ifdef USE_QSPI
	input         QSCK,
	input         QCSn,
	inout   [3:0] QDAT,
`endif
`ifndef NO_DIRECT_UPLOAD
	input         SPI_SS4,
`endif

	output [12:0] SDRAM_A,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nWE,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nCS,
	output  [1:0] SDRAM_BA,
	output        SDRAM_CLK,
	output        SDRAM_CKE,

`ifdef DUAL_SDRAM
	output [12:0] SDRAM2_A,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_DQML,
	output        SDRAM2_DQMH,
	output        SDRAM2_nWE,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nCS,
	output  [1:0] SDRAM2_BA,
	output        SDRAM2_CLK,
	output        SDRAM2_CKE,
`endif

	output        AUDIO_L,
	output        AUDIO_R,
`ifdef I2S_AUDIO
	output        I2S_BCK,
	output        I2S_LRCK,
	output        I2S_DATA,
`endif
`ifdef SPDIF_AUDIO
	output        SPDIF,
`endif
`ifdef USE_AUDIO_IN
	input         AUDIO_IN,
`endif
	input         UART_RX,
	output        UART_TX

);

`ifdef NO_DIRECT_UPLOAD
localparam bit DIRECT_UPLOAD = 0;
wire SPI_SS4 = 1;
`else
localparam bit DIRECT_UPLOAD = 1;
`endif

`ifdef USE_QSPI
localparam bit QSPI = 1;
assign QDAT = 4'hZ;
`else
localparam bit QSPI = 0;
`endif

`ifdef VGA_8BIT
localparam VGA_BITS = 8;
`else
localparam VGA_BITS = 6;
`endif

`ifdef BIG_OSD
localparam bit BIG_OSD = 1;
`define SEP "-;",
`else
localparam bit BIG_OSD = 0;
`define SEP
`endif

`ifdef USE_HDMI
localparam bit HDMI = 1;
assign HDMI_RST = 1'b1;
`else
localparam bit HDMI = 0;
`endif

// remove this if the 2nd chip is actually used
`ifdef DUAL_SDRAM
assign SDRAM2_A = 13'hZZZZ;
assign SDRAM2_BA = 0;
assign SDRAM2_DQML = 0;
assign SDRAM2_DQMH = 0;
assign SDRAM2_CKE = 0;
assign SDRAM2_CLK = 0;
assign SDRAM2_nCS = 1;
assign SDRAM2_DQ = 16'hZZZZ;
assign SDRAM2_nCAS = 1;
assign SDRAM2_nRAS = 1;
assign SDRAM2_nWE = 1;
`endif

`include "build_id.v"

localparam CONF_STR = {
	"TVC;CAS;",
	"F,ROMCRT,Load Cartridge;",
	"S0U,DSK,Mount Drive A;",
	"S1U,DSK,Mount Drive B;",
	`SEP
	"O23,Scandoubler Fx,None,CRT 25%,CRT 50%,CRT 75%;",
	`SEP
	"O45,CPU Speed,3.125MHz,6.25MHz,12.5MHz;",
	"O6,Joystick Swap,Off,On;",
	"O7,Composite Blend,Off,On;",
	`SEP
	"T1,Cart Unload;",
	"T0,Reset;",
	"V,v1.0.",`BUILD_DATE
};

wire        cart_unload = status[1];
wire  [1:0] scanlines = status[3:2];
wire  [1:0] speedsel  = status[5:4];
wire        joyswap   = status[6];
wire        blend     = status[7];

wire CLK_75M, CLK_50M;
wire pll_locked;
pll pll(
	.inclk0(CLOCK_27),
	.c0(CLK_75M),
	.c1(CLK_50M),
	.locked(pll_locked)
	);

wire [63:0] status;
wire  [1:0] buttons;
wire  [1:0] switches;
wire  [7:0] joystick_0;
wire  [7:0] joystick_1;
wire        scandoublerD;
wire        ypbpr;
wire        no_csync;
wire        kbdclk;
wire        kbddat;

wire [31:0] sd_lba;
wire  [1:0] sd_rd;
wire  [1:0] sd_wr;
wire        sd_ack;
wire        sd_ack_conf;
wire        sd_conf;
wire        sd_sdhc = 1'b1;
wire  [7:0] sd_dout;
wire        sd_dout_strobe;
wire  [7:0] sd_din;
wire        sd_din_strobe;
wire  [8:0] sd_buff_addr;
wire  [1:0] img_mounted;
wire [31:0] img_size;

`ifdef USE_HDMI
wire        i2c_start;
wire        i2c_read;
wire  [6:0] i2c_addr;
wire  [7:0] i2c_subaddr;
wire  [7:0] i2c_dout;
wire  [7:0] i2c_din;
wire        i2c_ack;
wire        i2c_end;
`endif

user_io #(
	.STRLEN(($size(CONF_STR)>>3)),
	.ROM_DIRECT_UPLOAD(1'b0),
	.FEATURES(32'h0 | (BIG_OSD << 13) | (HDMI << 14)))
user_io(
	.clk_sys        (CLK_50M        ),
	.conf_str       (CONF_STR       ),
	.SPI_CLK        (SPI_SCK        ),
	.SPI_SS_IO      (CONF_DATA0     ),
	.SPI_MISO       (SPI_DO         ),
	.SPI_MOSI       (SPI_DI         ),
	.buttons        (buttons        ),
	.switches       (switches       ),
	.scandoubler_disable (scandoublerD	  ),
	.ypbpr          (ypbpr          ),
	.no_csync       (no_csync       ),
	.ps2_kbd_clk    (kbdclk         ),
	.ps2_kbd_data   (kbddat         ),
	.joystick_0     (joystick_0     ),
	.joystick_1     (joystick_1     ),
	.status         (status         ),
`ifdef USE_HDMI
	.i2c_start      (i2c_start      ),
	.i2c_read       (i2c_read       ),
	.i2c_addr       (i2c_addr       ),
	.i2c_subaddr    (i2c_subaddr    ),
	.i2c_dout       (i2c_dout       ),
	.i2c_din        (i2c_din        ),
	.i2c_ack        (i2c_ack        ),
	.i2c_end        (i2c_end        ),
`endif
	.clk_sd         ( CLK_50M       ),
	.sd_lba         ( sd_lba        ),
	.sd_rd          ( sd_rd         ),
	.sd_wr          ( sd_wr         ),
	.sd_ack         ( sd_ack        ),
	.sd_ack_conf    ( sd_ack_conf   ),
	.sd_conf        ( sd_conf       ),
	.sd_sdhc        ( sd_sdhc       ),
	.sd_dout        ( sd_dout       ),
	.sd_dout_strobe ( sd_dout_strobe),
	.sd_din         ( sd_din        ),
	.sd_din_strobe  ( sd_din_strobe ),
	.sd_buff_addr   ( sd_buff_addr  ),
	.img_mounted    ( img_mounted   ),
	.img_size       ( img_size      )
	);

wire        ioctl_downl;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        ioctl_clkref;

data_io #(.ROM_DIRECT_UPLOAD(1'b0)) data_io(
	.clk_sys       ( CLK_50M      ),
	.SPI_SCK       ( SPI_SCK      ),
	.SPI_SS2       ( SPI_SS2      ),
	.SPI_DI        ( SPI_DI       ),
	.clkref_n      ( ~ioctl_clkref),
	.ioctl_download( ioctl_downl  ),
	.ioctl_index   ( ioctl_index  ),
	.ioctl_wr      ( ioctl_wr     ),
	.ioctl_addr    ( ioctl_addr   ),
	.ioctl_dout    ( ioctl_dout   )
);

// reset signal generation
reg reset = 1;
reg rom_loaded = 0;
always @(posedge CLK_50M) begin
	reg ioctl_downlD;
	reg [15:0] reset_count;
	ioctl_downlD <= ioctl_downl;

	if (status[0] | buttons[1] | ~rom_loaded) reset_count <= 16'hffff;
	else if (reset_count != 0) reset_count <= reset_count - 1'd1;

	if (ioctl_index == 0 && ioctl_downlD && ~ioctl_downl) rom_loaded <= 1;
	reset <= reset_count != 16'h0000;

end

wire [5:0] R,G,B;
wire       HSync, VSync, HBlank, VBlank;
wire [7:0] audio;

tvctop tvctop (
	.CLK75M(CLK_75M),
	.CLK50M(CLK_50M),
	.PLLLOCKED(pll_locked),
	.RESET(reset),
	.LED(LED),
	.SPEEDSEL(speedsel),

	// ROM/CAS download
	.DN_GO(ioctl_downl),
	.DN_WR(ioctl_wr),
	.DN_ADDR(ioctl_addr),
	.DN_DATA(ioctl_dout),
	.DN_IDX(ioctl_index[5:0]),
	.DN_CLKREF(ioctl_clkref),
	.CART_UNLOAD(cart_unload),

	// Video output
	.R(R),
	.G(G),
	.B(B),
	.HS(HSync),
	.VS(VSync),
	.HBLANK(HBlank),
	.VBLANK(VBlank),

	// Audio output
	.AOUT(audio),

	// HID
	.PS2CLK(kbdclk),
	.PS2DAT(kbddat),
	.JOY0(joyswap ? joystick_1[7:0] : joystick_0[7:0]),
	.JOY1(joyswap ? joystick_0[7:0] : joystick_1[7:0]),

	.IMG_MOUNTED(img_mounted),
	.IMG_WP(2'b00),
	.IMG_SIZE(img_size),

	.SD_LBA(sd_lba),
	.SD_RD(sd_rd),
	.SD_WR(sd_wr),
	.SD_ACK(sd_ack),
	.SD_BUFF_ADDR(sd_buff_addr),
	.SD_DOUT(sd_dout),
	.SD_DIN(sd_din),
	.SD_DOUT_STROBE(sd_dout_strobe),

	// SDRAM
	.SDRAM_nCS(SDRAM_nCS),
	.SDRAM_DQ(SDRAM_DQ),
	.SDRAM_A(SDRAM_A),
	.SDRAM_DQMH(SDRAM_DQMH),
	.SDRAM_DQML(SDRAM_DQML),
	.SDRAM_nWE(SDRAM_nWE),
	.SDRAM_nCAS(SDRAM_nCAS),
	.SDRAM_nRAS(SDRAM_nRAS),
	.SDRAM_BA(SDRAM_BA),
	.SDRAM_CLK(SDRAM_CLK),
	.SDRAM_CKE(SDRAM_CKE)
);

mist_video #(.COLOR_DEPTH(6), .SD_HCNT_WIDTH(12), .USE_BLANKS(1'b1), .OUT_COLOR_DEPTH(VGA_BITS), .BIG_OSD(BIG_OSD)) mist_video(
	.clk_sys        ( CLK_50M          ),
	.SPI_SCK        ( SPI_SCK          ),
	.SPI_SS3        ( SPI_SS3          ),
	.SPI_DI         ( SPI_DI           ),
	.R              ( R                ),
	.G              ( G                ),
	.B              ( B                ),
	.HBlank         ( HBlank           ),
	.VBlank         ( VBlank           ),
	.HSync          ( HSync            ),
	.VSync          ( VSync            ),
	.VGA_R          ( VGA_R            ),
	.VGA_G          ( VGA_G            ),
	.VGA_B          ( VGA_B            ),
	.VGA_VS         ( VGA_VS           ),
	.VGA_HS         ( VGA_HS           ),
	.rotate         ( 2'b00            ),
	.ce_divider     ( 3'd1             ),
	.scandoubler_disable( scandoublerD ),
	.scanlines      ( scanlines        ),
	.blend          ( blend            ),
	.ypbpr          ( ypbpr            ),
	.no_csync       ( no_csync         )
	);

dac #(
	.C_bits(8))
dacl(
	.clk_i(CLK_50M),
	.res_n_i(1),
	.dac_i(audio),
	.dac_o(AUDIO_L)
	);

assign AUDIO_R = AUDIO_L;

`ifdef I2S_AUDIO
i2s i2s (
	.reset(1'b0),
	.clk(CLK_50M),
	.clk_rate(32'd50_000_000),

	.sclk(I2S_BCK),
	.lrclk(I2S_LRCK),
	.sdata(I2S_DATA),

	.left_chan({audio, audio}),
	.right_chan({audio, audio})
);
`endif

`ifdef SPDIF_AUDIO
spdif spdif
(
	.clk_i(CLK_50M),
	.rst_i(reset),
	.clk_rate_i(32'd50_000_000),
	.spdif_o(SPDIF),
	.sample_i({audio, audio, audio, audio})
);
`endif

`ifdef USE_HDMI
i2c_master #(50_000_000) i2c_master (
	.CLK         (CLK_50M),
	.I2C_START   (i2c_start),
	.I2C_READ    (i2c_read),
	.I2C_ADDR    (i2c_addr),
	.I2C_SUBADDR (i2c_subaddr),
	.I2C_WDATA   (i2c_dout),
	.I2C_RDATA   (i2c_din),
	.I2C_END     (i2c_end),
	.I2C_ACK     (i2c_ack),

	//I2C bus
	.I2C_SCL     (HDMI_SCL),
	.I2C_SDA     (HDMI_SDA)
);

mist_video #(.COLOR_DEPTH(6), .SD_HCNT_WIDTH(12), .USE_BLANKS(1'b1), .OUT_COLOR_DEPTH(8), .BIG_OSD(BIG_OSD), .VIDEO_CLEANER(1'b1)) hdmi_video(
	.clk_sys        ( CLK_50M          ),
	.SPI_SCK        ( SPI_SCK          ),
	.SPI_SS3        ( SPI_SS3          ),
	.SPI_DI         ( SPI_DI           ),
	.R              ( R                ),
	.G              ( G                ),
	.B              ( B                ),
	.HBlank         ( HBlank           ),
	.VBlank         ( VBlank           ),
	.HSync          ( HSync            ),
	.VSync          ( VSync            ),
	.VGA_R          ( HDMI_R           ),
	.VGA_G          ( HDMI_G           ),
	.VGA_B          ( HDMI_B           ),
	.VGA_VS         ( HDMI_VS          ),
	.VGA_HS         ( HDMI_HS          ),
	.VGA_DE         ( HDMI_DE          ),
	.rotate         ( 2'b00            ),
	.ce_divider     ( 3'd1             ),
	.scandoubler_disable( 1'b0         ),
	.scanlines      ( scanlines        ),
	.blend          ( blend            ),
	.ypbpr          ( 1'b0             ),
	.no_csync       ( 1'b0             )
	);

assign HDMI_PCLK = CLK_50M;

`endif


endmodule 
