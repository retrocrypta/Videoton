module TVC_MiST(
	output        LED,
	output  [5:0] VGA_R,
	output  [5:0] VGA_G,
	output  [5:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        AUDIO_L,
	output        AUDIO_R,
	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,
	input         SPI_SS3,
	input         SPI_SS4,
	input         CONF_DATA0,
	input         CLOCK_27,

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
	output        SDRAM_CKE
);

`include "build_id.v" 

localparam CONF_STR = {
	"TVC;CAS;",
	"F,ROMCRT,Load Cartridge;",
	"O23,Scandoubler Fx,None,CRT 25%,CRT 50%,CRT 75%;",
	"O45,CPU Speed,3.125MHz,6.25MHz,12.5MHz;",
	"O6,Joystick Swap,Off,On;",
//	"O7,Composite Blend,Off,On;",
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

user_io #(
	.STRLEN(($size(CONF_STR)>>3)),
	.ROM_DIRECT_UPLOAD(1'b0))
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
	.status         (status         )
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
wire       blankn = ~(HBlank | VBlank);
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

mist_video #(.COLOR_DEPTH(6), .SD_HCNT_WIDTH(12)) mist_video(
	.clk_sys        ( CLK_50M          ),
	.SPI_SCK        ( SPI_SCK          ),
	.SPI_SS3        ( SPI_SS3          ),
	.SPI_DI         ( SPI_DI           ),
	.R              ( blankn ? R : 0   ),
	.G              ( blankn ? G : 0   ),
	.B              ( blankn ? B : 0   ),
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
	
endmodule 
