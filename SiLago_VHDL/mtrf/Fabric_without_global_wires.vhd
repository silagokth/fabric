-------------------col------------------------------------------------------------
-- Title      : MTRF_computationl_fabric
-- Project    : MTRF (former DRRA)
-------------------------------------------------------------------------------
-- File       : MTRF_fabric.vhd
-- Author     : Sadiq Hemani <sadiq@kth.se>
-- Company    : 
-- Created    : 2013-09-05
-- Last update: 2013-11-21
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: To be included in all MTRF computational components and the top module. Includes
-- all the required constants and type declarations for the Sequencer, DPU and
-- Register file (with AGUs). The sequencer constants
-- have been declared in a parametric style to facilitate any future instruction
-- format changes.
--
--
-- DRRA name rebranded to MTRCF (Multi-threaded-reconfigurable-computational-fabric)
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-- The VHDL code, the logic and concepts described in this file constitute
-- the intellectual property of the authors listed below, who are affiliated
-- to KTH(Kungliga Tekniska Hogskolan), School of ICT, Kista.
-- Any unauthorised use, copy or distribution is strictly prohibited.
-- Any authorised use, copy or distribution should carry this copyright notice
-- unaltered.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author        Description
-- 2013-09-05  1.0      Sadiq Hemani <sadiq@kth.se>
-- 2013-09-30  1.0      Nasim Farahini <farahini@kth.se>
-- 2013-11-21 1.0       Sadiq Hemani <sadiq@kth.se>
-- 2014-02-10  1.0      Nasim Farahini <farahini@kth.se>
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.seq_functions_package.all;
use work.util_package.all;
--USE ieee.std_logic_unsigned.ALL;
use work.top_consts_types_package.all;
use work.noc_types_n_constants.all;
use work.crossbar_types_n_constants.all;

	port(clk                  : in  std_logic;
		 rst_n                : in  std_logic;
		 instr_ld             : in  std_logic; --std_logic_vector(COLUMNS - 1 downto 0);
		 instr_inp            : in  std_logic_vector(INSTR_WIDTH downto 0); --v_bus_signal_ty;--std_logic_vector(OLD_INSTR_WIDTH-1 DOWNTO 0);
		 --seq_address : IN std_logic_vector(SEQ_ADDRS_WIDTH-1 DOWNTO 0)
		 seq_address_rb       : in  std_logic_vector(ROWS-1 downto 0); ---in order to generate addresses for sequencer
		 seq_address_cb       : in  std_logic_vector(COLUMNS - 1 downto 0); ---in order to generate addresses for sequencer
		 fabric_reg_wr_2      : in  std_logic;
		 fabric_reg_rd_2      : in  std_logic;
		 fabric_reg_wr_addr_2 : in  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
		 fabric_reg_rd_addr_2 : in  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
		 fabric_data_in_reg_2 : in  signed(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
		 fabric_data_out_2    : out signed(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
		 tb_or_dimarch        : in  std_logic;
		 --------------------------------------------------------
		 --SRAM initialisation from testbench
		 --------------------------------------------------------
		 tb_en                : in  STD_LOGIC; -- Write Enable from test bench
		 tb_addrs             : in  STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 downto 0); -- Write Address from test bench
		 tb_inp               : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		 tb_ROW               : in  UNSIGNED(ROW_WIDTH - 1 downto 0);
		 tb_COL               : in  UNSIGNED(COL_WIDTH - 1 downto 0)
	);
end entity fabric;

architecture rtl of fabric is
	type mem_address_ty is array (natural range <>, natural range <>) of std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
	signal fabric_reg_wr_addr_2_temp : mem_address_ty(0 to COLUMNS - 1, 0 to ROWS - 1);
	type mem_output_ty is array (natural range <>, natural range <>) of signed(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
	signal fabric_data_in_reg_2_temp : mem_output_ty(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal reg_read                  : SRAM_RW_ENABLE_TYPE(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal reg_write                 : SRAM_RW_ENABLE_TYPE(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal reg_write_temp            : SRAM_RW_ENABLE_TYPE(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal reg_address_in            : SRAM_RW_address_type(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal reg_address_out           : SRAM_RW_address_type(0 to COLUMNS - 1, 0 to ROWS - 1);

	signal reg_in         : SRAM_RW_DATA_TYPE(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal reg_out_signed : SRAM_RW_SIGNED_DATA_TYPE(0 to COLUMNS - 1, 0 to ROWS - 1);


	signal s_bus_out_w      : s_bus_out_ty(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal sel_config_w_reg : s_bus_switchbox_2d_ty(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal sel_config_w_dpu : s_bus_switchbox_2d_ty(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal v_lanes          : v_lane_ty(0 to COLUMNS - 1);

	---------------------------------------------
	-- NOC SIGNALS
	---------------------------------------------

	signal DATA_SOUTH : DATA_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);
	signal DATA_NORTH : DATA_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);
	signal DATA_WEST  : DATA_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);
	signal DATA_EAST  : DATA_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);


	signal SRAM_INST_hor_left_in    : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_hor_right_in   : INST_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW);
	signal SRAM_INST_hor_top_in     : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_hor_bottom_in  : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_hor_left_out   : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_hor_right_out  : INST_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW);
	signal SRAM_INST_hor_top_out    : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_hor_bottom_out : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);

	signal SRAM_INST_ver_left_in   : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_ver_right_in  : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);

	signal SRAM_INST_ver_left_out  : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal SRAM_INST_ver_right_out : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);


	signal SRAM_INST_hor_in, SRAM_INST_hor_out : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);

	signal splitter_direction_ver : DIRECTION_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal splitter_direction_hor : DIRECTION_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	--  

	signal PARTITION_INST_top    : PARTITION_INSTRUCTION_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal PARTITION_INST_bottom : PARTITION_INSTRUCTION_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal PARTITION_INST_left   : PARTITION_INSTRUCTION_TYPE(0 to COLUMNS, 0 to MAX_ROW);
	signal PARTITION_INST_right  : PARTITION_INSTRUCTION_TYPE(0 to COLUMNS, 0 to MAX_ROW);

	signal DATA_SEQ_IN  : CROSSBAR_DATA_TYPE(0 to COLUMNS - 1);
	signal DATA_SEQ_OUT : CROSSBAR_DATA_TYPE(0 to COLUMNS - 1);

	-- lcc signals 

	signal MLFC_CONFIG : LOADER_ARRAY_TYPE(COLUMNS - 1 downto 0);
	signal row_sel     : row_sel_ty;


	signal h_bus_reg_seg_0   : h_bus_seg_ty(0 to 2*COLUMNS+1, 0 to 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus register file output 0
	signal h_bus_reg_seg_1   : h_bus_seg_ty(0 to 2*COLUMNS+1, 0 to 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus register file output 1
	signal h_bus_dpu_seg_0   : h_bus_seg_ty(0 to 2*COLUMNS+1, 0 to 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus dpu output 0
	signal h_bus_dpu_seg_1   : h_bus_seg_ty(0 to 2*COLUMNS+1, 0 to 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus dpu output 1
	signal sel_r_seg         : s_bus_switchbox_2d_ty(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal v_bus             : v_bus_ty_2d(0 to COLUMNS - 1, 0 to ROWS - 1);
	signal bus_direction_hor : PARTITION_INSTRUCTION_STATUS_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);
	signal bus_direction_ver : PARTITION_INSTRUCTION_STATUS_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);
	signal noc_bus_in : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to ROWS-1);
	signal noc_bus_out : NOC_BUS_ARRAY_TYPE(0 to COLUMNS);
	signal dimarch_silego_data_in : DATA_IO_SIGNAL_TYPE(COLUMNS-1 downto 0);
	signal dimarch_silego_data_out : DATA_SIGNAL_TYPE(0 to COLUMNS-1 , 0 to ROWS-1);
	signal dimarch_silego_rd_2_out : DATA_RD_TYPE(0 to COLUMNS-1 , 0 to ROWS-1);
	
	
begin                                   -- ARCHITECTURE rtl

	DATA_WEST(COLUMNS, 0)<=(others =>'0'); -- data west input tied to zero
	DATA_EAST(0, 0) <= (others=>'0'); -- data east input tied to zero
	SRAM_INST_hor_right_in (1,0) <= IDLE_BUS;	
	SRAM_INST_hor_right_out(0,0) <= IDLE_BUS;
	bus_direction_hor(COLUMNS,0) <= (others => '0');
	--DiMArch_COLS
	DiMArch_COLS : for i in 0 to COLUMNS-1 generate
	begin
	
	SRAM_INST_ver_right_out(i, MAX_ROW-1) <= noc_bus_out(i);

	DATA_SOUTH(i,  MAX_ROW) <= (others=>'0'); -- data north input tied to zero
	SRAM_INST_ver_left_in  (i, MAX_ROW-1) <=IDLE_BUS;
	SRAM_INST_ver_right_in (i,MAX_ROW-1) <=IDLE_BUS;
	bus_direction_ver(i,MAX_ROW) <= (others =>'0');
	bus_direction_ver(i, 0) <=(others => '0');
	SRAM_INST_ver_left_out(i,0)<= IDLE_BUS;

	DiMArch_ROWS : for j in 0 to MAX_ROW - 1 generate
		begin

			u_segmented_bus_hor : entity work.segmented_bus
				port map(
					clk               => clk,
					rst               => rst_n,
					left_instruction  => PARTITION_INST_left    (i, j),
					right_instruction => PARTITION_INST_right   (i, j),
					left_in           => SRAM_INST_hor_left_in  (i, j),
					left_out          => SRAM_INST_hor_left_out (i, j),
					right_in          => SRAM_INST_hor_right_in (i + 1, j),
					right_out         => SRAM_INST_hor_right_out(i + 1, j),
					bus_direction     => bus_direction_hor      (i, j)
				);

			u_STILE : entity work.STile(behv_rtl)
				generic map(
					This_ROW => to_unsigned(1 + j, ROW_WIDTH),
					This_COL => to_unsigned(i, COL_WIDTH)
				)
				port map(
					rst_n                    => rst_n,
					clk                      => clk,
					-- data interconnect
					north_out                => DATA_NORTH(i, j + 1),
					south_in                 => DATA_NORTH(i, j),
					north_in                 => DATA_SOUTH(i, j + 1),
					south_out                => DATA_SOUTH(i, j),
					east_in                  => DATA_WEST(i + 1, j),
					west_out                 => DATA_WEST(i, j),
					east_out                 => DATA_EAST(i + 1, j),
					west_in                  => DATA_EAST(i, j),

					-- direction of neibouring busses
					north_splitter_direction => bus_direction_ver(i, j+1),
					south_splitter_direction => bus_direction_ver(i, j), --: in
					east_splitter_direction  => bus_direction_hor(i, j), --: in
					west_splitter_direction  => bus_direction_hor(i+1, j),

					-- direction to neibouring busses
					top_instruction_out      => PARTITION_INST_bottom(i, j + 1),
					left_instruction_out     => PARTITION_INST_right (i, j),
					bottom_instruction_out   => PARTITION_INST_top   (i, j+1), --: out
					right_instruction_out    => PARTITION_INST_left  (i, j), --: out
					--------------------------------------------------------------------------------------------
					-- segmented bus
					--------------------------------------------------------------------------------------------

					HOR_BUS_LEFT_IN          => SRAM_INST_hor_right_out (i, j),
					HOR_BUS_RIGHT_IN         => SRAM_INST_hor_left_out (i+1, j),
					HOR_BUS_LEFT_OUT         => SRAM_INST_hor_right_in (i+1, j),
					HOR_BUS_RIGHT_OUT        => SRAM_INST_hor_left_in  (i, j),
					VER_BUS_BOTTOM_IN        => noc_bus_out(i),--)SRAM_INST_ver_right_out(i, j),
					VER_BUS_TOP_OUT          => SRAM_INST_ver_left_in  (i, j),
					VER_BUS_BOTTOM_OUT       => SRAM_INST_ver_right_in (i, j),
					VER_BUS_TOP_IN           => SRAM_INST_ver_left_out (i, j),
					tb_en                    => tb_en, -- Write Enable from test bench
					tb_addrs                 => tb_addrs, -- Write Address from test bench
					tb_inp                   => tb_inp,
					tb_ROW                   => tb_ROW,
					tb_COL                   => tb_COL
				);
		end generate DiMArch_ROWS;

	end generate DiMArch_COLS;

	inp_assignloop1 : for i in 0 to COLUMNS - 1 generate
		reg_write_temp(i, 0)            <= seq_address_cb(i) and seq_address_rb(0) and fabric_reg_wr_2;-- when tb_or_dimarch = '0' else reg_write(i, 0);
		reg_write_temp(i, 1)            <= seq_address_cb(i) and seq_address_rb(1) and fabric_reg_wr_2;-- when tb_or_dimarch = '0' else reg_write(i, 1);
		fabric_data_in_reg_2_temp(i, 0) <= fabric_data_in_reg_2;-- when tb_or_dimarch = '0' else signed(DATA_SOUTH(i, 0));
		fabric_data_in_reg_2_temp(i, 1) <= fabric_data_in_reg_2;-- when tb_or_dimarch = '0' else signed(DATA_SOUTH(i, 1));

		DATA_NORTH(i,0)  <= dimarch_silego_data_out(i,0) WHEN (dimarch_silego_rd_2_out(i,0)= '1' )else
							dimarch_silego_data_out(i,1) WHEN (dimarch_silego_rd_2_out(i,1)= '1' )else
							zero_block;
		dimarch_silego_data_in(i) <= DATA_SOUTH(i,0);
		 
	end generate;


	inp_assignloop2 : for i in 0 to COLUMNS - 1 generate
		inp_assignloop3 : for j in 0 to ROWS - 1 generate
			-- TODO: CHECK HERE: reg_address_in has never been initialized!
			--
			fabric_reg_wr_addr_2_temp(i, j) <= fabric_reg_wr_addr_2 when tb_or_dimarch = '0' else reg_address_in(i, j);

		end generate;
	end generate;



	MTRF_COLS : for i in 0 to COLUMNS - 1 generate


begin
	-------------------------------------------------------------------------------- 
	-- BUS SELECTOR
	--------------------------------------------------------------------------------
	-- this is just a patch, final version will have a distributed selector
	u_bus_selector : entity work.bus_selector
		generic map(this_column => i)
		port map(
			noc_bus_in0  => noc_bus_in(i,0),
			noc_bus_in1  => noc_bus_in(i,1),			
			noc_bus_out => noc_bus_out(i)
		);
	
		MTRF_ROWS : for j in 0 to ROWS - 1 generate

		begin

			--j_invesrse <= 1 when j=0 else 0;
			h_bus_reg_seg_0(0, 0) <= (others => '0');
			h_bus_reg_seg_0(0, 1) <= (others => '0');
			h_bus_reg_seg_1(0, 0) <= (others => '0');
			h_bus_reg_seg_1(0, 1) <= (others => '0');
			h_bus_dpu_seg_0(0, 0) <= (others => '0');
			h_bus_dpu_seg_0(0, 1) <= (others => '0');
			h_bus_dpu_seg_1(0, 0) <= (others => '0');
			h_bus_dpu_seg_1(0, 1) <= (others => '0');

			h_bus_reg_seg_0(1, 0) <= (others => '0');
			h_bus_reg_seg_0(1, 1) <= (others => '0');
			h_bus_reg_seg_1(1, 0) <= (others => '0');
			h_bus_reg_seg_1(1, 1) <= (others => '0');
			h_bus_dpu_seg_0(1, 0) <= (others => '0');
			h_bus_dpu_seg_0(1, 1) <= (others => '0');
			h_bus_dpu_seg_1(1, 0) <= (others => '0');
			h_bus_dpu_seg_1(1, 1) <= (others => '0');

			h_bus_reg_seg_0(2 * COLUMNS, 3) <= (others => '0');
			h_bus_reg_seg_0(2 * COLUMNS, 4) <= (others => '0');
			h_bus_reg_seg_1(2 * COLUMNS, 3) <= (others => '0');
			h_bus_reg_seg_1(2 * COLUMNS, 4) <= (others => '0');
			h_bus_dpu_seg_0(2 * COLUMNS, 3) <= (others => '0');
			h_bus_dpu_seg_0(2 * COLUMNS, 4) <= (others => '0');
			h_bus_dpu_seg_1(2 * COLUMNS, 3) <= (others => '0');
			h_bus_dpu_seg_1(2 * COLUMNS, 4) <= (others => '0');

			h_bus_reg_seg_0(2 * COLUMNS + 1, 3) <= (others => '0');
			h_bus_reg_seg_0(2 * COLUMNS + 1, 4) <= (others => '0');
			h_bus_reg_seg_1(2 * COLUMNS + 1, 3) <= (others => '0');
			h_bus_reg_seg_1(2 * COLUMNS + 1, 4) <= (others => '0');
			h_bus_dpu_seg_0(2 * COLUMNS + 1, 3) <= (others => '0');
			h_bus_dpu_seg_0(2 * COLUMNS + 1, 4) <= (others => '0');
			h_bus_dpu_seg_1(2 * COLUMNS + 1, 3) <= (others => '0');
			h_bus_dpu_seg_1(2 * COLUMNS + 1, 4) <= (others => '0');
			reg_read(i,j) <= '0';

			SILEGO_cell : entity work.silego
				port map(
							dimarch_data_in => dimarch_silego_data_in(i),
							dimarch_data_out => dimarch_silego_data_out(i,j),
							dimarch_rd_2_out => dimarch_silego_rd_2_out(i,j),
				noc_bus_out => noc_bus_in(i,j),
				clk                        => clk,
					     rst_n                      => rst_n,
					     instr_ld                   => instr_ld, --(i),                                                                                       
					     instr_inp                  => instr_inp(INSTR_WIDTH-1 downto 0), --instr_output(i)(OLD_INSTR_WIDTH-1 downto INSTR_WIDTH_DIFF),                  
					     --seq_address 			                                                        
					     seq_address_rb             => seq_address_rb(j),
					     seq_address_cb             => seq_address_cb(i),
					     --RegFile                                                                       
					     reg_wr_2                   => reg_write_temp(i, j),
					     reg_rd_2                   => reg_read(i, j),
					     reg_wr_addr_2              => fabric_reg_wr_addr_2_temp(i, j), --reg_address_in(i,j),    
					     reg_rd_addr_2              => fabric_reg_rd_addr_2, --reg_address_out(i,j),  
					     data_in_reg_2              => signed(fabric_data_in_reg_2_temp(i, j)),
					     data_out_2                 => fabric_data_out_2,
					     --Horizontal Busses
					     h_bus_reg_in_out0_0_left   => h_bus_reg_seg_0(2 * i + j, 0), -- h_bus_reg_seg_0(i+1,0) ,
					     h_bus_reg_in_out0_1_left   => h_bus_reg_seg_0(2 * i + j, 1), --h_bus_reg_seg_0(i+1,1),
					     h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
					     h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
					     h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
					     h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
					     h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
					     h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),
					     h_bus_reg_in_out1_0_left   => h_bus_reg_seg_1(2 * i + j, 0),
					     h_bus_reg_in_out1_1_left   => h_bus_reg_seg_1(2 * i + j, 1),
					     h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
					     h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
					     h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
					     h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
					     h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
					     h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

					     h_bus_dpu_in_out0_0_left   => h_bus_dpu_seg_0(2 * i + j, 0),
					     h_bus_dpu_in_out0_1_left   => h_bus_dpu_seg_0(2 * i + j, 1),
					     h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
					     h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
					     h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
					     h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
					     h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
					     h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),
					     h_bus_dpu_in_out1_0_left   => h_bus_dpu_seg_1(2 * i + j, 0),
					     h_bus_dpu_in_out1_1_left   => h_bus_dpu_seg_1(2 * i + j, 1),
					     h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
					     h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
					     h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
					     h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
					     h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
					     h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),
					     --Vertical Busses
					     --sel_r_ext_in               
					     sel_r_ext_in_0             => sel_r_seg(i, j)(0),
					     sel_r_ext_in_1             => sel_r_seg(i, j)(1),
					     sel_r_ext_in_2             => sel_r_seg(i, j)(2),
					     sel_r_ext_in_3             => sel_r_seg(i, j)(3),
					     sel_r_ext_in_4             => sel_r_seg(i, j)(4),
					     sel_r_ext_in_5             => sel_r_seg(i, j)(5),
					     --ext_v_input_bus_in        =>    
					     ext_v_input_bus_in_0       => v_bus(i, j)(0),
					     ext_v_input_bus_in_1       => v_bus(i, j)(1),
					     ext_v_input_bus_in_2       => v_bus(i, j)(2),
					     ext_v_input_bus_in_3       => v_bus(i, j)(3),
					     ext_v_input_bus_in_4       => v_bus(i, j)(4),
					     ext_v_input_bus_in_5       => v_bus(i, j)(5),
					     --sel_r_ext_out             =>    
					     sel_r_ext_out_0            => sel_r_seg(i, (j + 1) mod 2)(0),
					     sel_r_ext_out_1            => sel_r_seg(i, (j + 1) mod 2)(1),
					     sel_r_ext_out_2            => sel_r_seg(i, (j + 1) mod 2)(2),
					     sel_r_ext_out_3            => sel_r_seg(i, (j + 1) mod 2)(3),
					     sel_r_ext_out_4            => sel_r_seg(i, (j + 1) mod 2)(4),
					     sel_r_ext_out_5            => sel_r_seg(i, (j + 1) mod 2)(5),
					     --ext_v_input_bus_out       =>    
					     ext_v_input_bus_out_0      => v_bus(i, (j + 1) mod 2)(0),
					     ext_v_input_bus_out_1      => v_bus(i, (j + 1) mod 2)(1),
					     ext_v_input_bus_out_2      => v_bus(i, (j + 1) mod 2)(2),
					     ext_v_input_bus_out_3      => v_bus(i, (j + 1) mod 2)(3),
					     ext_v_input_bus_out_4      => v_bus(i, (j + 1) mod 2)(4),
					     ext_v_input_bus_out_5      => v_bus(i, (j + 1) mod 2)(5)


				);

		end generate MTRF_ROWS;

	end generate MTRF_COLS;

end architecture rtl;
