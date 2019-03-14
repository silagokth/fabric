-------------------------------------------------------
--! @file
--! @brief drra_types_n_constants
-------------------------------------------------------
---------------- Copyright (c) notice -----------------------------------------
--
-- The VHDL code, the logic and concepts described in this file constitute
-- the intellectual property of the authors listed below, who are affiliated
-- to KTH(Kungliga Tekniska Högskolan), School of ICT, Kista.
-- Any unauthorised use, copy or distribution is strictly prohibited.
-- Any authorised use, copy or distribution should carry this copyright notice
-- unaltered.
-------------------------------------------------------------------------------
-- Title      : DRRA + DiMArch fabric
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : fabric.vhd
-- Author     : Ahmed Hemani
-- Company    : KTH
-- Created    : 
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author            Description
--             1.0      Ahmed Hemani 
--             2.0      Muhammad Ali Shami
--             3.0      Omer Malik 
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	   #
--    General Public License as published by the Free Software Foundation, #
--    either version 3 of the License, or (at your option) any             #
--    later version.                                                       #
--                                                                         #
--    SiLago is distributed in the hope that it will be useful,            #
--    but WITHOUT ANY WARRANTY; without even the implied warranty of       #
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        #
--    GNU General Public License for more details.                         #
--                                                                         #
--    You should have received a copy of the GNU General Public License    #
--    along with SiLago.  If not, see <https://www.gnu.org/licenses/>.     #
--                                                                         #
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

--! IEEE Library
LIBRARY ieee;
--! Use standard library
USE ieee.std_logic_1164.ALL;
--! Use numeric standard library for arithmetic operations
USE ieee.numeric_std.ALL;
--! Use misc package
USE work.misc.ALL;

--! @brief DRRA type and constant package
--! @detail Contain constants and data type definitions for the DRRA
--! fabric.
PACKAGE drra_types_n_constants IS

	--Constans for SRAM_Sequencer
	CONSTANT MAX_DELAY           : INTEGER := 512;
	CONSTANT MAX_INCR_DECR_VALUE : INTEGER := 64;
	CONSTANT MAX_REPETITION      : INTEGER := 512;
	CONSTANT RAM_DEPTH           : INTEGER := 128;
	CONSTANT RAM_ADDRS_WIDTH     : INTEGER := log2_ceil(RAM_DEPTH);
	CONSTANT AGU_INSTR_WIDTH     : INTEGER := 100;

	---------------------AGU Signals----------------------------------------------------------------
	------------------------------------------------------------------------------------------------
	--------First Part---------------------
	--------Common Part--------------
	CONSTANT new_instr_s           : INTEGER := 1;
	CONSTANT new_instr_e           : INTEGER := 1;
	CONSTANT new_instr_WIDTH       : INTEGER := 1;
	CONSTANT mode_s                : INTEGER := 2;
	CONSTANT mode_e                : INTEGER := 2;
	CONSTANT mode_WIDTH            : INTEGER := 1;
	CONSTANT start_addrs_s         : INTEGER := 3;
	CONSTANT start_addrs_e         : INTEGER := 9;
	CONSTANT start_addrs_WIDTH     : INTEGER := 7;
	CONSTANT end_addrs_s           : INTEGER := 10;
	CONSTANT end_addrs_e           : INTEGER := 16;
	CONSTANT end_addrs_WIDTH       : INTEGER := 7;
	--------Vector Addressing--------
	CONSTANT incr_decr_s           : INTEGER := 17;
	CONSTANT incr_decr_e           : INTEGER := 17;
	CONSTANT incr_decr_WIDTH       : INTEGER := 1;
	CONSTANT incr_decr_value_s     : INTEGER := 18;
	CONSTANT incr_decr_value_e     : INTEGER := 24;
	CONSTANT incr_decr_value_WIDTH : INTEGER := 7;
	CONSTANT initial_delay_s       : INTEGER := 25;
	CONSTANT initial_delay_e       : INTEGER := 30;
	CONSTANT initial_delay_WIDTH   : INTEGER := 6;
	CONSTANT outputcontrol_s       : INTEGER := 31;
	CONSTANT outputcontrol_e       : INTEGER := 32;
	CONSTANT outputcontrol_WIDTH   : INTEGER := 2;

	--------Bit Reverse Addressing---
	CONSTANT start_stage_s     : INTEGER := 17;
	CONSTANT start_stage_e     : INTEGER := 19;
	CONSTANT start_stage_WIDTH : INTEGER := 3;
	CONSTANT end_stage_s       : INTEGER := 20;
	CONSTANT end_stage_e       : INTEGER := 22;
	CONSTANT end_stage_WIDTH   : INTEGER := 3;

	-------Second Part-------------------- 
	CONSTANT instruction_complete_s           : INTEGER := 33;
	CONSTANT instruction_complete_e           : INTEGER := 33;
	CONSTANT instruction_complete_WIDTH       : INTEGER := 1;
	CONSTANT infinite_loop_s                  : INTEGER := 34;
	CONSTANT infinite_loop_e                  : INTEGER := 34;
	CONSTANT infinite_loop_WIDTH              : INTEGER := 1;
	CONSTANT repetition_delay_s               : INTEGER := 35;
	CONSTANT repetition_delay_e               : INTEGER := 43;
	CONSTANT repetition_delay_WIDTH           : INTEGER := 9; --9
	CONSTANT no_of_repetitions_s              : INTEGER := 44;
	CONSTANT no_of_repetitions_e              : INTEGER := 52;
	CONSTANT no_of_repetitions_WIDTH          : INTEGER := 9;
	CONSTANT repetition_incr_decr_s           : INTEGER := 53;
	CONSTANT repetition_incr_decr_e           : INTEGER := 53;
	CONSTANT repetition_incr_decr_WIDTH       : INTEGER := 1; --1
	CONSTANT repetition_incr_decr_value_s     : INTEGER := 54;
	CONSTANT repetition_incr_decr_value_e     : INTEGER := 59;
	CONSTANT repetition_incr_decr_value_WIDTH : INTEGER := 6; --6
	CONSTANT middle_delay_s                   : INTEGER := 60;
	CONSTANT middle_delay_e                   : INTEGER := 68;
	CONSTANT middle_delay_WIDTH               : INTEGER := 9;

	------Third Part----------------------
	CONSTANT SRAM_inout_select_s        : INTEGER := 69;
	CONSTANT SRAM_inout_select_e        : INTEGER := 69;
	CONSTANT SRAM_inout_select_WIDTH    : INTEGER := 1;
	CONSTANT range_counter_s            : INTEGER := 70;
	CONSTANT range_counter_e            : INTEGER := 76;
	CONSTANT range_counter_WIDTH        : INTEGER := 7;
	CONSTANT hault_delay_s              : INTEGER := 77;
	CONSTANT hault_delay_e              : INTEGER := 85;
	CONSTANT hault_delay_WIDTH          : INTEGER := 9;
	CONSTANT hault_counter_s            : INTEGER := 86;
	CONSTANT hault_counter_e            : INTEGER := 91;
	CONSTANT hault_counter_WIDTH        : INTEGER := 6;
	CONSTANT demux_incr_dec_s           : INTEGER := 92;
	CONSTANT demux_incr_dec_e           : INTEGER := 93;
	CONSTANT demux_incr_dec_WIDTH       : INTEGER := 2;
	CONSTANT butterfly_loop_count_s     : INTEGER := 94;
	CONSTANT butterfly_loop_count_e     : INTEGER := 95;
	CONSTANT butterfly_loop_count_WIDTH : INTEGER := 2;
	CONSTANT butterfly_loop_delay_s     : INTEGER := 96;
	CONSTANT butterfly_loop_delay_e     : INTEGER := 99;
	CONSTANT butterfly_loop_delay_WIDTH : INTEGER := 4;
	CONSTANT inverter_select_s          : INTEGER := 100;
	CONSTANT inverter_select_e          : INTEGER := 100;
	CONSTANT inverter_select_WIDTH      : INTEGER := 1;

	--CONSTANT concat_range_s      : INTEGER := 94; CONSTANT concat_range_e              	 : INTEGER :=95;  CONSTANT concat_range_WIDTH                : INTEGER :=2;
	--CONSTANT butterfly_count_s   : INTEGER := 96; CONSTANT butterfly_count_e             : INTEGER :=97;  CONSTANT butterfly_count_WIDTH             : INTEGER :=2;
	--CONSTANT SRAM_inout_select_s          : INTEGER :=77; CONSTANT SRAM_inout_select_e           : INTEGER :=77;  CONSTANT SRAM_inout_select_WIDTH           : INTEGER :=1;


	---------------------------------------
	-- Types
	---------------------------------------
	TYPE Refi_AGU_st_ty IS (IDLE_ST, COUNT_ST, RD_WR_ST, BIT_REVRS_ST, REPETITION_ST, BUTTERFLYWAIT);

	--Constans for SRAM_Sequencer
	CONSTANT SRAM_SEQUENCER_INSTRUCTIONS : INTEGER := 16;
	--CONSTANT SRAM_WIDTH                  : INTEGER := 256;
	-- CONSTANT SRAM_DEPTH                  : INTEGER := RAM_DEPTH;
	CONSTANT SRAM_NumberOfInstrRegs      : INTEGER := 64;
	--CONSTANT SRAM_INSTR_WIDTH            : INTEGER := 36;
	--CONSTANT SRAM_AGU_INSTR_WIDTH        : INTEGER := AGU_INSTR_WIDTH;
	-- CONSTANT RFile_AGU_INSTR_WIDTH       : INTEGER := 68;
	-- CONSTANT SRAM_AGU_INSTR_WIDTH             : INTEGER := 104;
	-- CONSTANT SRAM_INCR_DECR_VALUE              : INTEGER := 64;
	-- CONSTANT SRAM_DELAY                        : INTEGER := 512;
	-- CONSTANT SRAM_REPETITIONS                  : INTEGER := 512;

	-- -------------------------RFile Signals for MEM Communications----------------------------------
	--   TYPE reg_INOUT_type                   IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF signed (SRAM_width-1 downto 0);
	--   
	-- fundamental architectural constants
	constant REGFILEADRWITH     : integer := 6;
	constant REGFILEMEMADRWITH  : integer := 2;
	constant REGFILEDATAWITH    : integer := 16;
	constant REGFILEMEMDATAWITH : integer := 256;
	constant REGFILEDEPTH       : integer := 64;
	constant FIFODEPTH          : integer := 2;
	constant FIFOADRWIDTH       : integer := 1;
	--constant SRAM_WIDTH :integer:=256;
	constant configwidth        : integer := 40;

	CONSTANT VERTICAL_WIRES          : INTEGER := 12;
	CONSTANT BITWIDTH                : INTEGER := 16;
	constant fraction                : integer := 0; -- for fixed point otherwise keep 0
	CONSTANT REGFILE_CONTEXTS        : INTEGER := 2;
	CONSTANT NrOfHop                 : INTEGER := 3;
	CONSTANT NrOfOutp                : INTEGER := 2; -- # of outp for Reg File/mDPU - 2
	CONSTANT COLUMNS                 : INTEGER := 5;
	CONSTANT ROWS                    : INTEGER := 2; -- # rows of reg files, mDPUs - 2
	CONSTANT ROWS_TOTAL              : INTEGER := 4; -- # total nr of rows - 4
	CONSTANT NumberOfRegFile         : INTEGER := 64;
	CONSTANT NumberOfInstrRegs       : INTEGER := 64;
	CONSTANT NumberOfInstrRegs49     : INTEGER := 49;
	CONSTANT NumberOfInstrRegs54     : INTEGER := 55;
	CONSTANT NumberOfInstrRegs24     : INTEGER := 24;
	-- CONSTANT NumberOfInstrRegs128       : INTEGER := 128;
	constant AdrOfInstrRegs          : integer := 6;
	constant AdrOfInstrRegs24        : integer := 5;
	--constant AdrOfInstrRegs128 :integer:=7;
	constant ADROFREGFILEWIDTH       : integer := 6;
	CONSTANT OUTP0                   : INTEGER := 0;
	CONSTANT OUTP1                   : INTEGER := 1;
	CONSTANT MaxNrOfOutpNHopAway     : INTEGER := 2 * NrOfHop + 1; -- Max # outps in NrOfHop range
	CONSTANT N                       : INTEGER := 12; -- # inps in a column
	CONSTANT MDPU_CONFIG_BITS        : INTEGER := 7;
	CONSTANT REG_FILE_OUTP_CTRL_BITS : INTEGER := 2;
	CONSTANT REGFILE_MODE_BITS       : INTEGER := 4;
	CONSTANT INSTR_WIDTH             : INTEGER := 36;
	--CONSTANT INSTR_WIDTH_REG_W       : INTEGER := 29;
	--  CONSTANT INSTR_WIDTH_REG_R       : INTEGER := 31;
	CONSTANT INSTR_WIDTH_IND_REG     : INTEGER := 30;
	CONSTANT INSTR_WIDTH_REG         : INTEGER := 32;
	CONSTANT NoOfContexts            : INTEGER := 2;
	CONSTANT COUNT_MAX               : INTEGER := 2 ** 10 - 1;
	CONSTANT DELAY_MAX               : INTEGER := 128;
	CONSTANT STATUS_NR_OF_BITs       : INTEGER := 11;

	CONSTANT cnt    : INTEGER := STATUS_NR_OF_BITs - 1;
	CONSTANT ovfl1  : INTEGER := STATUS_NR_OF_BITs - 2;
	CONSTANT undfl1 : INTEGER := STATUS_NR_OF_BITs - 3;
	CONSTANT gt1    : INTEGER := STATUS_NR_OF_BITs - 4;
	CONSTANT lt1    : INTEGER := STATUS_NR_OF_BITs - 5;
	CONSTANT eq1    : INTEGER := STATUS_NR_OF_BITs - 6;
	CONSTANT ovfl2  : INTEGER := STATUS_NR_OF_BITs - 7;
	CONSTANT undfl2 : INTEGER := STATUS_NR_OF_BITs - 8;
	CONSTANT gt2    : INTEGER := STATUS_NR_OF_BITs - 9;
	CONSTANT lt2    : INTEGER := STATUS_NR_OF_BITs - 10;
	CONSTANT eq2    : INTEGER := STATUS_NR_OF_BITs - 11;

	CONSTANT instr_code : INTEGER := 4;
	CONSTANT reg_mode   : INTEGER := 4;
	CONSTANT start_addr : INTEGER := 6;
	CONSTANT end_addr   : INTEGER := 6;
	CONSTANT delay      : INTEGER := 7;
	CONSTANT incr_dcr   : INTEGER := 1;
	CONSTANT offset     : INTEGER := 5;
	CONSTANT out_port   : INTEGER := 2;
	--	CONSTANT out_b                  : INTEGER := 2;
	CONSTANT valid      : INTEGER := 1;

	CONSTANT dpu_mode       : INTEGER := 4;
	CONSTANT saturation     : INTEGER := 5;
	CONSTANT count          : INTEGER := 10;
	CONSTANT out_a          : INTEGER := 2;
	CONSTANT out_b          : INTEGER := 2;
	CONSTANT dpu_count_acc  : INTEGER := 8;
	CONSTANT dpu_other_bits : INTEGER := 1;

	CONSTANT status                : INTEGER := 11;
	CONSTANT address               : INTEGER := 6;
	CONSTANT wait_other_bits       : INTEGER := 14;
	--Added by Ali
	CONSTANT const_instr_const1    : INTEGER := 16;
	CONSTANT const_instr_otherbits : INTEGER := 16;

	CONSTANT port_type              : INTEGER := 2;
	CONSTANT i_rept_ty              : INTEGER := 2;
	CONSTANT i_valid                : INTEGER := 1;
	CONSTANT i_mode                 : INTEGER := 1;
	CONSTANT i_start_addrs          : INTEGER := 6;
	CONSTANT i_end_addrs            : INTEGER := 6;
	CONSTANT i_incr_decr            : INTEGER := 1;
	CONSTANT i_incr_decr_value      : INTEGER := 5;
	CONSTANT i_initial_delay        : INTEGER := 6;
	CONSTANT i_outputcontrol        : INTEGER := 2;
	CONSTANT i_instruction_complete : INTEGER := 1;
	CONSTANT i_infinite_loop        : INTEGER := 1;

	CONSTANT i_repetition_delay           : INTEGER := 6;
	CONSTANT i_no_of_repetitions          : INTEGER := 6;
	CONSTANT i_repetition_incr_decr       : INTEGER := 1;
	CONSTANT i_repetition_incr_decr_value : INTEGER := 5;
	CONSTANT i_middle_delay               : INTEGER := 6;
	CONSTANT i_range_counter              : INTEGER := 6;
	CONSTANT i_remaining_bits             : INTEGER := 2;
	CONSTANT i_start_stage                : INTEGER := 3;
	CONSTANT i_end_stage                  : INTEGER := 3;

	CONSTANT i_offset_RA     : INTEGER := 6;
	CONSTANT i_RA_incr_dcr   : INTEGER := 1;
	CONSTANT i_offset_RB     : INTEGER := 6;
	CONSTANT i_RB_incr_dcr   : INTEGER := 1;
	CONSTANT i_offset_WA     : INTEGER := 6;
	CONSTANT i_WA_incr_dcr   : INTEGER := 1;
	CONSTANT i_offset_WB     : INTEGER := 6;
	CONSTANT i_WB_incr_dcr   : INTEGER := 1;
	CONSTANT i_offset_others : INTEGER := 2;

	CONSTANT and_or_cmp : INTEGER := 1;

	CONSTANT nops_count       : INTEGER := 10;
	CONSTANT nops_unused_bits : INTEGER := 22;
	CONSTANT count_cnfg       : INTEGER := 2;
	CONSTANT count_valid_flag : INTEGER := 1;
	CONSTANT count_valid_reg  : INTEGER := 1;
	CONSTANT count_other_bits : INTEGER := 18;

	CONSTANT sb_dav        : INTEGER := 1;
	CONSTANT sb_row        : INTEGER := 2;
	CONSTANT sb_v_index    : INTEGER := 4;
	CONSTANT sb_hb_index   : INTEGER := 3;
	CONSTANT sb_outp_index : INTEGER := 1;
	CONSTANT sb_other_bits : INTEGER := 21;

	-----------------------------------------------------------------------------
	-- BEQ CONSTANTS
	-----------------------------------------------------------------------------
	CONSTANT TRUE_ADDRESS    : INTEGER := 6;
	CONSTANT FALSE_ADDRESS   : INTEGER := 6;
	CONSTANT BEQ_UNUSED_BITS : INTEGER := 8;
	------------------------------------------------------------------------------
	--Regfile Constants
	--TYPE Refi_mode_ty IS (RD_WR,CB,BIT_REVERSE,RESERVED);
	--CONSTANT MAX_DELAY               : INTEGER := 64;
	--CONSTANT MAX_INCR_DECR_VALUE     : INTEGER := 32;
	-- CONSTANT	MAX_REPETITION          : INTEGER := 64;
	type vbussignals is array (COLUMNS - 1 downto 0) of std_logic_vector(INSTR_WIDTH+1 downto 0); 
	type vbussignals_array is array (NATURAL range <>) of vbussignals;
	TYPE instr_fifo_type is ARRAY (0 to NoOfContexts - 1) of UNSIGNED(2 * INSTR_WIDTH_REG + 1 downto 0);
	TYPE reg_context_type is ARRAY (0 to NoOfContexts - 1) of UNSIGNED(2 * INSTR_WIDTH_REG - 1 downto 0); --Added two more bits for middle/repetition delay valid on or off
	-- TYPE Refi_AGU_st_ty IS (IDLE_ST, COUNT_ST,  RD_WR_ST, BIT_REVRS_ST, REPETITION_ST);
	--	 TYPE Refi_AGU_addrs_state IS (RD_WR_ST, BIT_REVRS_ST);

	TYPE reg_port_type_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(1 downto 0);
	TYPE reg_instruction_input_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(INSTR_WIDTH_REG - 1 DOWNTO 0);
	TYPE reg_delay_offset_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(INSTR_WIDTH_REG - 1 DOWNTO 0);

	-----------------------RFile-TO-MEM-Communication---------------------------
	TYPE reg_mem_transfer_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
	TYPE reg_read_write_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
	TYPE reg_addrs_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(log2_ceil(NumberOfRegFile / 16) - 1 downto 0);

	-------------------------------------------------------------------------------
	CONSTANT HC_OUT_BITS : INTEGER := 4;
	TYPE All_HC_in_bus_type is ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(HC_OUT_BITS - 1 DOWNTO 0);
	--Sequencer

	CONSTANT HIERARICHAL_CONTROL_BIT : INTEGER := 32;

	TYPE HC_out_w_type is ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(HC_OUT_BITS - 1 downto 0);
	TYPE HC_in_bus_type is ARRAY (NATURAL RANGE <>) OF std_logic_vector(HC_OUT_BITS - 1 DOWNTO 0);
	--  TYPE HC_Hbus IS ARRAY (NATURAL RANGE <>) OF SIGNED (HC_OUT_BITS-1 DOWNTO 0);
	TYPE HC_zbus_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(HC_OUT_BITS - 1 DOWNTO 0);
	TYPE SBUS_OUT_TYPE is ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(10 DOWNTO 0);
	type reg_inst_temp_type is array (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(33 DOWNTO 0);
	---------------------------------------------------------------------------------
	TYPE FFT_DIRECT32_TYPE IS ARRAY (0 TO COLUMNS - 1, 0 TO ROWS) OF SIGNED(BITWIDTH * 2 DOWNTO 0);


	TYPE Hbus IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF SIGNED(BITWIDTH - 1 DOWNTO 0);
	TYPE Vbus IS ARRAY (0 TO N - 1) OF SIGNED(BITWIDTH - 1 DOWNTO 0);
	TYPE Sbus_Switchbox IS ARRAY (0 to N - 1) OF std_logic_vector(5 downto 0);

	TYPE Sbus IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(0 TO 3);

	TYPE SbusF IS ARRAY (0 TO MaxNrOfOutpNHopAway - 1, 0 TO NrOfOutp - 1) OF STD_LOGIC_VECTOR(N - 1 downto 0);
	TYPE vlane_type IS ARRAY (NATURAL RANGE <>) OF Vbus;
	TYPE data_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF SIGNED(BITWIDTH - 1 DOWNTO 0);
	TYPE inverter_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic;

	TYPE signal_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
	TYPE mDPU_config_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR(MDPU_CONFIG_BITS - 1 DOWNTO 0);
	TYPE reg_addrs_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF unsigned(log2_ceil(NumberOfRegFile) - 1 DOWNTO 0); --added by ali, changed std to unsigned

	TYPE Sbus_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF SbusF;
	TYPE RegF_Inp_Sel_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR(0 TO 3);

	TYPE reg_type is array (0 to NumberOfRegFile - 1) of signed(BITWIDTH - 1 downto 0);

	TYPE config_mem_type is array (4 * 2 * MaxNrOfOutpNHopAway - 1 downto 0) of UNSIGNED(0 to N - 1);

	TYPE middle_delay_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(5 DOWNTO 0);
	TYPE full_signal_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic;
	TYPE initPoint_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(1 DOWNTO 0);

	------------------mDPU Types Declaration-----------------------------------------

	TYPE mdpu_delay_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF integer range 0 to DELAY_MAX - 1;
	TYPE mdpu_saturation_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(4 downto 0); --integer range 0 to 5  ;
	TYPE cfg_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(20 downto 0);
	TYPE const_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(BITWIDTH - 1 downto 0);
	TYPE count_acc_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(7 downto 0);
	TYPE count_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF integer range 0 to COUNT_MAX - 1;
	TYPE counter_cfg_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(1 downto 0);
	TYPE valid_flag_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;

	TYPE outputcontrol_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(1 downto 0);

	TYPE status_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(STATUS_NR_OF_BITs - 1 downto 0);
	TYPE addressout_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF SIGNED(5 downto 0);
	TYPE forward_array_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(1 DOWNTO 0);
	type end_adr_mode_type is array (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(3 DOWNTO 0);

	type std_array_type is array (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic;
	TYPE origin_type   IS (FROM_SOURCE,FROM_DESTINATION);
	------------------------------LCC ------------------------------------------------------------------------
	type lccelements is array (COLUMNS - 1 downto 0) of std_logic_vector(COLUMNS + ROWS + configwidth downto 0);
	
	
END drra_types_n_constants;


