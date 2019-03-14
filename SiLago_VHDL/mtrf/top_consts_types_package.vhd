-------------------------------------------------------
--! @file
--! @brief Top Constant Package for the DRRA fabric.
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
-- Title      : top_consts_types_package
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : top_consts_types_package.vhd
-- Author     : Sadiq Hemani <sadiq@kth.se>, Hojat Khoshrowjerdi <hojatk@kth.se>, Jingying Dong <jdon@kth.se>
-- Company    : KTH
-- Created    : 2013-09-05
-- Last update: 2014-10-26
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: To be included in all MTRF components and the top module. Includes
-- all the required constants and type declarations for the Sequencer, DPU and
-- Register file (with AGUs). The sequencer constants
-- have been declared in a parametric style to facilitate any future instruction
-- format changes.
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		  Description
-- 2013-09-05  1.0      sadiq			  Created
-- 2014-02-25  2.0      Nasim Farahini    Modified
-- 2014-05-10  3.0      Naism Farahini    Raccu and loop instructions are added
-- 2015-02-22  4.0      Hassan Sohofi     Updating branch and jump instructions
-- 2019-03-11  4.1      Dimitrios Stathis Include lic. and comments
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	     #
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

--! IEEE standard library.
LIBRARY ieee;
--! Use standard library.
USE ieee.std_logic_1164.ALL;
--! Use numeric library.
USE ieee.numeric_std.ALL;
--! Import and use the utility package.
USE work.util_package.ALL;
--! Import and use the hw setting package.
USE work.hw_setting.ALL;


--! @brief Top Constant Package for the DRRA fabric.
--! @details This package contain the basic constants
--! and type definitions for the DRRA fabric.
PACKAGE top_consts_types_package IS

	--<GLOBAL CONSTANTS BEGIN>--
	CONSTANT BITWIDTH    : natural := 16; --h_bus bitwidth
	CONSTANT INSTR_WIDTH : natural := 27;
	--  CONSTANT OLD_INSTR_WIDTH : natural := 36;
	--  CONSTANT INSTR_WIDTH_DIFF : natural := OLD_INSTR_WIDTH - INSTR_WIDTH; -- 12
	CONSTANT INSTR_DEPTH : natural := HW_INSTR_DEPTH;

	--<GLOBAL CONSTANTS END>--

	--<GLOBAL TYPES BEGIN>--
	--<GLOBAL TYPES END>--

	--<SEQUENCER CONSTANTS BEGIN>--

	CONSTANT SEQ_ADDRS_WIDTH : natural                                        := 4;
	CONSTANT SEQ_ADDRS       : std_logic_vector(SEQ_ADDRS_WIDTH - 1 DOWNTO 0) := "0000"; -- address of the sequencer
	-- for testing purposes
	CONSTANT PC_SIZE         : natural                                        := log2(INSTR_DEPTH); -- for a 64x24 bit REGISTER
	CONSTANT INSTR_REG_DEPTH : natural                                        := log2(INSTR_DEPTH);
	CONSTANT PC_BASE_PROG    : natural                                        := 0; -- base pc value, where PC is "pc <= PC + 1" Set to pc as default in the sequencer
	CONSTANT PC_INCREM_WIDTH : natural                                        := 2; --
	--width of the PC_INCREM signal

	CONSTANT INCREMENT_OPTS : natural := 1; -- Should be ceil2(log2(nr_of_increment_options)). In this case we have 2 options
	CONSTANT NR_OF_OUTPORTS : natural := 1; -- should be ceil2(log2(nr_of_outports)). In this case we have 2 outports

	--<SEQUENCER SAMPLE INSTRUCTION TYPE CONSTANTS>--
	CONSTANT INCREMENT_RANGE_BASE           : natural := INSTR_WIDTH - 1; -- start of range declaration for the increment portion of the instruction
	CONSTANT INCREMENT_RANGE_END            : natural := INCREMENT_RANGE_BASE - (INCREMENT_OPTS - 1);
	CONSTANT INCREMENT_VECTOR_SIZE          : natural := INCREMENT_RANGE_BASE - INCREMENT_RANGE_END;
	CONSTANT OUTPORT_RANGE_BASE             : natural := INCREMENT_RANGE_END - 1;
	CONSTANT OUTPORT_RANGE_END              : natural := OUTPORT_RANGE_BASE - (NR_OF_OUTPORTS - 1);
	CONSTANT OUTPORT_VECTOR_SIZE            : natural := OUTPORT_RANGE_BASE - OUTPORT_RANGE_END;
	CONSTANT JUMP_INSTR_ADDRESS_BASE        : natural := OUTPORT_RANGE_END - 1;
	CONSTANT JUMP_INSTR_ADDRESS_END         : natural := JUMP_INSTR_ADDRESS_BASE - (PC_SIZE - 1); --
	--changed PC_SIZE - 1 ==> PC_SIZE
	CONSTANT JUMP_INSTR_ADDRESS_VECTOR_SIZE : natural := JUMP_INSTR_ADDRESS_BASE - JUMP_INSTR_ADDRESS_END;
	CONSTANT VACANT_BITS_BASE               : natural := JUMP_INSTR_ADDRESS_END - 1;
	CONSTANT VACANT_BITS_END                : natural := 0;
	CONSTANT VACANT_BITS_VECTOR_SIZE        : natural := VACANT_BITS_BASE - VACANT_BITS_END;
	CONSTANT VACANT_BITS_REMAIN             : natural := INSTR_WIDTH - ((INCREMENT_VECTOR_SIZE + 1) + (OUTPORT_VECTOR_SIZE + 1) + (JUMP_INSTR_ADDRESS_VECTOR_SIZE + 1));

	--<GENERAL INSTRUCTION TYPE CONSTANTS>--
	CONSTANT NR_OF_INSTR_TYPES : natural := 4;

	CONSTANT REFI1      : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0001"; -- used 1
	CONSTANT REFI2      : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0010"; -- used 2
	CONSTANT REFI3      : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0011"; -- used 3
	CONSTANT DPU        : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0100"; -- used 4
	CONSTANT SWB        : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0101"; -- used 5
	CONSTANT JUMP       : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0110"; -- used 6
	CONSTANT DELAY      : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "0111"; -- used 7
	CONSTANT FOR_HEADER : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1000"; --used 8
	CONSTANT FOR_TAIL   : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1001"; --used 9
	CONSTANT RACCU      : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1010"; -- used 10
	CONSTANT BRANCH     : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1011"; -- used 11
	CONSTANT ROUTE      : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1100"; -- used 12
	CONSTANT READ_SRAM  : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1101"; -- used 13
	CONSTANT WRITE_SRAM : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1110"; -- used 14

	CONSTANT HALT : std_logic_vector(NR_OF_INSTR_TYPES - 1 DOWNTO 0) := "1111"; -- NOT USED 15

	CONSTANT INSTR_CODE_RANGE_BASE        : natural := INSTR_WIDTH - 1;
	CONSTANT INSTR_CODE_RANGE_END         : natural := INSTR_CODE_RANGE_BASE - (NR_OF_INSTR_TYPES - 1);
	CONSTANT INSTR_CODE_RANGE_VECTOR_SIZE : natural := INSTR_CODE_RANGE_BASE - INSTR_CODE_RANGE_END + 1;

	--<REFI1 INSTRUCTION TYPE CONSTANTS>--
	CONSTANT NR_OF_REG_FILE_PORTS : natural := 2;
	CONSTANT NR_OF_INSTRS         : natural := 2;
	CONSTANT STARTING_ADDRS       : natural := 6;
	CONSTANT NR_OF_ADDRS          : natural := 6;
	CONSTANT INIT_DELAY           : natural := 4;

	CONSTANT NR_OF_REG_FILE_PORTS_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT NR_OF_REG_FILE_PORTS_RANGE_END   : natural := NR_OF_REG_FILE_PORTS_RANGE_BASE - (NR_OF_REG_FILE_PORTS - 1);
	CONSTANT NR_OF_REG_FILE_PORTS_VECTOR_SIZE : natural := NR_OF_REG_FILE_PORTS_RANGE_BASE - NR_OF_REG_FILE_PORTS_RANGE_END + 1;

	CONSTANT NR_OF_INSTRS_RANGE_BASE  : natural := NR_OF_REG_FILE_PORTS_RANGE_END - 1;
	CONSTANT NR_OF_INSTRS_RANGE_END   : natural := NR_OF_INSTRS_RANGE_BASE - (NR_OF_INSTRS - 1 );
	CONSTANT NR_OF_INSTRS_VECTOR_SIZE : natural := NR_OF_INSTRS_RANGE_BASE - NR_OF_INSTRS_RANGE_END + 1;

	CONSTANT STARTING_ADDRS_RANGE_BASE  : natural := NR_OF_INSTRS_RANGE_END - 2; --one is for static or dynamic
	CONSTANT STARTING_ADDRS_RANGE_END   : natural := STARTING_ADDRS_RANGE_BASE - (STARTING_ADDRS - 1);
	CONSTANT STARTING_ADDRS_VECTOR_SIZE : natural := STARTING_ADDRS_RANGE_BASE - STARTING_ADDRS_RANGE_END + 1;

	CONSTANT NR_OF_ADDRS_RANGE_BASE  : natural := STARTING_ADDRS_RANGE_END - 2;
	CONSTANT NR_OF_ADDRS_RANGE_END   : natural := NR_OF_ADDRS_RANGE_BASE - (NR_OF_ADDRS - 1 );
	CONSTANT NR_OF_ADDRS_VECTOR_SIZE : natural := NR_OF_ADDRS_RANGE_BASE - NR_OF_ADDRS_RANGE_END + 1;

	CONSTANT INIT_DELAY_RANGE_BASE  : natural := NR_OF_ADDRS_RANGE_END - 2;
	CONSTANT INIT_DELAY_RANGE_END   : natural := INIT_DELAY_RANGE_BASE - (INIT_DELAY - 1 );
	CONSTANT INIT_DELAY_VECTOR_SIZE : natural := INIT_DELAY_RANGE_BASE - INIT_DELAY_RANGE_END + 1;

	--<REFI2 INSTRUCTION TYPE CONSTANTS>--
	CONSTANT STEP_VALUE            : natural := 6;
	CONSTANT STEP_VALUE_SIGN       : natural := 1;
	CONSTANT REG_FILE_MIDDLE_DELAY : natural := 4;
	CONSTANT NUM_OF_REPT           : natural := 5;
	CONSTANT REP_STEP_VALUE        : natural := 4;

	CONSTANT STEP_VALUE_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 2;
	CONSTANT STEP_VALUE_RANGE_END   : natural := STEP_VALUE_RANGE_BASE - (STEP_VALUE - 1);
	CONSTANT STEP_VALUE_VECTOR_SIZE : natural := STEP_VALUE_RANGE_BASE - STEP_VALUE_RANGE_END + 1;

	CONSTANT STEP_VALUE_SIGN_RANGE_BASE  : natural := STEP_VALUE_RANGE_END - 1;
	CONSTANT STEP_VALUE_SIGN_RANGE_END   : natural := STEP_VALUE_SIGN_RANGE_BASE - (STEP_VALUE_SIGN - 1);
	CONSTANT STEP_VALUE_SIGN_VECTOR_SIZE : natural := STEP_VALUE_SIGN_RANGE_BASE - STEP_VALUE_SIGN_RANGE_END + 1;

	CONSTANT REG_FILE_MIDDLE_DELAY_RANGE_BASE  : natural := STEP_VALUE_SIGN_RANGE_END - 2;
	CONSTANT REG_FILE_MIDDLE_DELAY_RANGE_END   : natural := REG_FILE_MIDDLE_DELAY_RANGE_BASE - (REG_FILE_MIDDLE_DELAY - 1 );
	CONSTANT REG_FILE_MIDDLE_DELAY_VECTOR_SIZE : natural := REG_FILE_MIDDLE_DELAY_RANGE_BASE - REG_FILE_MIDDLE_DELAY_RANGE_END + 1;

	CONSTANT NUM_OF_REPT_RANGE_BASE  : natural := REG_FILE_MIDDLE_DELAY_RANGE_END - 2;
	CONSTANT NUM_OF_REPT_RANGE_END   : natural := NUM_OF_REPT_RANGE_BASE - (NUM_OF_REPT - 1);
	CONSTANT NUM_OF_REPT_VECTOR_SIZE : natural := NUM_OF_REPT_RANGE_BASE - NUM_OF_REPT_RANGE_END + 1;

	CONSTANT REP_STEP_VALUE_RANGE_BASE  : natural := NUM_OF_REPT_RANGE_END - 1;
	CONSTANT REP_STEP_VALUE_RANGE_END   : natural := REP_STEP_VALUE_RANGE_BASE - (REP_STEP_VALUE - 1);
	CONSTANT REP_STEP_VALUE_VECTOR_SIZE : natural := REP_STEP_VALUE_RANGE_BASE - REP_STEP_VALUE_RANGE_END + 1;

	--<REFI3 INSTRUCTION TYPE CONSTANTS>--
	CONSTANT REPT_DELAY                : natural := 6;
	CONSTANT MODE_SEL                  : natural := 1;
	CONSTANT OUTPUT_CONTROL            : natural := 2;
	CONSTANT FFT_STAGE_SEL             : natural := 3;
	CONSTANT REG_FILE_MIDDLE_DELAY_EXT : natural := 2;
	CONSTANT NUM_OF_REPT_EXT           : natural := 1;
	CONSTANT REP_STEP_VALUE_EXT        : natural := 2;
	CONSTANT FFT_END_STAGE             : natural := 3;
	CONSTANT REFI3_UNUSED              : natural := 1; -- changed from 2 to 1 for sram_interface

	CONSTANT REPT_DELAY_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 2;
	CONSTANT REPT_DELAY_RANGE_END   : natural := REPT_DELAY_RANGE_BASE - (REPT_DELAY - 1);
	CONSTANT REPT_DELAY_VECTOR_SIZE : natural := REPT_DELAY_RANGE_BASE - REPT_DELAY_RANGE_END + 1;

	CONSTANT MODE_SEL_RANGE_BASE  : natural := REPT_DELAY_RANGE_END - 1;
	CONSTANT MODE_SEL_RANGE_END   : natural := MODE_SEL_RANGE_BASE - (MODE_SEL - 1);
	CONSTANT MODE_SEL_VECTOR_SIZE : natural := MODE_SEL_RANGE_BASE - MODE_SEL_RANGE_END + 1;

	CONSTANT OUTPUT_CONTROL_RANGE_BASE  : natural := MODE_SEL_RANGE_END - 1;
	CONSTANT OUTPUT_CONTROL_RANGE_END   : natural := OUTPUT_CONTROL_RANGE_BASE - (OUTPUT_CONTROL - 1);
	CONSTANT OUTPUT_CONTROL_VECTOR_SIZE : natural := OUTPUT_CONTROL_RANGE_BASE - OUTPUT_CONTROL_RANGE_END + 1;

	CONSTANT FFT_STAGE_SEL_RANGE_BASE  : natural := OUTPUT_CONTROL_RANGE_END - 1;
	CONSTANT FFT_STAGE_SEL_RANGE_END   : natural := FFT_STAGE_SEL_RANGE_BASE - (FFT_STAGE_SEL - 1);
	CONSTANT FFT_STAGE_SEL_VECTOR_SIZE : natural := FFT_STAGE_SEL_RANGE_BASE - FFT_STAGE_SEL_RANGE_END + 1;

	CONSTANT REG_FILE_MIDDLE_DELAY_EXT_RANGE_BASE  : natural := FFT_STAGE_SEL_RANGE_END - 1;
	CONSTANT REG_FILE_MIDDLE_DELAY_EXT_RANGE_END   : natural := REG_FILE_MIDDLE_DELAY_EXT_RANGE_BASE - (REG_FILE_MIDDLE_DELAY_EXT - 1);
	CONSTANT REG_FILE_MIDDLE_DELAY_EXT_VECTOR_SIZE : natural := REG_FILE_MIDDLE_DELAY_EXT_RANGE_BASE - REG_FILE_MIDDLE_DELAY_EXT_RANGE_END + 1;

	CONSTANT REG_FILE_MIDDLE_DELAY_PORT_SIZE : natural := REG_FILE_MIDDLE_DELAY_VECTOR_SIZE + REG_FILE_MIDDLE_DELAY_EXT_VECTOR_SIZE;

	CONSTANT NUM_OF_REPT_EXT_RANGE_BASE  : natural := REG_FILE_MIDDLE_DELAY_EXT_RANGE_END - 1;
	CONSTANT NUM_OF_REPT_EXT_RANGE_END   : natural := NUM_OF_REPT_EXT_RANGE_BASE - (NUM_OF_REPT_EXT - 1);
	CONSTANT NUM_OF_REPT_EXT_VECTOR_SIZE : natural := NUM_OF_REPT_EXT_RANGE_BASE - NUM_OF_REPT_EXT_RANGE_END + 1;

	CONSTANT NUM_OF_REPT_PORT_SIZE : natural := NUM_OF_REPT_VECTOR_SIZE + NUM_OF_REPT_EXT_VECTOR_SIZE;

	CONSTANT REP_STEP_VALUE_EXT_RANGE_BASE  : natural := NUM_OF_REPT_EXT_RANGE_END - 1;
	CONSTANT REP_STEP_VALUE_EXT_RANGE_END   : natural := REP_STEP_VALUE_EXT_RANGE_BASE - (REP_STEP_VALUE_EXT - 1);
	CONSTANT REP_STEP_VALUE_EXT_VECTOR_SIZE : natural := REP_STEP_VALUE_EXT_RANGE_BASE - REP_STEP_VALUE_EXT_RANGE_END + 1;

	CONSTANT REP_STEP_VALUE_PORT_SIZE : natural := REP_STEP_VALUE_VECTOR_SIZE + REP_STEP_VALUE_EXT_VECTOR_SIZE;

	CONSTANT FFT_END_STAGE_RANGE_BASE  : natural := REP_STEP_VALUE_EXT_RANGE_END - 1;
	CONSTANT FFT_END_STAGE_RANGE_END   : natural := FFT_END_STAGE_RANGE_BASE - (FFT_END_STAGE - 1);
	CONSTANT FFT_END_STAGE_VECTOR_SIZE : natural := FFT_END_STAGE_RANGE_BASE - FFT_END_STAGE_RANGE_END + 1;

	CONSTANT DIMARCH_MODE_BIT         : natural := FFT_END_STAGE_RANGE_END - 1;
	CONSTANT REFI3_UNUSED_RANGE_BASE  : natural := DIMARCH_MODE_BIT - 1;
	CONSTANT REFI3_UNUSED_RANGE_END   : natural := REFI3_UNUSED_RANGE_BASE - (REFI3_UNUSED - 1);
	CONSTANT REFI3_UNUSED_VECTOR_SIZE : natural := REFI3_UNUSED_RANGE_BASE - REFI3_UNUSED_RANGE_END + 1;

	--<DPU INSTRUCTION TYPE CONSTANTS>--

	CONSTANT DPU_MODE_SEL      : natural := 5;
	CONSTANT DPU_SATURAT       : natural := 2;
	CONSTANT DPU_OUTP_A        : natural := 2;
	CONSTANT DPU_OUTP_B        : natural := 2;
	CONSTANT DPU_ACC_CLEAR_BIT : natural := HW_DPU_CONSTANT_WIDTH;
	CONSTANT DPU_PROCESS_INOUT : natural := 2;

	CONSTANT DPU_MODE_SEL_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT DPU_MODE_SEL_RANGE_END   : natural := DPU_MODE_SEL_RANGE_BASE - (DPU_MODE_SEL - 1);
	CONSTANT DPU_MODE_SEL_VECTOR_SIZE : natural := DPU_MODE_SEL_RANGE_BASE - DPU_MODE_SEL_RANGE_END + 1;

	CONSTANT DPU_SATURAT_RANGE_BASE  : natural := DPU_MODE_SEL_RANGE_END - 1;
	CONSTANT DPU_SATURAT_RANGE_END   : natural := DPU_SATURAT_RANGE_BASE - (DPU_SATURAT - 1);
	CONSTANT DPU_SATURAT_VECTOR_SIZE : natural := DPU_SATURAT_RANGE_BASE - DPU_SATURAT_RANGE_END + 1;

	CONSTANT DPU_OUTP_A_RANGE_BASE  : natural := DPU_SATURAT_RANGE_END - 1;
	CONSTANT DPU_OUTP_A_RANGE_END   : natural := DPU_OUTP_A_RANGE_BASE - (DPU_OUTP_A - 1);
	CONSTANT DPU_OUTP_A_VECTOR_SIZE : natural := DPU_OUTP_A_RANGE_BASE - DPU_OUTP_A_RANGE_END + 1;

	CONSTANT DPU_OUTP_B_RANGE_BASE  : natural := DPU_OUTP_A_RANGE_END - 1;
	CONSTANT DPU_OUTP_B_RANGE_END   : natural := DPU_OUTP_B_RANGE_BASE - (DPU_OUTP_B - 1);
	CONSTANT DPU_OUTP_B_VECTOR_SIZE : natural := DPU_OUTP_B_RANGE_BASE - DPU_OUTP_B_RANGE_END + 1;

	CONSTANT DPU_ACC_CLEAR_RANGE_BASE  : natural := DPU_OUTP_B_RANGE_END - 3; --one is for acc_clear_sd; one bit for acc_clear_rst
	CONSTANT DPU_ACC_CLEAR_RANGE_END   : natural := DPU_ACC_CLEAR_RANGE_BASE - (DPU_ACC_CLEAR_BIT - 1);
	CONSTANT DPU_ACC_CLEAR_VECTOR_SIZE : natural := DPU_ACC_CLEAR_RANGE_BASE - DPU_ACC_CLEAR_RANGE_END + 1;

	CONSTANT DPU_PROCESS_INOUT_RANGE_BASE  : natural := DPU_ACC_CLEAR_RANGE_END - 1;
	CONSTANT DPU_PROCESS_INOUT_RANGE_END   : natural := DPU_PROCESS_INOUT_RANGE_BASE - (DPU_PROCESS_INOUT - 1);
	CONSTANT DPU_PROCESS_INOUT_VECTOR_SIZE : natural := DPU_PROCESS_INOUT_RANGE_BASE - DPU_PROCESS_INOUT_RANGE_END + 1;

	--<SWITCHBOX INSTRUCTION TYPE CONSTANTS>--
	CONSTANT SWB_DAV               : natural := 1;
	CONSTANT SWB_SRC_ADDR_ROW      : natural := 1;
	CONSTANT SWB_SRC_DPU_REFI      : natural := 1;
	CONSTANT SWB_SRC_OUTPUT_NR     : natural := 1;
	CONSTANT SWB_HB_INDEX          : natural := 3;
	CONSTANT SWB_SEND_TO_OTHER_ROW : natural := 1;
	CONSTANT SWB_V_INDEX           : natural := 3;
	CONSTANT SWB_INSTR_WIDTH       : natural := SWB_DAV + SWB_SRC_ADDR_ROW + SWB_SRC_DPU_REFI + SWB_SRC_OUTPUT_NR + SWB_HB_INDEX + SWB_SEND_TO_OTHER_ROW + SWB_V_INDEX;
	--CONSTANT SWB_TO_PORT   : natural := 2;
	CONSTANT SWB_UNUSED            : natural := 3;

	CONSTANT SWB_DAV_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT SWB_DAV_RANGE_END   : natural := SWB_DAV_RANGE_BASE - (SWB_DAV - 1);
	CONSTANT SWB_DAV_VECTOR_SIZE : natural := SWB_DAV_RANGE_BASE - SWB_DAV_RANGE_END + 1;

	CONSTANT SWB_SRC_ADDR_ROW_BASE        : natural := SWB_DAV_RANGE_END - 1;
	CONSTANT SWB_SRC_ADDR_ROW_END         : natural := SWB_SRC_ADDR_ROW_BASE - (SWB_SRC_ADDR_ROW - 1);
	CONSTANT SWB_SRC_ADDR_ROW_VECTOR_SIZE : natural := SWB_SRC_ADDR_ROW_BASE - SWB_SRC_ADDR_ROW_END + 1;

	CONSTANT SWB_SRC_DPU_REFI_BASE        : natural := SWB_SRC_ADDR_ROW_END - 1;
	CONSTANT SWB_SRC_DPU_REFI_END         : natural := SWB_SRC_DPU_REFI_BASE - (SWB_SRC_DPU_REFI - 1);
	CONSTANT SWB_SRC_DPU_REFI_VECTOR_SIZE : natural := SWB_SRC_DPU_REFI_BASE - SWB_SRC_DPU_REFI_END + 1;

	CONSTANT SWB_SRC_OUTPUT_NR_BASE        : natural := SWB_SRC_DPU_REFI_END - 1;
	CONSTANT SWB_SRC_OUTPUT_NR_END         : natural := SWB_SRC_OUTPUT_NR_BASE - (SWB_SRC_OUTPUT_NR - 1);
	CONSTANT SWB_SRC_OUTPUT_NR_VECTOR_SIZE : natural := SWB_SRC_OUTPUT_NR_BASE - SWB_SRC_OUTPUT_NR_END + 1;

	CONSTANT SWB_HB_INDEX_BASE        : natural := SWB_SRC_OUTPUT_NR_END - 1;
	CONSTANT SWB_HB_INDEX_END         : natural := SWB_HB_INDEX_BASE - (SWB_HB_INDEX - 1);
	CONSTANT SWB_HB_INDEX_VECTOR_SIZE : natural := SWB_HB_INDEX_BASE - SWB_HB_INDEX_END + 1;

	CONSTANT SWB_SEND_TO_OTHER_ROW_BASE        : natural := SWB_HB_INDEX_END - 1;
	CONSTANT SWB_SEND_TO_OTHER_ROW_END         : natural := SWB_SEND_TO_OTHER_ROW_BASE - (SWB_SEND_TO_OTHER_ROW - 1);
	CONSTANT SWB_SEND_TO_OTHER_ROW_VECTOR_SIZE : natural := SWB_SEND_TO_OTHER_ROW_BASE - SWB_SEND_TO_OTHER_ROW_END + 1;

	CONSTANT SWB_V_INDEX_BASE        : natural := SWB_SEND_TO_OTHER_ROW_END - 1;
	CONSTANT SWB_V_INDEX_END         : natural := SWB_V_INDEX_BASE - (SWB_V_INDEX - 1);
	CONSTANT SWB_V_INDEX_VECTOR_SIZE : natural := SWB_V_INDEX_BASE - SWB_V_INDEX_END + 1;

	CONSTANT SWB_UNUSED_RANGE_BASE  : natural := SWB_V_INDEX_END - 1;
	CONSTANT SWB_UNUSED_RANGE_END   : natural := SWB_UNUSED_RANGE_BASE - (SWB_UNUSED - 1);
	CONSTANT SWB_UNUSED_VECTOR_SIZE : natural := SWB_UNUSED_RANGE_BASE - SWB_UNUSED_RANGE_END + 1;

	CONSTANT SWB_INSTR_PORT_SIZE : natural := SWB_DAV + SWB_SRC_ADDR_ROW + SWB_SRC_DPU_REFI + SWB_SRC_OUTPUT_NR + SWB_HB_INDEX + SWB_SEND_TO_OTHER_ROW + SWB_V_INDEX;

	--<BRANCH INSTRUCTION TYPE CONSTANTS>--
	--  CONSTANT BR_STATUS   : natural := 4;
	--  CONSTANT BR_IFTRUE   : natural := 6;
	--  CONSTANT BR_MASK     : natural := 1;
	--  CONSTANT BR_IFFALSE  : natural := 6;
	--  CONSTANT BR_SELECTOR : natural := 3;
	--  CONSTANT BR_UNUSED : natural := 3;
	--  
	--  CONSTANT BR_STATUS_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	--  CONSTANT BR_STATUS_RANGE_END   : natural := BR_STATUS_RANGE_BASE - (BR_STATUS - 1);
	--  CONSTANT BR_STATUS_VECTOR_SIZE : natural := BR_STATUS_RANGE_BASE - BR_STATUS_RANGE_END;
	--
	--  CONSTANT BR_IFTRUE_RANGE_BASE  : natural := BR_STATUS_RANGE_END - 1;
	--  CONSTANT BR_IFTRUE_RANGE_END   : natural := BR_IFTRUE_RANGE_BASE - (BR_IFTRUE - 1);
	--  CONSTANT BR_IFTRUE_VECTOR_SIZE : natural := BR_IFTRUE_RANGE_BASE - BR_IFTRUE_RANGE_END;
	--
	--  CONSTANT BR_MASK_RANGE_BASE  : natural := BR_IFTRUE_RANGE_END - 1;
	--  CONSTANT BR_MASK_RANGE_END   : natural := BR_MASK_RANGE_BASE - (BR_MASK - 1);
	--  CONSTANT BR_MASK_VECTOR_SIZE : natural := BR_MASK_RANGE_BASE - BR_MASK_RANGE_END;
	--
	--  CONSTANT BR_IFFALSE_RANGE_BASE  : natural := BR_MASK_RANGE_END - 1;
	--  CONSTANT BR_IFFALSE_RANGE_END   : natural := BR_IFFALSE_RANGE_BASE - (BR_IFFALSE - 1);
	--  CONSTANT BR_IFFALSE_VECTOR_SIZE : natural := BR_IFFALSE_RANGE_BASE - BR_IFFALSE_RANGE_END;
	--
	--  CONSTANT BR_SELECTOR_RANGE_BASE  : natural := BR_IFFALSE_RANGE_END - 1;
	--  CONSTANT BR_SELECTOR_RANGE_END   : natural := BR_SELECTOR_RANGE_BASE - (BR_SELECTOR - 1);
	--  CONSTANT BR_SELECTOR_VECTOR_SIZE : natural := BR_SELECTOR_RANGE_BASE - BR_SELECTOR_RANGE_END;
	--  
	--  CONSTANT BR_UNUSED_RANGE_BASE  : natural := BR_SELECTOR_RANGE_END - 1;
	--  CONSTANT BR_UNUSED_RANGE_END   : natural := BR_UNUSED_RANGE_BASE - (BR_UNUSED - 1);
	--  CONSTANT BR_UNUSED_VECTOR_SIZE : natural := BR_UNUSED_RANGE_BASE - BR_UNUSED_RANGE_END;

	CONSTANT BR_MODE             : natural := 2;
	CONSTANT BR_FALSE_ADDRS_SIZE : natural := PC_SIZE;
	CONSTANT BR_UNUSED           : natural := INSTR_WIDTH - BR_MODE - BR_FALSE_ADDRS_SIZE - INSTR_CODE_RANGE_VECTOR_SIZE;

	CONSTANT BR_MODE_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT BR_MODE_RANGE_END   : natural := BR_MODE_RANGE_BASE - (BR_MODE - 1);
	CONSTANT BR_MODE_VECTOR_SIZE : natural := BR_MODE_RANGE_BASE - BR_MODE_RANGE_END + 1;

	CONSTANT BR_FALSE_ADDRS_RANGE_BASE  : natural := BR_MODE_RANGE_END - 1;
	CONSTANT BR_FALSE_ADDRS_RANGE_END   : natural := BR_FALSE_ADDRS_RANGE_BASE - (BR_FALSE_ADDRS_SIZE - 1);
	CONSTANT BR_FALSE_ADDRS_VECTOR_SIZE : natural := BR_FALSE_ADDRS_RANGE_BASE - BR_FALSE_ADDRS_RANGE_END + 1;

	CONSTANT BR_UNUSED_RANGE_BASE  : natural := BR_FALSE_ADDRS_RANGE_END - 1;
	CONSTANT BR_UNUSED_RANGE_END   : natural := BR_UNUSED_RANGE_BASE - (BR_UNUSED - 1);
	CONSTANT BR_UNUSED_VECTOR_SIZE : natural := BR_UNUSED_RANGE_BASE - BR_UNUSED_RANGE_END + 1;

	--<JUMP INSTRUCTION TYPE CONSTANTS>--
	CONSTANT TRUE_ADDRS_SIZE  : natural := PC_SIZE; --log2(INSTR_DEPTH);
	CONSTANT JUMP_UNUSED_BITS : natural := INSTR_WIDTH - TRUE_ADDRS_SIZE - INSTR_CODE_RANGE_VECTOR_SIZE; -- 23-PC_SIZE;  -- instr_code:4 || true_addrs:6

	CONSTANT TRUE_ADDRS_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT TRUE_ADDRS_RANGE_END   : natural := TRUE_ADDRS_RANGE_BASE - (TRUE_ADDRS_SIZE - 1);
	CONSTANT TRUE_ADDRS_VECTOR_SIZE : natural := TRUE_ADDRS_RANGE_BASE - TRUE_ADDRS_RANGE_END + 1;

	CONSTANT JUMP_UNUSED_RANGE_BASE       : natural := TRUE_ADDRS_RANGE_END - 1;
	CONSTANT JUMP_UNUSED_RANGE_END        : natural := JUMP_UNUSED_RANGE_BASE - (JUMP_UNUSED_BITS - 1);
	CONSTANT JUMP_UNUSED_BITS_VECTOR_SIZE : natural := JUMP_UNUSED_RANGE_BASE - JUMP_UNUSED_RANGE_END + 1;

	--<DELAY INSTRUCTION TYPE CONSTANTS>--
	CONSTANT DLY_CYCLES      : natural := 15;
	CONSTANT DLY_UNUSED_BITS : natural := 7;

	CONSTANT DLY_CYCLES_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 2; -- one bit for static or dynamic selection
	CONSTANT DLY_CYCLES_RANGE_END   : natural := DLY_CYCLES_RANGE_BASE - (DLY_CYCLES - 1);
	CONSTANT DLY_CYCLES_VECTOR_SIZE : natural := DLY_CYCLES_RANGE_BASE - DLY_CYCLES_RANGE_END;

	CONSTANT DLY_UNUSED_BITS_RANGE_BASE  : natural := DLY_CYCLES_RANGE_END - 1;
	CONSTANT DLY_UNUSED_BITS_RANGE_END   : natural := DLY_UNUSED_BITS_RANGE_BASE - (DLY_UNUSED_BITS - 1);
	CONSTANT DLY_UNUSED_BITS_VECTOR_SIZE : natural := DLY_UNUSED_BITS_RANGE_BASE - DLY_UNUSED_BITS_RANGE_END;

	--<RACCU INSTRUCTION TYPE CONSTANTS>--
	CONSTANT RACCU_MODE_SEL    : natural := 3;
	CONSTANT RACCU_OPERAND     : natural := 7;
	CONSTANT RACCU_RESULT_ADDR : natural := 4;

	CONSTANT RACCU_MODE_SEL_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT RACCU_MODE_SEL_RANGE_END   : natural := RACCU_MODE_SEL_RANGE_BASE - (RACCU_MODE_SEL - 1);
	CONSTANT RACCU_MODE_SEL_VECTOR_SIZE : natural := RACCU_MODE_SEL_RANGE_BASE - RACCU_MODE_SEL_RANGE_END + 1;

	CONSTANT RACCU_OPERAND1_RANGE_BASE  : natural := RACCU_MODE_SEL_RANGE_END - 2; --one to determine if it is constant or indirect address
	CONSTANT RACCU_OPERAND1_RANGE_END   : natural := RACCU_OPERAND1_RANGE_BASE - (RACCU_OPERAND - 1);
	CONSTANT RACCU_OPERAND1_VECTOR_SIZE : natural := RACCU_OPERAND1_RANGE_BASE - RACCU_OPERAND1_RANGE_END + 1;

	CONSTANT RACCU_OPERAND2_RANGE_BASE  : natural := RACCU_OPERAND1_RANGE_END - 2;
	CONSTANT RACCU_OPERAND2_RANGE_END   : natural := RACCU_OPERAND2_RANGE_BASE - (RACCU_OPERAND - 1);
	CONSTANT RACCU_OPERAND2_VECTOR_SIZE : natural := RACCU_OPERAND2_RANGE_BASE - RACCU_OPERAND2_RANGE_END + 1;

	CONSTANT RACCU_RESULT_ADDR_RANGE_BASE  : natural := RACCU_OPERAND2_RANGE_END - 1;
	CONSTANT RACCU_RESULT_ADDR_RANGE_END   : natural := RACCU_RESULT_ADDR_RANGE_BASE - (RACCU_RESULT_ADDR - 1);
	CONSTANT RACCU_RESULT_ADDR_VECTOR_SIZE : natural := RACCU_RESULT_ADDR_RANGE_BASE - RACCU_RESULT_ADDR_RANGE_END + 1;

	--<FOR HEADER INSTRUCTION TYPE CONSTANTS>--
	CONSTANT FOR_INDEX_ADDR    : natural := 4; -- up to 16 loops
	CONSTANT FOR_INDEX_START   : natural := 6;
	CONSTANT FOR_ITER_NO       : natural := 6; -- up to 64 iterations
	CONSTANT FOR_HEADER_UNUSED : natural := 6;

	CONSTANT FOR_INDEX_ADDR_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1; --22
	CONSTANT FOR_INDEX_ADDR_RANGE_END   : natural := FOR_INDEX_ADDR_RANGE_BASE - (FOR_INDEX_ADDR - 1); --22-3+1= 20
	CONSTANT FOR_INDEX_ADDR_VECTOR_SIZE : natural := FOR_INDEX_ADDR_RANGE_BASE - FOR_INDEX_ADDR_RANGE_END + 1;

	CONSTANT FOR_INDEX_START_RANGE_BASE  : natural := FOR_INDEX_ADDR_RANGE_END - 1;
	CONSTANT FOR_INDEX_START_RANGE_END   : natural := FOR_INDEX_START_RANGE_BASE - (FOR_INDEX_START - 1);
	CONSTANT FOR_INDEX_START_VECTOR_SIZE : natural := FOR_INDEX_START_RANGE_BASE - FOR_INDEX_START_RANGE_END + 1;

	CONSTANT FOR_ITER_NO_RANGE_BASE  : natural := FOR_INDEX_START_RANGE_END - 2; -- one bit for dynamic or static
	CONSTANT FOR_ITER_NO_RANGE_END   : natural := FOR_ITER_NO_RANGE_BASE - (FOR_ITER_NO - 1);
	CONSTANT FOR_ITER_NO_VECTOR_SIZE : natural := FOR_ITER_NO_RANGE_BASE - FOR_ITER_NO_RANGE_END + 1;

	CONSTANT FOR_HEADER_UNUSED_RANGE_BASE  : natural := FOR_ITER_NO_RANGE_END - 1;
	CONSTANT FOR_HEADER_UNUSED_RANGE_END   : natural := FOR_HEADER_UNUSED_RANGE_BASE - (FOR_HEADER_UNUSED - 1);
	CONSTANT FOR_HEADER_UNUSED_VECTOR_SIZE : natural := FOR_HEADER_UNUSED_RANGE_BASE - FOR_HEADER_UNUSED_RANGE_END + 1;

	--<FOR TAIL INSTRUCTION TYPE CONSTANTS>--
	CONSTANT FOR_TAIL_INDEX_ADDR : natural := 4;
	CONSTANT FOR_INDEX_STEP      : natural := 6;
	CONSTANT FOR_PC_TOGO         : natural := PC_SIZE;
	CONSTANT FOR_TAIL_UNUSED     : natural := 13 - PC_SIZE;

	CONSTANT FOR_INDEX_STEP_RANGE_BASE  : natural := INSTR_CODE_RANGE_END - 1;
	CONSTANT FOR_INDEX_STEP_RANGE_END   : natural := FOR_INDEX_STEP_RANGE_BASE - (FOR_INDEX_STEP - 1);
	CONSTANT FOR_INDEX_STEP_VECTOR_SIZE : natural := FOR_INDEX_STEP_RANGE_BASE - FOR_INDEX_STEP_RANGE_END + 1;

	CONSTANT FOR_PC_TOGO_RANGE_BASE  : natural := FOR_INDEX_STEP_RANGE_END - 1;
	CONSTANT FOR_PC_TOGO_RANGE_END   : natural := FOR_PC_TOGO_RANGE_BASE - (FOR_PC_TOGO - 1);
	CONSTANT FOR_PC_TOGO_VECTOR_SIZE : natural := FOR_PC_TOGO_RANGE_BASE - FOR_PC_TOGO_RANGE_END + 1;

	CONSTANT FOR_TAIL_INDEX_ADDR_RANGE_BASE  : natural := FOR_PC_TOGO_RANGE_END - 1;
	CONSTANT FOR_TAIL_INDEX_ADDR_RANGE_END   : natural := FOR_TAIL_INDEX_ADDR_RANGE_BASE - (FOR_TAIL_INDEX_ADDR - 1);
	CONSTANT FOR_TAIL_INDEX_ADDR_VECTOR_SIZE : natural := FOR_TAIL_INDEX_ADDR_RANGE_BASE - FOR_TAIL_INDEX_ADDR_RANGE_END + 1;

	CONSTANT FOR_TAIL_UNUSED_RANGE_BASE  : natural := FOR_TAIL_INDEX_ADDR_RANGE_END - 1;
	CONSTANT FOR_TAIL_UNUSED_RANGE_END   : natural := FOR_TAIL_UNUSED_RANGE_BASE - (FOR_TAIL_UNUSED - 1);
	CONSTANT FOR_TAIL_UNUSED_VECTOR_SIZE : natural := FOR_TAIL_UNUSED_RANGE_BASE - FOR_TAIL_UNUSED_RANGE_END + 1;

	CONSTANT clk_half_per : natural := 1;

	--<SEQUENCER CONSTANTS END>--
	------------------SRAM AGU Signals----------------------------------------------------------------
	---- widths 
	----------First Part---------------------
	----------Common Part--------------
	--CONSTANT new_instr_WIDTH   : INTEGER :=1;   CONSTANT new_instr_s       : INTEGER :=1              ; CONSTANT new_instr_e   : INTEGER :=new_instr_s   + new_instr_WIDTH  -1;
	--CONSTANT mode_WIDTH        : INTEGER :=1;   CONSTANT mode_s            : INTEGER :=new_instr_e  +1; CONSTANT mode_e        : INTEGER :=mode_s        + mode_WIDTH       -1;
	--CONSTANT start_addrs_WIDTH : INTEGER :=7;   CONSTANT start_addrs_s     : INTEGER :=mode_e       +1; CONSTANT start_addrs_e : INTEGER :=start_addrs_s + start_addrs_WIDTH-1;
	--CONSTANT end_addrs_WIDTH   : INTEGER :=7;   CONSTANT end_addrs_s       : INTEGER :=start_addrs_e+1; CONSTANT end_addrs_e   : INTEGER :=end_addrs_s   + end_addrs_WIDTH  -1;
	--                                                      --------Vector Addressing--------
	--CONSTANT incr_decr_WIDTH             : INTEGER :=1;   CONSTANT incr_decr_s       : INTEGER :=end_addrs_e+1;       CONSTANT incr_decr_e       : INTEGER :=incr_decr_s      + incr_decr_WIDTH      -1; 
	--CONSTANT incr_decr_value_WIDTH       : INTEGER :=7;   CONSTANT incr_decr_value_s : INTEGER :=incr_decr_e+1;       CONSTANT incr_decr_value_e : INTEGER :=incr_decr_value_s+ incr_decr_value_WIDTH-1; 
	--CONSTANT INITIAL_DELAY_WIDTH         : INTEGER :=4;   CONSTANT initial_delay_s   : INTEGER :=incr_decr_value_e+1; CONSTANT initial_delay_e   : INTEGER :=initial_delay_s+INITIAL_DELAY_WIDTH-1; 
	--													  --------Bit Reverse Addressing---
	--CONSTANT start_stage_WIDTH           : INTEGER :=3; CONSTANT start_stage_s                : INTEGER :=17; CONSTANT start_stage_e           : INTEGER :=19; 
	--CONSTANT end_stage_WIDTH             : INTEGER :=3; CONSTANT end_stage_s                  : INTEGER :=20; CONSTANT end_stage_e             : INTEGER :=22; 
	--
	--
	--
	---------Second Part-------------------- 
	--CONSTANT outputcontrol_WIDTH               : INTEGER :=1; CONSTANT outputcontrol_s              : INTEGER :=initial_delay_e             +1; CONSTANT outputcontrol_e               : INTEGER :=outputcontrol_s             +outputcontrol_WIDTH             -1; 
	--CONSTANT instruction_complete_WIDTH        : INTEGER :=1; CONSTANT instruction_complete_s       : INTEGER :=outputcontrol_e             +1; CONSTANT instruction_complete_e        : INTEGER :=instruction_complete_s      +instruction_complete_WIDTH      -1;--removed  
	--CONSTANT infinite_loop_WIDTH               : INTEGER :=1; CONSTANT infinite_loop_s              : INTEGER :=outputcontrol_e             +1; CONSTANT infinite_loop_e               : INTEGER :=infinite_loop_s             +infinite_loop_WIDTH             -1;  
	--CONSTANT repetition_delay_WIDTH            : INTEGER :=9; CONSTANT repetition_delay_s           : INTEGER :=infinite_loop_e             +1; CONSTANT repetition_delay_e            : INTEGER :=repetition_delay_s          +repetition_delay_WIDTH          -1;   --9
	--CONSTANT no_of_repetitions_WIDTH           : INTEGER :=9; CONSTANT no_of_repetitions_s          : INTEGER :=repetition_delay_e          +1; CONSTANT no_of_repetitions_e           : INTEGER :=no_of_repetitions_s         +no_of_repetitions_WIDTH         -1;   
	--CONSTANT repetition_incr_decr_WIDTH        : INTEGER :=1; CONSTANT repetition_incr_decr_s       : INTEGER :=no_of_repetitions_e         +1; CONSTANT repetition_incr_decr_e        : INTEGER :=repetition_incr_decr_s      +repetition_incr_decr_WIDTH      -1;     --1
	--CONSTANT repetition_incr_decr_value_WIDTH  : INTEGER :=6; CONSTANT repetition_incr_decr_value_s : INTEGER :=repetition_incr_decr_e      +1; CONSTANT repetition_incr_decr_value_e  : INTEGER :=repetition_incr_decr_value_s+repetition_incr_decr_value_WIDTH-1;   --6
	--CONSTANT middle_delay_WIDTH                : INTEGER :=9; CONSTANT middle_delay_s               : INTEGER :=repetition_incr_decr_value_e+1; CONSTANT middle_delay_e                : INTEGER :=middle_delay_s              +middle_delay_WIDTH              -1; --56 to 64 
	--constant sram_start_address_sd_witdh  : integer :=1;constant s_sram_start_address_sd_witdh  : integer :=middle_delay_e                +1;  constant e_sram_start_address_sd_witdh  : integer :=s_sram_start_address_sd_witdh +sram_start_address_sd_witdh-1; 
	--constant sram_end_address_sd_witdh    : integer :=1;constant s_sram_end_address_sd_witdh    : integer :=e_sram_start_address_sd_witdh +1;  constant e_sram_end_address_sd_witdh    : integer :=s_sram_end_address_sd_witdh   +sram_end_address_sd_witdh  -1; 
	--constant sram_initial_delay_sd_witdh  : integer :=1;constant s_sram_initial_delay_sd_witdh  : integer :=e_sram_end_address_sd_witdh   +1;  constant e_sram_initial_delay_sd_witdh  : integer :=s_sram_initial_delay_sd_witdh +sram_initial_delay_sd_witdh-1; 
	--constant sram_rpt_delay_sd_witdh      : integer :=1;constant s_sram_rpt_delay_sd_witdh      : integer :=e_sram_initial_delay_sd_witdh +1;  constant e_sram_rpt_delay_sd_witdh      : integer :=s_sram_rpt_delay_sd_witdh     +sram_rpt_delay_sd_witdh    -1; 
	--constant sram_rpt_incrvalue_sd_witdh  : integer :=1;constant s_sram_rpt_incrvalue_sd_witdh  : integer :=e_sram_rpt_delay_sd_witdh     +1;  constant e_sram_rpt_incrvalue_sd_witdh  : integer :=s_sram_rpt_incrvalue_sd_witdh +sram_rpt_incrvalue_sd_witdh-1; 
	--constant sram_middle_delay_sd_witdh   : integer :=1;constant s_sram_middle_delay_sd_witdh   : integer :=e_sram_rpt_incrvalue_sd_witdh +1;  constant e_sram_middle_delay_sd_witdh   : integer :=s_sram_middle_delay_sd_witdh  +sram_middle_delay_sd_witdh -1; 
	--constant sram_nr_of_rpt_sd_witdh      : integer :=1;constant s_sram_nr_of_rpt_sd_witdh      : integer :=e_sram_middle_delay_sd_witdh +1;   constant e_sram_nr_of_rpt_sd_witdh      : integer :=s_sram_nr_of_rpt_sd_witdh     +sram_nr_of_rpt_sd_witdh    -1; 
	--
	--
	--
	--
	--
	--
	--
	--------Third Part----------------------
	--CONSTANT SRAM_inout_select_WIDTH           : INTEGER :=1; CONSTANT SRAM_inout_select_s          : INTEGER :=middle_delay_e     +1; CONSTANT SRAM_inout_select_e           : INTEGER :=SRAM_inout_select_s+SRAM_inout_select_WIDTH-1;
	--CONSTANT range_counter_WIDTH               : INTEGER :=7; CONSTANT range_counter_s              : INTEGER :=SRAM_inout_select_e+1; CONSTANT range_counter_e               : INTEGER :=range_counter_s    +range_counter_WIDTH    -1;
	--CONSTANT hault_delay_WIDTH                 : INTEGER :=9; CONSTANT hault_delay_s                : INTEGER :=range_counter_e    +1; CONSTANT hault_delay_e                 : INTEGER :=hault_delay_s      +hault_delay_WIDTH      -1;
	--CONSTANT hault_counter_WIDTH               : INTEGER :=6; CONSTANT hault_counter_s              : INTEGER :=hault_delay_e      +1; CONSTANT hault_counter_e               : INTEGER :=hault_counter_s    +hault_counter_WIDTH    -1;

	----------------------------------------------------------------------------------------------------------------------------------------------------------------------
	--  New SRAM AGU instructions
	----------------------------------------------------------------------------------------------------------------------------------------------------------------------
	CONSTANT RAM_DEPTH         : INTEGER := HW_RAM_DEPTH;
	CONSTANT RAM_ADDRESS_WIDTH : INTEGER := LOG2(RAM_DEPTH);
	constant sr_delays         : integer := 6;

	----------------- new sram agu instruction widths-------------------
	constant sr_en_width              : integer := 1; -- this is send from the tile itself
	constant sr_mode_width            : integer := 1;
	constant sr_initial_address_width : integer := RAM_ADDRESS_WIDTH;
	constant sr_initial_delay_width   : integer := 4;
	-- loop1
	constant sr_loop1_iteration_width : integer := RAM_ADDRESS_WIDTH;
	constant sr_loop1_increment_width : integer := RAM_ADDRESS_WIDTH + 1;
	constant sr_loop1_delay_width     : integer := sr_delays;
	-- loop2
	constant sr_loop2_iteration_width : integer := RAM_ADDRESS_WIDTH;
	constant sr_loop2_increment_width : integer := RAM_ADDRESS_WIDTH + 1;
	constant sr_loop2_delay_width     : integer := sr_delays;

	---------------------Start index
	constant sr_en_s              : integer := 1;
	constant sr_en_e              : integer := sr_en_width + sr_en_s - 1;
	constant sr_mode_s            : integer := sr_en_e + 1;
	constant sr_mode_e            : integer := sr_mode_width + sr_mode_s - 1;
	constant sr_initial_address_s : integer := sr_mode_e + 1;
	constant sr_initial_address_e : integer := sr_initial_address_width + sr_initial_address_s - 1;
	constant sr_initial_delay_s   : integer := sr_initial_address_e + 1;
	constant sr_initial_delay_e   : integer := sr_initial_delay_width + sr_initial_delay_s - 1;
	constant sr_loop1_iteration_s : integer := sr_initial_delay_e + 1;
	constant sr_loop1_iteration_e : integer := sr_loop1_iteration_width + sr_loop1_iteration_s - 1;
	constant sr_loop1_increment_s : integer := sr_loop1_iteration_e + 1;
	constant sr_loop1_increment_e : integer := sr_loop1_increment_width + sr_loop1_increment_s - 1;
	constant sr_loop1_delay_s     : integer := sr_loop1_increment_e + 1;
	constant sr_loop1_delay_e     : integer := sr_loop1_delay_width + sr_loop1_delay_s - 1;
	constant sr_loop2_iteration_s : integer := sr_loop1_delay_e + 1;
	constant sr_loop2_iteration_e : integer := sr_loop2_iteration_width + sr_loop2_iteration_s - 1;
	constant sr_loop2_increment_s : integer := sr_loop2_iteration_e + 1;
	constant sr_loop2_increment_e : integer := sr_loop2_increment_width + sr_loop2_increment_s - 1;
	constant sr_loop2_delay_s     : integer := sr_loop2_increment_e + 1;
	constant sr_loop2_delay_e     : integer := sr_loop2_delay_width + sr_loop2_delay_s - 1;

	constant sr_Initial_address_sd : integer := sr_loop2_delay_e + 1;
	constant sr_Loop1_iteration_sd : integer := sr_Initial_address_sd + 1;
	constant sr_Loop2_iteration_sd : integer := sr_Loop1_iteration_sd + 1;
	constant sr_initial_delay_sd   : integer := sr_Loop2_iteration_sd + 1;
	constant sr_Loop1_delay_sd     : integer := sr_initial_delay_sd + 1;
	constant sr_Loop2_delay_sd     : integer := sr_Loop1_delay_sd + 1;
	constant sr_Loop1_increment_sd : integer := sr_Loop2_delay_sd + 1;
	constant sr_Loop2_increment_sd : integer := sr_Loop1_increment_sd + 1;

	constant sr_rw : integer := sr_loop2_delay_e + 1; -- static and dynamic bits are  used by sequencer and read write information is appended at the end. 

	constant sram_agu_width      : integer := sr_rw;
	constant sram_tb_instr_width : integer := sr_Loop2_increment_sd - sr_en_width;
	constant sram_raccu_flags    : integer := sr_Loop2_increment_sd - sr_loop2_delay_e;

	--constant SRAM_AGU_INSTR_WIDTH : integer := 
	--	new_instr_WIDTH                  +
	--    mode_WIDTH                       +
	--    start_addrs_WIDTH                +
	--    end_addrs_WIDTH                  +
	--    incr_decr_WIDTH                  +
	--    incr_decr_value_WIDTH            +
	--    INITIAL_DELAY_WIDTH              +
	--    outputcontrol_WIDTH              +
	--    instruction_complete_WIDTH       +
	--    infinite_loop_WIDTH              +
	--    repetition_delay_WIDTH           +
	--    no_of_repetitions_WIDTH          +
	--    repetition_incr_decr_WIDTH       +
	--    repetition_incr_decr_value_WIDTH +
	--    middle_delay_WIDTH               +
	--    SRAM_inout_select_WIDTH          +
	--    range_counter_WIDTH              +
	--    hault_delay_WIDTH                +
	--    hault_counter_WIDTH;     
	-- 
	-- CONSTANT AGU_INSTR_WIDTH         : INTEGER := SRAM_AGU_INSTR_WIDTH; 
	-- 
	-- 
	-- 
	-- 
	-- 
	-- 
	-- 
	-- CONSTANT DEPRICATED_BITS_WIDTH  : INTEGER :=
	--    instruction_complete_WIDTH       +
	--    SRAM_inout_select_WIDTH          +
	--    range_counter_WIDTH              +
	--    hault_delay_WIDTH                +
	--    hault_counter_WIDTH;
	-- 		instrcode       + -- should not be part of instruction as we have seperate record for that
	--		memvalid        + -- not needed can be generated by iSwitch
	--		memmode         +
	--		memstartaddress +
	--		memendaddress   +
	--		1               + -- increment or decrement.  should be joined with increment value
	--		memincrvalue    +
	--		meminitialdelay +
	--		memoutputcontrol+
	--		1               + -- instruction complete  not needed as we have one part instruction
	--		1               + -- infinite loop
	--		mrptdelay       +
	--		mnumofrept      +
	--		1               + --Repetition_Increment
	--		mrptincrvalue   + -- should be combined as signed value
	--		mmiddledelay;
	--		-- inout  removed 
	--		-- range counter removed
	--		-- halt delay removed  it will be replaced with  burst mode 
	--		-- halt counter     removed  it will be replaced with  burst mode 

	--Constant NoC_Bus_instr_width : integer :=SRAM_AGU_INSTR_WIDTH -DEPRICATED_BITS_WIDTH;
	Constant NoC_Bus_instr_width : integer := sram_agu_width;

	--<SEQUENCER TYPES BEGIN>--

	TYPE State_ty IS (IDLE_ST, SEQ_LOADING_ST, INSTR_DECODE_ST);
	TYPE Tb_State_ty IS (TB_IDLE_ST, TB_INIT_ST, TB_LOAD_ST);

	--TYPE Instr_ty IS RECORD
	--  increment          : std_logic_vector(INCREMENT_VECTOR_SIZE DOWNTO 0);  -- 0:pc = pc + 1, 1:jump instruction, address in bits (INSTR_WIDTH-7 downto 2)
	--  outport            : std_logic_vector(OUTPORT_VECTOR_SIZE DOWNTO 0);  -- 0: dpu, 1: reg file
	--  jump_instr_address : std_logic_vector(JUMP_INSTR_ADDRESS_VECTOR_SIZE DOWNTO 0);  -- address to instruction for pc to jump to when the increment field is set to '1'
	--  vacant_bits        : std_logic_vector(VACANT_BITS_REMAIN-1 DOWNTO 0);  -- no use at the moment
	--END RECORD Instr_ty;

	TYPE Refi1_instr_ty IS RECORD
		instr_code       : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		reg_file_port    : std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE - 1 DOWNTO 0);
		subseq_instrs    : std_logic_vector(NR_OF_INSTRS_VECTOR_SIZE - 1 DOWNTO 0);
		start_addrs_sd   : std_logic;
		start_addrs      : std_logic_vector(STARTING_ADDRS_VECTOR_SIZE - 1 DOWNTO 0);
		no_of_addrs_sd   : std_logic;
		no_of_addrs      : std_logic_vector(NR_OF_ADDRS_VECTOR_SIZE - 1 DOWNTO 0);
		initial_delay_sd : std_logic;
		initial_delay    : std_logic_vector(INIT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE Refi2_instr_ty IS RECORD
		instr_code           : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		step_val_sd          : std_logic;
		step_val             : std_logic_vector(STEP_VALUE_VECTOR_SIZE - 1 DOWNTO 0);
		step_val_sign        : std_logic_vector(STEP_VALUE_SIGN_VECTOR_SIZE - 1 DOWNTO 0);
		refi_middle_delay_sd : std_logic;
		refi_middle_delay    : std_logic_vector(REG_FILE_MIDDLE_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
		no_of_reps_sd        : std_logic;
		no_of_reps           : std_logic_vector(NUM_OF_REPT_VECTOR_SIZE - 1 DOWNTO 0);
		rpt_step_value       : std_logic_vector(REP_STEP_VALUE_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE Refi3_instr_ty IS RECORD
		instr_code            : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		rpt_delay_sd          : std_logic;
		rpt_delay             : std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
		mode                  : std_logic_vector(MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
		outp_cntrl            : std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 DOWNTO 0);
		fft_stage             : std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
		refi_middle_delay_ext : std_logic_vector(REG_FILE_MIDDLE_DELAY_EXT_VECTOR_SIZE - 1 DOWNTO 0);
		no_of_rpt_ext         : std_logic_vector(NUM_OF_REPT_EXT_VECTOR_SIZE - 1 DOWNTO 0);
		rpt_step_value_ext    : std_logic_vector(REP_STEP_VALUE_EXT_VECTOR_SIZE - 1 DOWNTO 0);
		end_fft_stage         : std_logic_vector(FFT_END_STAGE_VECTOR_SIZE - 1 DOWNTO 0);
		dimarch_mode          : std_logic;
		refi3_unused          : std_logic_vector(REFI3_UNUSED_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE Dpu_instr_ty IS RECORD
		instr_code        : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		dpu_mode          : std_logic_vector(DPU_MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
		dpu_saturation    : std_logic_vector(DPU_SATURAT_VECTOR_SIZE - 1 DOWNTO 0);
		dpu_out_a         : std_logic_vector(DPU_OUTP_A_VECTOR_SIZE - 1 DOWNTO 0);
		dpu_out_b         : std_logic_vector(DPU_OUTP_B_VECTOR_SIZE - 1 DOWNTO 0);
		dpu_acc_clear_rst : std_logic;
		dpu_acc_clear_sd  : std_logic;
		dpu_acc_clear     : std_logic_vector(DPU_ACC_CLEAR_VECTOR_SIZE - 1 DOWNTO 0);
		dpu_process_inout : std_logic_vector(DPU_PROCESS_INOUT_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE Swb_instr_ty IS RECORD
		instr_code        : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		swb_dav           : std_logic_vector(SWB_DAV_VECTOR_SIZE - 1 DOWNTO 0);
		src_addr_row      : std_logic_vector(SWB_SRC_ADDR_ROW_VECTOR_SIZE - 1 DOWNTO 0);
		from_block        : std_logic_vector(SWB_SRC_DPU_REFI_VECTOR_SIZE - 1 DOWNTO 0);
		from_port         : std_logic_vector(SWB_SRC_OUTPUT_NR_VECTOR_SIZE - 1 DOWNTO 0);
		hb_index          : std_logic_vector(SWB_HB_INDEX_VECTOR_SIZE - 1 DOWNTO 0);
		send_to_other_row : std_logic_vector(SWB_SEND_TO_OTHER_ROW_VECTOR_SIZE - 1 DOWNTO 0);
		v_index           : std_logic_vector(SWB_V_INDEX_VECTOR_SIZE - 1 DOWNTO 0);
		--to_port       : std_logic_vector(SWB_TO_PORT_VECTOR_SIZE DOWNTO 0);
		swb_unused        : std_logic_vector(SWB_UNUSED_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	--  TYPE Branch_instr_ty IS RECORD
	--    instr_code     : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE-1 DOWNTO 0);
	--    brnch_status   : std_logic_vector(BR_STATUS_VECTOR_SIZE DOWNTO 0);
	--    brnch_iftrue   : std_logic_vector(BR_IFFALSE_VECTOR_SIZE DOWNTO 0);
	--    brnch_mask     : std_logic_vector(BR_MASK_VECTOR_SIZE DOWNTO 0);
	--    brnch_iffalse  : std_logic_vector(BR_IFFALSE_VECTOR_SIZE DOWNTO 0);
	--    brnch_selector : std_logic_vector(BR_SELECTOR_VECTOR_SIZE DOWNTO 0);
	--    brnch_unused   : std_logic_vector(BR_UNUSED_VECTOR_SIZE DOWNTO 0);    
	--  END RECORD;

	TYPE Branch_instr_ty IS RECORD
		instr_code       : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		brnch_mode       : std_logic_vector(BR_MODE_VECTOR_SIZE - 1 DOWNTO 0);
		brnch_false_addr : std_logic_vector(BR_FALSE_ADDRS_VECTOR_SIZE - 1 DOWNTO 0);
		brnch_unused     : std_logic_vector(BR_UNUSED_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE Jump_instr_ty IS RECORD
		instr_code  : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		true_addrs  : std_logic_vector(TRUE_ADDRS_VECTOR_SIZE - 1 DOWNTO 0);
		jump_unused : std_logic_vector(JUMP_UNUSED_BITS_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE Delay_instr_ty IS RECORD
		instr_code    : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		del_cycles_sd : std_logic;
		del_cycles    : std_logic_vector(DLY_CYCLES_VECTOR_SIZE DOWNTO 0);
		del_unused    : std_logic_vector(DLY_UNUSED_BITS_VECTOR_SIZE DOWNTO 0);
	END RECORD;

	TYPE Raccu_instr_ty IS RECORD
		instr_code         : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		raccu_mode         : std_logic_vector(RACCU_MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
		raccu_op1_sd       : std_logic;
		raccu_op1          : std_logic_vector(RACCU_OPERAND1_VECTOR_SIZE - 1 DOWNTO 0);
		raccu_op2_sd       : std_logic;
		raccu_op2          : std_logic_vector(RACCU_OPERAND2_VECTOR_SIZE - 1 DOWNTO 0);
		raccu_result_addrs : std_logic_vector(RACCU_RESULT_ADDR_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE For_header_instr_ty IS RECORD
		instr_code       : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		index_raccu_addr : std_logic_vector(FOR_INDEX_ADDR_VECTOR_SIZE - 1 DOWNTO 0);
		index_start      : std_logic_vector(FOR_INDEX_START_VECTOR_SIZE - 1 DOWNTO 0);
		iter_no_sd       : std_logic;
		iter_no          : std_logic_vector(FOR_ITER_NO_VECTOR_SIZE - 1 DOWNTO 0);
		header_unused    : std_logic_vector(FOR_HEADER_UNUSED_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE For_tail_instr_ty IS RECORD
		instr_code       : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		index_step       : std_logic_vector(FOR_INDEX_STEP_VECTOR_SIZE - 1 DOWNTO 0);
		pc_togo          : std_logic_vector(FOR_PC_TOGO_VECTOR_SIZE - 1 DOWNTO 0);
		index_raccu_addr : std_logic_vector(FOR_INDEX_ADDR_VECTOR_SIZE - 1 DOWNTO 0);
		tail_unused      : std_logic_vector(FOR_TAIL_UNUSED_VECTOR_SIZE - 1 DOWNTO 0);
	END RECORD;

	TYPE sram_instr_type is record
		instr_code              : std_logic_vector(INSTR_CODE_RANGE_VECTOR_SIZE - 1 DOWNTO 0);
		agu_mode                : std_logic_vector(0 downto 0);
		Initial_Address         : std_logic_vector(sr_initial_address_width - 1 downto 0);
		Initial_Delay           : std_logic_vector(sr_initial_delay_width - 1 downto 0);
		Loop1_Increment         : std_logic_vector(sr_loop1_increment_width - 1 downto 0);
		Loop1_iteration         : std_logic_vector(sr_loop1_iteration_width - 1 downto 0); -- 7 bit 
		Loop1_Delay             : std_logic_vector(sr_loop1_delay_width - 1 downto 0); -- 6 bit 
		Loop2_Increment         : std_logic_vector(sr_loop2_increment_width - 1 downto 0);
		Loop2_iteration         : std_logic_vector(sr_loop2_iteration_width - 1 downto 0);
		Loop2_Delay             : std_logic_vector(sr_loop2_delay_width - 1 downto 0);
		sram_Initial_address_sd : std_logic;
		sram_initial_delay_sd   : std_logic;
		sram_Loop1_iteration_sd : std_logic;
		sram_Loop1_increment_sd : std_logic;
		sram_Loop1_delay_sd     : std_logic;
		sram_Loop2_iteration_sd : std_logic;
		sram_Loop2_increment_sd : std_logic;
		sram_Loop2_delay_sd     : std_logic;
		rw                      : std_logic;

	end record;

	TYPE Instr_reg_ty IS ARRAY (0 TO INSTR_DEPTH - 1) OF std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0);

	--TYPE Instr_reg_ty IS ARRAY (0 TO INSTR_DEPTH-1) OF Instr_ty;

	--CONSTANT INSTR_INIT : Instr_ty := (increment => (OTHERS => '0'), outport => (OTHERS => '0'), jump_instr_address => (OTHERS => '0'), vacant_bits => (OTHERS => '0'));  -- used to initialize instructions of type Instr_ty
	CONSTANT REFI1_INIT : Refi1_instr_ty := (instr_code => (OTHERS => '0'), reg_file_port => (OTHERS => '0'), subseq_instrs => (OTHERS => '0'), start_addrs_sd => '0', start_addrs => (OTHERS => '0'), no_of_addrs_sd => '0', no_of_addrs => (OTHERS => '0'), initial_delay_sd => '0', initial_delay => (OTHERS => '0'));

	--<SEQUENCER TYPES END>--

	--<DPU CONSTANTS BEGIN>--

	CONSTANT NR_OF_DPU_IN_PORTS      : natural := 4;
	CONSTANT DPU_CTRL_OUT_WIDTH      : integer := 2;
	CONSTANT DPU_IN_WIDTH            : integer := BITWIDTH;
	CONSTANT DPU_MODE_CFG_WIDTH      : integer := DPU_MODE_SEL; --5
	CONSTANT DPU_OUT_WIDTH           : integer := BITWIDTH;
	CONSTANT DPU_SAT_CTRL_WIDTH      : integer := DPU_SATURAT;
	CONSTANT SEQ_COND_STATUS_WIDTH   : integer := 2;
	CONSTANT DPU_ACC_CLEAR_WIDTH     : integer := DPU_ACC_CLEAR_VECTOR_SIZE;
	CONSTANT DPU_PROCESS_INOUT_WIDTH : integer := 2;

	--dpu decimal and fractions of different parts
	CONSTANT ACC_WIDTH : integer := 2*BITWIDTH + 2;

	--dpu modes functions
	CONSTANT MODE_0  : integer := 0;    -- idle mode
	CONSTANT MODE_1  : integer := 1;    -- out_0 = [(in_0 + in_1) *in_2] + out_0_reg, out_1 = [(in_0 + in_1) *in_2] 
	CONSTANT MODE_2  : integer := 2;    -- out_0 = (in_0 * in_2) + out_0_reg, out_1 = (in_0 * in_2)
	CONSTANT MODE_3  : integer := 3;    -- out_0 = (in_0 * in_2) + in_3_reg, out_1 = (in_0 * in_2)
	CONSTANT MODE_4  : integer := 4;    -- out_0 = (in_0 * in_2) + in_3, out_1 = in_1 - (in_0 * in_2)
	CONSTANT MODE_5  : integer := 5;    -- out_0 = (in_0_reg * in_2) + in_3, out_1 = in_1 - (in_0_reg * in_2)
	CONSTANT MODE_6  : integer := 6;    -- MAX MODE out0 = max(in0, max_tmp_reg), out1 = min(in0, min_tmp_reg)
	CONSTANT MODE_7  : integer := 7;    -- absolute function sum(abs(a-b))
	CONSTANT MODE_8  : integer := 8;    -- Counter Mode;
	CONSTANT MODE_9  : integer := 9;    -- out_0 = arithmetic left shift; out_1 = arithmetic right shift 
	CONSTANT MODE_10 : integer := 10;   -- out_0 = (in_2 + in_3), out_1 = (in_0 - in_1) 
	CONSTANT MODE_11 : integer := 11;   -- in0 > in1 => out0 = 111...11 ;in2 > in3 => out1 = 111...11
	CONSTANT MODE_12 : integer := 12;   -- dpu_acc_clear is used as a constant value
	CONSTANT MODE_13 : integer := 13;   -- comparison with constant
	CONSTANT MODE_20 : integer := 20;   -- sigmoid and tanh

	-- dpu  to process inputs output  

	CONSTANT PROCESS_NONE : natural := 0;
	CONSTANT NEGATE_IN0   : natural := 1; -- negates in_0
	CONSTANT NEGATE_IN1   : natural := 2; -- negates in_1
	CONSTANT ABSOLUTE_OUT : natural := 3; -- abs(out_0) and abs(out_1)

	-- dpu output control modes select between right and left

	CONSTANT OUT_NONE  : natural := 0;
	CONSTANT OUT_RIGHT : natural := 1;
	CONSTANT OUT_LEFT  : natural := 2;
	CONSTANT OUT_BOTH  : natural := 3;

	--  CONSTANT UP_COUNT    : signed (dpu_in_width-1 DOWNTO 0) :=TO_SIGNED(7, BITWIDTH);--x"00000007";
	--  CONSTANT DOWN_COUNT  : signed (dpu_in_width-1 DOWNTO 0) :=TO_SIGNED(4, BITWIDTH);
	--  CONSTANT SET_COUNT   : signed (dpu_in_width-1 DOWNTO 0) := TO_SIGNED(5, BITWIDTH);
	--  CONSTANT RESET_COUNT : signed (dpu_in_width-1 DOWNTO 0) := TO_SIGNED(6, BITWIDTH);
	--  CONSTANT STOP_COUNT  : signed (dpu_in_width-1 DOWNTO 0) := TO_SIGNED(8, BITWIDTH);

	CONSTANT UP_COUNT    : integer range 0 to 2**DPU_IN_WIDTH := 7; --x"00000007";
	CONSTANT DOWN_COUNT  : integer range 0 to 2**DPU_IN_WIDTH := 4;
	CONSTANT SET_COUNT   : integer range 0 to 2**DPU_IN_WIDTH := 5; --x"00000005";
	CONSTANT RESET_COUNT : integer range 0 to 2**DPU_IN_WIDTH := 6; --x"00000006";
	CONSTANT STOP_COUNT  : integer range 0 to 2**DPU_IN_WIDTH := 8; --x"00000008";

	--<DPU CONSTANTS END>--

	--<DPU TYPES BEGIN>--

	SUBTYPE dpu_cfg_mode_type IS std_logic_vector(DPU_MODE_CFG_WIDTH - 1 DOWNTO 0);
	SUBTYPE dpu_ctrl_out_type IS std_logic_vector(DPU_CTRL_OUT_WIDTH - 1 DOWNTO 0);
	SUBTYPE dpu_in_type IS signed(DPU_IN_WIDTH - 1 DOWNTO 0);

	--<DPU TYPES END>--
	--<DiMArch Constants BEGIN>--
	CONSTANT SRAM_SEQUENCER_INSTRUCTIONS : INTEGER := 16;
	CONSTANT SRAM_DEPTH                  : INTEGER := HW_RAM_DEPTH;
	--<DiMArch Constants END>--

	--<REFI CONSTANTS BEGIN>--

	CONSTANT NR_OF_REG_FILE_IN_PORTS : natural := 2;
	CONSTANT REG_FILE_DATA_WIDTH     : integer := BITWIDTH;

	CONSTANT REG_FILE_ADDR_WIDTH : integer := 6;
	CONSTANT REG_FILE_DEPTH      : integer := HW_REG_FILE_DEPTH;

	-- TODO: CHECK HERE: NR_REG_FILE_DATA_BLOCKS should be REG_FILE_DEPTH/16
	CONSTANT WORDS_PER_BLOCK         : integer := 16;
	CONSTANT NR_REG_FILE_DATA_BLOCKS : integer := REG_FILE_DEPTH/WORDS_PER_BLOCK; --to be made more parametric in the future
	CONSTANT INITIAL_DELAY_WIDTH     : integer := 4;
	CONSTANT ADDR_COUNTER_WIDTH      : integer := 6;
	CONSTANT START_ADDR_WIDTH        : integer := 6;
	CONSTANT ADDR_OFFSET_WIDTH       : integer := 6;
	CONSTANT ADDR_OFFSET_SIGN_WIDTH  : integer := 1;
	CONSTANT INITIAL_DELAY_BIT       : integer := 31;
	CONSTANT START_ADDR_BIT          : integer := 27;
	CONSTANT ADDR_OFFSET_BIT         : integer := 21;
	CONSTANT ADDR_COUNTER_BIT        : integer := 16;
	CONSTANT PORT_NUMBER_WIDTH       : integer := 2;

	--<REFI CONSTANTS END>--

	--<REFI TYPES BEGIN>--

	--<REFI TYPES END>--

	--<COMPUTATIONAL FABRIC CONSTANTS BEGIN>--

	CONSTANT COLUMNS                 : natural := HW_COLUMNS;
	CONSTANT ROWS                    : natural := HW_ROWS;
	CONSTANT DiMArch_Rows            : natural := HW_DIMARCH_ROWS;
	CONSTANT DiMArch_Row_Width       : natural := log2(DiMArch_Rows + 1) + 1;
	CONSTANT NR_OF_HOPS              : natural := 2; --sliding window connectivity RANGE
	CONSTANT MAX_NR_OF_OUTP_N_HOPS   : integer := 2*NR_OF_HOPS + 1; -- Max # outps in NR_OF_HOPS range
	CONSTANT NR_OF_OUTP              : natural := 2; -- # of outp for Reg File/DPU - 2
	CONSTANT NR_OF_COL_INPS_ONE_CELL : natural := 6;
	CONSTANT NR_OF_COL_INPS          : natural := 12; -- # of inps in a column,
	-- formerly known as "N" in
	-- the previous version
	CONSTANT HC_OUT_BITS             : natural := 4;
	CONSTANT OUTP0                   : natural := 0;
	CONSTANT OUTP1                   : natural := 1;
	CONSTANT NR_OF_VLANE_IN_PORTS    : natural := NR_OF_REG_FILE_IN_PORTS + NR_OF_DPU_IN_PORTS; --
	--to determine the correc v_lane for each dpu/reg_file in port in the fabric
	CONSTANT TRISTATE_SEL            : natural := 5;

	--<COMPUTATIONAL FABRIC CONSTANTS END>--

	--<MEMORY FABRIC CONSTANTS BEGIN>--
	CONSTANT MEM_BLOCK_SIZE    : INTEGER := 16;
	CONSTANT NUM_OF_REG_LOC    : INTEGER := REG_FILE_DEPTH/MEM_BLOCK_SIZE;
	CONSTANT REG_ADDRESS_WIDTH : INTEGER := log2(NUM_OF_REG_LOC);
	CONSTANT SRAM_WIDTH        : INTEGER := MEM_BLOCK_SIZE * BITWIDTH;

	CONSTANT REG_FILE_MEM_ADDR_WIDTH : natural := REG_ADDRESS_WIDTH;
	CONSTANT REG_FILE_MEM_DATA_WIDTH : natural := 256;
	CONSTANT CONFIG_WIDTH            : natural := 40;

	--<MEMORY FABRIC CONSTANTS END>--

	--<COMPUTATIONAL FABRIC TYPES BEGIN>--

	--TYPE v_bus_ty_2d  IS ARRAY (natural RANGE <>) OF signed (BITWIDTH-1 DOWNTO 0);
	TYPE h_bus_seg_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF signed(BITWIDTH - 1 DOWNTO 0);
	TYPE h_bus_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF signed(BITWIDTH - 1 DOWNTO 0);
	TYPE hc_out_w_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(HC_OUT_BITS - 1 DOWNTO 0);
	TYPE hc_in_bus_ty IS ARRAY (natural RANGE <>) OF std_logic_vector(HC_OUT_BITS - 1 DOWNTO 0);
	TYPE all_hc_in_bus_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(HC_OUT_BITS - 1 DOWNTO 0);
	-- TYPE v6_bus_ty IS ARRAY (0 TO NR_OF_COL_INPS/2-1) OF signed (BITWIDTH-1 DOWNTO 0);  
	--TYPE v6_bus_ty_array IS ARRAY (natural RANGE <>) OF v6_bus_ty;
	-- TYPE v6_bus_ty_2d IS ARRAY (natural RANGE <>, NATURAL RANGE <>) OF v6_bus_ty; 

	TYPE v_bus_ty IS ARRAY (0 TO NR_OF_COL_INPS/2 - 1) OF signed(BITWIDTH - 1 DOWNTO 0);
	TYPE v_bus_ty_array IS ARRAY (natural RANGE <>) OF v_bus_ty;
	TYPE v_bus_ty_2d IS ARRAY (natural RANGE <>, NATURAL RANGE <>) OF v_bus_ty;
	-- TYPE sel_swb_ty IS ARRAY (0 to NR_OF_COL_INPS-1) of unsigned (5 downto 0);
	-- Type sel_swb_ty_2d is ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) of sel_swb_ty;
	TYPE s_bus_switchbox_ty IS ARRAY (0 TO NR_OF_COL_INPS/2 - 1) OF std_logic_vector(5 DOWNTO 0); --5) OF --std_logic_vector (NR_OF_COL_INPS_ONE_CELL-1 DOWNTO 0);--NR_OF_COL_INPS-1) OF std_logic_vector (5 DOWNTO 0);  --
	TYPE s_bus_switchbox_2d_ty is ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) of s_bus_switchbox_ty; --
	TYPE s_bus_switchbox_ty_array IS ARRAY (natural RANGE <>) OF s_bus_switchbox_ty;

	--MAKE GENERIC
	TYPE s_bus_out_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(SWB_INSTR_PORT_SIZE - 1 DOWNTO 0); --
	--MAKE GENERIC
	TYPE v_lane_ty IS ARRAY (natural RANGE <>) OF v_bus_ty;

	TYPE data_array_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF signed(BITWIDTH - 1 DOWNTO 0);
	TYPE dpu_cfg_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(DPU_MODE_SEL_VECTOR_SIZE DOWNTO 0);
	TYPE dpu_acc_clear_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(DPU_ACC_CLEAR_WIDTH DOWNTO 0);
	TYPE dpu_output_ctrl_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(DPU_CTRL_OUT_WIDTH - 1 DOWNTO 0);
	TYPE dpu_sat_ctrl_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(DPU_SAT_CTRL_WIDTH - 1 DOWNTO 0);

	TYPE reg_initial_delay_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(INIT_DELAY_VECTOR_SIZE DOWNTO 0);
	TYPE reg_instr_start_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic;
	TYPE reg_start_addrs_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(STARTING_ADDRS_VECTOR_SIZE DOWNTO 0);
	TYPE reg_step_val_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(STEP_VALUE_VECTOR_SIZE DOWNTO 0);
	TYPE reg_step_val_sign_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(STEP_VALUE_SIGN_VECTOR_SIZE DOWNTO 0);
	TYPE reg_no_of_addrs_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(NR_OF_ADDRS_VECTOR_SIZE DOWNTO 0);
	TYPE reg_port_type_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE DOWNTO 0);
	TYPE reg_outp_cntrl_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE DOWNTO 0);
	TYPE reg_middle_delay_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE DOWNTO 0);
	TYPE reg_no_of_rpts_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(NUM_OF_REPT_PORT_SIZE DOWNTO 0);
	TYPE reg_rpt_step_value_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(REP_STEP_VALUE_PORT_SIZE DOWNTO 0);
	TYPE reg_rpt_delay_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(REPT_DELAY_VECTOR_SIZE DOWNTO 0);
	TYPE reg_mode_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(MODE_SEL_VECTOR_SIZE DOWNTO 0);
	TYPE reg_fft_stage_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE DOWNTO 0);
	TYPE seq_cond_status_ty IS ARRAY (natural RANGE <>, natural RANGE <>) OF std_logic_vector(SEQ_COND_STATUS_WIDTH - 1 DOWNTO 0);

	--<COMPUTATIONAL FABRIC TYPES END>-- 

	--<MEMORY FABRIC TYPES BEGIN>--
	TYPE v_bus_signal_ty is array (COLUMNS - 1 downto 0) of std_logic_vector(INSTR_WIDTH + 1 downto 0);
	TYPE lcc_elements_ty is array (COLUMNS - 1 downto 0) of std_logic_vector(COLUMNS + ROWS + CONFIG_WIDTH downto 0);
	TYPE row_sel_ty is array (0 to COLUMNS) of std_logic_vector(ROWS downto 0);
	--  TYPE origin_ty   IS (FROM_SOURCE,FROM_DESTINATION);
	--<MEMORY FABRIC TYPES END>--

	--<RACCU CONSTANTS BEGIN>--
	--RACCU modes functions

	CONSTANT RAC_MODE_LOOP_HEADER         : integer := 1;
	CONSTANT RAC_MODE_LOOP_TAIL           : integer := 2;
	CONSTANT RAC_MODE_ADD                 : integer := 3;
	CONSTANT RAC_MODE_SUB                 : integer := 4;
	CONSTANT RAC_MODE_SHFT_R              : integer := 5;
	CONSTANT RAC_MODE_SHFT_L              : integer := 6;
	CONSTANT RAC_MODE_ADD_WITH_LOOP_INDEX : integer := 7;
	--CONSTANT RAC_MODE_COUNT : integer := 6;

	--<RACCU CONSTANTS END>--
	CONSTANT RACCU_REG_ADDRS_WIDTH : natural := 3; --4
	CONSTANT RACCU_REG_BITWIDTH    : natural := 7; --to be able to access memory addresses --6
	CONSTANT RACCU_REGFILE_DEPTH   : natural := HW_RACCU_REGFILE_DEPTH;
	CONSTANT MAX_NO_OF_RACCU_LOOPS : natural := HW_MAX_NO_OF_RACCU_LOOPS;
	
	------------------------------------------------------------------------------
	-- MODIFICATION: DEFINE A CONSTANT TO REFER THE WIDTH OF SIGNAL THAT ADDRESS
	--               THE LOOP REGISTERS
	------------------------------------------------------------------------------
	-- ORIGINAL CODE: (DON'T KNOW WHERE THIS CONSTANT IS USED BEFORE)
	-- CONSTANT lOOP_REG_WIDTH        : natural := RACCU_REG_BITWIDTH + RACCU_REG_BITWIDTH + 1;
	------------------------------------------------------------------------------
	-- MODIFIED CODE:
	CONSTANT LOOP_REG_WIDTH        : natural := LOG2(HW_MAX_NO_OF_RACCU_LOOPS);
	------------------------------------------------------------------------------
	-- MODIFICATION END
	------------------------------------------------------------------------------
	
	--<RACCU TYPES BEGIN>--

	TYPE raccu_loop_reg_ty IS RECORD
		loop_index_value : std_logic_vector(RACCU_REG_BITWIDTH - 1 DOWNTO 0);
		loop_counter     : std_logic_vector(RACCU_REG_BITWIDTH - 1 DOWNTO 0);
		loop_end_flag    : std_logic;
	END RECORD;

	TYPE raccu_loop_array_ty is array (MAX_NO_OF_RACCU_LOOPS - 1 downto 0) of raccu_loop_reg_ty; --std_logic_vector(lOOP_REG_WIDTH-1 downto 0);
	TYPE raccu_reg_out_ty is array (RACCU_REGFILE_DEPTH - 1 downto 0) of std_logic_vector(RACCU_REG_BITWIDTH - 1 downto 0);
	--<RACCU TYPES END>--

	--<DiMArch Constants BEGIN>--
	constant RF0   : integer := 0;
	constant RF1   : integer := 1;
	constant SEQ   : integer := 2;
	constant UNSEL : integer := 3;

	constant FROM_SOURCE            : std_logic := '1';
	constant FROM_DESTINATION       : std_logic := '0';
	--<DiMArch Constants BEGIN>--
	-- CONSTANT SRAM_SEQUENCER_INSTRUCTIONS : INTEGER := 16;
	-- CONSTANT SRAM_DEPTH                  : INTEGER := 128;
	CONSTANT SRAM_ADDRESS_WIDTH     : INTEGER   := log2(SRAM_DEPTH);
	CONSTANT SRAM_NumberOfInstrRegs : INTEGER   := 256;
	--CONSTANT SRAM_WIDTH      : INTEGER := 256;

	-----------------------------------------------------------------------------------------
	--Constants for SRAM_Sequencer
	CONSTANT MAX_DELAY           : INTEGER := 512;
	CONSTANT MAX_INCR_DECR_VALUE : INTEGER := 64;
	CONSTANT MAX_REPETITION      : INTEGER := 512;
	--CONSTANT RAM_DEPTH               : INTEGER := 128;
	--CONSTANT AGU_INSTR_WIDTH         : INTEGER := 91; 

	---------------------------------------
	-- Types
	---------------------------------------
	TYPE Refi_AGU_st_ty IS (IDLE_ST, COUNT_ST, RD_WR_ST, BIT_REVRS_ST, REPETITION_ST);

END top_consts_types_package;
