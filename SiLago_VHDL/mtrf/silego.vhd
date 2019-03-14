-------------------------------------------------------
--! @file
--! @brief Core silago cell (DRRA cell)
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
-- Title      : silego DRRA cell
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : silego.vhd
-- Author     : Nasim Farahini  <farahini@kth.se>
-- Company    : KTH
-- Created    : 2014-02-26
-- Last update: 2014-02-26
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		Description
-- 2014-02-25  1.0      Nasim Farahini  Created
-- 2019-03-11  2.0      Dimitrios       Updated
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
--! Use numeric library for signed and unsigned operations
USE ieee.numeric_std.ALL;
--! Use the sequencer function package
USE work.seq_functions_package.ALL;
--! Use the utility package
USE work.util_package.ALL;
--! Use the top package for constatnts and data types
USE work.top_consts_types_package.ALL;
--! Import the NOC_BUS_TYPE from the DiMArch package
USE work.noc_types_n_constants.NOC_BUS_TYPE;
--! Import the DATA_IO type from the DiMArch package
use work.noc_types_n_constants.DATA_IO_SIGNAL_TYPE;

--! @brief Core Silago cell, aka DRRA
--! @detail The core computation core of the DRRA fabric.
ENTITY silego IS
	PORT(clk                        : IN  std_logic;
	     rst_n                      : IN  std_logic;
	     instr_ld                   : IN  std_logic;
	     instr_inp                  : IN  std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0);
	     --seq_address 			: IN std_logic_vector(SEQ_ADDRS_WIDTH-1 DOWNTO 0);
	     seq_address_rb             : IN  std_logic;
	     seq_address_cb             : IN  std_logic;
	     --RegFile
	     reg_wr_2                   : IN  std_logic;
	     reg_rd_2                   : IN  std_logic;
	     reg_wr_addr_2              : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
	     reg_rd_addr_2              : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
	     data_in_reg_2              : IN  signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
	     dimarch_data_in            : in  STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
	     dimarch_data_out           : out STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
	     dimarch_rd_2_out           : out std_logic;
	     data_out_2                 : OUT signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
	     -- DiMArch bus output
	     noc_bus_out                : out NOC_BUS_TYPE;
	     --Horizontal Busses
	     h_bus_reg_in_out0_0_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out0_1_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out0_3_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out0_4_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out0_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out0_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out0_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out0_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out1_0_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out1_1_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out1_3_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_in_out1_4_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out1_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out1_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out1_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_reg_out_out1_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out0_0_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out0_1_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out0_3_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out0_4_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out0_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out0_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out0_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out0_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out1_0_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out1_1_left   : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out1_3_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_in_out1_4_right  : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out1_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out1_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out1_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     h_bus_dpu_out_out1_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     --sel_r_ext_in               : IN  s_bus_switchbox_ty;
	     sel_r_ext_in_0             : IN  std_logic_vector(5 downto 0);
	     sel_r_ext_in_1             : IN  std_logic_vector(5 downto 0);
	     sel_r_ext_in_2             : IN  std_logic_vector(5 downto 0);
	     sel_r_ext_in_3             : IN  std_logic_vector(5 downto 0);
	     sel_r_ext_in_4             : IN  std_logic_vector(5 downto 0);
	     sel_r_ext_in_5             : IN  std_logic_vector(5 downto 0);
	     --ext_v_input_bus_in         : IN  v_bus_ty;
	     ext_v_input_bus_in_0       : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_in_1       : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_in_2       : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_in_3       : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_in_4       : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_in_5       : IN  signed(BITWIDTH - 1 DOWNTO 0);
	     --sel_r_ext_out              : OUT s_bus_switchbox_ty;
	     sel_r_ext_out_0            : OUT std_logic_vector(5 downto 0);
	     sel_r_ext_out_1            : OUT std_logic_vector(5 downto 0);
	     sel_r_ext_out_2            : OUT std_logic_vector(5 downto 0);
	     sel_r_ext_out_3            : OUT std_logic_vector(5 downto 0);
	     sel_r_ext_out_4            : OUT std_logic_vector(5 downto 0);
	     sel_r_ext_out_5            : OUT std_logic_vector(5 downto 0);
	     --ext_v_input_bus_out        : OUT v_bus_ty
	     ext_v_input_bus_out_0      : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_out_1      : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_out_2      : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_out_3      : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_out_4      : OUT signed(BITWIDTH - 1 DOWNTO 0);
	     ext_v_input_bus_out_5      : OUT signed(BITWIDTH - 1 DOWNTO 0)
	    );
END ENTITY silego;
--! @brief Architecture for the core silago cell
--! @details This is the core DRRA cell used for computation
--! it includes: SwitchBox and MTRF (aka Computation DRRA cell).
ARCHITECTURE rtl OF silego IS

	SIGNAL sel_r_int, sel_r_ext_out, sel_r_ext_in                                                                                     : s_bus_switchbox_ty;
	SIGNAL s_bus_out                                                                                                                  : std_logic_vector(SWB_INSTR_PORT_SIZE - 1 DOWNTO 0);
	SIGNAL int_v_input_bus, ext_v_input_bus_out                                                                                       : v_bus_ty;
	SIGNAL data_in_reg_0_w, data_in_reg_1_w, reg_data_out0_left_w, reg_data_out1_left_w, reg_data_out0_right_w, reg_data_out1_right_w : signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	SIGNAL dpu_in_0_w, dpu_in_1_w, dpu_in_2_w, dpu_in_3_w, dpu_out0_left_w, dpu_out1_left_w, dpu_out0_right_w, dpu_out1_right_w       : signed(DPU_IN_WIDTH - 1 DOWNTO 0);
	SIGNAL data_from_other_row                                                                                                        : std_logic_vector(5 DOWNTO 0);
	SIGNAL h_bus_reg_in, h_bus_dpu_in                                                                                                 : h_bus_ty(0 TO MAX_NR_OF_OUTP_N_HOPS - 1, 0 TO NR_OF_OUTP - 1);
	--SIGNAL dpu_ctrl_out_0_w,dpu_ctrl_out_1_w  :  std_logic_vector(1 DOWNTO 0);

BEGIN

	data_in_reg_0_w <= int_v_input_bus(0) when data_from_other_row(0) = '0' else ext_v_input_bus_in_0;
	data_in_reg_1_w <= int_v_input_bus(1) when data_from_other_row(1) = '0' else ext_v_input_bus_in_1;
	dpu_in_0_w      <= int_v_input_bus(2) when data_from_other_row(2) = '0' else ext_v_input_bus_in_2;
	dpu_in_1_w      <= int_v_input_bus(3) when data_from_other_row(3) = '0' else ext_v_input_bus_in_3;
	dpu_in_2_w      <= int_v_input_bus(4) when data_from_other_row(4) = '0' else ext_v_input_bus_in_4;
	dpu_in_3_w      <= int_v_input_bus(5) when data_from_other_row(5) = '0' else ext_v_input_bus_in_5;

	h_bus_reg_in(0,0) <= h_bus_reg_in_out0_0_left;
	h_bus_reg_in(1,0) <= h_bus_reg_in_out0_1_left;
	h_bus_reg_in(2,0) <= reg_data_out0_left_w;
	h_bus_reg_in(3,0) <= h_bus_reg_in_out0_3_right;
	h_bus_reg_in(4,0) <= h_bus_reg_in_out0_4_right;

	h_bus_reg_out_out0_0_right <= h_bus_reg_in_out0_1_left;
	h_bus_reg_out_out0_1_right <= reg_data_out0_right_w;
	h_bus_reg_out_out0_3_left  <= reg_data_out0_left_w;
	h_bus_reg_out_out0_4_left  <= h_bus_reg_in_out0_3_right;

	h_bus_reg_in(0,1) <= h_bus_reg_in_out1_0_left;
	h_bus_reg_in(1,1) <= h_bus_reg_in_out1_1_left;
	h_bus_reg_in(2,1) <= reg_data_out1_left_w;
	h_bus_reg_in(3,1) <= h_bus_reg_in_out1_3_right;
	h_bus_reg_in(4,1) <= h_bus_reg_in_out1_4_right;

	h_bus_reg_out_out1_0_right <= h_bus_reg_in_out1_1_left;
	h_bus_reg_out_out1_1_right <= reg_data_out1_right_w;
	h_bus_reg_out_out1_3_left  <= reg_data_out1_left_w;
	h_bus_reg_out_out1_4_left  <= h_bus_reg_in_out1_3_right;

	h_bus_dpu_in(0,0) <= h_bus_dpu_in_out0_0_left;
	h_bus_dpu_in(1,0) <= h_bus_dpu_in_out0_1_left;
	h_bus_dpu_in(2,0) <= dpu_out0_left_w; --when  dpu_ctrl_out_0_w ="01" else dpu_out0_right_w;
	h_bus_dpu_in(3,0) <= h_bus_dpu_in_out0_3_right;
	h_bus_dpu_in(4,0) <= h_bus_dpu_in_out0_4_right;

	h_bus_dpu_out_out0_0_right <= h_bus_dpu_in_out0_1_left;
	h_bus_dpu_out_out0_1_right <= dpu_out0_right_w;
	h_bus_dpu_out_out0_3_left  <= dpu_out0_left_w;
	h_bus_dpu_out_out0_4_left  <= h_bus_dpu_in_out0_3_right;

	h_bus_dpu_in(0,1) <= h_bus_dpu_in_out1_0_left;
	h_bus_dpu_in(1,1) <= h_bus_dpu_in_out1_1_left;
	h_bus_dpu_in(2,1) <= dpu_out1_left_w; --when  dpu_ctrl_out_0_w ="01" else dpu_out0_right_w;
	h_bus_dpu_in(3,1) <= h_bus_dpu_in_out1_3_right;
	h_bus_dpu_in(4,1) <= h_bus_dpu_in_out1_4_right;

	h_bus_dpu_out_out1_0_right <= h_bus_dpu_in_out1_1_left;
	h_bus_dpu_out_out1_1_right <= dpu_out1_right_w;
	h_bus_dpu_out_out1_3_left  <= dpu_out1_left_w;
	h_bus_dpu_out_out1_4_left  <= h_bus_dpu_in_out1_3_right;

	--This signal is unrolled just to make the interface signals one-dimensional, this can be rolled back and this part can be eliminated
	sel_r_ext_out_0 <= sel_r_ext_out(0);
	sel_r_ext_out_1 <= sel_r_ext_out(1);
	sel_r_ext_out_2 <= sel_r_ext_out(2);
	sel_r_ext_out_3 <= sel_r_ext_out(3);
	sel_r_ext_out_4 <= sel_r_ext_out(4);
	sel_r_ext_out_5 <= sel_r_ext_out(5);

	--This signal is unrolled just to make the interface signals one-dimensional, this can be rolled back and this part can be eliminated
	sel_r_ext_in(0)       <= sel_r_ext_in_0;
	sel_r_ext_in(1)       <= sel_r_ext_in_1;
	sel_r_ext_in(2)       <= sel_r_ext_in_2;
	sel_r_ext_in(3)       <= sel_r_ext_in_3;
	sel_r_ext_in(4)       <= sel_r_ext_in_4;
	sel_r_ext_in(5)       <= sel_r_ext_in_5;
	--This signal is unrolled just to make the interface signals one-dimensional, this can be rolled back and this part can be eliminated
	ext_v_input_bus_out_0 <= ext_v_input_bus_out(0);
	ext_v_input_bus_out_1 <= ext_v_input_bus_out(1);
	ext_v_input_bus_out_2 <= ext_v_input_bus_out(2);
	ext_v_input_bus_out_3 <= ext_v_input_bus_out(3);
	ext_v_input_bus_out_4 <= ext_v_input_bus_out(4);
	ext_v_input_bus_out_5 <= ext_v_input_bus_out(5);

	config_swb : ENTITY work.cell_config_swb
		PORT MAP(clk                 => clk,
		         rst_n               => rst_n,
		         swb_instr_in        => s_bus_out,
		         sel_r_int           => sel_r_int,
		         sel_r_ext_out       => sel_r_ext_out,
		         data_from_other_row => data_from_other_row);

	swb : ENTITY work.switchbox
		GENERIC MAP(BITWIDTH => BITWIDTH,
		            M        => MAX_NR_OF_OUTP_N_HOPS)
		PORT MAP(inputs_reg          => h_bus_reg_in,
		         inputs_dpu          => h_bus_dpu_in,
		         sel_r_int           => sel_r_int,
		         int_v_input_bus     => int_v_input_bus,
		         sel_r_ext_in        => sel_r_ext_in,
		         ext_v_input_bus_out => ext_v_input_bus_out);

	MTRF_cell : ENTITY work.MTRF_cell
		PORT MAP(
			dimarch_data_in      => dimarch_data_in,
			dimarch_data_out     => dimarch_data_out,
			dimarch_rd_2_out     => dimarch_rd_2_out,
			noc_bus_out          => noc_bus_out,
			clk                  => clk,
			rst_n                => rst_n,
			instr_ld             => instr_ld, --(i),
			instr_inp            => instr_inp, --instr_output(i)(OLD_INSTR_WIDTH-1 downto INSTR_WIDTH_DIFF),
			seq_address_rb       => seq_address_rb,
			seq_address_cb       => seq_address_cb,
			reg_wr_2             => reg_wr_2,
			reg_rd_2             => reg_rd_2,
			reg_wr_addr_2        => reg_wr_addr_2, --reg_address_in(i,j),
			reg_rd_addr_2        => reg_rd_addr_2, --reg_address_out(i,j),
			data_in_reg_0        => data_in_reg_0_w,
			data_in_reg_1        => data_in_reg_1_w,
			data_in_reg_2        => data_in_reg_2,
			data_out_reg_0_right => reg_data_out0_right_w,
			data_out_reg_0_left  => reg_data_out0_left_w,
			data_out_reg_1_right => reg_data_out1_right_w,
			data_out_reg_1_left  => reg_data_out1_left_w,
			data_out_2           => data_out_2,
			dpu_in_0             => dpu_in_0_w, --this expression is not synthesizable. Should be changed to numbers
			dpu_in_1             => dpu_in_1_w,
			dpu_in_2             => dpu_in_2_w,
			dpu_in_3             => dpu_in_3_w,
			dpu_out_0_left       => dpu_out0_left_w,
			dpu_out_0_right      => dpu_out0_right_w,
			-- dpu_ctrl_out_0_w     => dpu_ctrl_out_0_w,
			dpu_out_1_left       => dpu_out1_left_w,
			dpu_out_1_right      => dpu_out1_right_w,
			-- dpu_ctrl_out_1_w     => dpu_ctrl_out_1_w,
			s_bus_out            => s_bus_out
			--h_bus_reg_in        => h_bus_reg,
			--h_bus_dpu_in        => h_bus_dpu
			--outputs_sw           =>v_lanes(i)

		);

END ARCHITECTURE rtl;
