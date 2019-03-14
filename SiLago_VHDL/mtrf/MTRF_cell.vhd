-------------------------------------------------------
--! @file
--! @brief MTRF cell
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
-- Title      : MTRF_cell
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : MTRF_cell.vhd
-- Author     : Nasim Farahini  <farahini@kth.se>
-- Company    : KTH
-- Created    : 2014-02-26
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		  Description
-- 2014-02-25  1.0      Nasim Farahini    Created
-- 2019-03-11  2.0      Dimitrios Stathis Updated
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
library ieee;
--! Use standard library
use ieee.std_logic_1164.all;
--! Use numeric library for signed and unsigned arithmetics
use ieee.numeric_std.all;
--! Use the sequencer function package
USE work.seq_functions_package.ALL;
--! Use the utility package
USE work.util_package.ALL;
--! Import the NOC_BUS_TYPE type from the DiMArch package
use work.noc_types_n_constants.NOC_BUS_TYPE;
--! Use the top constant package that includes all the constants and type definitions
use work.top_consts_types_package.all;


--! @brief This is the MTRF cell, aka DRRA cell
--! @detail The cell includes the register file, DPU, and sequencer [sequencer includes the RACCU]
ENTITY MTRF_cell IS
	PORT(clk                  : IN  std_logic;
	     rst_n                : IN  std_logic;
	     instr_ld             : IN  std_logic;
	     instr_inp            : IN  std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0);
	     --seq_address 			: IN std_logic_vector(SEQ_ADDRS_WIDTH-1 DOWNTO 0);
	     seq_address_rb       : IN  std_logic;
	     seq_address_cb       : IN  std_logic;
	     --RegFile
	     reg_wr_2             : IN  std_logic;
	     reg_rd_2             : IN  std_logic;
	     reg_wr_addr_2        : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
	     reg_rd_addr_2        : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
	     data_in_reg_0        : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	     data_in_reg_1        : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	     data_in_reg_2        : IN  signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
	     dimarch_data_in      : in  STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
	     dimarch_data_out     : out STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
	     dimarch_rd_2_out     : out std_logic;
	     data_out_reg_0_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	     data_out_reg_0_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	     data_out_reg_1_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	     data_out_reg_1_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	     data_out_2           : OUT signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
	     --DPU
	     dpu_in_0             : IN  signed(DPU_IN_WIDTH - 1 DOWNTO 0); --signed
	     dpu_in_1             : IN  signed(DPU_IN_WIDTH - 1 DOWNTO 0); --signed
	     dpu_in_2             : IN  signed(DPU_IN_WIDTH - 1 DOWNTO 0); --signed
	     dpu_in_3             : IN  signed(DPU_IN_WIDTH - 1 DOWNTO 0); --signed
	     dpu_out_0_left       : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
	     dpu_out_0_right      : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
	     --dpu_ctrl_out_0_w     : OUT std_logic_vector(DPU_CTRL_OUT_WIDTH - 1 DOWNTO 0);
	     dpu_out_1_left       : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
	     dpu_out_1_right      : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
	     --dpu_ctrl_out_1_w     : OUT std_logic_vector(DPU_CTRL_OUT_WIDTH - 1 DOWNTO 0);
	     --SWB_
	     --inputs_sw_reg        : IN  h_bus_ty(0 TO MAX_NR_OF_OUTP_N_HOPS - 1, 0 TO NR_OF_OUTP - 1);
	     --inputs_sw_dpu        : IN  h_bus_ty(0 TO MAX_NR_OF_OUTP_N_HOPS - 1, 0 TO NR_OF_OUTP - 1);
	     --sel_r_ext_in         : IN  s_bus_switchbox_ty;
	     --ext_v_input_bus_in   : IN  v_bus_ty;
	     --sel_r_ext_out        : OUT s_bus_switchbox_ty;
	     --ext_v_input_bus_out  : OUT v_bus_ty
	     --data_from_other_row    :OUT   std_logic_vector(5 downto 0);
	     --data_to_other_row      :OUT   std_logic_vector(5 downto 0);
	     --sel_sw_reg           : IN  s_bus_switchbox_ty;
	     -- outputs_sw_reg       : OUT v_bus_ty;
	     --sel_r_ext            :  OUT s_bus_switchbox_ty
	     --sel_sw_dpu           : IN  s_bus_switchbox_ty;
	     --outputs_sw_dpu       : OUT v_bus_ty;
	     --outputs_sw			  : OUT v_bus_ty
	     noc_bus_out          : out NOC_BUS_TYPE;
	     s_bus_out            : OUT std_logic_vector(SWB_INSTR_PORT_SIZE - 1 DOWNTO 0)
	     -- HC
	     --hb_hc_in             : IN  hc_in_bus_ty(0 TO 2*(MAX_NR_OF_OUTP_N_HOPS-1)+1);
	     --hc_out_left          : OUT std_logic_vector(HC_OUT_BITS-1 DOWNTO 0) ;
	     --hc_out_right         : OUT std_logic_vector(HC_OUT_BITS-1 DOWNTO 0)
	    );
END ENTITY MTRF_cell;

ARCHITECTURE rtl OF MTRF_cell IS

	SIGNAL dpu_mode_cfg_w                       : std_logic_vector(DPU_MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL dpu_acc_clear_w                      : std_logic_vector(DPU_ACC_CLEAR_WIDTH - 1 DOWNTO 0);
	SIGNAL dpu_ctrl_out_0_w                     : std_logic_vector(DPU_CTRL_OUT_WIDTH - 1 DOWNTO 0);
	SIGNAL dpu_ctrl_out_1_w                     : std_logic_vector(DPU_CTRL_OUT_WIDTH - 1 DOWNTO 0);
	SIGNAL dpu_sat_ctrl_w                       : std_logic_vector(DPU_SAT_CTRL_WIDTH - 1 DOWNTO 0);
	SIGNAL dpu_process_inout_w                  : std_logic_vector(DPU_PROCESS_INOUT_WIDTH - 1 DOWNTO 0);
	SIGNAL reg_initial_delay_w                  : std_logic_vector(INIT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL instr_start_w, dpu_acc_clear_rst_w   : std_logic;
	SIGNAL reg_start_addrs_w                    : std_logic_vector(STARTING_ADDRS_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_step_val_w                       : std_logic_vector(STEP_VALUE_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_step_val_sign_w                  : std_logic_vector(STEP_VALUE_SIGN_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_no_of_addrs_w                    : std_logic_vector(NR_OF_ADDRS_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_port_type_w                      : std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_outp_cntrl_w                     : std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_middle_delay_w                   : std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 DOWNTO 0);
	SIGNAL reg_no_of_rpts_w                     : std_logic_vector(NUM_OF_REPT_PORT_SIZE - 1 DOWNTO 0);
	SIGNAL reg_rpt_step_value_w                 : std_logic_vector(REP_STEP_VALUE_PORT_SIZE - 1 DOWNTO 0);
	SIGNAL reg_rpt_delay_w                      : std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_mode_w                           : std_logic_vector(MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL reg_fft_stage_w, reg_end_fft_stage_w : std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
	SIGNAL seq_cond_status_w                    : std_logic_vector(SEQ_COND_STATUS_WIDTH - 1 DOWNTO 0);
	SIGNAL dimarch_mode                         : std_logic;
	--  SIGNAL dimarch_mode : std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE-1 downto 0);
	SIGNAL dpu_process_inout                    : std_logic_vector(DPU_PROCESS_INOUT_WIDTH - 1 DOWNTO 0);

	--SIGNAL sel_r_int				 : s_bus_switchbox_ty;
	--SIGNAL s_bus_out				 :  std_logic_vector(SWB_INSTR_PORT_SIZE - 1 DOWNTO 0);
	--SIGNAL int_v_input_bus           : v_bus_ty;
	--SIGNAL reg_in_0_w,reg_in_1_w  : signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
	--SIGNAL dpu_in_0_w,dpu_in_1_w,dpu_in_2_w,dpu_in_3_w: signed(DPU_IN_WIDTH - 1 DOWNTO 0);
	--SIGNAL data_from_other_row    :  std_logic_vector(5 DOWNTO 0);
	--SIGNAL data_from_other_row     :  std_logic_vector(5 downto 0);
	-- SIGNAL reg_read        		 : STD_LOGIC; --SRAM_RW_ENABLE_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);
	--SIGNAL reg_write       		 : STD_LOGIC; --SRAM_RW_ENABLE_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);
	-- signal reg_address_in  		 : std_logic_vector (REG_ADDRESS_WIDTH-1	downto 0);--SRAM_RW_address_type(0 TO COLUMNS - 1, 0 TO ROWS - 1);
	--signal reg_address_out         : std_logic_vector (REG_ADDRESS_WIDTH-1	downto 0);--SRAM_RW_address_type(0 TO COLUMNS - 1, 0 TO ROWS - 1);
	--signal reg_in          		 : STD_LOGIC_VECTOR (SRAM_WIDTH-1 downto 0);--SRAM_RW_DATA_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);
	--signal reg_out_signed  		 : SIGNED (SRAM_WIDTH-1 downto 0);--SRAM_RW_SIGNED_DATA_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);

BEGIN

	seq_gen : ENTITY work.sequencer
		GENERIC MAP(M => MAX_NR_OF_OUTP_N_HOPS - 1)
		PORT MAP(reg_dimarch_mode   => dimarch_mode,
		         NOC_BUS_OUT        => noc_bus_out,
		         clk                => clk,
		         rst_n              => rst_n,
		         instr_ld           => instr_ld,
		         instr_inp          => instr_inp,
		         --seq_address          => seq_address,
		         seq_address_rb     => seq_address_rb,
		         seq_address_cb     => seq_address_cb,
		         seq_cond_status    => seq_cond_status_w,
		         dpu_cfg            => dpu_mode_cfg_w,
		         dpu_ctrl_out_2     => dpu_ctrl_out_0_w,
		         dpu_ctrl_out_3     => dpu_ctrl_out_1_w,
		         dpu_acc_clear_rst  => dpu_acc_clear_rst_w,
		         dpu_acc_clear      => dpu_acc_clear_w,
		         dpu_sat_ctrl       => dpu_sat_ctrl_w,
		         dpu_process_inout  => dpu_process_inout_w,
		         instr_start        => instr_start_w,
		         reg_port_type      => reg_port_type_w,
		         reg_start_addrs    => reg_start_addrs_w,
		         reg_no_of_addrs    => reg_no_of_addrs_w,
		         reg_initial_delay  => reg_initial_delay_w,
		         reg_step_val       => reg_step_val_w,
		         reg_step_val_sign  => reg_step_val_sign_w,
		         reg_middle_delay   => reg_middle_delay_w,
		         reg_no_of_rpts     => reg_no_of_rpts_w,
		         reg_rpt_step_value => reg_rpt_step_value_w,
		         reg_rpt_delay      => reg_rpt_delay_w,
		         reg_mode           => reg_mode_w,
		         reg_outp_cntrl     => reg_outp_cntrl_w,
		         reg_fft_stage      => reg_fft_stage_w,
		         reg_end_fft_stage  => reg_end_fft_stage_w,
		         --hc_h_bus_in        => hb_hc_in,
		         --hc_out_left        => hc_out_left,
		         --hc_out_right       => hc_out_right,
		         s_bus_out          => s_bus_out);

	dpu_gen : ENTITY work.DPU
		PORT MAP(clk               => clk,
		         rst_n             => rst_n,
		         dpu_ctrl_out_0    => dpu_ctrl_out_0_w,
		         dpu_ctrl_out_1    => dpu_ctrl_out_1_w,
		         dpu_in_0          => dpu_in_0,
		         dpu_in_1          => dpu_in_1,
		         dpu_in_2          => dpu_in_2,
		         dpu_in_3          => dpu_in_3,
		         dpu_mode_cfg      => dpu_mode_cfg_w,
		         dpu_acc_clear_rst => dpu_acc_clear_rst_w,
		         dpu_acc_clear     => dpu_acc_clear_w,
		         dpu_process_inout => dpu_process_inout_w,
		         dpu_out_0_left    => dpu_out_0_left,
		         dpu_out_0_right   => dpu_out_0_right,
		         dpu_out_1_left    => dpu_out_1_left,
		         dpu_out_1_right   => dpu_out_1_right,
		         dpu_sat_ctrl      => dpu_sat_ctrl_w,
		         seq_cond_status   => seq_cond_status_w);

	reg_top : ENTITY work.register_file_top
		PORT MAP(
			dimarch_data_in      => dimarch_data_in,
			dimarch_data_out     => dimarch_data_out,
			dimarch_rd_2_out     => dimarch_rd_2_out,
			dimarch_mode         => dimarch_mode,
			rst_n                => rst_n,
			clk                  => clk,
			instr_start          => instr_start_w,
			wr_2                 => reg_wr_2,
			rd_2                 => reg_rd_2,
			wr_addr_2            => reg_wr_addr_2,
			rd_addr_2            => reg_rd_addr_2,
			reg_port_type        => reg_port_type_w,
			instr_initial_delay  => reg_initial_delay_w,
			instr_start_addrs    => reg_start_addrs_w,
			instr_step_val       => reg_step_val_w,
			instr_step_val_sign  => reg_step_val_sign_w,
			instr_no_of_addrs    => reg_no_of_addrs_w,
			instr_middle_delay   => reg_middle_delay_w,
			instr_no_of_rpts     => reg_no_of_rpts_w,
			instr_rpt_step_value => reg_rpt_step_value_w,
			instr_rpt_delay      => reg_rpt_delay_w,
			instr_mode           => reg_mode_w,
			instr_fft_stage      => reg_fft_stage_w,
			instr_end_fft_stage  => reg_end_fft_stage_w,
			data_in_reg_0        => data_in_reg_0,
			data_in_reg_1        => data_in_reg_1,
			data_in_reg_2        => data_in_reg_2,
			reg_outp_cntrl       => reg_outp_cntrl_w,
			data_out_reg_0_right => data_out_reg_0_right,
			data_out_reg_0_left  => data_out_reg_0_left,
			data_out_reg_1_right => data_out_reg_1_right,
			data_out_reg_1_left  => data_out_reg_1_left,
			data_out_2           => data_out_2);

END ARCHITECTURE rtl;
