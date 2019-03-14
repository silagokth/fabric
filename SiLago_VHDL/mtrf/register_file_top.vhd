-------------------------------------------------------
--! @file
--! @brief Register file
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
-- Title      : Register file
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File        : register_file.vhd
-- Author      : sadiq  <sadiq@drrasystem>
-- Company     : 
-- Created     : 2013-07-19
-- Last update : 2019-03-11
-- Platform    : 
-- Standard    : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author    Description
-- 2013-07-19  1.0      sadiq 	  Created
-- 2019-03-11  2.0      Dimitris  Fix some DiMArch bugs
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

--! IEEE Library and the WORK Library
library ieee, work;
--! Use standard library
use ieee.std_logic_1164.all;
--! Use numeric library for signed and unsigned arithmetics
use ieee.numeric_std.all;
--! Use the sequencer function package
USE work.seq_functions_package.ALL;
--! Use the utility package
USE work.util_package.ALL;
--! Use unsigned library, treat all std_vector as unsigned numbers
USE ieee.std_logic_unsigned.ALL; -- @TODO Need to remove the unsigned library, only the numeric_std library should be used
--! Use the top constant package that includes all the constants and type definitions
use work.top_consts_types_package.all;


--! @brief This is the top register file entity of the DRRA
--! @detail The register file top, includes all the necessary modules.
--! That inculdes 4 AGUS, one for each port and the register file.
ENTITY register_file_top IS
  PORT(
  	clk                  : IN  std_logic;
  	rst_n                : IN  std_logic;
  	instr_start          : IN  std_logic;
  	reg_port_type        : IN  std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE - 1 DOWNTO 0);
  	instr_initial_delay  : IN  std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
  	instr_start_addrs    : IN  std_logic_vector(START_ADDR_WIDTH - 1 DOWNTO 0);
  	instr_step_val       : IN  std_logic_vector(ADDR_OFFSET_WIDTH - 1 DOWNTO 0);
  	instr_step_val_sign  : IN  std_logic_vector(ADDR_OFFSET_SIGN_WIDTH - 1 DOWNTO 0);
  	instr_no_of_addrs    : IN  std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
  	instr_middle_delay   : In  std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 DOWNTO 0);
  	instr_no_of_rpts     : In  std_logic_vector(NUM_OF_REPT_PORT_SIZE - 1 DOWNTO 0);
  	instr_rpt_step_value : In  std_logic_vector(REP_STEP_VALUE_PORT_SIZE - 1 DOWNTO 0);
  	instr_rpt_delay      : IN  std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
  	instr_mode           : IN  std_logic_vector(MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
  	instr_fft_stage      : IN  std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
  	instr_end_fft_stage  : IN  std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
  	data_in_reg_0        : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  	data_in_reg_1        : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  	data_in_reg_2        : IN  signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
  	reg_outp_cntrl       : IN  std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 DOWNTO 0);
  	dimarch_mode         : in std_logic;
--  	dimarch_mode         : in  std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE-1 downto 0); -- when 1 use for dimarch
  	dimarch_data_in      : in  STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
  	dimarch_data_out     : out STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
  	dimarch_rd_2_out                 : out std_logic;
  	
  	wr_2                 : in  std_logic;
  	rd_2                 : in  std_logic;
  	wr_addr_2            : in  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
  	rd_addr_2            : in  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
  	data_out_reg_0_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  	data_out_reg_0_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  	data_out_reg_1_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  	data_out_reg_1_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  	data_out_2           : OUT signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0)
  );		

END ENTITY register_file_top;

ARCHITECTURE rtl OF register_file_top IS

  SIGNAL wr_addr_0, wr_addr_1                                                           : std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_data_ready_0, wr_data_ready_1                                               : std_logic;
  SIGNAL rd_addr_0, rd_addr_1                                                           : std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_instr_start_0, wr_instr_start_1, rd_instr_start_0, rd_instr_start_1         : std_logic;
  SIGNAL wr_initial_delay_0, wr_initial_delay_1, rd_initial_delay_0, rd_initial_delay_1 : std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_start_addr_0, wr_start_addr_1, rd_start_addr_0, rd_start_addr_1             : std_logic_vector(START_ADDR_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_addr_offset_0, wr_addr_offset_1, rd_addr_offset_0, rd_addr_offset_1         : std_logic_vector(ADDR_OFFSET_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_addr_offset_sign_0, wr_addr_offset_sign_1, rd_addr_offset_sign_0, rd_addr_offset_sign_1         : std_logic_vector(ADDR_OFFSET_SIGN_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_addr_counter_0, wr_addr_counter_1, rd_addr_counter_0, rd_addr_counter_1     : std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_instr_middle_delay_0, wr_instr_middle_delay_1, rd_instr_middle_delay_0, rd_instr_middle_delay_1 : std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE-1 DOWNTO 0);
  SIGNAL wr_instr_no_of_rpts_0, wr_instr_no_of_rpts_1, rd_instr_no_of_rpts_0 , rd_instr_no_of_rpts_1      : std_logic_vector(NUM_OF_REPT_PORT_SIZE-1 DOWNTO 0);
  SIGNAL wr_instr_rpt_step_value_0, wr_instr_rpt_step_value_1, rd_instr_rpt_step_value_0, rd_instr_rpt_step_value_1    : std_logic_vector(REP_STEP_VALUE_PORT_SIZE-1 DOWNTO 0);
  SIGNAL wr_instr_rpt_delay_0, wr_instr_rpt_delay_1, rd_instr_rpt_delay_0, rd_instr_rpt_delay_1 : std_logic_vector(REPT_DELAY_VECTOR_SIZE-1 DOWNTO 0);
  SIGNAL wr_instr_mode_0, wr_instr_mode_1 , rd_instr_mode_0 , rd_instr_mode_1       :  std_logic_vector(MODE_SEL_VECTOR_SIZE-1 DOWNTO 0);
  SIGNAL wr_instr_end_fft_stage_0, wr_instr_end_fft_stage_1, rd_instr_end_fft_stage_0, rd_instr_end_fft_stage_1 :  std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE-1 DOWNTO 0);
  SIGNAL wr_instr_fft_stage_0, wr_instr_fft_stage_1, rd_instr_fft_stage_0, rd_instr_fft_stage_1 :  std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE-1 DOWNTO 0);
  SIGNAL rd_data_ready_0                                                                : std_logic;
  SIGNAL rd_data_ready_1                                                                : std_logic;


--   signal    wr_2				:  std_logic;
--   signal    rd_2				:  std_logic;
--   signal    wr_addr_2			:   std_logic_vector(REG_FILE_MEM_ADDR_WIDTH-1 downto 0);
--   signal	 rd_addr_2     		:   std_logic_vector(REG_FILE_MEM_ADDR_WIDTH-1 downto 0);

BEGIN  -- ARCHITECTURE rtl

  
  RegisterFile : ENTITY work.register_file

    PORT MAP (
      dimarch_data_in => dimarch_data_in,
      dimarch_data_out => dimarch_data_out,
		dimarch_rd_2_out    => dimarch_rd_2_out,
      clk             => clk,
      rst_n           => rst_n,
      rd_tb			  => rd_2,
	 dimarch_mode  => dimarch_mode,
	 rd_inst_start_0  => rd_instr_start_0,
	 wr_inst_start_0 => wr_instr_start_0,
      wr_addr_0       => wr_addr_0,
      wr_addr_1       => wr_addr_1,
      wr_addr_tb       => wr_addr_2,
      wr_data_ready_0 => wr_data_ready_0,
      wr_data_ready_1 => wr_data_ready_1,
      wr_data_ready_tb => wr_2,
      rd_addr_0       => rd_addr_0,
      rd_addr_1       => rd_addr_1,
      rd_addr_tb       => rd_addr_2,
      rd_0			  => rd_data_ready_0,
      rd_1            => rd_data_ready_1,
      data_in_0       => data_in_reg_0,
      data_in_1       => data_in_reg_1,
      data_in_2       => data_in_reg_2,
      reg_outp_cntrl => reg_outp_cntrl,
      data_out_reg_0_right      => data_out_reg_0_right,
      data_out_reg_0_left      => data_out_reg_0_left,
      data_out_reg_1_right      => data_out_reg_1_right,
      data_out_reg_1_left      => data_out_reg_1_left,
      data_out_2	  => data_out_2);




  AGU_Wr_0_instantiate : ENTITY work.AGU
    PORT MAP (
      clk                 => clk,
      rst_n               => rst_n,
      instr_start         => wr_instr_start_0,
      instr_initial_delay => wr_initial_delay_0,
      instr_start_addrs   => wr_start_addr_0,
      instr_step_val      => wr_addr_offset_0,
      instr_step_val_sign      => wr_addr_offset_sign_0,
      instr_no_of_addrs   => wr_addr_counter_0,
      instr_middle_delay => wr_instr_middle_delay_0,
      instr_no_of_rpts        => wr_instr_no_of_rpts_0,
      instr_rpt_step_value    => wr_instr_rpt_step_value_0,
      instr_rpt_delay    => wr_instr_rpt_delay_0,    
      instr_mode       => wr_instr_mode_0,
      instr_fft_stage  => wr_instr_fft_stage_0,
      instr_end_fft_stage  => instr_end_fft_stage,
      addr_out            => wr_addr_0,
      addr_en             => wr_data_ready_0);


  AGU_Wr_1_instantiate : ENTITY work.AGU
    PORT MAP (
      clk                 => clk,
      rst_n               => rst_n,
      instr_start         => wr_instr_start_1,
      instr_initial_delay => wr_initial_delay_1,
      instr_start_addrs   => wr_start_addr_1,
      instr_step_val      => wr_addr_offset_1,
      instr_step_val_sign      => wr_addr_offset_sign_1,      
      instr_no_of_addrs   => wr_addr_counter_1,
      instr_middle_delay => wr_instr_middle_delay_1,
      instr_no_of_rpts        => wr_instr_no_of_rpts_1,
      instr_rpt_step_value    => wr_instr_rpt_step_value_1,
      instr_rpt_delay    => wr_instr_rpt_delay_1, 
      instr_mode       => wr_instr_mode_1,
      instr_fft_stage  => wr_instr_fft_stage_1,
      instr_end_fft_stage  => wr_instr_end_fft_stage_1,
      addr_out            => wr_addr_1,
      addr_en             => wr_data_ready_1);

  AGU_Rd_0_instantiate : ENTITY work.AGU
    PORT MAP (
      clk                 => clk,
      rst_n               => rst_n,
      instr_start         => rd_instr_start_0,
      instr_initial_delay => rd_initial_delay_0,
      instr_start_addrs   => rd_start_addr_0,
      instr_step_val      => rd_addr_offset_0,
      instr_step_val_sign      => rd_addr_offset_sign_0,      
      instr_no_of_addrs   => rd_addr_counter_0,
      instr_middle_delay => rd_instr_middle_delay_0,
      instr_no_of_rpts        => rd_instr_no_of_rpts_0,
      instr_rpt_step_value    => rd_instr_rpt_step_value_0,
      instr_rpt_delay    => rd_instr_rpt_delay_0, 
      instr_mode       => rd_instr_mode_0,
      instr_fft_stage  => rd_instr_fft_stage_0,
      instr_end_fft_stage  => rd_instr_end_fft_stage_0,
      addr_out            => rd_addr_0,
      addr_en             => rd_data_ready_0);

  AGU_Rd_1_instantiate : ENTITY work.AGU
    PORT MAP (
      clk                 => clk,
      rst_n               => rst_n,
      instr_start         => rd_instr_start_1,
      instr_initial_delay => rd_initial_delay_1,
      instr_start_addrs   => rd_start_addr_1,
      instr_step_val      => rd_addr_offset_1,
      instr_step_val_sign     => rd_addr_offset_sign_1,     
      instr_no_of_addrs   => rd_addr_counter_1,
      instr_middle_delay => rd_instr_middle_delay_1,
      instr_no_of_rpts        => rd_instr_no_of_rpts_1,
      instr_rpt_step_value    => rd_instr_rpt_step_value_1,
      instr_rpt_delay    => rd_instr_rpt_delay_1, 
      instr_mode       => rd_instr_mode_1,
      instr_fft_stage  => rd_instr_fft_stage_1,
      instr_end_fft_stage  => rd_instr_end_fft_stage_1,
      addr_out            => rd_addr_1,
      addr_en             => rd_data_ready_1);



  AGU_Wr_0 : PROCESS (reg_port_type, instr_start, instr_initial_delay, instr_start_addrs, instr_step_val,instr_step_val_sign , instr_no_of_addrs, instr_middle_delay, instr_no_of_rpts, instr_rpt_step_value, instr_mode, instr_rpt_delay, instr_fft_stage, instr_end_fft_stage)
  BEGIN
    IF reg_port_type = "00" THEN
      wr_instr_start_0   <= instr_start;
      wr_initial_delay_0 <= instr_initial_delay;
      wr_start_addr_0    <= instr_start_addrs;
      wr_addr_offset_0   <= instr_step_val;
      wr_addr_offset_sign_0   <= instr_step_val_sign;
      wr_addr_counter_0  <= instr_no_of_addrs;
      wr_instr_middle_delay_0 <= instr_middle_delay;
      wr_instr_no_of_rpts_0 <= instr_no_of_rpts;       
      wr_instr_rpt_step_value_0 <= instr_rpt_step_value;  
      wr_instr_rpt_delay_0 <= instr_rpt_delay;
      wr_instr_mode_0      <= instr_mode ;
      wr_instr_fft_stage_0  <= instr_fft_stage;
      wr_instr_end_fft_stage_0  <= instr_end_fft_stage;

    ELSE
      wr_instr_start_0   <= '0';
      wr_initial_delay_0 <= (OTHERS => '0');
      wr_start_addr_0    <= (OTHERS => '0');
      wr_addr_offset_0   <= (OTHERS => '0');
      wr_addr_offset_sign_0   <= (OTHERS => '0');
      wr_addr_counter_0  <= (OTHERS => '0');
      wr_instr_middle_delay_0 <= (OTHERS => '0');
      wr_instr_no_of_rpts_0 <= (OTHERS => '0');    
      wr_instr_rpt_step_value_0 <= (OTHERS => '0'); 
      wr_instr_rpt_delay_0 <= (OTHERS => '0'); 
      wr_instr_mode_0      <= (OTHERS => '0') ;
      wr_instr_fft_stage_0  <= (OTHERS => '0');
      wr_instr_end_fft_stage_0  <= (OTHERS => '0');
    END IF;
  END PROCESS AGU_Wr_0;

  AGU_Wr_1 : PROCESS (reg_port_type, instr_start, instr_initial_delay, instr_start_addrs, instr_step_val,instr_step_val_sign, instr_no_of_addrs, instr_middle_delay, instr_no_of_rpts, instr_rpt_step_value,instr_mode, instr_rpt_delay, instr_fft_stage, instr_end_fft_stage)
  BEGIN
    IF reg_port_type = "01" THEN
      wr_instr_start_1   <= instr_start;
      wr_initial_delay_1 <= instr_initial_delay;
      wr_start_addr_1    <= instr_start_addrs;
      wr_addr_offset_1   <= instr_step_val;
      wr_addr_offset_sign_1   <= instr_step_val_sign;
      wr_addr_counter_1  <= instr_no_of_addrs;
      wr_instr_middle_delay_1 <= instr_middle_delay;
      wr_instr_no_of_rpts_1 <= instr_no_of_rpts;       
      wr_instr_rpt_step_value_1 <= instr_rpt_step_value;  
      wr_instr_rpt_delay_1 <= instr_rpt_delay;
      wr_instr_mode_1      <= instr_mode ;
      wr_instr_fft_stage_1  <= instr_fft_stage;
      wr_instr_end_fft_stage_1  <= instr_end_fft_stage; 
    ELSE
      wr_instr_start_1   <= '0';
      wr_initial_delay_1 <= (OTHERS => '0');
      wr_start_addr_1    <= (OTHERS => '0');
      wr_addr_offset_1   <= (OTHERS => '0');
      wr_addr_offset_sign_1   <= (OTHERS => '0');
      wr_addr_counter_1  <= (OTHERS => '0');
      wr_instr_middle_delay_1 <= (OTHERS => '0');
      wr_instr_no_of_rpts_1 <= (OTHERS => '0');       
      wr_instr_rpt_step_value_1 <= (OTHERS => '0');
      wr_instr_rpt_delay_1 <= (OTHERS => '0'); 
      wr_instr_mode_1      <= (OTHERS => '0');
      wr_instr_fft_stage_1  <=(OTHERS => '0');
      wr_instr_end_fft_stage_1  <=(OTHERS => '0');
    END IF;
  END PROCESS AGU_Wr_1;


  AGU_Rd_0 : PROCESS (reg_port_type, instr_start, instr_initial_delay, instr_start_addrs, instr_step_val,instr_step_val_sign, instr_no_of_addrs, instr_middle_delay, instr_no_of_rpts, instr_rpt_step_value,instr_mode, instr_rpt_delay, instr_fft_stage, instr_end_fft_stage)
  BEGIN
    IF reg_port_type = "10" THEN
      rd_instr_start_0          <= instr_start;
      rd_initial_delay_0        <= instr_initial_delay;
      rd_start_addr_0           <= instr_start_addrs;
      rd_addr_offset_0          <= instr_step_val;
      rd_addr_offset_sign_0     <= instr_step_val_sign;
      rd_addr_counter_0         <= instr_no_of_addrs;
      rd_instr_middle_delay_0   <= instr_middle_delay;
      rd_instr_no_of_rpts_0     <= instr_no_of_rpts;
      rd_instr_rpt_step_value_0 <= instr_rpt_step_value;
      rd_instr_rpt_delay_0      <= instr_rpt_delay;
      rd_instr_mode_0           <= instr_mode;
      rd_instr_fft_stage_0      <= instr_fft_stage;
      rd_instr_end_fft_stage_0  <= instr_end_fft_stage;  
    ELSE
      rd_instr_start_0          <= '0';
      rd_initial_delay_0        <= (OTHERS => '0');
      rd_start_addr_0           <= (OTHERS => '0');
      rd_addr_offset_0          <= (OTHERS => '0');
      rd_addr_offset_sign_0     <= (OTHERS => '0');
      rd_addr_counter_0         <= (OTHERS => '0');
      rd_instr_middle_delay_0   <= (OTHERS => '0');
      rd_instr_no_of_rpts_0     <= (OTHERS => '0');
      rd_instr_rpt_step_value_0 <= (OTHERS => '0');
      rd_instr_rpt_delay_0      <= (OTHERS => '0');
      rd_instr_mode_0           <= (OTHERS => '0');
      rd_instr_fft_stage_0      <= (OTHERS => '0');
      rd_instr_end_fft_stage_0  <= (OTHERS => '0');
    END IF;
  END PROCESS AGU_Rd_0;

  AGU_Rd_1 : PROCESS (reg_port_type, instr_start, instr_initial_delay, instr_start_addrs, instr_step_val,instr_step_val_sign, instr_no_of_addrs, instr_middle_delay, instr_no_of_rpts, instr_rpt_step_value,instr_mode, instr_rpt_delay, instr_fft_stage, instr_end_fft_stage)
  BEGIN
    IF reg_port_type = "11" THEN
      rd_instr_start_1   <= instr_start;
      rd_initial_delay_1 <= instr_initial_delay;
      rd_start_addr_1    <= instr_start_addrs;
      rd_addr_offset_1   <= instr_step_val;
      rd_addr_offset_sign_1   <= instr_step_val_sign;
      rd_addr_counter_1  <= instr_no_of_addrs;
      rd_instr_middle_delay_1 <= instr_middle_delay;
      rd_instr_no_of_rpts_1        <= instr_no_of_rpts;       
      rd_instr_rpt_step_value_1 <= instr_rpt_step_value;  
      rd_instr_rpt_delay_1 <= instr_rpt_delay;
      rd_instr_mode_1     <= instr_mode ;
      rd_instr_fft_stage_1  <= instr_fft_stage;
      rd_instr_end_fft_stage_1  <= instr_end_fft_stage; 
    ELSE
      rd_instr_start_1   <= '0';
      rd_initial_delay_1 <= (OTHERS => '0');
      rd_start_addr_1    <= (OTHERS => '0');
      rd_addr_offset_1   <= (OTHERS => '0');
      rd_addr_offset_sign_1   <=  (OTHERS => '0');
      rd_addr_counter_1  <= (OTHERS => '0');
      rd_instr_middle_delay_1 <= (OTHERS => '0');
      rd_instr_no_of_rpts_1        <= (OTHERS => '0'); 
      rd_instr_rpt_step_value_1 <= (OTHERS => '0'); 
      rd_instr_rpt_delay_1 <= (OTHERS => '0');
      rd_instr_mode_1      <= (OTHERS => '0');
      rd_instr_fft_stage_1  <=(OTHERS => '0');
      rd_instr_end_fft_stage_1  <=(OTHERS => '0');
    END IF;
  END PROCESS AGU_Rd_1;

END ARCHITECTURE rtl;