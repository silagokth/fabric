-------------------------------------------------------
--! @file
--! @brief Sequencer function package
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
-- Title      : seq_functions_package
-- Project    : MTRF Fabric
-------------------------------------------------------------------------------
-- File       : seq_functions_package.vhd
-- Author     : sadiq  <sadiq@kth.se>
-- Company    : KTH
-- Created    : 2013-07-19
-- Last update: 2013-10-06
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		    Description
-- 2013-07-19  1.0      sadiq			      Created
-- 2014-02-25  2.0      Nasim Farahini  Modified
-- 2019-03-11  2.1      Dimitrios       Updated
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

--! IEEE Library
library ieee;
--! Use standard library
use ieee.std_logic_1164.all;
--! Use numeric library for signed and unsigned arithmetics
use ieee.numeric_std.all;
--! Use the top constant package that includes all the constants and type definitions
use work.top_consts_types_package.all;
--! Use the tb instruction package for the instruction fuctions and types
use work.tb_instructions.all;
--! Use the noc package from the DiMArch that includes all the type definitions and constants
use work.noc_types_n_constants.all;

--! @brief This is the sequencer function package
--! @detail This package includes functions used to upack the instructions of the sequencer
PACKAGE seq_functions_package IS
  FUNCTION unpack_refi1_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Refi1_instr_ty;
  FUNCTION unpack_refi2_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Refi2_instr_ty;
  FUNCTION unpack_refi3_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Refi3_instr_ty;
  FUNCTION unpack_dpu_record(arg    : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Dpu_instr_ty;
  FUNCTION unpack_branch_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Branch_instr_ty;
  FUNCTION unpack_jump_record (arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Jump_instr_ty;
  FUNCTION unpack_delay_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Delay_instr_ty;
  FUNCTION unpack_raccu_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Raccu_instr_ty;  
  FUNCTION unpack_for_header_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN For_header_instr_ty;
  FUNCTION unpack_for_tail_record(arg  : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN For_tail_instr_ty;
 
  Function unpack_sram_tb_instruction(arg     :std_logic_vector(size_of_sram_instruction_regs-1 DOWNTO 0))RETURN sram_instr_type;
  Function unpack_route_tb_instruction(arg     :std_logic_vector(INSTR_WIDTH-1 DOWNTO 0))RETURN NOC_BUS_TYPE;
  	
  Function pack_sram_noc_instruction(arg     :sram_instr_type)RETURN NOC_BUS_TYPE;
  
END;

PACKAGE BODY seq_functions_package IS

  FUNCTION unpack_refi1_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Refi1_instr_ty IS
    VARIABLE result : Refi1_instr_ty;
  BEGIN
    result.instr_code    := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.reg_file_port := arg(NR_OF_REG_FILE_PORTS_RANGE_BASE DOWNTO NR_OF_REG_FILE_PORTS_RANGE_END);
    result.subseq_instrs := arg(NR_OF_INSTRS_RANGE_BASE DOWNTO NR_OF_INSTRS_RANGE_END);
    result.start_addrs_sd := arg(NR_OF_INSTRS_RANGE_END-1);
    result.start_addrs   := arg(STARTING_ADDRS_RANGE_BASE DOWNTO STARTING_ADDRS_RANGE_END);
    result.no_of_addrs_sd   := arg(STARTING_ADDRS_RANGE_END-1);
    result.no_of_addrs   := arg(NR_OF_ADDRS_RANGE_BASE DOWNTO NR_OF_ADDRS_RANGE_END);
    result.initial_delay_sd := arg(NR_OF_ADDRS_RANGE_END-1);
    result.initial_delay := arg(INIT_DELAY_RANGE_BASE DOWNTO INIT_DELAY_RANGE_END);
    RETURN result;
  END;

  FUNCTION unpack_refi2_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Refi2_instr_ty IS
    VARIABLE result : Refi2_instr_ty;
  BEGIN
    result.instr_code        := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.step_val_sd       := arg(INSTR_CODE_RANGE_END-1);
    result.step_val          := arg(STEP_VALUE_RANGE_BASE DOWNTO STEP_VALUE_RANGE_END);
    result.step_val_sign     := arg(STEP_VALUE_SIGN_RANGE_BASE DOWNTO STEP_VALUE_SIGN_RANGE_END);
    result.refi_middle_delay_sd := arg(STEP_VALUE_SIGN_RANGE_END-1);        
    result.refi_middle_delay := arg(REG_FILE_MIDDLE_DELAY_RANGE_BASE DOWNTO REG_FILE_MIDDLE_DELAY_RANGE_END);
    result.no_of_reps_sd        := arg(REG_FILE_MIDDLE_DELAY_RANGE_END-1);
    result.no_of_reps        := arg(NUM_OF_REPT_RANGE_BASE DOWNTO NUM_OF_REPT_RANGE_END);
    result.rpt_step_value    := arg(REP_STEP_VALUE_RANGE_BASE DOWNTO REP_STEP_VALUE_RANGE_END);
    RETURN result;
  END;

  FUNCTION unpack_refi3_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Refi3_instr_ty IS
    VARIABLE result : Refi3_instr_ty;
  BEGIN
    result.instr_code            := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.rpt_delay_sd            := arg(INSTR_CODE_RANGE_END-1);
    result.rpt_delay             := arg(REPT_DELAY_RANGE_BASE DOWNTO REPT_DELAY_RANGE_END);
    result.mode                  := arg(MODE_SEL_RANGE_BASE DOWNTO MODE_SEL_RANGE_END);
    result.outp_cntrl            := arg(OUTPUT_CONTROL_RANGE_BASE DOWNTO OUTPUT_CONTROL_RANGE_END);
    result.fft_stage             := arg(FFT_STAGE_SEL_RANGE_BASE DOWNTO FFT_STAGE_SEL_RANGE_END);
    result.refi_middle_delay_ext := arg(REG_FILE_MIDDLE_DELAY_EXT_RANGE_BASE DOWNTO REG_FILE_MIDDLE_DELAY_EXT_RANGE_END);
    result.no_of_rpt_ext         := arg(NUM_OF_REPT_EXT_RANGE_BASE DOWNTO NUM_OF_REPT_EXT_RANGE_END);
    result.rpt_step_value_ext    := arg(REP_STEP_VALUE_EXT_RANGE_BASE DOWNTO REP_STEP_VALUE_EXT_RANGE_END);
    result.end_fft_stage           := arg(FFT_END_STAGE_RANGE_BASE DOWNTO FFT_END_STAGE_RANGE_END);
    result.dimarch_mode  := arg(1);
    result.refi3_unused           := arg(REFI3_UNUSED_RANGE_BASE DOWNTO REFI3_UNUSED_RANGE_END);
    RETURN result;
  END;

  FUNCTION unpack_dpu_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Dpu_instr_ty IS
    VARIABLE result : Dpu_instr_ty;
  BEGIN
    result.instr_code     := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.dpu_mode       := arg(DPU_MODE_SEL_RANGE_BASE DOWNTO DPU_MODE_SEL_RANGE_END);
    result.dpu_saturation := arg(DPU_SATURAT_RANGE_BASE DOWNTO DPU_SATURAT_RANGE_END);
    result.dpu_out_a      := arg(DPU_OUTP_A_RANGE_BASE DOWNTO DPU_OUTP_A_RANGE_END);
    result.dpu_out_b      := arg(DPU_OUTP_B_RANGE_BASE DOWNTO DPU_OUTP_B_RANGE_END);
    result.dpu_acc_clear_rst  := arg(DPU_OUTP_B_RANGE_END-1);   
    result.dpu_acc_clear_sd   := arg(DPU_OUTP_B_RANGE_END-2);   
    result.dpu_acc_clear      := arg(DPU_ACC_CLEAR_RANGE_BASE DOWNTO DPU_ACC_CLEAR_RANGE_END);
    result.dpu_process_inout  := arg(DPU_PROCESS_INOUT_RANGE_BASE DOWNTO DPU_PROCESS_INOUT_RANGE_END);
    RETURN result;
  END;


--  FUNCTION unpack_branch_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Branch_instr_ty IS
--    VARIABLE result : Branch_instr_ty;
--  BEGIN
--    result.instr_code     := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
--    result.brnch_status   := arg(BR_STATUS_RANGE_BASE DOWNTO BR_STATUS_RANGE_END);
--    result.brnch_iftrue   := arg(BR_IFTRUE_RANGE_BASE DOWNTO BR_IFTRUE_RANGE_END);
--    result.brnch_mask     := arg(BR_MASK_RANGE_BASE DOWNTO BR_MASK_RANGE_END);
--    result.brnch_iffalse  := arg(BR_IFFALSE_RANGE_BASE DOWNTO BR_IFFALSE_RANGE_END);
--    result.brnch_selector := arg(BR_SELECTOR_RANGE_BASE DOWNTO BR_SELECTOR_RANGE_END);
--    result.brnch_unused := arg(BR_UNUSED_RANGE_BASE DOWNTO BR_UNUSED_RANGE_END);
--    RETURN result;
--  END;

  FUNCTION unpack_branch_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Branch_instr_ty IS
    VARIABLE result : Branch_instr_ty;
  BEGIN
    result.instr_code       := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.brnch_mode       := arg(BR_MODE_RANGE_BASE DOWNTO BR_MODE_RANGE_END);
    result.brnch_false_addr := arg(BR_FALSE_ADDRS_RANGE_BASE DOWNTO BR_FALSE_ADDRS_RANGE_END);
    result.brnch_unused     := arg(BR_UNUSED_RANGE_BASE DOWNTO BR_UNUSED_RANGE_END);
    RETURN result;
  END;

  FUNCTION unpack_jump_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Jump_instr_ty IS
    VARIABLE result : Jump_instr_ty;
  BEGIN
    result.instr_code  := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.true_addrs  := arg(TRUE_ADDRS_RANGE_BASE DOWNTO TRUE_ADDRS_RANGE_END);
    result.jump_unused := arg(JUMP_UNUSED_RANGE_BASE DOWNTO JUMP_UNUSED_RANGE_END);
    RETURN result;
  END;

  FUNCTION unpack_delay_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Delay_instr_ty IS
    VARIABLE result : Delay_instr_ty;
  BEGIN
    result.instr_code := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.del_cycles_sd := arg(INSTR_CODE_RANGE_END-1);    
    result.del_cycles := arg(DLY_CYCLES_RANGE_BASE DOWNTO DLY_CYCLES_RANGE_END);
    result.del_unused := arg(DLY_UNUSED_BITS_RANGE_BASE DOWNTO DLY_UNUSED_BITS_RANGE_END);
    RETURN result;
  END;
  
  FUNCTION unpack_raccu_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN Raccu_instr_ty IS
    VARIABLE result : Raccu_instr_ty;
  BEGIN
    result.instr_code     := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.raccu_mode       := arg(RACCU_MODE_SEL_RANGE_BASE DOWNTO RACCU_MODE_SEL_RANGE_END);
    result.raccu_op1_sd 		:= arg(RACCU_MODE_SEL_RANGE_END-1);
    result.raccu_op1 		:= arg(RACCU_OPERAND1_RANGE_BASE DOWNTO RACCU_OPERAND1_RANGE_END);
    result.raccu_op2_sd 		:= arg(RACCU_OPERAND1_RANGE_END-1);
    result.raccu_op2      := arg(RACCU_OPERAND2_RANGE_BASE DOWNTO RACCU_OPERAND2_RANGE_END);
    result.raccu_result_addrs  := arg(RACCU_RESULT_ADDR_RANGE_BASE DOWNTO RACCU_RESULT_ADDR_RANGE_END);
    RETURN result;
  END;
 
  FUNCTION unpack_for_header_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN For_header_instr_ty IS
    VARIABLE result : For_header_instr_ty;
  BEGIN
    result.instr_code     := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.index_raccu_addr       := arg(FOR_INDEX_ADDR_RANGE_BASE DOWNTO FOR_INDEX_ADDR_RANGE_END);
    result.index_start := arg(FOR_INDEX_START_RANGE_BASE DOWNTO FOR_INDEX_START_RANGE_END);
    result.iter_no_sd      := arg(FOR_INDEX_START_RANGE_END-1);
    result.iter_no      := arg(FOR_ITER_NO_RANGE_BASE DOWNTO FOR_ITER_NO_RANGE_END);
    result.header_unused      := arg(FOR_HEADER_UNUSED_RANGE_BASE DOWNTO FOR_HEADER_UNUSED_RANGE_END);
    RETURN result;
  END;
 
  FUNCTION unpack_for_tail_record(arg : std_logic_vector(INSTR_WIDTH-1 DOWNTO 0)) RETURN For_tail_instr_ty IS
    VARIABLE result : For_tail_instr_ty;
  BEGIN
    result.instr_code     := arg(INSTR_CODE_RANGE_BASE DOWNTO INSTR_CODE_RANGE_END);
    result.index_step       := arg(FOR_INDEX_STEP_RANGE_BASE DOWNTO FOR_INDEX_STEP_RANGE_END);
    result.pc_togo       := arg(FOR_PC_TOGO_RANGE_BASE DOWNTO FOR_PC_TOGO_RANGE_END);
    result.index_raccu_addr       := arg(FOR_TAIL_INDEX_ADDR_RANGE_BASE DOWNTO FOR_TAIL_INDEX_ADDR_RANGE_END);
    result.tail_unused      := arg(FOR_TAIL_UNUSED_RANGE_BASE DOWNTO FOR_TAIL_UNUSED_RANGE_END);
    RETURN result;
END;



Function unpack_route_tb_instruction(arg     :std_logic_vector(INSTR_WIDTH-1 downto 0))RETURN NOC_BUS_TYPE IS 
	variable result :NOC_BUS_TYPE; 
	
	
BEGIN
	    result.INSTRUCTION(SR_e downto SR_s) := arg(INSTR_WIDTH - source_row_address_s downto INSTR_WIDTH - source_row_address_e);  
	    result.INSTRUCTION(SC_e downto SC_s) := arg(INSTR_WIDTH - source_col_address_s downto INSTR_WIDTH - source_col_address_e);  
	    result.INSTRUCTION(DR_e downto DR_s) := arg(INSTR_WIDTH - dest_row_address_s   downto INSTR_WIDTH - dest_row_address_e  );      
	    result.INSTRUCTION(DC_e downto DC_s) := arg(INSTR_WIDTH - dest_col_address_s   downto INSTR_WIDTH - dest_col_address_e  );      
	    result.INSTRUCTION(IR_e downto IR_s) := arg(INSTR_WIDTH - intr_row_address_s   downto INSTR_WIDTH - intr_row_address_e  );
	    result.INSTRUCTION(IC_e downto IC_s) := arg(INSTR_WIDTH - intr_col_address_s   downto INSTR_WIDTH - intr_col_address_e  );
	    result.INSTRUCTION(ON_l)             := arg(INSTR_WIDTH - Origin_l);
	    result.INSTRUCTION(RF_e downto RF_s) := arg(INSTR_WIDTH - RFNODE_s downto INSTR_WIDTH - RFNODE_E);
	    result.INSTRUCTION(UNION_FLAG_l)     := arg(INSTR_WIDTH-union_l);
	    result.INSTRUCTION(UNION_PORT_e downto UNION_PORT_s) :=arg(INSTR_WIDTH-UNION_PORT_NR_s downto INSTR_WIDTH-UNION_PORT_NR_e);
	    
	
		if arg(INSTR_WIDTH-union_l) = '1' then 
			result.instr_code := route_instruction; --READ
		else
			result.instr_code := both_instruction; --READ
		end if;
		result.bus_enable := '1';
		
	RETURN result;
END;
                                                           
Function unpack_sram_tb_instruction(arg     :std_logic_vector(size_of_sram_instruction_regs-1 DOWNTO 0))RETURN sram_instr_type IS 
	variable result : sram_instr_type;
	-- since their is no enable signal in this pack sr_en_s value is added to balance the shift in values 
	constant size_of_sram_tb_instruction : integer := size_of_sram_instruction_regs -seq_instr + sr_en_s;
	
BEGIN
		result.instr_code               :=arg(size_of_sram_instruction_regs -1                               downto size_of_sram_instruction_regs-seq_instr  ); 

		result.agu_mode                 :=arg(size_of_sram_tb_instruction- sr_mode_s            downto size_of_sram_tb_instruction-sr_mode_e           );
		result.Initial_Address          :=arg(size_of_sram_tb_instruction- sr_initial_address_s downto size_of_sram_tb_instruction-sr_initial_address_e);
		result.Initial_Delay            :=arg(size_of_sram_tb_instruction- sr_initial_delay_s   downto size_of_sram_tb_instruction-sr_initial_delay_e  );
		result.Loop1_iteration          :=arg(size_of_sram_tb_instruction- sr_loop1_iteration_s downto size_of_sram_tb_instruction-sr_loop1_iteration_e);
		result.Loop1_Increment          :=arg(size_of_sram_tb_instruction- sr_loop1_increment_s downto size_of_sram_tb_instruction-sr_loop1_increment_e);
		result.Loop1_Delay              :=arg(size_of_sram_tb_instruction- sr_loop1_delay_s     downto size_of_sram_tb_instruction-sr_loop1_delay_e    );
		result.Loop2_iteration          :=arg(size_of_sram_tb_instruction- sr_loop2_iteration_s downto size_of_sram_tb_instruction-sr_loop2_iteration_e);
		result.Loop2_Increment          :=arg(size_of_sram_tb_instruction- sr_loop2_increment_s downto size_of_sram_tb_instruction-sr_loop2_increment_e);
		result.Loop2_Delay              :=arg(size_of_sram_tb_instruction- sr_loop2_delay_s     downto size_of_sram_tb_instruction-sr_loop2_delay_e    );
		result.sram_Initial_address_sd  :=arg(size_of_sram_tb_instruction- sr_Initial_address_sd);
		result.sram_Loop1_iteration_sd  :=arg(size_of_sram_tb_instruction- sr_Loop1_iteration_sd);
		result.sram_Loop2_iteration_sd  :=arg(size_of_sram_tb_instruction- sr_Loop2_iteration_sd);
		result.sram_initial_delay_sd    :=arg(size_of_sram_tb_instruction- sr_initial_delay_sd  );
		result.sram_Loop1_delay_sd      :=arg(size_of_sram_tb_instruction- sr_Loop1_delay_sd    ); 
		result.sram_Loop2_delay_sd      :=arg(size_of_sram_tb_instruction- sr_Loop2_delay_sd    ); 
		result.sram_Loop1_increment_sd  :=arg(size_of_sram_tb_instruction- sr_Loop1_increment_sd); 
		result.sram_Loop2_increment_sd  :=arg(size_of_sram_tb_instruction- sr_Loop2_increment_sd); 
		

	RETURN result;
END;
Function pack_sram_noc_instruction(arg     :sram_instr_type)RETURN NOC_BUS_TYPE IS 
	variable result :NOC_BUS_TYPE; 
BEGIN
		--instruction code is  no longer needed so it is removed 
--		result.INSTRUCTION(NoC_Bus_instr_width -1                                       downto NoC_Bus_instr_width-seq_instr  )                           :=arg.instr_code          ; 
		result.INSTRUCTION(NoC_Bus_instr_width -sr_en_s                downto NoC_Bus_instr_width-sr_en_e             ):="1";
		result.INSTRUCTION(NoC_Bus_instr_width -sr_mode_s              downto NoC_Bus_instr_width-sr_mode_e           ):=arg.agu_mode            ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_initial_address_s   downto NoC_Bus_instr_width-sr_initial_address_e):=arg.Initial_Address       ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_initial_delay_s     downto NoC_Bus_instr_width-sr_initial_delay_e  ):=arg.Initial_Delay         ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_loop1_iteration_s   downto NoC_Bus_instr_width-sr_loop1_iteration_e):=arg.Loop1_iteration           ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_loop1_increment_s   downto NoC_Bus_instr_width-sr_loop1_increment_e):=arg.Loop1_Increment     ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_loop1_delay_s       downto NoC_Bus_instr_width-sr_loop1_delay_e    ):=arg.Loop1_Delay       ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_loop2_iteration_s   downto NoC_Bus_instr_width-sr_loop2_iteration_e):=arg.Loop2_iteration      ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_loop2_increment_s   downto NoC_Bus_instr_width-sr_loop2_increment_e):=arg.Loop2_Increment       ;
		result.INSTRUCTION(NoC_Bus_instr_width -sr_loop2_delay_s       downto NoC_Bus_instr_width-sr_loop2_delay_e    ):=arg.Loop2_Delay    ;
		result.INSTRUCTION(NoC_Bus_instr_width-sr_rw):=arg.rw;
		



--		result.Range_counter       :=arg(size_of_sram_instruction_regs --1  downto size_of_sram_instruction_regs-);

		result.instr_code := AGU_instruction; --READ
		result.bus_enable := '1';
		
	RETURN result;
END;






END;

	
	