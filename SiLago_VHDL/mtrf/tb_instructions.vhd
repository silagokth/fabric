-------------------------------------------------------
--! @file tb_instructions.vhd
--! @brief 
--! @details 
--! @author 
--! @version 1.0
--! @date 
--! @bug NONE
--! @todo NONE
--! @copyright  GNU Public License [GPL-3.0].
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
-- Title      : 
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : tb_instructions.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 
-- Last update: 
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU          #
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
LIBRARY IEEE;
USE ieee.std_logic_1164.ALL;
USE ieee.NUMERIC_STD.ALL;
USE work.util_package.ALL;
USE std.textio.ALL;

USE work.top_consts_types_package.ALL;
USE work.top_consts_types_package.COLUMNS;
USE work.top_consts_types_package.INSTR_WIDTH;
USE work.top_consts_types_package.SRAM_ADDRESS_WIDTH;

PACKAGE tb_instructions IS
    CONSTANT instrcode : INTEGER := 4;
    CONSTANT dav       : INTEGER := 1;
    CONSTANT rowindex  : INTEGER := 2;
    CONSTANT vindex    : INTEGER := 4;
    CONSTANT hbindex   : INTEGER := 3;
    CONSTANT outindex  : INTEGER := 1;
    CONSTANT SBunused  : INTEGER := 21;

    CONSTANT rfport9           : INTEGER := 2;
    CONSTANT rfvalid9          : INTEGER := 1;
    CONSTANT mode_width        : INTEGER := 1;
    CONSTANT startaddress9     : INTEGER := 6;
    CONSTANT numberOfAdr9      : INTEGER := 6;
    CONSTANT incr9             : INTEGER := 1;
    CONSTANT incrvalue9        : INTEGER := 5;
    CONSTANT initialdelay9     : INTEGER := 6;
    CONSTANT outputcontrol9    : INTEGER := 2;
    CONSTANT complete9         : INTEGER := 1;
    CONSTANT abnormal          : INTEGER := 1;
    CONSTANT rptdelay10        : INTEGER := 6;
    CONSTANT numofrept10       : INTEGER := 6;
    CONSTANT rptincr10         : INTEGER := 1;
    CONSTANT rptincrvalue10    : INTEGER := 5;
    CONSTANT rfmiddledelay10   : INTEGER := 7;
    CONSTANT addsub15          : INTEGER := 1;
    CONSTANT SkipPoint         : INTEGER := 1;
    CONSTANT FFTStage          : INTEGER := 3;
    CONSTANT initalpoint       : INTEGER := 2;
    CONSTANT NUM_OF_INSTRS_SEQ : INTEGER := 4;
    ------------------------------------------------------------------
    -- Constants for route instruction
    -------------------------------------------------------------------
    CONSTANT COLUMN_WIDTH : INTEGER := INTEGER(log2(NATURAL(COLUMNS)));

    CONSTANT tb_union_flag_width : INTEGER := 1;
    CONSTANT SourceRow           : INTEGER := DiMArch_Row_Width;
    CONSTANT SourceCol           : INTEGER := COLUMN_WIDTH;

    CONSTANT DestinRow    : INTEGER := DiMArch_Row_Width;
    CONSTANT DestinCol    : INTEGER := COLUMN_WIDTH;
    CONSTANT intermediate : INTEGER := 1;
    CONSTANT intermRow    : INTEGER := DiMArch_Row_Width;
    CONSTANT intermCol    : INTEGER := COLUMN_WIDTH;
    --	CONSTANT priority             : INTEGER := 1;
    CONSTANT originnode             : INTEGER := 1;
    CONSTANT RFNODE                 : INTEGER := 2;
    CONSTANT UNION_PORT_NR          : INTEGER := 2;
    CONSTANT route_instruction_size : NATURAL :=
    instrcode +
    tb_union_flag_width +
    SourceRow +
    SourceCol +
    DestinRow +
    DestinCol +
    intermediate +
    intermRow +
    intermCol +
    --		priority      +
    originnode +
    UNION_PORT_NR +
    RFNODE;
    CONSTANT routeExtras : INTEGER := INSTR_WIDTH - route_instruction_size;

    --------------------------------------------------------------
    --SRAM AGU instruction constants
    --------------------------------------------------------------
    --	CONSTANT memloadExtras        : INTEGER := 8;
    --
    --	CONSTANT memmode              : INTEGER := mode_width;
    --	CONSTANT memstartaddress      : INTEGER := start_addrs_WIDTH;
    --	CONSTANT memendaddress        : INTEGER := end_addrs_WIDTH;
    --	CONSTANT memincr              : INTEGER := incr_decr_WIDTH;
    --	CONSTANT memincrvalue         : INTEGER := incr_decr_value_WIDTH;
    --	CONSTANT meminitialdelay      : INTEGER := INITIAL_DELAY_WIDTH;
    --	CONSTANT memoutputcontrol     : INTEGER := outputcontrol_WIDTH;       -- 1 bit reduced
    --	CONSTANT minstrcomplete       : INTEGER := instruction_complete_WIDTH;-- not needed
    --	CONSTANT minfiniteloop        : INTEGER := infinite_loop_WIDTH;
    --	CONSTANT mrptdelay            : INTEGER := repetition_delay_WIDTH;
    --	CONSTANT mnumofrept           : INTEGER := no_of_repetitions_WIDTH;
    --	CONSTANT mrptincr             : INTEGER := repetition_incr_decr_WIDTH;
    --	CONSTANT mrptincrvalue        : INTEGER := repetition_incr_decr_value_WIDTH;
    --	CONSTANT mmiddledelay         : INTEGER := middle_delay_WIDTH;
    --	CONSTANT inout_sel            : INTEGER := SRAM_inout_select_WIDTH;
    --	CONSTANT SRAM_range_counter   : INTEGER := range_counter_WIDTH;
    --	constant halt_delay           : integer := hault_delay_WIDTH;
    --	constant halt_counter         : integer := hault_counter_WIDTH;
    --	CONSTANT mioothers            : INTEGER := 28;
    --	CONSTANT delayvalue           : INTEGER := 15;
    -- raccu flags
    CONSTANT memstartaddress_sd : INTEGER := 1;
    CONSTANT memendaddress_sd   : INTEGER := 1;
    CONSTANT meminitialdelay_sd : INTEGER := 1;
    CONSTANT mrptdelay_sd       : INTEGER := 1;
    CONSTANT mrptincrvalue_sd   : INTEGER := 1;
    CONSTANT mmiddledelay_sd    : INTEGER := 1;
    CONSTANT mnrofrpt_sd        : INTEGER := 1;
    -- width calculation 
    --	constant sram_raccu_flags : integer := 
    --	memstartaddress_sd +
    --	memendaddress_sd   +
    --	meminitialdelay_sd +
    --	mrptdelay_sd       +
    --	mrptincrvalue_sd   +
    --	mmiddledelay_sd    +
    --	mnrofrpt_sd;
    CONSTANT sram_instruction_size : INTEGER := sram_tb_instr_width + instrcode;

    CONSTANT memwothers                       : INTEGER := 17;
    CONSTANT number_of_sram_instruction_parts : INTEGER := ((sram_instruction_size)/INSTR_WIDTH) + 1;
    CONSTANT size_of_sram_instruction_regs    : NATURAL := number_of_sram_instruction_parts * INSTR_WIDTH;
    -- it will cause one bit extra if values are exactly divisible
    --	constant remainder_of_sram_over_instruction: unsigned := mod(natural(sram_instruction_size),unsigned(INSTR_WIDTH,5));
    ------------------------------------------------------------------------

    CONSTANT seq_instr : INTEGER := 4;
    CONSTANT start     : INTEGER := 1;
    --	CONSTANT mode                 : INTEGER := 1;
    CONSTANT rf_start_addr        : INTEGER := 2;
    CONSTANT rf_end_addr          : INTEGER := 2;
    CONSTANT incr                 : INTEGER := 1;
    CONSTANT incr_val             : INTEGER := 2;
    CONSTANT initialdelay         : INTEGER := 6;
    CONSTANT OUTPUT_CONTROL       : INTEGER := 2;
    CONSTANT instruction_complete : INTEGER := 1;
    CONSTANT infinite_loop        : INTEGER := 1;
    --	CONSTANT halt_delay           : INTEGER := 9;
    --	CONSTANT halt_counter         : INTEGER := 4;
    CONSTANT rep_delay        : INTEGER   := 9;
    CONSTANT no_of_rep        : INTEGER   := 9;
    CONSTANT inc_dec          : INTEGER   := 1;
    CONSTANT inc_dec_val      : INTEGER   := 2;
    CONSTANT middledelay      : INTEGER   := 9;
    CONSTANT addrs_range      : INTEGER   := 2;
    CONSTANT halt             : INTEGER   := 2;
    CONSTANT rfdothers        : INTEGER   := 2;
    CONSTANT startstage       : INTEGER   := 3;
    CONSTANT endstage         : INTEGER   := 3;
    CONSTANT null_field       : INTEGER   := 2;
    CONSTANT outputcontrol    : INTEGER   := 2;
    CONSTANT start_stage      : INTEGER   := 1;
    CONSTANT end_stage        : INTEGER   := 1;
    CONSTANT static_1         : INTEGER   := 1;
    CONSTANT from_source      : std_logic := '0';
    CONSTANT from_destination : std_logic := '1';

    --	CONSTANT NUM_OF_INSTRS_SEQ    : INTEGER := 60;

    -----------------------------------------------------
    -- connect constants
    -----------------------------------------------------
    CONSTANT REFI        : INTEGER            := 0;
    CONSTANT LATA        : INTEGER            := 1;
    CONSTANT SIGNED_TWO  : SIGNED(2 DOWNTO 0) := "010";
    CONSTANT SIGNED_ZERO : SIGNED(2 DOWNTO 0) := "000";
    CONSTANT SIGNED_ONE  : SIGNED(2 DOWNTO 0) := "001";
    -----------------------------------------------------
    -- RFILE constants
    -----------------------------------------------------
    CONSTANT rfmode : INTEGER := 1;
    CONSTANT WA     : INTEGER := 0;
    CONSTANT WB     : INTEGER := 1;
    CONSTANT RA     : INTEGER := 2;
    CONSTANT RB     : INTEGER := 3;

    CONSTANT linear_mode : INTEGER   := 0;
    CONSTANT fft_mode    : INTEGER   := 1;
    CONSTANT plus        : std_logic := '1';
    CONSTANT minus       : std_logic := '0';

    ----------------------------------------------------------
    -- DPU constants
    --------------------------------------------------------
    CONSTANT mdpumode12 : INTEGER := 5;
    --	CONSTANT dpuNotUsed12        : INTEGER :=9; 
    CONSTANT mdpusaturation12      : INTEGER := 2;
    CONSTANT outa12                : INTEGER := 2;
    CONSTANT outb12                : INTEGER := 2;
    CONSTANT mdpuacccount12        : INTEGER := 11;
    CONSTANT dpu_instruction_width : INTEGER :=

    mdpumode12 +
    mdpusaturation12 +
    outa12 +
    outb12 +
    mdpuacccount12;
    CONSTANT dpuunused12 : INTEGER := INSTR_WIDTH - dpu_instruction_width;

    TYPE AGU_INST_TYPE IS ARRAY (0 TO number_of_sram_instruction_parts) OF std_logic_vector (INSTR_WIDTH DOWNTO 0);
    --
    --function route        (	
    --	Source_Row,
    --	Source_Col,
    --	Destination_Row,
    --	Destination_Col,
    --	DRRA_SEL: integer;
    --	origIn :std_logic)return std_logic_vector;

    FUNCTION route (
        Source_Row,
        Source_Col,
        Destination_Row,
        Destination_Col,
        DRRA_SEL,
        Union_Flag,
        Union_Port : INTEGER;
        origIn     : std_logic
    ) RETURN std_logic_vector;
    FUNCTION SRAMAGU_read(
        mode,
        Initial_Address,
        Initial_Delay,
        Loop1_Iterations, -- n+1 
        Loop1_Increment,
        Loop1_Delay,
        Loop2_Iterations, -- n+1
        Loop2_Increment,
        Loop2_Delay : INTEGER;
        sram_Initial_address_sd,
        sram_Loop1_iteration_sd,
        sram_Loop2_iteration_sd,
        sram_initial_delay_sd,
        sram_Loop1_delay_sd,
        sram_Loop2_delay_sd,
        sram_Loop1_increment_sd,
        sram_Loop2_increment_sd : std_logic
    ) RETURN AGU_INST_TYPE;
    FUNCTION SRAMAGU_write(
        mode,
        Initial_Address, Initial_Delay,
        Loop1_Iterations, Loop1_Increment, Loop1_Delay,
        Loop2_Iterations, Loop2_Increment, Loop2_Delay : INTEGER;
        sram_Initial_address_sd,
        sram_Loop1_iteration_sd, sram_Loop2_iteration_sd,
        sram_initial_delay_sd, sram_Loop1_delay_sd, sram_Loop2_delay_sd,
        sram_Loop1_increment_sd, sram_Loop2_increment_sd : std_logic
    ) RETURN AGU_INST_TYPE;

    FUNCTION REFI_1(reg_file_port, subseq_instrs, start_addrs_sd, start_addrs, no_of_addrs_sd, no_of_addrs, initial_delay_sd, initial_delay : INTEGER
    ) RETURN std_logic_vector;

    FUNCTION REFI_2(step_val_sd, step_val, step_val_sign, refi_middle_delay_sd, refi_middle_delay, no_of_reps_sd, no_of_reps, rpt_step_value : INTEGER
    ) RETURN std_logic_vector;

    FUNCTION REFI_3(rpt_delay_sd, rpt_delay, mode, outp_cntrl, fft_stage, refi_middle_delay_ext, no_of_rpt_ext, rpt_step_value_ext, fft_end_stage, DiMArch_Mode, use_compr : INTEGER
    ) RETURN std_logic_vector;

    FUNCTION DPU (dpu_mode, dpu_saturation, dpu_out_1, dpu_out_2, dpu_acc_clear_rst, dpu_acc_clear_sd, dpu_acc_clear, process_inout : INTEGER) RETURN std_logic_vector;

    FUNCTION DELAY (del_cycles_sd, del_cycles : INTEGER) RETURN std_logic_vector;

    FUNCTION RACCU (raccu_mode, raccu_op1_sd, raccu_op1, raccu_op2_sd, raccu_op2, raccu_result_addrs : INTEGER) RETURN std_logic_vector;

    FUNCTION LOOP_HEADER (index_raccu_addr, index_start, iter_no_sd, iter_no : INTEGER) RETURN std_logic_vector;

    FUNCTION LOOP_TAIL (index_step, pc_togo, index_raccu_addr : INTEGER) RETURN std_logic_vector;

    FUNCTION SWB (from_block, from_address, from_port, to_block, to_address, to_port : INTEGER) RETURN std_logic_vector;

    FUNCTION BRANCH (brnch_mode, brnch_false_addr : INTEGER) RETURN std_logic_vector;

    FUNCTION JUMP (true_addrs : INTEGER) RETURN std_logic_vector;

END PACKAGE tb_instructions;

PACKAGE BODY tb_instructions IS
    ------------------------------------------------------------------------------------------------------
    --  ROUTE 
    ------------------------------------------------------------------------------------------------------

    FUNCTION route (Source_Row, Source_Col, Destination_Row, Destination_Col, DRRA_SEL, Union_Flag, Union_Port : INTEGER; origIn : std_logic)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector (INSTR_WIDTH DOWNTO 0);

    BEGIN
        --assert false report "ROW WIDTH : " & integer'image(SourceRow);
        --assert false report "Column WIDTH : " & integer'image(SourceCol);
        instruction_bus :=
            "1" &
            STD_LOGIC_VECTOR(TO_UNSIGNED(12, instrcode)) &
            --			   STD_LOGIC_VECTOR(TO_UNSIGNED(1, tb_union_flag_width                  ))& 
            STD_LOGIC_VECTOR(TO_UNSIGNED(Source_Row, SourceRow)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Source_Col, SourceCol)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Destination_Row, DestinRow)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Destination_Col, DestinCol)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(0, intermediate)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(0, intermRow)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(0, intermCol)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Union_Flag, tb_union_flag_width)) &
            origIn &                                          -- 0 if source is origin 1 if destination is origin 
            STD_LOGIC_VECTOR(TO_UNSIGNED(DRRA_SEL, RFNODE)) & -- 2 bit 0 =rf0 1=rf1  2=seq  3=UN SELECT
            STD_LOGIC_VECTOR(TO_UNSIGNED(Union_Port, UNION_PORT_NR)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(0, routeExtras));
        RETURN instruction_bus;
    END FUNCTION route;

    --sram_instruction_size
    ------------------------------------------------------------------------------------------------------
    --  SRAM AGU READ 
    ------------------------------------------------------------------------------------------------------
    FUNCTION SRAMAGU_read (
        mode,
        Initial_Address,
        Initial_Delay,
        Loop1_Iterations,
        Loop1_Increment,
        Loop1_Delay,
        Loop2_Iterations,
        Loop2_Increment,
        Loop2_Delay : INTEGER;
        sram_Initial_address_sd,
        sram_Loop1_iteration_sd,
        sram_Loop2_iteration_sd,
        sram_initial_delay_sd,
        sram_Loop1_delay_sd,
        sram_Loop2_delay_sd,
        sram_Loop1_increment_sd,
        sram_Loop2_increment_sd
        : std_logic)
        RETURN AGU_INST_TYPE IS
        VARIABLE instruction_bus  : AGU_INST_TYPE;
        VARIABLE sram_instruction : std_logic_vector (sram_instruction_size - 1 DOWNTO 0);
        -- variables for paramter checking

        VARIABLE start_index, end_index : INTEGER;

    BEGIN

        sram_instruction :=
            STD_LOGIC_VECTOR(TO_UNSIGNED(13, instrcode)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(mode, sr_mode_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Initial_Address, sr_initial_address_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Initial_Delay, sr_initial_delay_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop1_Iterations, sr_loop1_iteration_width)) &
            STD_LOGIC_VECTOR(TO_SIGNED (Loop1_Increment, sr_loop1_increment_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop1_Delay, sr_loop1_delay_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop2_Iterations, sr_loop2_iteration_width)) &
            STD_LOGIC_VECTOR(TO_SIGNED (Loop2_Increment, sr_loop2_increment_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop2_Delay, sr_loop2_delay_width)) &
            sram_Initial_address_sd &
            sram_Loop1_iteration_sd &
            sram_Loop2_iteration_sd &
            sram_initial_delay_sd &
            sram_Loop1_delay_sd &
            sram_Loop2_delay_sd &
            sram_Loop1_increment_sd &
            sram_Loop2_increment_sd;
        FOR i IN 0 TO number_of_sram_instruction_parts - 1 LOOP
            start_index := sram_instruction_size - INSTR_WIDTH * i - 1;
            end_index   := sram_instruction_size - INSTR_WIDTH * (i + 1);
            --assert false report "Index: " & integer'image(i) & " sram_instruction_size =" & integer'image(sram_instruction_size);
            IF end_index < 0 THEN -- for last few bits 
                --			assert false report "Start index =" & integer'image(start_index);
                instruction_bus(i)                                                   := (OTHERS => '0');
                instruction_bus(i)(INSTR_WIDTH DOWNTO INSTR_WIDTH - start_index - 1) := '1' & sram_instruction(start_index DOWNTO 0);
                --			assert false report "instruction bus index : " & integer'image(INSTR_WIDTH - 1) & " downto " & integer'image(INSTR_WIDTH - start_index - 2);
            ELSE
                --			assert false report "From : " & integer'image(start_index) & " downto " & integer'image(end_index);
                instruction_bus(i) := '1' & sram_instruction(start_index DOWNTO end_index);
            END IF;
        END LOOP;
        RETURN instruction_bus;
    END FUNCTION SRAMAGU_read;

    ------------------------------------------------------------------------------------------------------
    --  SRAM AGU WRITE 
    ------------------------------------------------------------------------------------------------------
    FUNCTION SRAMAGU_write (
        mode,
        Initial_Address,
        Initial_Delay,
        Loop1_Iterations,
        Loop1_Increment,
        Loop1_Delay,
        Loop2_Iterations,
        Loop2_Increment,
        Loop2_Delay : INTEGER;
        sram_Initial_address_sd,
        sram_Loop1_iteration_sd,
        sram_Loop2_iteration_sd,
        sram_initial_delay_sd,
        sram_Loop1_delay_sd,
        sram_Loop2_delay_sd,
        sram_Loop1_increment_sd,
        sram_Loop2_increment_sd
        : std_logic)
        RETURN AGU_INST_TYPE IS
        VARIABLE instruction_bus  : AGU_INST_TYPE;
        VARIABLE sram_instruction : std_logic_vector (sram_instruction_size - 1 DOWNTO 0);
        -- variables for paramter checking

        VARIABLE start_index, end_index : INTEGER;

    BEGIN

        sram_instruction :=
            STD_LOGIC_VECTOR(TO_UNSIGNED(14, instrcode)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(mode, sr_mode_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Initial_Address, sr_initial_address_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Initial_Delay, sr_initial_delay_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop1_Iterations, sr_loop1_iteration_width)) &
            STD_LOGIC_VECTOR(TO_SIGNED (Loop1_Increment, sr_loop1_increment_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop1_Delay, sr_loop1_delay_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop2_Iterations, sr_loop2_iteration_width)) &
            STD_LOGIC_VECTOR(TO_SIGNED (Loop2_Increment, sr_loop2_increment_width)) &
            STD_LOGIC_VECTOR(TO_UNSIGNED(Loop2_Delay, sr_loop2_delay_width)) &
            sram_Initial_address_sd &
            sram_Loop1_iteration_sd &
            sram_Loop2_iteration_sd &
            sram_initial_delay_sd &
            sram_Loop1_delay_sd &
            sram_Loop2_delay_sd &
            sram_Loop1_increment_sd &
            sram_Loop2_increment_sd;
        FOR i IN 0 TO number_of_sram_instruction_parts - 1 LOOP
            start_index := sram_instruction_size - INSTR_WIDTH * i - 1;
            end_index   := sram_instruction_size - INSTR_WIDTH * (i + 1);
            --assert false report "Index: " & integer'image(i) & " sram_instruction_size =" & integer'image(sram_instruction_size);
            IF end_index < 0 THEN -- for last few bits 
                --assert false report "Start index =" & integer'image(start_index);
                instruction_bus(i)                                                   := (OTHERS => '0');
                instruction_bus(i)(INSTR_WIDTH DOWNTO INSTR_WIDTH - start_index - 1) := '1' & sram_instruction(start_index DOWNTO 0);
                --assert false report "instruction bus index : " & integer'image(INSTR_WIDTH - 1) & " downto " & integer'image(INSTR_WIDTH - start_index - 2);
            ELSE
                --assert false report "From : " & integer'image(start_index) & " downto " & integer'image(end_index);
                instruction_bus(i) := '1' & sram_instruction(start_index DOWNTO end_index);
            END IF;
        END LOOP;
        RETURN instruction_bus;
    END FUNCTION SRAMAGU_write;
    -----------------------------------------------------------------------------
    -- DPU
    -----------------------------------------------------------------------------
    FUNCTION DPU (
        dpu_mode, dpu_saturation, dpu_out_1, dpu_out_2, dpu_acc_clear_rst, dpu_acc_clear_sd, dpu_acc_clear, process_inout : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN instruction_bus := "1" & std_logic_vector(TO_UNSIGNED(4, instrcode)) &
        std_logic_vector(TO_UNSIGNED(dpu_mode, DPU_MODE_SEL)) &
        std_logic_vector(TO_UNSIGNED(dpu_saturation, DPU_SATURAT)) &
        std_logic_vector(TO_UNSIGNED(dpu_out_1, DPU_OUTP_A)) &
        std_logic_vector(TO_UNSIGNED(dpu_out_2, DPU_OUTP_B)) &
        std_logic_vector(TO_UNSIGNED(dpu_acc_clear_rst, 1)) &
        std_logic_vector(TO_UNSIGNED(dpu_acc_clear_sd, 1)) &
        std_logic_vector(TO_UNSIGNED(dpu_acc_clear, DPU_ACC_CLEAR_BIT)) &
        std_logic_vector(TO_UNSIGNED(process_inout, DPU_PROCESS_INOUT))
        ;
        RETURN instruction_bus;
    END FUNCTION DPU;
    ------------------------------------------------------------------------------------------------------
    --  REFI 1 AGU for DRRA
    ------------------------------------------------------------------------------------------------------
    FUNCTION REFI_1(reg_file_port, subseq_instrs, start_addrs_sd, start_addrs, no_of_addrs_sd, no_of_addrs, initial_delay_sd, initial_delay : INTEGER
    ) RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(TO_UNSIGNED(1, instrcode)) &
            std_logic_vector(to_unsigned(reg_file_port, NR_OF_REG_FILE_PORTS)) &
            std_logic_vector(to_unsigned(subseq_instrs, NR_OF_INSTRS)) &
            std_logic_vector(tO_unsigned(start_addrs_sd, 1)) &
            std_logic_vector(tO_unsigned(start_addrs, STARTING_ADDRS)) &
            std_logic_vector(to_unsigned(no_of_addrs_sd, 1)) &
            std_logic_vector(to_unsigned(no_of_addrs, NR_OF_ADDRS)) &
            std_logic_vector(to_unsigned(initial_delay_sd, 1)) &
            std_logic_vector(to_unsigned(initial_delay, INIT_DELAY))--&
            --std_logic_vector(to_unsigned(0, 9))
            ;
        RETURN instruction_bus;
    END FUNCTION REFI_1;

    -----------------------------------------------------------------------------
    -- REFI 2 AGU for DRRA
    -----------------------------------------------------------------------------
    FUNCTION REFI_2 (step_val_sd, step_val, step_val_sign, refi_middle_delay_sd, refi_middle_delay, no_of_reps_sd, no_of_reps, rpt_step_value : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(2, instrcode)) &
            std_logic_vector(to_unsigned(step_val_sd, 1)) &
            std_logic_vector(to_unsigned(step_val, STEP_VALUE)) &
            std_logic_vector(to_unsigned(step_val_sign, STEP_VALUE_SIGN)) &
            std_logic_vector(to_unsigned(refi_middle_delay_sd, 1)) &
            std_logic_vector(to_unsigned(refi_middle_delay, REG_FILE_MIDDLE_DELAY)) &
            std_logic_vector(to_unsigned(no_of_reps_sd, 1)) &
            std_logic_vector(to_unsigned(no_of_reps, NUM_OF_REPT)) &
            std_logic_vector(to_unsigned(rpt_step_value, REP_STEP_VALUE))--&
            --std_logic_vector(to_unsigned(0, 9))
            ;
        RETURN instruction_bus;
    END FUNCTION REFI_2;

    -------------------------------------------------------------------------------
    --  REFI 3 AGU for DRRA
    -------------------------------------------------------------------------------
    FUNCTION REFI_3 (rpt_delay_sd, rpt_delay, mode, outp_cntrl, fft_stage, refi_middle_delay_ext, no_of_rpt_ext, rpt_step_value_ext, fft_end_stage, DiMArch_Mode, use_compr : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(3, instrcode)) &
            std_logic_vector(to_unsigned(rpt_delay_sd, 1)) &
            std_logic_vector(to_unsigned(rpt_delay, REPT_DELAY)) &
            std_logic_vector(to_unsigned(mode, MODE_SEL)) &
            std_logic_vector(to_unsigned(outp_cntrl, OUTPUT_CONTROL)) &
            std_logic_vector(to_unsigned(fft_stage, FFT_STAGE_SEL)) &
            std_logic_vector(to_unsigned(refi_middle_delay_ext, REG_FILE_MIDDLE_DELAY_EXT)) &
            std_logic_vector(to_unsigned(no_of_rpt_ext, NUM_OF_REPT_EXT)) &
            std_logic_vector(to_unsigned(rpt_step_value_ext, REP_STEP_VALUE_EXT)) &
            std_logic_vector(to_unsigned(fft_end_stage, 3)) & --FFT_END_STAGE ))&
            std_logic_vector(to_unsigned(DiMArch_Mode, 1)) &  -- mode for register file agu to read/write from DiMArch 
            std_logic_vector(to_unsigned(use_compr, 1));
        RETURN instruction_bus;
    END FUNCTION REFI_3;

    -----------------------------------------------------------------------------
    -- switch box
    -----------------------------------------------------------------------------
    FUNCTION SWB (from_block, from_address, from_port, to_block, to_address, to_port : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus                                    : std_logic_vector(INSTR_WIDTH DOWNTO 0);
        VARIABLE src_addr_row, hb_index, send_to_other_row, v_index : INTEGER;
        VARIABLE
        from_column_address,
        to_column_address,
        column_diff,
        max_hops                                            : INTEGER;
        VARIABLE unsigned_to_address, unsigned_from_address : unsigned (2 * COLUMNS - 1 DOWNTO 0);

    BEGIN

        ------------------------------------------------------
        -- initial calculations
        ------------------------------------------------------
        from_column_address   := from_address/2;
        to_column_address     := to_address/2;
        column_diff           := from_column_address - to_column_address;
        unsigned_to_address   := TO_UNSIGNED(to_address, 2 * COLUMNS);--TO_UNSIGNED(to_block, 2*columns);
        unsigned_from_address := TO_UNSIGNED(from_address, 2 * COLUMNS);--(from_block, 2*columns);
        ------------------------------------------------------
        IF unsigned_to_address(0) /= unsigned_from_address(0) THEN
            send_to_other_row := 1;
        ELSE
            send_to_other_row := 0;
        END IF;
        IF unsigned_from_address(0) = '0' THEN
            src_addr_row := 0;
        ELSE
            src_addr_row := 1;
        END IF;

        --if   to_column_address <=2 then
        --	max_hops := to_column_address;
        --else 
        max_hops := NR_OF_HOPS;
        --end if;
        hb_index := column_diff + max_hops;

        ------------------------------------------------------
        -- for vertical  index
        ------------------------------------------------------		
        -- All output ports should be numbered 0 to 1 from RF or 0 to 3 MDPU 
        IF to_block = REFI THEN
            v_index := to_port;
        ELSE
            v_index := to_port + 2;
        END IF;

        --v_index(0,1) : refi_in (0,1), v_index(2 to 5) : dpu_in(0 to 3)
        ------------------------------------------------------
        -- final instruction
        ------------------------------------------------------	  			   
        instruction_bus := "1" & std_logic_vector(to_unsigned(5, instrcode)) &
            std_logic_vector(to_unsigned(1, SWB_DAV)) &
            std_logic_vector(to_unsigned(src_addr_row, SWB_SRC_ADDR_ROW)) & --0,1
            std_logic_vector(to_unsigned(from_block, SWB_SRC_DPU_REFI)) &   --0=refi, 1=dpu
            std_logic_vector(to_unsigned(from_port, SWB_SRC_OUTPUT_NR)) &   --out0 or out1
            std_logic_vector(to_unsigned(hb_index, SWB_HB_INDEX)) &         --source column difference to the destination cell (0 to 6) 3 is the destination col                   
            std_logic_vector(to_unsigned(send_to_other_row, SWB_SEND_TO_OTHER_ROW)) &
            std_logic_vector(to_unsigned(v_index, SWB_V_INDEX)) & --0=refi, 1=dpu
            --std_logic_vector(to_unsigned(to_port, SWB_TO_PORT))&                     
            std_logic_vector(to_unsigned(0, INSTR_WIDTH - (instrcode + SWB_INSTR_PORT_SIZE)));
        RETURN instruction_bus;
    END FUNCTION SWB;
    FUNCTION BRANCH (brnch_mode, brnch_false_addr : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(11, instrcode)) &
            std_logic_vector(to_unsigned(brnch_mode, BR_MODE)) &
            std_logic_vector(to_unsigned(brnch_false_addr, BR_FALSE_ADDRS_SIZE)) &
            std_logic_vector(to_unsigned(0, BR_UNUSED
            --    	+ INSTR_WIDTH_DIFF
            ));

        RETURN instruction_bus;
    END FUNCTION BRANCH;

    -------------------------------------------------------------------------------
    -- Jump instruction for SEQ
    -------------------------------------------------------------------------------
    FUNCTION JUMP (true_addrs : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(6, instrcode)) &
            std_logic_vector(to_unsigned(true_addrs, TRUE_ADDRS_SIZE)) &
            std_logic_vector(to_unsigned(0, JUMP_UNUSED_BITS
            --+ INSTR_WIDTH_DIFF

            ));
        RETURN instruction_bus;
    END FUNCTION JUMP;
    -------------------------------------------------------------------------------
    -- DELAY
    -------------------------------------------------------------------------------
    FUNCTION DELAY (
        del_cycles_sd, del_cycles : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(7, instrcode)) &
            std_logic_vector(to_unsigned(del_cycles_sd, 1)) &
            std_logic_vector(to_unsigned(del_cycles, DLY_CYCLES)) &
            std_logic_vector(to_unsigned(0, DLY_UNUSED_BITS));--&
        --std_logic_vector(to_unsigned(0,9)

        --          );

        RETURN instruction_bus;
    END FUNCTION DELAY;
    -------------------------------------------------------------------------------
    -- Loop Header
    -------------------------------------------------------------------------------
    FUNCTION LOOP_HEADER (
        index_raccu_addr, index_start, iter_no_sd, iter_no : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(8, instrcode)) &
            std_logic_vector(to_unsigned(index_raccu_addr, FOR_INDEX_ADDR)) &
            std_logic_vector(to_unsigned(index_start, FOR_INDEX_START)) &
            std_logic_vector(to_unsigned(iter_no_sd, 1)) &
            std_logic_vector(to_unsigned(iter_no, FOR_ITER_NO)) &
            std_logic_vector(to_unsigned(0, FOR_HEADER_UNUSED))
            ;

        RETURN instruction_bus;
    END FUNCTION LOOP_HEADER;
    -------------------------------------------------------------------------------
    -- Loop TAIL
    ------------------------------------------------------------------------------- 
    FUNCTION LOOP_TAIL (
        index_step, pc_togo, index_raccu_addr : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(9, instrcode)) &
            std_logic_vector(to_unsigned(index_step, FOR_INDEX_STEP)) &
            std_logic_vector(to_unsigned(pc_togo, FOR_PC_TOGO)) &
            std_logic_vector(to_unsigned(index_raccu_addr, FOR_INDEX_ADDR)) &
            std_logic_vector(to_unsigned(0, FOR_TAIL_UNUSED))
            ;

        RETURN instruction_bus;
    END FUNCTION LOOP_TAIL;
    -------------------------------------------------------------------------------
    -- RACCU
    -------------------------------------------------------------------------------  
    FUNCTION RACCU (
        raccu_mode, raccu_op1_sd, raccu_op1, raccu_op2_sd, raccu_op2, raccu_result_addrs : INTEGER)
        RETURN std_logic_vector IS
        VARIABLE instruction_bus : std_logic_vector(INSTR_WIDTH DOWNTO 0);
    BEGIN
        instruction_bus := "1" & std_logic_vector(to_unsigned(10, instrcode)) &
            std_logic_vector(to_unsigned(raccu_mode, RACCU_MODE_SEL)) &
            std_logic_vector(to_unsigned(raccu_op1_sd, 1)) &
            std_logic_vector(to_signed(raccu_op1, RACCU_OPERAND)) &
            std_logic_vector(to_unsigned(raccu_op2_sd, 1)) &
            std_logic_vector(to_signed(raccu_op2, RACCU_OPERAND)) &
            std_logic_vector(to_signed(raccu_result_addrs, RACCU_RESULT_ADDR))
            ;

        RETURN instruction_bus;
    END FUNCTION RACCU;
END PACKAGE BODY tb_instructions;