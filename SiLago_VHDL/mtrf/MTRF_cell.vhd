-------------------------------------------------------
--! @file MTRF_cell.vhd
--! @brief 
--! @details 
--! @author Nasim Farahini
--! @version 1.0
--! @date 2014-02-26
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
-- File       : MTRF_cell.vhd
-- Author     : Nasim Farahini
-- Company    : KTH
-- Created    : 2014-02-26
-- Last update: 2014-02-26
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2014-02-26  1.0      Nasim Farahini      Created
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

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.seq_functions_package.ALL;
USE work.util_package.ALL;
USE work.top_consts_types_package.ALL;
USE work.noc_types_n_constants.NOC_BUS_TYPE;

ENTITY MTRF_cell IS
    PORT
    (
        clk            : IN std_logic;
        rst_n          : IN std_logic;
        instr_ld       : IN std_logic;
        instr_inp      : IN std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0);
        seq_address_rb : IN std_logic;
        seq_address_cb : IN std_logic;
        --RegFile
        --reg_wr_2             : IN  std_logic;
        --reg_rd_2             : IN  std_logic;
        --reg_wr_addr_2        : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
        --reg_rd_addr_2        : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
        data_in_reg_0 : IN signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_in_reg_1 : IN signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        --data_in_reg_2        : IN  signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
        dimarch_data_in      : IN STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
        dimarch_data_out     : OUT STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
        dimarch_rd_2_out     : OUT std_logic;
        data_out_reg_0_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_out_reg_0_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_out_reg_1_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_out_reg_1_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        --data_out_2           : OUT signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
        --DPU
        dpu_in_0        : IN signed(DPU_IN_WIDTH - 1 DOWNTO 0);   --signed
        dpu_in_1        : IN signed(DPU_IN_WIDTH - 1 DOWNTO 0);   --signed
        dpu_in_2        : IN signed(DPU_IN_WIDTH - 1 DOWNTO 0);   --signed
        dpu_in_3        : IN signed(DPU_IN_WIDTH - 1 DOWNTO 0);   --signed
        dpu_out_0_left  : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
        dpu_out_0_right : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
        dpu_out_1_left  : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
        dpu_out_1_right : OUT signed(DPU_OUT_WIDTH - 1 DOWNTO 0); --signed
        noc_bus_out     : OUT NOC_BUS_TYPE;
        s_bus_out       : OUT std_logic_vector(SWB_INSTR_PORT_SIZE - 1 DOWNTO 0)
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
    SIGNAL use_compr                            : std_logic;
    SIGNAL seq_cond_status_w                    : std_logic_vector(SEQ_COND_STATUS_WIDTH - 1 DOWNTO 0);
    SIGNAL dimarch_mode                         : std_logic;
    SIGNAL dpu_process_inout                    : std_logic_vector(DPU_PROCESS_INOUT_WIDTH - 1 DOWNTO 0);

    -- Compression Engine Signals
    SIGNAL rf_in_from_dimarch, rf_out_to_dimarch, rle_engine_in, rle_engine_out                              : std_logic_vector(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
    SIGNAL rf_block_write, rf_block_write_regin, rf_block_read, rf_block_read_regin, rle_valid_in, dec_com_n : std_logic;

BEGIN

    seq_gen : ENTITY work.sequencer
        GENERIC
        MAP(M => MAX_NR_OF_OUTP_N_HOPS - 1)
        PORT MAP
        (
            reg_dimarch_mode   => dimarch_mode,
            NOC_BUS_OUT        => noc_bus_out,
            clk                => clk,
            rst_n              => rst_n,
            instr_ld           => instr_ld,
            instr_inp          => instr_inp,
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
            reg_use_compr      => use_compr,
            s_bus_out          => s_bus_out);

    dpu_gen : ENTITY work.DPU
        PORT
        MAP(
        clk               => clk,
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
        dpu_out_0_left    => dpu_out_0_left,
        dpu_out_0_right   => dpu_out_0_right,
        dpu_out_1_left    => dpu_out_1_left,
        dpu_out_1_right   => dpu_out_1_right,
        seq_cond_status   => seq_cond_status_w
        );

    -- #################### RLE Engine ####################

    rf_block_write_regin <= '1' WHEN (reg_port_type_w = "00" AND dimarch_mode = '1') ELSE
        '0';
    rf_block_read_regin <= '1' WHEN (reg_port_type_w = "10" AND dimarch_mode = '1') ELSE
        '0';
    rle_valid_in <= '1' WHEN ((rf_block_write OR rf_block_read) AND use_compr) = '1' ELSE
        '0';
    dec_com_n <= '1' WHEN (rf_block_write AND use_compr) = '1' ELSE
        '0';

    reg_write_rf_block_ops : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            rf_block_write <= '0';
        ELSIF rising_edge(clk) THEN
            IF (instr_start_w = '1' AND dimarch_mode = '1') THEN
                rf_block_write <= rf_block_write_regin;
            END IF;
        END IF;
    END PROCESS;

    reg_read_rf_block_ops : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            rf_block_read <= '0';
        ELSIF rising_edge(clk) THEN
            IF (instr_start_w = '1' AND dimarch_mode = '1') THEN
                rf_block_read <= rf_block_read_regin;
            END IF;
        END IF;
    END PROCESS;

    write_to_rf_proc : PROCESS (rf_block_write, use_compr, rle_engine_out, dimarch_data_in)
    BEGIN
        IF rf_block_write = '1' THEN -- Writing to RF
            IF use_compr = '1' THEN
                rf_in_from_dimarch <= rle_engine_out;
            ELSE
                rf_in_from_dimarch <= dimarch_data_in;
            END IF;
        ELSE
            rf_in_from_dimarch <= (OTHERS => '0');
        END IF;
    END PROCESS;

    read_from_rf_proc : PROCESS (rf_block_read, use_compr, rle_engine_out, rf_out_to_dimarch)
    BEGIN
        IF rf_block_read = '1' THEN -- Reading from RF
            IF use_compr = '1' THEN
                dimarch_data_out <= rle_engine_out;
            ELSE
                dimarch_data_out <= rf_out_to_dimarch;
            END IF;
        ELSE
            dimarch_data_out <= (OTHERS => '0');
        END IF;
    END PROCESS;

    RLE_input_proc : PROCESS (dimarch_data_in, rf_out_to_dimarch, rf_block_write, rf_block_read, use_compr)
    BEGIN
        IF use_compr = '1' THEN
            IF rf_block_write = '1' THEN
                rle_engine_in <= dimarch_data_in;
            ELSIF rf_block_read = '1' THEN
                rle_engine_in <= rf_out_to_dimarch;
            ELSE
                rle_engine_in <= (OTHERS => '0');
            END IF;
        ELSE
            rle_engine_in <= (OTHERS => '0');
        END IF;
    END PROCESS;

    RLE_engine : ENTITY work.RLE_engine
        GENERIC
        MAP(
        Nw => MEM_BLOCK_SIZE,
        Wp => RLE_WP, -- Optimal number of Words in parallel
        Nb => BITWIDTH
        )
        PORT
        MAP(
        clk       => clk,
        rst_n     => rst_n,
        valid_in  => rle_valid_in,
        dec_com_n => dec_com_n,
        d_in      => rle_engine_in,
        d_out     => rle_engine_out
        );

    -- #################### End RLE Engine ####################

    reg_top : ENTITY work.register_file_top
        PORT
        MAP(
        dimarch_data_in  => rf_in_from_dimarch,
        dimarch_data_out => rf_out_to_dimarch,
        dimarch_rd_2_out => dimarch_rd_2_out,
        rst_n            => rst_n,
        clk              => clk,
        instr_start      => instr_start_w,
        --wr_2                 => reg_wr_2,
        --rd_2                 => reg_rd_2,
        --wr_addr_2            => reg_wr_addr_2,
        --rd_addr_2            => reg_rd_addr_2,
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
        data_in_reg_0        => data_in_reg_0,
        data_in_reg_1        => data_in_reg_1,
        --data_in_reg_2        => data_in_reg_2,
        reg_outp_cntrl       => reg_outp_cntrl_w,
        dimarch_mode         => dimarch_mode,
        data_out_reg_0_right => data_out_reg_0_right,
        data_out_reg_0_left  => data_out_reg_0_left,
        data_out_reg_1_right => data_out_reg_1_right,
        data_out_reg_1_left  => data_out_reg_1_left
        --data_out_2           => data_out_2
        );

END ARCHITECTURE rtl;