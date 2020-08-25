-------------------------------------------------------
--! @file AGU_RFblock.vhd
--! @brief AGU register file block
--! @details 
--! @author Nasim Farahini
--! @version 2.0
--! @date 2013-09-10
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
-- Title      : AGU register file block
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : AGU_RFblock.vhd
-- Author     : Nasim Farahini
-- Company    : KTH
-- Created    : 2013-09-10
-- Last update: 2014-02-15
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2013-09-10  1.0      Nasim Farahini  Created  -- Covers implementation of one level affine loop, 
-- 2014-02-15  2.0      Nasim Farahini  Modified -- Covers Repetition, Repetition Delay, Middle Delay
-- -------------------------------------------------------------------------------

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

LIBRARY ieee, work;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE ieee.std_logic_unsigned.ALL;
USE work.top_consts_types_package.ALL;

ENTITY AGU_RFblock IS

    PORT
    (
        clk                  : IN std_logic;
        rst_n                : IN std_logic;
        instr_start          : IN std_logic;
        instr_initial_delay  : IN std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
        instr_start_addrs    : IN std_logic_vector(START_ADDR_WIDTH - 1 DOWNTO 0);
        instr_step_val       : IN std_logic_vector(ADDR_OFFSET_WIDTH - 1 DOWNTO 0);
        instr_step_val_sign  : IN std_logic_vector(ADDR_OFFSET_SIGN_WIDTH - 1 DOWNTO 0);
        instr_no_of_addrs    : IN std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
        instr_middle_delay   : IN std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 DOWNTO 0);
        instr_no_of_rpts     : IN std_logic_vector(NUM_OF_REPT_PORT_SIZE - 1 DOWNTO 0);
        instr_rpt_step_value : IN std_logic_vector(REP_STEP_VALUE_PORT_SIZE - 1 DOWNTO 0);
        instr_rpt_delay      : IN std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
        addr_out             : OUT std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 DOWNTO 0); -- changed from REG_FILE_ADDR_WIDTH
        addr_en              : OUT std_logic
    );

END AGU_RFblock;

ARCHITECTURE behave OF AGU_RFblock IS

    TYPE state_type IS (IDLE_ST, INITIAL_DELAY_ST, LINEAR_ST, RPT_DELAY_ST, RPT_ST);

    SIGNAL pres_state, next_state                                                                                                                          : state_type;
    SIGNAL delay_counter                                                                                                                                   : std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
    SIGNAL addr_counter                                                                                                                                    : std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
    SIGNAL one_addr, delay_count_en, addr_count_en, addr_value_en, init_zero, all_done_temp, no_more_rpt, add_sub_addr_en                                  : std_logic;
    SIGNAL rpt_no_count_en, rpt_start_addrs_en, rpt_delay_count_en, middle_delay_flag, middle_delay_first_cycle, middle_delay_count_en, addr_count_halt_en : std_logic;
    SIGNAL step_val_reg, step_val_temp_in                                                                                                                  : std_logic_vector(ADDR_OFFSET_WIDTH_BLOCK - 1 DOWNTO 0); -- changed from ADDR_OFFSET_WIDTH
    SIGNAL no_of_addrs_reg                                                                                                                                 : std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
    SIGNAL start_addrs_reg, rpt_start_addrs_reg_in, rpt_start_addrs_reg                                                                                    : std_logic_vector(START_ADDR_WIDTH_BLOCK - 1 DOWNTO 0); -- changed from START_ADDR_WIDTH
    SIGNAL initial_delay_reg                                                                                                                               : std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
    SIGNAL rpt_no_counter                                                                                                                                  : std_logic_vector(5 DOWNTO 0);
    SIGNAL middle_delay_reg, middle_delay_counter                                                                                                          : std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 DOWNTO 0);
    SIGNAL no_of_rpts_reg                                                                                                                                  : std_logic_vector(NUM_OF_REPT_PORT_SIZE - 1 DOWNTO 0);
    SIGNAL rpt_step_value_reg                                                                                                                              : std_logic_vector(ADDR_OFFSET_WIDTH_BLOCK - 1 DOWNTO 0); -- changed from REP_STEP_VALUE_PORT_SIZE
    SIGNAL rpt_delay_reg, rpt_delay_counter                                                                                                                : std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 DOWNTO 0);
    SIGNAL step_val_sign_reg, add_sub_addr                                                                                                                 : std_logic_vector(ADDR_OFFSET_SIGN_WIDTH - 1 DOWNTO 0);
    SIGNAL addr_temp_in, addr_temp_out                                                                                                                     : std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 DOWNTO 0); -- changed from REG_FILE_ADDR_WIDTH 

    ---------------- Definition of instruction fields with new address base -----------------------
    SIGNAL instr_start_addrs_block    : std_logic_vector(START_ADDR_WIDTH_BLOCK - 1 DOWNTO 0);
    SIGNAL instr_step_val_block       : std_logic_vector(ADDR_OFFSET_WIDTH_BLOCK - 1 DOWNTO 0);
    SIGNAL instr_rpt_step_value_block : std_logic_vector(ADDR_OFFSET_WIDTH_BLOCK - 1 DOWNTO 0);
    -----------------------------------------------------------------------------------------------

BEGIN

    ---------------- Input instruction fields with new address base -----------------------

    instr_start_addrs_block    <= instr_start_addrs(START_ADDR_WIDTH_BLOCK - 1 DOWNTO 0);
    instr_step_val_block       <= instr_step_val(ADDR_OFFSET_WIDTH_BLOCK - 1 DOWNTO 0);
    instr_rpt_step_value_block <= instr_rpt_step_value(ADDR_OFFSET_WIDTH_BLOCK - 1 DOWNTO 0);

    ---------------------------------------------------------------------------------------
    one_addr <= '1' WHEN instr_no_of_addrs = "000000" ELSE
        '0';
    init_zero <= '1' WHEN (instr_start = '1' AND instr_initial_delay = "0000") ELSE
        '0';
    rpt_start_addrs_reg_in <= start_addrs_reg WHEN rpt_no_counter = "000000" ELSE
        rpt_start_addrs_reg;
    no_more_rpt <= '1' WHEN rpt_no_counter = no_of_rpts_reg ELSE
        '0';
    middle_delay_flag <= '0' WHEN middle_delay_counter = middle_delay_reg ELSE
        '1';
    middle_delay_first_cycle <= '0' WHEN middle_delay_counter = "000000" ELSE
        '1';
    add_sub_addr_en <= '0' WHEN add_sub_addr = "0" ELSE
        '1';

    reg_input_param : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            step_val_reg       <= (OTHERS => '0');
            step_val_sign_reg  <= (OTHERS => '0');
            no_of_addrs_reg    <= (OTHERS => '0');
            start_addrs_reg    <= (OTHERS => '0');
            initial_delay_reg  <= (OTHERS => '0');
            middle_delay_reg   <= (OTHERS => '0');
            no_of_rpts_reg     <= (OTHERS => '0');
            rpt_step_value_reg <= (OTHERS => '0');
            rpt_delay_reg      <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF instr_start = '1' THEN
                step_val_reg       <= instr_step_val_block;
                step_val_sign_reg  <= instr_step_val_sign;
                no_of_addrs_reg    <= instr_no_of_addrs;
                start_addrs_reg    <= instr_start_addrs_block;
                initial_delay_reg  <= instr_initial_delay;
                middle_delay_reg   <= instr_middle_delay;
                no_of_rpts_reg     <= instr_no_of_rpts;
                rpt_step_value_reg <= instr_rpt_step_value_block;
                rpt_delay_reg      <= instr_rpt_delay;
            ELSIF all_done_temp = '1' THEN
                step_val_reg       <= (OTHERS => '0');
                step_val_sign_reg  <= (OTHERS => '0');
                no_of_addrs_reg    <= (OTHERS => '0');
                start_addrs_reg    <= (OTHERS => '0');
                initial_delay_reg  <= (OTHERS => '0');
                middle_delay_reg   <= (OTHERS => '0');
                no_of_rpts_reg     <= (OTHERS => '0');
                rpt_step_value_reg <= (OTHERS => '0');
                rpt_delay_reg      <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS reg_input_param;

    init_del_cnt : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            delay_counter <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF delay_count_en = '1' THEN
                delay_counter <= delay_counter + 1;
            ELSE
                delay_counter <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS init_del_cnt;
    rpt_del_cnt : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            rpt_delay_counter <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF rpt_delay_count_en = '1' THEN
                rpt_delay_counter <= rpt_delay_counter + 1;
            ELSE
                rpt_delay_counter <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS rpt_del_cnt;
    mdl_del_cnt : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            middle_delay_counter <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF middle_delay_count_en = '1' THEN
                middle_delay_counter <= middle_delay_counter + 1;
            ELSE
                middle_delay_counter <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS mdl_del_cnt;
    rpt_no_cnt : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            rpt_no_counter <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF rpt_no_count_en = '1' THEN
                rpt_no_counter <= rpt_no_counter + 1;
            ELSIF all_done_temp = '1' THEN
                rpt_no_counter <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS rpt_no_cnt;

    addr_cnt : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            addr_counter <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF addr_count_en = '1' AND addr_count_halt_en = '0'THEN
                addr_counter <= addr_counter + 1;
            ELSIF addr_count_en = '0' THEN
                addr_counter <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS addr_cnt;
    rpt_start_addrs_value : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            rpt_start_addrs_reg <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF rpt_start_addrs_en = '1' THEN
                rpt_start_addrs_reg <= rpt_start_addrs_reg_in + rpt_step_value_reg;
            END IF;
        END IF;
    END PROCESS rpt_start_addrs_value;
    addr_value : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            addr_temp_out <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF addr_value_en = '1' THEN
                IF add_sub_addr_en = '0' THEN
                    addr_temp_out <= addr_temp_in + step_val_temp_in;
                ELSE
                    addr_temp_out <= addr_temp_in - step_val_temp_in;
                END IF;
            ELSE
                addr_temp_out <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS addr_value;

    AGU_FSM : PROCESS (pres_state, middle_delay_flag, middle_delay_first_cycle, rpt_start_addrs_en, no_more_rpt, rpt_no_count_en, instr_step_val_block, instr_start_addrs_block,
        instr_no_of_addrs, instr_start, instr_initial_delay, one_addr, addr_counter, start_addrs_reg,
        initial_delay_reg, delay_counter, addr_temp_out, step_val_sign_reg, no_of_addrs_reg, rpt_delay_reg, rpt_delay_counter, instr_middle_delay, instr_no_of_rpts,
        instr_step_val_sign, instr_rpt_delay, step_val_reg, instr_rpt_step_value_block, no_of_rpts_reg, rpt_step_value_reg, rpt_start_addrs_reg)
    BEGIN
        next_state            <= pres_state;
        addr_count_en         <= '0';
        addr_out              <= (OTHERS => '0');
        addr_en               <= '0';
        all_done_temp         <= '0';
        addr_temp_in          <= (OTHERS => '0');
        delay_count_en        <= '0';
        step_val_temp_in      <= step_val_reg;
        rpt_no_count_en       <= '0';
        addr_value_en         <= '0';
        rpt_start_addrs_en    <= '0';
        rpt_delay_count_en    <= '0';
        middle_delay_count_en <= '0';
        addr_count_halt_en    <= '0';
        add_sub_addr          <= step_val_sign_reg;

        CASE pres_state IS
            WHEN IDLE_ST =>
                addr_en    <= '0';
                next_state <= IDLE_ST;

                IF instr_start = '1' THEN
                    IF instr_initial_delay = "0000" THEN
                        IF instr_middle_delay = "000000" THEN
                            step_val_temp_in <= instr_step_val_block;
                            add_sub_addr     <= instr_step_val_sign;
                            IF one_addr = '1' THEN
                                IF instr_no_of_rpts = "000000" THEN
                                    next_state    <= IDLE_ST;
                                    addr_en       <= '1';
                                    all_done_temp <= '1';
                                    addr_out      <= instr_start_addrs_block;
                                ELSE
                                    IF instr_rpt_delay = "000000" THEN
                                        addr_temp_in       <= instr_start_addrs_block;
                                        add_sub_addr       <= (OTHERS => '0');
                                        rpt_no_count_en    <= '1';
                                        rpt_start_addrs_en <= '1';
                                        addr_en            <= '1';
                                        addr_count_en      <= '0';
                                        addr_value_en      <= '1';
                                        next_state         <= LINEAR_ST;
                                        addr_out           <= instr_start_addrs_block;
                                        step_val_temp_in   <= instr_rpt_step_value_block;
                                    ELSE
                                        next_state         <= RPT_DELAY_ST;
                                        rpt_no_count_en    <= '1';
                                        rpt_start_addrs_en <= '1';
                                        rpt_delay_count_en <= '0';
                                        addr_value_en      <= '1';
                                        step_val_temp_in   <= (OTHERS => '0');
                                        add_sub_addr       <= (OTHERS => '0');
                                        addr_count_en      <= '1';
                                        addr_en            <= '1';
                                        addr_temp_in       <= instr_start_addrs_block;
                                        addr_out           <= instr_start_addrs_block;
                                    END IF;
                                END IF;
                            ELSE
                                addr_temp_in  <= instr_start_addrs_block;
                                addr_en       <= '1';
                                addr_count_en <= '1';
                                addr_value_en <= '1';
                                next_state    <= LINEAR_ST;
                                addr_out      <= instr_start_addrs_block;
                                add_sub_addr  <= instr_step_val_sign;
                            END IF;
                        ELSE
                            middle_delay_count_en <= '1';
                            addr_count_halt_en    <= '1';
                            step_val_temp_in      <= (OTHERS => '0');
                            add_sub_addr          <= (OTHERS => '0');
                            addr_temp_in          <= instr_start_addrs_block;
                            addr_en               <= '1';
                            addr_count_en         <= '1';
                            addr_value_en         <= '1';
                            next_state            <= LINEAR_ST;
                            addr_out              <= instr_start_addrs_block;
                        END IF;
                    ELSE
                        IF instr_initial_delay = "0001" THEN
                            step_val_temp_in <= (OTHERS => '0');
                            add_sub_addr     <= (OTHERS => '0');
                            addr_temp_in     <= instr_start_addrs_block;
                            addr_en          <= '0';
                            addr_count_en    <= '0';
                            addr_value_en    <= '1';
                            next_state       <= LINEAR_ST;
                        ELSE
                            next_state     <= INITIAL_DELAY_ST;
                            addr_en        <= '0';
                            delay_count_en <= '1';
                            addr_value_en  <= '0';
                            addr_count_en  <= '0';
                        END IF;

                    END IF;
                END IF;

            WHEN INITIAL_DELAY_ST =>
                next_state     <= INITIAL_DELAY_ST;
                delay_count_en <= '1';
                IF delay_counter = initial_delay_reg - 1 THEN
                    delay_count_en   <= '0';
                    addr_en          <= '0';
                    addr_count_en    <= '0';
                    addr_value_en    <= '1';
                    step_val_temp_in <= (OTHERS => '0');
                    addr_temp_in     <= start_addrs_reg;
                    next_state       <= LINEAR_ST;
                END IF;

            WHEN LINEAR_ST =>
                addr_count_en  <= '1';
                addr_value_en  <= '1';
                delay_count_en <= '0';
                addr_out       <= addr_temp_out;
                add_sub_addr   <= step_val_sign_reg;
                addr_temp_in   <= addr_temp_out;
                next_state     <= LINEAR_ST;
                IF middle_delay_first_cycle = '0' THEN --Remove this condition if you need adr_en=1 during the middle_delay
                    addr_en <= '1';
                ELSE
                    addr_en <= '0';
                END IF;

                IF addr_counter = no_of_addrs_reg THEN
                    addr_count_en <= '0';
                    addr_value_en <= '1';

                    IF no_of_rpts_reg = "000000" OR no_more_rpt = '1' THEN
                        next_state    <= IDLE_ST;
                        all_done_temp <= '1';
                    ELSE
                        rpt_no_count_en    <= '1';
                        rpt_start_addrs_en <= '1';
                        IF rpt_delay_reg = "000000" THEN
                            IF no_of_addrs_reg = "000000" THEN
                                next_state       <= LINEAR_ST;
                                step_val_temp_in <= rpt_step_value_reg;
                                add_sub_addr     <= (OTHERS => '0');
                            ELSE
                                next_state <= RPT_ST;
                            END IF;
                        ELSE
                            next_state         <= RPT_DELAY_ST;
                            rpt_delay_count_en <= '0';
                            addr_value_en      <= '1';
                            step_val_temp_in   <= (OTHERS => '0');
                            add_sub_addr       <= (OTHERS => '0');

                        END IF;
                    END IF;

                ELSE
                    IF middle_delay_flag = '1' THEN
                        addr_count_halt_en    <= '1';
                        step_val_temp_in      <= (OTHERS => '0');
                        add_sub_addr          <= (OTHERS => '0');
                        middle_delay_count_en <= '1';

                    END IF;
                END IF;

            WHEN RPT_DELAY_ST =>
                next_state         <= RPT_DELAY_ST;
                rpt_delay_count_en <= '1';
                addr_temp_in       <= addr_temp_out;
                step_val_temp_in   <= (OTHERS => '0');
                add_sub_addr       <= (OTHERS => '0');
                addr_value_en      <= '1';
                IF rpt_delay_counter = rpt_delay_reg - 1 THEN
                    rpt_delay_count_en <= '0';
                    IF no_of_addrs_reg = "000000" THEN
                        next_state       <= LINEAR_ST;
                        step_val_temp_in <= rpt_step_value_reg;
                        add_sub_addr     <= (OTHERS => '0');
                    ELSE
                        next_state <= RPT_ST;
                    END IF;
                END IF;

            WHEN RPT_ST =>
                addr_count_en  <= '1';
                addr_value_en  <= '1';
                delay_count_en <= '0';
                addr_out       <= rpt_start_addrs_reg;
                next_state     <= LINEAR_ST;
                addr_temp_in   <= rpt_start_addrs_reg;

                IF middle_delay_first_cycle = '0' THEN --Remove this condition if you need adr_en=1 during the middle_delay
                    addr_en <= '1';
                ELSE
                    addr_en <= '0';
                END IF;
                IF middle_delay_flag = '1' THEN
                    middle_delay_count_en <= '1';
                    addr_count_halt_en    <= '1';
                    step_val_temp_in      <= (OTHERS => '0');
                    add_sub_addr          <= (OTHERS => '0');
                END IF;

            WHEN OTHERS => NULL;
        END CASE;
    END PROCESS AGU_FSM;
    State_Reg : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            pres_state <= IDLE_ST;
        ELSIF clk'event AND clk = '1' THEN
            pres_state <= next_state;
        END IF;
    END PROCESS State_Reg;

END behave;