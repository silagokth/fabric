-------------------------------------------------------
--! @file sram_agu.vhd
--! @brief 
--! @details 
--! @author Muhammad Adeel Tajammul
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
-- File       : sram_agu.vhd
-- Author     : Muhammad Adeel Tajammul
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
--             1.0      Muhammad Adeel Tajammul      Created
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

-------------------------------------------------------------------------------
-- This is a single port rd/wr agu.
--
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
--use work.util_package.all;

USE work.top_consts_types_package.ALL;
USE work.noc_types_n_constants.sram_agu_instruction_type;
USE work.noc_types_n_constants.sram_instr_zero;

ENTITY sram_agu IS --(RD/WR)

    PORT (
        rst_n        : IN std_logic;
        clk          : IN std_logic;
        instr        : IN sram_agu_instruction_type;
        rw           : OUT std_logic;
        reorder      : OUT std_logic;
        rw_addrs_out : OUT unsigned(RAM_ADDRESS_WIDTH - 1 DOWNTO 0)
    );

END sram_agu;

ARCHITECTURE behv_rtl OF sram_agu IS
    SIGNAL instr_reg : sram_agu_instruction_type;
    -- interation count signals and flags
    SIGNAL loop1_count_reg             : unsigned(sr_loop1_iteration_width - 1 DOWNTO 0);
    SIGNAL loop1_count_up, loop1_reset : std_logic;
    SIGNAL loop2_count_reg             : unsigned(sr_loop2_iteration_width - 1 DOWNTO 0);
    SIGNAL loop2_count_up, loop2_reset : std_logic;
    --- delay signals and flags
    SIGNAL loop1_delay_reg                   : unsigned(sr_loop1_iteration_width - 1 DOWNTO 0);
    SIGNAL loop1_delay_en, loop1_delay_reset : std_logic;
    SIGNAL loop2_delay_reg                   : unsigned(sr_loop2_iteration_width - 1 DOWNTO 0);
    SIGNAL loop2_delay_en, loop2_delay_reset : std_logic;

    SIGNAL initial_delay_reg : unsigned(sr_loop1_iteration_width - 1 DOWNTO 0);
    SIGNAL initial_delay_en  : std_logic;

    ------------------------ address datapath signals----------------

    SIGNAL address_adder_input1, address_adder_input2, address_adder_result, address_reg : unsigned(RAM_ADDRESS_WIDTH DOWNTO 0);
    SIGNAL loop1_increment                                                               : unsigned(RAM_ADDRESS_WIDTH DOWNTO 0);
    SIGNAL loop2_increment                                                               : unsigned(RAM_ADDRESS_WIDTH DOWNTO 0);

    SIGNAL initial_select_input1         : std_logic; -- if 1 then select initial address else adder_reg
    SIGNAL increment_select_input2       : std_logic; -- if 0 then select loop1 increment else loop2 increment 
    SIGNAL address_initial_select_output : std_logic; -- if 1 then select initial address else select adder output
    SIGNAL retain_address_output         : std_logic; -- if 1 then retain 1 then retain output value else load adder output
    ------------------------------------ fsm  states ------------------
    TYPE state IS (idle, initial_state, loops);
    SIGNAL current_state, next_state : state;
    SIGNAL rw_flag                   : std_logic;
    SIGNAL complete                  : std_logic;
BEGIN
    reorder <= instr_reg.agu_mode;
    ---------------- Register new instruction-------------------------
    p_instr_reg : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            instr_reg <= sram_instr_zero;
        ELSIF rising_edge(clk) THEN
            IF complete = '1' THEN
                instr_reg <= instr;

            ELSIF instr_reg.enable = '1' THEN -- after one cycle make enable zero 
                instr_reg.enable <= '0';
            END IF;
            IF initial_select_input1 = '1' AND increment_select_input2 = '1' THEN -- update initial address for loop2 iterations
                instr_reg.Initial_Address <= address_adder_result(sr_initial_address_width - 1 DOWNTO 0);
            END IF;
        END IF;
    END PROCESS p_instr_reg;
    ---------------- loop1 counter-------------------------
    p_loop1_counter : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            loop1_count_reg <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF loop1_reset = '1' THEN -- syncronous reset to avoid latch
                loop1_count_reg <= (OTHERS => '0');
            ELSIF loop1_count_up = '1' THEN
                loop1_count_reg <= loop1_count_reg + 1;
            END IF;

        END IF;
    END PROCESS;
    ---------------- loop2 counter-------------------------
    p_loop2_counter : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            loop2_count_reg <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF loop2_reset = '1' THEN -- syncronous reset to avoid latch 
                loop2_count_reg <= (OTHERS => '0');
            ELSIF loop2_count_up = '1' THEN
                loop2_count_reg <= loop2_count_reg + 1;
            END IF;
        END IF;
    END PROCESS;
    ---------------- loop1 delay-------------------------
    p_loop1_delay : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            loop1_delay_reg <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF loop1_delay_en = '1' THEN
                loop1_delay_reg <= loop1_delay_reg + 1;
            ELSE
                loop1_delay_reg <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS;
    ---------------- loop2 delay-------------------------
    p_loop2_delay : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            loop2_delay_reg <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF loop2_delay_en = '1' THEN
                loop2_delay_reg <= loop2_delay_reg + 1;
            ELSE
                loop2_delay_reg <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS;
    ---------------- initial delay-------------------------
    p_initial_delay : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            initial_delay_reg <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF initial_delay_en = '1' THEN
                initial_delay_reg <= initial_delay_reg + 1;
            ELSE
                initial_delay_reg <= (OTHERS => '0');
            END IF;
        END IF;
    END PROCESS;
    ------------------------ address datapath----------------
    ---- Adder input1 mux 
    address_adder_input1 <= address_reg WHEN (initial_select_input1 = '0') ELSE
        instr_reg.Initial_Address(RAM_ADDRESS_WIDTH - 1) & instr_reg.Initial_Address;
    ---- Adder input2 mux
    address_adder_input2 <= loop1_increment WHEN (increment_select_input2 = '0') ELSE
        loop2_increment;

    ----- Adder 
    loop1_increment      <= unsigned(instr_reg.Loop1_increment);
    loop2_increment      <= unsigned(instr_reg.Loop2_increment);
    address_adder_result <= address_adder_input1 + address_adder_input2;

    --- output register
    p_address_reg : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            address_reg <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF address_initial_select_output = '1' THEN
                address_reg <= instr_reg.Initial_Address(RAM_ADDRESS_WIDTH - 1) & instr_reg.Initial_Address; -- assign sign extented 
            ELSIF retain_address_output = '0' THEN
                address_reg <= address_adder_result;
            END IF;
        END IF;
    END PROCESS p_address_reg;
    rw_addrs_out <= address_reg(RAM_ADDRESS_WIDTH - 1 DOWNTO 0);
    ----------------------------state register --------------
    p_state_reg : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            current_state <= idle;
            rw            <= '0';
        ELSIF rising_edge(clk) THEN
            current_state <= next_state;
            rw            <= rw_flag;
        END IF;
    END PROCESS p_state_reg;
    --------------------------- next state logic --------------
    p_next_state : PROCESS (instr_reg, initial_delay_reg, loop1_count_reg, loop1_delay_reg, loop2_count_reg, loop2_delay_reg, current_state) IS
    BEGIN
        rw_flag                       <= '0';
        initial_delay_en              <= '0';
        address_initial_select_output <= '0';
        loop1_delay_en                <= '0';
        loop2_delay_en                <= '0';
        loop1_count_up                <= '0';
        loop1_reset                   <= '0';
        loop2_count_up                <= '0';
        loop2_reset                   <= '0';
        loop1_delay_reset             <= '0';
        loop2_delay_reset             <= '0';
        retain_address_output         <= '1';
        increment_select_input2       <= '0';
        initial_select_input1         <= '0';
        complete                      <= '0';
        CASE current_state IS
            WHEN idle =>
                IF (instr_reg.enable = '1') THEN     -- new instruction 
                    IF instr_reg.Initial_Delay /= 0 THEN -- check if their is initial delay 
                        initial_delay_en <= '1';
                        next_state       <= initial_state;
                    ELSE -- initial delay is 0 go stright to loop1
                        rw_flag                       <= '1';
                        address_initial_select_output <= '1'; -- load initial address in address_reg
                        initial_select_input1         <= '1';
                        next_state                    <= loops;
                        --assert false report "start_loop" severity note;

                    END IF;
                ELSE
                    next_state <= idle;
                    complete   <= '1';
                END IF;
            WHEN initial_state =>
                IF initial_delay_reg < instr_reg.Initial_Delay THEN -- still in initial delay
                    initial_delay_en <= '1';
                    next_state       <= initial_state;
                ELSE -- initial delay is complete goto loop1
                    next_state <= loops;

                    -- skip datapath and use initial address
                    rw_flag                       <= '1';
                    address_initial_select_output <= '1';
                    initial_select_input1         <= '1';
                END IF;
            WHEN loops =>
                IF loop1_count_reg < instr_reg.Loop1_interations THEN --  in inner loop iterations
                    IF loop1_delay_reg < instr_reg.Loop1_Delay THEN       -- in loop1 delay
                        next_state <= loops;

                        loop1_delay_en <= '1'; -- increment loop1 delay
                    ELSE                   -- loop1_iteration, reset loop1_delay
                        next_state            <= loops;
                        rw_flag               <= '1';
                        loop1_count_up        <= '1'; -- increment loop1 counter
                        retain_address_output <= '0'; -- do not retain output and load from adder 

                    END IF;
                ELSIF loop2_count_reg < instr_reg.Loop2_iterations THEN -- in outer loop iterations
                    IF loop2_delay_reg < instr_reg.Loop2_Delay THEN         -- in loop2 delay
                        loop2_delay_en <= '1';
                        next_state     <= loops;

                    ELSE -- loop2 iteration and reset loop1
                        loop1_reset    <= '1';
                        loop2_count_up <= '1';
                        next_state     <= loops;
                        ------r/w first value of next iteration ------------
                        rw_flag                 <= '1';
                        retain_address_output   <= '0'; -- do not retain output and load from adder
                        increment_select_input2 <= '1'; -- add loop2 increment
                        initial_select_input1   <= '1'; -- with loop2 index

                    END IF;

                ELSE -- all iterations complete
                    --					assert false report "completed iterations" severity error;
                    loop1_reset <= '1';
                    loop2_reset <= '1';
                    next_state  <= idle;
                    complete    <= '1';
                END IF;

        END CASE;
    END PROCESS p_next_state;

END ARCHITECTURE;