-------------------------------------------------------
--! @file RACCU.vhd
--! @brief 
--! @details 
--! @author Nasim Farahini
--! @version 1.0
--! @date 2014-04-15
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
-- File       : RACCU.vhd
-- Author     : Nasim Farahini
-- Company    : KTH
-- Created    : 2014-04-15
-- Last update: 2014-04-15
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2014-04-15  1.0      Nasim Farahini      Created
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

LIBRARY IEEE, work;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE IEEE.std_logic_signed.ALL;
USE work.top_consts_types_package.ALL;

ENTITY RACCU IS
    PORT
    (
        clk               : IN std_logic;
        rst_n             : IN std_logic;
        raccu_in1_sd      : std_logic;
        raccu_in1         : IN std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE - 1 DOWNTO 0); --signed
        raccu_in2_sd      : std_logic;
        raccu_in2         : IN std_logic_vector (RACCU_OPERAND2_VECTOR_SIZE - 1 DOWNTO 0);    --signed
        raccu_cfg_mode    : IN std_logic_vector (RACCU_MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);    --signed
        raccu_res_address : IN std_logic_vector (RACCU_RESULT_ADDR_VECTOR_SIZE - 1 DOWNTO 0); --signed
        raccu_regout      : OUT raccu_reg_out_ty;
        raccu_loop_reg    : OUT raccu_loop_array_ty
    );
END RACCU;

ARCHITECTURE beh OF RACCU IS
    SIGNAL loop_reg, loop_reg_tmp                                                 : raccu_loop_array_ty;
    SIGNAL data_reg                                                               : raccu_reg_out_ty;
    SIGNAL add_res, add_in1, add_in2, sub_in1, sub_in2, sub_res, no_of_iterations : std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE - 1 DOWNTO 0);
    SIGNAL wr_enb, l_index_val_wr_enb, l_end_flag_wr_enb, l_counter_wr_enb        : std_logic;
    SIGNAL result                                                                 : std_logic_vector (RACCU_REG_BITWIDTH - 1 DOWNTO 0);
    SIGNAL debug_sig                                                              : unsigned(RACCU_MODE_SEL_VECTOR_SIZE - 1 DOWNTO 0);
BEGIN
    raccu_regout   <= data_reg;
    raccu_loop_reg <= loop_reg;
    add_res        <= add_in1 + add_in2;
    sub_res        <= sub_in1 - sub_in2;
    raccu_regout_process : PROCESS (rst_n, clk)
    BEGIN
        IF rst_n = '0' THEN
            data_reg <= (OTHERS => (OTHERS => '0'));
        ELSIF clk'event AND clk = '1' THEN
            IF wr_enb = '1' THEN
                data_reg(CONV_INTEGER(raccu_res_address)) <= result;
            END IF;
        END IF;
    END PROCESS raccu_regout_process;

    loop_reg_process : PROCESS (rst_n, clk)
    BEGIN
        IF rst_n = '0' THEN
            loop_reg <= (OTHERS => (loop_index_value => (OTHERS => '0'), loop_counter => (OTHERS => '0'), loop_end_flag => '0'));
        ELSIF clk'event AND clk = '1' THEN
            IF l_index_val_wr_enb = '1' THEN
                loop_reg(CONV_INTEGER(raccu_res_address)).loop_index_value <= loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_index_value;
            END IF;
            IF l_counter_wr_enb = '1' THEN
                loop_reg(CONV_INTEGER(raccu_res_address)).loop_counter <= loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_counter;
            END IF;
            IF l_end_flag_wr_enb = '1' THEN
                loop_reg(CONV_INTEGER(raccu_res_address)).loop_end_flag <= loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_end_flag;
            END IF;
        END IF;
    END PROCESS loop_reg_process;

    raccu_mode_process : PROCESS (raccu_cfg_mode, raccu_res_address, data_reg, loop_reg, raccu_in2_sd, raccu_in1_sd, raccu_in1, raccu_in2, sub_res, add_res, debug_sig)
        VARIABLE raccu_in1_tmp : std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE - 1 DOWNTO 0) := (OTHERS => '0');
        VARIABLE raccu_in2_tmp : std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE - 1 DOWNTO 0) := (OTHERS => '0');
        VARIABLE debug_var     : INTEGER;
    BEGIN -- process raccu_mode_process

        FOR i IN 0 TO MAX_NO_OF_RACCU_LOOPS - 1 LOOP
            loop_reg_tmp(i) <= (loop_index_value => loop_reg(i).loop_index_value, loop_counter => loop_reg(i).loop_counter, loop_end_flag => loop_reg(i).loop_end_flag);
        END LOOP;

        l_index_val_wr_enb <= '0';
        l_end_flag_wr_enb  <= '0';
        l_counter_wr_enb   <= '0';
        no_of_iterations   <= (OTHERS => '0');
        add_in1            <= (OTHERS => '0');
        add_in2            <= (OTHERS => '0');
        wr_enb             <= '0';
        result             <= (OTHERS => '0');
        sub_in1            <= (OTHERS => '0');
        sub_in2            <= (OTHERS => '0');
        --raccu_in1_tmp <=(others=>'0');
        --raccu_in2_tmp <=(others=>'0');
        debug_var := to_integer(unsigned(raccu_cfg_mode));
        debug_sig <= unsigned(raccu_cfg_mode);
        CASE debug_var IS

            WHEN RAC_MODE_LOOP_HEADER =>

                add_in1 <= loop_reg(CONV_INTEGER(raccu_res_address)).loop_counter;
                add_in2 <= "0000001";

                IF loop_reg(CONV_INTEGER(raccu_res_address)).loop_counter = 0 THEN
                    loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_index_value <= raccu_in1;--loading start index value
                    l_index_val_wr_enb                                             <= '1';
                END IF;

                IF raccu_in2_sd = '0' THEN
                    no_of_iterations <= raccu_in2;
                ELSE
                    no_of_iterations <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;

                IF raccu_in2 = add_res THEN --if no_of_iterations= add_res then , bug of delta delay
                    l_end_flag_wr_enb                                           <= '1';
                    loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_end_flag <= '1';
                ELSE
                    l_counter_wr_enb                                           <= '1';
                    loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_counter <= add_res;
                END IF;

            WHEN RAC_MODE_LOOP_TAIL =>

                IF loop_reg(CONV_INTEGER(raccu_res_address)).loop_end_flag = '0' THEN
                    add_in1                                                        <= loop_reg(CONV_INTEGER(raccu_res_address)).loop_index_value;
                    add_in2                                                        <= raccu_in1;
                    loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_index_value <= add_res;
                    l_index_val_wr_enb                                             <= '1';
                ELSE
                    l_end_flag_wr_enb  <= '1';
                    l_index_val_wr_enb <= '1';
                    l_counter_wr_enb   <= '1';
                    loop_reg_tmp       <= (OTHERS => (loop_index_value => (OTHERS => '0'), loop_counter => (OTHERS => '0'), loop_end_flag => '0'));
                END IF;
            WHEN RAC_MODE_ADD =>
                IF raccu_in1_sd = '0' THEN
                    add_in1 <= raccu_in1;
                ELSE
                    add_in1 <= data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;
                IF raccu_in2_sd = '0' THEN
                    add_in2 <= raccu_in2;
                ELSE
                    add_in2 <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;
                result <= add_res;
                wr_enb <= '1';
            WHEN RAC_MODE_SUB =>
                IF raccu_in1_sd = '0' THEN
                    sub_in1 <= raccu_in1;
                ELSE
                    sub_in1 <= data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;
                IF raccu_in2_sd = '0' THEN
                    sub_in2 <= raccu_in2;
                ELSE
                    sub_in2 <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;
                result <= sub_res;
                wr_enb <= '1';
            WHEN RAC_MODE_SHFT_R =>

                IF raccu_in1_sd = '0' THEN
                    raccu_in1_tmp := raccu_in1;
                ELSE
                    raccu_in1_tmp := data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;
                IF raccu_in2_sd = '0' THEN
                    raccu_in2_tmp := raccu_in2;
                ELSE
                    raccu_in2_tmp := data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;

                result <= std_logic_vector(unsigned(raccu_in1_tmp) SRL CONV_INTEGER(raccu_in2_tmp));
                wr_enb <= '1';

            WHEN RAC_MODE_SHFT_L =>

                IF raccu_in1_sd = '0' THEN
                    raccu_in1_tmp := raccu_in1;
                ELSE
                    raccu_in1_tmp := data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;
                IF raccu_in2_sd = '0' THEN
                    raccu_in2_tmp := raccu_in2;
                ELSE
                    raccu_in2_tmp := data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;

                result <= std_logic_vector(unsigned(raccu_in1_tmp) SLL CONV_INTEGER(raccu_in2_tmp));
                wr_enb <= '1';

                -- TODO: check if the loop_counter contains the correct value from start_val to the end

                -- In this mode, op1 determines the loop address and RACCU should add op2 value (whether static or dynamic)
                -- to the current index value (loop counter) of the specified loop and write the result to the res_address
            WHEN RAC_MODE_ADD_WITH_LOOP_INDEX =>
                ------------------------------------------------------------------------
                -- MODIFICATION: CHANGE THE RANGE FOR raccu_in_1 TO LOOP_REG_WIDTH TO
                --               AVOID RANGE MISMATCH IN TRANSITION OF STATES.
                ------------------------------------------------------------------------
                -- ORIGINAL CODE:
                -- add_in1 <= loop_reg(to_integer(unsigned(raccu_in1(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)))).loop_index_value;
                ------------------------------------------------------------------------
                -- MODIFIED CODE:
                add_in1 <= loop_reg(to_integer(unsigned(raccu_in1(LOOP_REG_WIDTH - 1 DOWNTO 0)))).loop_index_value;
                ------------------------------------------------------------------------
                -- MODIFICATION END
                ------------------------------------------------------------------------
                IF raccu_in2_sd = '0' THEN
                    add_in2 <= raccu_in2;
                ELSE
                    add_in2 <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH - 1 DOWNTO 0)));
                END IF;

                result <= add_res;
                wr_enb <= '1';
            WHEN OTHERS =>

        END CASE;
    END PROCESS raccu_mode_process;

END beh;