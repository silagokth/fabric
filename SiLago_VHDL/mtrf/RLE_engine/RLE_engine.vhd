-------------------------------------------------------
--! @file RLE_engine.vhd
--! @brief 
--! @details 
--! @author 
--! @version 1.0
--! @date 2020-01-24
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
-- File       : RLE_engine.vhd
-- Author     : 
-- Company    : KTH
-- Created    : 2020-01-24
-- Last update: 2020-01-24
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2020
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

LIBRARY ieee, work;
USE ieee.std_logic_1164.ALL;
USE IEEE.std_logic_signed.ALL;
USE ieee.numeric_std.ALL;
USE work.functions.log2c;
USE ieee.math_real.ALL;

ENTITY RLE_engine IS
    GENERIC
    (
        Nw : INTEGER;
        Wp : INTEGER;
        Nb : INTEGER
    );
    PORT
    (
        clk       : IN std_logic;
        rst_n     : IN std_logic;
        valid_in  : IN std_logic;
        dec_com_n : IN std_logic;
        d_in      : IN std_logic_vector(Nw * Nb - 1 DOWNTO 0);
        d_out     : OUT std_logic_vector(Nw * Nb - 1 DOWNTO 0)
    );
END ENTITY;

ARCHITECTURE struct OF RLE_engine IS

    CONSTANT data_len : INTEGER := Nw * Nb;
    --constant Nw_real	: real := real(Nw);
    --constant Wp_real	: real := real(Wp);
    CONSTANT num_steps : INTEGER := INTEGER(ceil(real(Nw)/real(Wp)));
    -- Types	
    TYPE data_layers_type IS ARRAY(0 TO Wp) OF std_logic_vector(data_len - 1 DOWNTO 0);
    TYPE l_output_array_type IS ARRAY(0 TO Nw - Wp - 1) OF std_logic_vector(Nb - 1 DOWNTO 0);
    TYPE out_decoded_type IS ARRAY(0 TO Wp - 1) OF std_logic_vector(Nb - 1 DOWNTO 0);
    TYPE reg_in_array_type IS ARRAY(0 TO Nw - 1) OF std_logic_vector(Nb - 1 DOWNTO 0);
    --type reg_in_type is array(0 to num_steps) of reg_in_array_type;
    --type reg_enb_type is array(0 to num_steps) of std_logic_vector(Nw-1 downto 0);

    SIGNAL input_sel, cnt_en, cnt_clear      : std_logic;
    SIGNAL d_layer                           : data_layers_type;
    SIGNAL l_output_tmp                      : std_logic_vector(data_len - Wp * Nb - 1 DOWNTO 0);
    SIGNAL l_output_array                    : l_output_array_type;
    SIGNAL step_num                          : std_logic_vector(log2c(1 + num_steps) - 1 DOWNTO 0);
    SIGNAL step_num_int                      : INTEGER RANGE 0 TO num_steps;
    SIGNAL step_m1_times_wp, sh_num_reg_data : std_logic_vector(log2c(Nw + 1) - 1 DOWNTO 0);
    SIGNAL reg_in_final, reg_out             : reg_in_array_type;
    SIGNAL d_in_com, d_in_final              : std_logic_vector(data_len - 1 DOWNTO 0);
    SIGNAL reg_in_to_shift, reg_in_shifted   : std_logic_vector(data_len - 1 DOWNTO 0);
    SIGNAL reg_out_to_shift, reg_out_shifted : std_logic_vector(data_len - 1 DOWNTO 0);
    SIGNAL reg_enb                           : std_logic_vector(Nw - 1 DOWNTO 0);
    SIGNAL reg_enb_final                     : std_logic_vector(Nw - 1 DOWNTO 0);
    SIGNAL outs_decoded                      : out_decoded_type;
    SIGNAL step_for_sh_debug                 : unsigned(log2c(1 + num_steps) - 1 DOWNTO 0);
    SIGNAL wp_var_debug                      : unsigned(log2c(Nw + 1) - 1 DOWNTO 0);
    SIGNAL result_debug                      : std_logic_vector(log2c(1 + num_steps) + log2c(Nw + 1) - 1 DOWNTO 0);

BEGIN

    cnt_en <= '1' WHEN ((unsigned(step_num) > 0) OR valid_in = '1') ELSE
        '0';
    cnt_clear <= '1' WHEN unsigned(step_num) = num_steps ELSE
        '0';

    -- Step Counter 
    step_cnt_proc : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            step_num <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF cnt_en = '1' THEN
                IF cnt_clear = '1' THEN
                    step_num <= (OTHERS => '0');
                ELSE
                    step_num <= step_num + 1;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    --step_num_int <= to_integer(unsigned(step_num));
    --sh_num_reg_data <= to_integer(step_num*Wp);

    step_times_wp_proc : PROCESS (step_num)
        VARIABLE step_for_sh : unsigned(log2c(1 + num_steps) - 1 DOWNTO 0);
        VARIABLE wp_var      : unsigned(log2c(Nw + 1) - 1 DOWNTO 0);
        VARIABLE result      : std_logic_vector(log2c(1 + num_steps) + log2c(Nw + 1) - 1 DOWNTO 0);
    BEGIN
        IF unsigned(step_num) = 0 THEN
            step_for_sh := to_unsigned(num_steps, step_for_sh'length);
        ELSE
            step_for_sh := unsigned(unsigned(step_num) - 1);
        END IF;
        wp_var := to_unsigned(Wp, wp_var'length);
        result := std_logic_vector(step_for_sh * wp_var);
        step_m1_times_wp <= result(step_m1_times_wp'length - 1 DOWNTO 0);

        step_for_sh_debug <= step_for_sh;
        wp_var_debug      <= wp_var;
        result_debug      <= result;
    END PROCESS;

    sh_num_reg_data_proc : PROCESS (step_num, step_m1_times_wp)
    BEGIN
        IF unsigned(step_num) = 0 THEN
            sh_num_reg_data <= std_logic_vector(to_unsigned(0, sh_num_reg_data'length));
        ELSE
            sh_num_reg_data <= step_m1_times_wp;
        END IF;
    END PROCESS;

    reg_in_gen : FOR i IN 0 TO Nw - 1 GENERATE
        reg_in_gen_cond_1 : IF i < Wp GENERATE
            reg_in_to_shift((i + 1) * Nb - 1 DOWNTO i * Nb) <= outs_decoded(i);
        END GENERATE;
        reg_out_gen_cond_0 : IF i >= Wp GENERATE
            reg_in_to_shift((i + 1) * Nb - 1 DOWNTO i * Nb) <= l_output_array(i - Wp);
        END GENERATE;
    END GENERATE;

    bar_sh_reg_in : ENTITY work.barrel_shifter_rle
        GENERIC
        MAP (
        Nw => Nw,
        Nb => Nb
        )
        PORT MAP
        (
            sh_l_rn => '1',
            sh_num  => sh_num_reg_data,
            d_in    => reg_in_to_shift,
            d_out   => reg_in_shifted
        );

    -- Encode all 0s of input data, to distinguish them from 0s introduced by barrel shifter
    d_in_com_proc : PROCESS (d_in)
        VARIABLE tmp_data, tmp_out : std_logic_vector(Nb - 1 DOWNTO 0);
    BEGIN
        FOR i IN 0 TO Nw - 1 LOOP
            tmp_data := d_in((i + 1) * Nb - 1 DOWNTO i * Nb);
            IF unsigned(tmp_data) = 0 THEN
                tmp_out(Nb/2 - 1 DOWNTO 0)  := (OTHERS => '0');
                tmp_out(Nb - 1 DOWNTO Nb/2) := (OTHERS => '1');
                d_in_com((i + 1) * Nb - 1 DOWNTO i * Nb) <= tmp_out;
            ELSE
                d_in_com((i + 1) * Nb - 1 DOWNTO i * Nb) <= tmp_data;
            END IF;
        END LOOP;
    END PROCESS;

    d_in_final <= d_in_com WHEN dec_com_n = '0' ELSE
        d_in;

    reg_in_final_gen : FOR i IN 0 TO Nw - 1 GENERATE
        reg_in_final(i) <= d_in_final((i + 1) * Nb - 1 DOWNTO i * Nb) WHEN unsigned(step_num) = 0
    ELSE
        reg_in_shifted((i + 1) * Nb - 1 DOWNTO i * Nb);
    END GENERATE;

    reg_enb <= (OTHERS => valid_in) WHEN unsigned(step_num) = 0 ELSE
        (OTHERS            => '1');

    bar_sh_reg_enb : ENTITY work.barrel_shifter_rle
        GENERIC
        MAP (
        Nw => Nw,
        Nb => 1
        )
        PORT
        MAP (
        sh_l_rn => '1',
        sh_num  => sh_num_reg_data,
        d_in    => reg_enb,
        d_out   => reg_enb_final
        );

    reg_file_gen : FOR i IN 0 TO Nw - 1 GENERATE
        register_array : ENTITY work.reg_n
            GENERIC
            MAP(Nb => Nb)
            PORT
            MAP(
            clk   => clk,
            reset => rst_n,
            clear => '0',
            en    => reg_enb_final(i),
            d_in  => reg_in_final(i),
            d_out => reg_out(i)
            );
    END GENERATE;

    reg_out_shift_gen : FOR i IN 0 TO Nw - 1 GENERATE
        reg_out_to_shift((i + 1) * Nb - 1 DOWNTO i * Nb) <= reg_out(i);
    END GENERATE;

    bar_sh_reg_out : ENTITY work.barrel_shifter_rle
        GENERIC
        MAP (
        Nw => Nw,
        Nb => Nb
        )
        PORT
        MAP (
        sh_l_rn => '0',
        sh_num  => sh_num_reg_data,
        d_in    => reg_out_to_shift,
        d_out   => reg_out_shifted
        );

    d_layer(0) <= reg_out_shifted;

    layers_gen : FOR i IN 0 TO Wp - 1 GENERATE
        layer_i : ENTITY work.RLE_layer
            GENERIC
            MAP(
            Nw => Nw - i,
            Nb => Nb
            )
            PORT
            MAP(
            dec_com_n => dec_com_n,
            d_in      => d_layer(i)(data_len - 1 DOWNTO i * Nb),
            proc_word => outs_decoded(i),
            d_out     => d_layer(i + 1)(data_len - 1 DOWNTO (i + 1) * Nb)
            );
    END GENERATE;

    l_output_tmp <= d_layer(Wp)(data_len - 1 DOWNTO Wp * Nb);

    l_out_array_gen : FOR i IN 0 TO Nw - Wp - 1 GENERATE
        l_output_array(i) <= l_output_tmp((i + 1) * Nb - 1 DOWNTO i * Nb);
    END GENERATE;

    outputs_gen : FOR i IN 0 TO Nw - 1 GENERATE
        d_out((i + 1) * Nb - 1 DOWNTO i * Nb) <= reg_out(i);
    END GENERATE;

END ARCHITECTURE;