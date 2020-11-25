-------------------------------------------------------
--! @file RLE_layer.vhd
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
-- File       : RLE_layer.vhd
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

ENTITY RLE_layer IS
    GENERIC
    (
        Nw : INTEGER;
        Nb : INTEGER
    );
    PORT
    (
        dec_com_n : IN std_logic; -- 1 for decompression, 0 for compression
        d_in      : IN std_logic_vector(Nw * Nb - 1 DOWNTO 0);
        proc_word : OUT std_logic_vector(Nb - 1 DOWNTO 0);
        d_out     : OUT std_logic_vector((Nw - 1) * Nb - 1 DOWNTO 0)
    );
END ENTITY;

ARCHITECTURE struct OF RLE_layer IS

    TYPE d_in_array_type IS ARRAY(0 TO Nw - 1) OF std_logic_vector(Nb - 1 DOWNTO 0);

    -- Common Signals
    SIGNAL d_to_bar_sh     : std_logic_vector((Nw - 1) * Nb - 1 DOWNTO 0);
    SIGNAL cnt_zeros_final : std_logic_vector(log2c(Nw) - 1 DOWNTO 0);

    -- Decompression Part signals
    SIGNAL data_is_enc           : std_logic;
    SIGNAL half_ones             : std_logic_vector(Nb/2 - 1 DOWNTO 0);
    SIGNAL word_to_dec, dec_word : std_logic_vector(Nb - 1 DOWNTO 0);
    SIGNAL cnt_zeros_dec         : std_logic_vector(log2c(Nw) - 1 DOWNTO 0);

    -- Compression Part signals 
    SIGNAL d_in_array                   : d_in_array_type;
    SIGNAL d_in_word_neq0               : std_logic_vector(Nw - 1 DOWNTO 0);
    SIGNAL all_ones, enc_word, com_word : std_logic_vector(Nb - 1 DOWNTO 0);
    SIGNAL cnt_zeros_com                : std_logic_vector(log2c(Nw) - 1 DOWNTO 0);

BEGIN
    half_ones   <= (OTHERS => '1');
    word_to_dec <= d_in(Nb - 1 DOWNTO 0);
    d_to_bar_sh <= d_in(Nw * Nb - 1 DOWNTO Nb);

    -- ########## Get #zeros and processed word for decompression ########## 

    data_is_enc <= '1' WHEN word_to_dec(Nb - 1 DOWNTO Nb/2) = half_ones ELSE
        '0';
    dec_word <= word_to_dec WHEN data_is_enc = '0' ELSE
        (OTHERS                  => '0');
    cnt_zeros_dec <= (OTHERS => '0') WHEN data_is_enc = '0' ELSE
        word_to_dec(cnt_zeros_dec'length - 1 DOWNTO 0);

    -- ########## Get #zeros and processed word for compression ##########

    all_ones <= (OTHERS => '1');

    d_in_array_gen : FOR i IN 0 TO Nw - 1 GENERATE
        d_in_array(i)     <= d_in((i + 1) * Nb - 1 DOWNTO i * Nb);
        d_in_word_neq0(i) <= '1' WHEN d_in_array(i)(Nb - 1 DOWNTO Nb/2) /= half_ones ELSE
        '0';
    END GENERATE;

    trail_zeros_unit : ENTITY work.trail_zeros_cnt
        GENERIC
        MAP(Nb => Nw)
        PORT MAP
        (
            d_in  => d_in_word_neq0,
            d_out => cnt_zeros_com
        );

    d_in_0_enc_proc : PROCESS (cnt_zeros_com)
    BEGIN
        enc_word(Nb - 1 DOWNTO Nb/2)                <= (OTHERS => '1');
        enc_word(Nb/2 - 1 DOWNTO 0)                 <= (OTHERS => '0');
        enc_word(cnt_zeros_com'length - 1 DOWNTO 0) <= cnt_zeros_com;
    END PROCESS;

    com_word <= d_in_array(0) WHEN d_in_word_neq0(0) = '1' ELSE
        enc_word;

    -- Choose final #zeros
    cnt_zeros_final <= cnt_zeros_dec WHEN dec_com_n = '1' ELSE
        cnt_zeros_com;

    -- Choose final processed word
    proc_word <= dec_word WHEN dec_com_n = '1' ELSE
        com_word;

    bar_sh_i : ENTITY work.barrel_shifter_rle
        GENERIC
        MAP (
        Nw => Nw - 1,
        Nb => Nb
        )
        PORT
        MAP (
        sh_l_rn => dec_com_n,
        sh_num  => cnt_zeros_final,
        d_in    => d_to_bar_sh,
        d_out   => d_out
        );

END ARCHITECTURE;