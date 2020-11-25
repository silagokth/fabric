-------------------------------------------------------
--! @file trail_zeros_cnt.vhd
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
-- File       : trail_zeros_cnt.vhd
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

ENTITY trail_zeros_cnt IS
    GENERIC
        (Nb : INTEGER);
    PORT
    (
        d_in  : IN std_logic_vector(Nb - 1 DOWNTO 0);
        d_out : OUT std_logic_vector(log2c(Nb) - 1 DOWNTO 0)
    );
END ENTITY;

ARCHITECTURE bhv OF trail_zeros_cnt IS
    TYPE bits_cmp_type IS ARRAY(1 TO Nb) OF std_logic_vector(Nb - 1 DOWNTO 0);
    SIGNAL bits_cmp : bits_cmp_type;
    SIGNAL cmp_res  : std_logic_vector(Nb DOWNTO 1);
    SIGNAL count    : std_logic_vector(log2c(Nb) - 1 DOWNTO 0);
BEGIN

    --bits_cmp(0) <= (others => '0');
    bits_cmp(Nb) <= (OTHERS => '0');
    bits_cmp_init : FOR i IN 1 TO Nb - 1 GENERATE
        if_mspart_exist : IF i < Nb - 1 GENERATE
            bits_cmp(i)(Nb - 1 DOWNTO i + 1) <= (OTHERS => '0');
        END GENERATE;
        bits_cmp(i)(i)              <= '1';
        bits_cmp(i)(i - 1 DOWNTO 0) <= (OTHERS => '0');
    END GENERATE;

    count_proc : PROCESS (d_in, bits_cmp)
        VARIABLE count : INTEGER;
    BEGIN
        count := 0;
        FOR i IN 1 TO Nb - 1 LOOP
            IF d_in(i DOWNTO 0) = bits_cmp(i)(i DOWNTO 0) THEN
                count := i - 1; -- Encode the (#zeros -1) inside the data word
            END IF;
        END LOOP;
        IF d_in = bits_cmp(Nb) THEN
            count := Nb - 1; -- Encode the (#zeros -1) inside the data word
        END IF;
        d_out <= std_logic_vector(to_unsigned(count, d_out'length));
    END PROCESS;

END ARCHITECTURE;