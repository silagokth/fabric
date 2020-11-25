-------------------------------------------------------
--! @file barrel_shifter_rle.vhd
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
-- File       : barrel_shifter_rle.vhd
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
USE work.functions.ALL;

ENTITY barrel_shifter_rle IS
    GENERIC
    (
        Nw : INTEGER;
        Nb : INTEGER

    );
    PORT
    (
        sh_l_rn : IN std_logic;
        sh_num  : IN std_logic_vector(log2c(Nw + 1) - 1 DOWNTO 0); -- CHECK input of log2c
        d_in    : IN std_logic_vector(Nw * Nb - 1 DOWNTO 0);
        d_out   : OUT std_logic_vector(Nw * Nb - 1 DOWNTO 0)
    );
END ENTITY;

-- Architecture that uses the function shift_left with a run-time changing number of
-- bits to shift. This way of describing the operation should induce the compiler
-- to instantiate a barrel shifter
ARCHITECTURE bhv_shift OF barrel_shifter_rle IS
    SIGNAL sh_am_signal : INTEGER;
BEGIN
    d_out_gen : PROCESS (d_in, sh_num, sh_l_rn)
        VARIABLE shift_amount : INTEGER RANGE 0 TO Nb * (2 ** (sh_num'length) - 1);
        --variable shift_amount 	: integer range 0 to Nw*Nb;
    BEGIN
        shift_amount := Nb * to_integer(unsigned(sh_num));
        sh_am_signal <= shift_amount;
        IF sh_l_rn = '1' THEN
            d_out <= std_logic_vector(shift_left(unsigned(d_in), shift_amount));
        ELSE
            d_out <= std_logic_vector(shift_right(unsigned(d_in), shift_amount));
        END IF;
    END PROCESS;

END ARCHITECTURE;