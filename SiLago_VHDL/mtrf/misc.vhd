-------------------------------------------------------
--! @file misc.vhd
--! @brief 
--! @details 
--! @author Muhammad Ali Shami
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
-- File       : misc.vhd
-- Author     : Muhammad Ali Shami
-- Company    : KTH
-- Created    : 
-- Last update: 
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2013
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


LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
PACKAGE misc IS
    -- This package is from http://www.velocityreviews.com/forums/t22799-log2n.html
    --- find minimum number of bits required to
    --- represent N as an unsigned binary number
    ---
    FUNCTION log2_ceil(N         : NATURAL) RETURN POSITIVE;
    FUNCTION b2n (B              : BIT_VECTOR) RETURN NATURAL;
    FUNCTION n2b (nat : IN NATURAL; length : IN NATURAL) RETURN BIT_VECTOR;
    FUNCTION sel_ip (inputlength : INTEGER) RETURN INTEGER;

    --Function shift is taken from http://forums.techarena.in/software-development/1115190.htm
    --  function shift (din:std_logic_vector; s:std_logic_vector) return std_logic_vector;
    FUNCTION shift (din : std_logic_vector; s : unsigned) RETURN std_logic_vector;

END;

PACKAGE BODY misc IS
    --- find minimum number of bits required to
    --- represent N as an unsigned binary number
    ---

    FUNCTION shift (din : std_logic_vector; s : unsigned) RETURN std_logic_vector IS
        VARIABLE x : bit_vector(15 DOWNTO 0);
        VARIABLE y : INTEGER RANGE 0 TO 15;
    BEGIN
        y := to_integer(unsigned(s(3 DOWNTO 0)));
        x := to_bitvector(din);
        IF (s(4) = '1') THEN
            x := x SRA y;
        ELSE
            x := x SLL y;
        END IF;
        RETURN To_stdlogicvector(x);

    END shift;

    FUNCTION divide_by2 (inputlength : INTEGER) RETURN INTEGER IS
        VARIABLE divide_2                : INTEGER RANGE 0 TO 15;

    BEGIN

        divide_2 := inputlength/2;
        IF (divide_2 > 0) THEN
            divide_2 := divide_2 - 1;
        END IF;

        RETURN divide_2;
    END divide_by2;

    FUNCTION sel_ip (inputlength : INTEGER) RETURN INTEGER IS
        VARIABLE retlength           : INTEGER RANGE 0 TO 16;

    BEGIN
        FOR i IN 0 TO 4 LOOP
            IF ((2 ** i) >= inputlength) THEN
                retlength := i - 1;
                EXIT;
            END IF;
        END LOOP;
        RETURN retlength;
    END sel_ip;

    FUNCTION log2_ceil(N : NATURAL) RETURN POSITIVE IS
    BEGIN
        IF N <= 2 THEN
            RETURN 1;
        ELSE
            RETURN 1 + log2_ceil(N/2);
        END IF;
    END;

    FUNCTION b2n (B : BIT_VECTOR) RETURN NATURAL IS
        VARIABLE S      : BIT_VECTOR(B'LENGTH - 1 DOWNTO 0) := B;
        VARIABLE N      : NATURAL                           := 0;
    BEGIN
        FOR i IN S'RIGHT TO S'LEFT LOOP
            IF S(i) = '1' THEN
                N := N + (2 ** i);
            END IF;
        END LOOP;
        RETURN N;
    END;

    FUNCTION n2b (nat : IN NATURAL;
        length            : IN NATURAL) RETURN BIT_VECTOR IS

        VARIABLE temp   : NATURAL                         := nat;
        VARIABLE result : BIT_VECTOR(length - 1 DOWNTO 0) := (OTHERS => '0');

    BEGIN
        FOR index IN result'REVERSE_RANGE LOOP
            result(index) := BIT'VAL(temp REM 2);
            temp          := temp / 2;
            EXIT WHEN temp = 0;
        END LOOP;
        RETURN result;
    END n2b;
END;