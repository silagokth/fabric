-------------------------------------------------------
--! @file
--! @brief MTRF cell
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
-- Title      : MTRF_cell
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : MTRF_cell.vhd
-- Author     : Muhammad Ali Shami 
-- Company    : KTH
-- Created    : 
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		          Description
--             1.0      Muhammad Ali Shami    Created
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	   #
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
--! IEEE Library 
library ieee;
--! Use standard library
use ieee.std_logic_1164.all;
--! Use numeric library for signed and unsigned arithmetics
use ieee.numeric_std.all;

PACKAGE misc IS
  FUNCTION log2_ceil(N         :    NATURAL) RETURN POSITIVE;
  FUNCTION b2n (B              :    BIT_VECTOR) RETURN NATURAL;
  FUNCTION n2b (nat            : IN NATURAL; length : IN NATURAL) RETURN BIT_VECTOR;
  FUNCTION sel_ip (inputlength :    INTEGER) RETURN INTEGER;
  FUNCTION shift (din:std_logic_vector; s:unsigned) return std_logic_vector;
 
END;

PACKAGE BODY misc IS
 
  function shift (din:std_logic_vector; s:unsigned) return std_logic_vector is
    variable x:bit_vector(15 downto 0);
    variable y:integer range 0 to 15;
    begin
      y:=to_integer(unsigned(s(3 downto 0)));   
      x:=to_bitvector(din);
      if(s(4)='1') then
        x:=x sra y;
      else
        x:=x sll y;
      end if;
      return To_stdlogicvector(x);
    end shift;
  
  
  
   FUNCTION divide_by2 (inputlength : INTEGER) RETURN INTEGER IS
    VARIABLE divide_2 : INTEGER RANGE 0 TO 15;

  BEGIN
    
    divide_2:=inputlength/2;
    if(divide_2>0) then
      divide_2:=divide_2-1;
    end if;
    
    RETURN divide_2;
  END divide_by2;
  
  
  
  FUNCTION sel_ip (inputlength : INTEGER) RETURN INTEGER IS
    VARIABLE retlength : INTEGER RANGE 0 TO 16;

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
    VARIABLE S : BIT_VECTOR(B'LENGTH - 1 DOWNTO 0) := B;
    VARIABLE N : NATURAL                           := 0;
  BEGIN
    FOR i IN S'RIGHT TO S'LEFT LOOP
      IF S(i) = '1' THEN
        N := N + (2**i);
      END IF;
    END LOOP;
    RETURN N;
  END;
  
  FUNCTION n2b (nat     : IN NATURAL;
                 length : IN NATURAL) RETURN BIT_VECTOR IS

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
