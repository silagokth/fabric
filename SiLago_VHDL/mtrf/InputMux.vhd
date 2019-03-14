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

-- Description 
--
-- Generic 8-14X1 tristate MUX
-- Used as a building block in Switchbox Version2
-------------------------
LIBRARY IEEE, work;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;  
--USE ieee.std_logic_unsigned.ALL;
USE work.top_consts_types_package.ALL;

ENTITY InputMux IS
  GENERIC(
    BITWIDTH : integer := BITWIDTH;
    M        : integer := MAX_NR_OF_OUTP_N_HOPS
    );

  PORT (
    --clk    : IN  std_logic;
    --rst_n  : IN  std_logic;
    inputs_reg : IN  h_bus_ty(0 TO M-1, 0 TO NR_OF_OUTP-1);
    inputs_dpu : IN  h_bus_ty(0 TO M-1, 0 TO NR_OF_OUTP-1);
    sel    : IN  std_logic_vector(TRISTATE_SEL DOWNTO 0);
    output : OUT signed (BITWIDTH-1 DOWNTO 0)  --changed only in this version 
    );
END InputMux;

ARCHITECTURE behaviour_RTL OF InputMux IS
  FUNCTION bit_to_int (in_bit : std_logic)
    RETURN integer IS
  BEGIN
    IF in_bit = '0' THEN
      RETURN 0;
    ELSE
      RETURN 1;
    END IF;
  END bit_to_int;
  signal data : h_bus_ty(0 TO M-1, 0 TO NR_OF_OUTP-1);
BEGIN
  data <= inputs_reg when (sel(4) = '0') else inputs_dpu;
  output <= (OTHERS => '0') when (sel(2 DOWNTO 0) = "111") else data(to_integer(unsigned(sel(2 DOWNTO 0))), bit_to_int(sel(3)));
--  PROCESS(sel, data, rst_n)
--  BEGIN
--    IF (rst_n = '0') THEN
--      output <= (OTHERS => '0');
--      if clk
--    ELSIF (sel(2 DOWNTO 0) = "111") THEN
--      output <= (OTHERS => '0');
--    ELSE
--      output <= data(to_integer(unsigned(sel(3 DOWNTO 1))), bit_to_int(sel(3)));
--    END IF;
--  END PROCESS;
END behaviour_RTL;

