-------------------------------------------------------
--! @file
--! @brief Register row
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
-- Title      : Register Row
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File        : register_row.vhd
-- Author      : sadiq  <sadiq@drrasystem>
-- Company     : 
-- Created     : 2013-07-19
-- Last update : 2013-10-06
-- Platform    : 
-- Standard    : VHDL'87
-------------------------------------------------------------------------------
-- Description: <cursor>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2013-07-19  1.0      sadiq	Created
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
--! Use the top constant package that includes all the constants and type definitions
use work.top_consts_types_package.all;

--! @brief Register row, one word of the data register file
--! @detail This is the word row of the register file
entity register_row is
	port(
		rst_n	:in  std_logic; --! Reset signal, negative
		clk     :in  std_logic; --! Clock 
		wr_enb	:in  std_logic; --!	Write eneable signal
		reg_in	:in  signed(REG_FILE_DATA_WIDTH-1 downto 0); --! Data input
		reg_out	:out signed(REG_FILE_DATA_WIDTH-1 downto 0)); --! Data output
end register_row;

--! @brief Architecture of the register file row
architecture behavioral of register_row is
begin
	process(rst_n,clk,reg_in)
	begin
		if rst_n = '0' then
			reg_out <= (others=>'0');
		elsif clk'event and clk = '1' then
			if wr_enb = '1' then
				reg_out <= reg_in;
			end if;
		end if;
	end process;
end behavioral;
