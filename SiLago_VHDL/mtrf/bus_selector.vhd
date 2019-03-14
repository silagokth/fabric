-------------------------------------------------------
--! @file
--! @brief bus_selector
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
-- Title      : bus_selector
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : bus_selector.vhd
-- Author     : Muhammad Adeel Tajammul
-- Company    : KTH
-- Created    : 
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                 Description
--          1.0     Muhammad Adeel Tajammul
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

---------------------------------------------------------------------------------
-- This is a bus selector  which is used to select which bus of silego should go 
-- through
---------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
--use work.drra_types_n_constants.ROWS;
--use work.drra_types_n_constants.COLUMNS;
--use work.drra_types_n_constants.INSTR_WIDTH;
use work.noc_types_n_constants.all;
--use WORK.SINGLEPORT_SRAM_AGU_types_n_constants.initial_delay_WIDTH;
use work.misc.all;

entity bus_selector is
	generic(this_column : integer); 
	port(
		noc_bus_in0 : in NOC_BUS_TYPE;--INST_SIGNAL_TYPE(0 to COLUMNS, 0 to ROWS-1);
		noc_bus_in1 : in NOC_BUS_TYPE;
		noc_bus_out : out NOC_BUS_TYPE
	);
end entity bus_selector;

architecture RTL of bus_selector is
begin
	noc_bus_out  <= noc_bus_in0 when (noc_bus_in0.bus_enable = '1' ) else
					noc_bus_in1 when (noc_bus_in1.bus_enable = '1' ) else
					IDLE_BUS;
	
--	p_bus_selector : process(noc_bus_in) is
--	begin
--			
--		for j in 0 to ROWS loop
--			if noc_bus_in(this_column, j).bus_enable = '1' then
--				noc_bus_out <= noc_bus_in(this_column, j);
--			else
--				noc_bus_out <= IDLE_BUS;
--			end if;
--		end loop;
--
--	end process p_bus_selector;

end architecture RTL;

