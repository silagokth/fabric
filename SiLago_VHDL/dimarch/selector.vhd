-------------------------------------------------------
--! @file
--! @brief selector
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
-- Title      : selector
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : selector.vhd
-- Author     : Muhammad Adeel Tajammul
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
--				1.0     Muhammad Adeel Tajammul
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.NUMERIC_STD.all;
--use work.top_consts_types_package.SRAM_AGU_INSTR_WIDTH;
--use work.noc_types_n_constants.SRAM_AGU_INSTR_WIDTH;
use work.noc_types_n_constants.NOC_BUS_TYPE;
use work.noc_types_n_constants.PARTITION_INSTRUCTION_RECORD_TYPE;
use work.noc_types_n_constants.IDLE_BUS;
use work.noc_types_n_constants.IDLE_PAR_INST;
use work.noc_types_n_constants.sram_agu_instruction_type;
use work.noc_types_n_constants.sram_instr_zero;
-----------------------------------------------------------
-- this code should be split based on outputs so that during layout relevant outputs 
-- can be placed closed the their edges . -- Adeel 

entity selector is
	port(
		left_turn_left, left_turn_right, right_turn_left, right_turn_right, top_turn_left, top_turn_right, bottom_turn_left, bottom_turn_right : in  std_logic;

		top_agu_en_r, top_agu_en_w                                                                                                             : in  std_logic;
		top_SRAM_AGU_instruction_r                                                                                                             : in  sram_agu_instruction_type;
		top_SRAM_AGU_instruction_w                                                                                                             : in  sram_agu_instruction_type;
		bottom_agu_en_r, bottom_agu_en_w                                                                                                       : in  std_logic;
		bottom_SRAM_AGU_instruction_r                                                                                                          : in  sram_agu_instruction_type;
		bottom_SRAM_AGU_instruction_w                                                                                                          : in  sram_agu_instruction_type;
		left_agu_en_r, left_agu_en_w                                                                                                           : in  std_logic;
		left_SRAM_AGU_instruction_r                                                                                                            : in  sram_agu_instruction_type; 
		left_SRAM_AGU_instruction_w                                                                                                            : in  sram_agu_instruction_type; 
		right_agu_en_r, right_agu_en_w                                                                                                         : in  std_logic;
		right_SRAM_AGU_instruction_r                                                                                                           : in  sram_agu_instruction_type;
		right_SRAM_AGU_instruction_w                                                                                                           : in  sram_agu_instruction_type;

		top_north_instruction_out, top_south_instruction_out, top_east_instruction_out, top_west_instruction_out                               : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		bottom_north_instruction_out, bottom_south_instruction_out, bottom_east_instruction_out, bottom_west_instruction_out                   : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		left_north_instruction_out, left_south_instruction_out, left_east_instruction_out, left_west_instruction_out                           : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		right_north_instruction_out, right_south_instruction_out, right_east_instruction_out, right_west_instruction_out                       : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		--------------------------------------------------------
		--	source decoder n fsm suggested bus out values
		--------------------------------------------------------

		top_bus_out                                                                                                                            : in  NOC_BUS_TYPE;
		bottom_bus_out                                                                                                                         : in  NOC_BUS_TYPE;
		left_bus_out                                                                                                                           : in  NOC_BUS_TYPE;
		right_bus_out                                                                                                                          : in  NOC_BUS_TYPE;
		-----------------------------------------------------------------------------
		--	source decoder n fsm suggested hor_sel and vertical select values 
		-----------------------------------------------------------------------------

		top_bus_ver_sel, top_bus_hor_sel                                                                                                       : in  std_logic;
		bottom_bus_ver_sel, bottom_bus_hor_sel                                                                                                 : in  std_logic;
		left_bus_ver_sel, left_bus_hor_sel                                                                                                     : in  std_logic;
		right_bus_ver_sel, right_bus_hor_sel                                                                                                   : in  std_logic;

		----------------------------
		--	iNoC segmented bus output  
		----------------------------

		north_bus_out                                                                                                                          : out NOC_BUS_TYPE;
		south_bus_out                                                                                                                          : out NOC_BUS_TYPE;
		east_bus_out                                                                                                                           : out NOC_BUS_TYPE;
		west_bus_out                                                                                                                           : out NOC_BUS_TYPE;

		----------------------------
		--	Partition setup I/0  
		----------------------------
		top_instruction                                                                                                                        : out PARTITION_INSTRUCTION_RECORD_TYPE;
		bottom_instruction                                                                                                                     : out PARTITION_INSTRUCTION_RECORD_TYPE;
		left_instruction                                                                                                                       : out PARTITION_INSTRUCTION_RECORD_TYPE;
		right_instruction                                                                                                                      : out PARTITION_INSTRUCTION_RECORD_TYPE;
		----------------------------
		--	AGUs and handles input  
		----------------------------
		SRAM_AGU_instruction_r                                                                                                                 : out sram_agu_instruction_type;
		SRAM_AGU_instruction_w                                                                                                                 : out sram_agu_instruction_type;
		agu_en_r                                                                                                                               : out std_logic;
		agu_en_w                                                                                                                               : out std_logic
	);
end entity selector;

architecture RTL of selector is
begin
	-- why does this process produce multiple drivers i.e. X when all output is changed to 1
	p_top_instruction : process(top_north_instruction_out, bottom_north_instruction_out, left_north_instruction_out, right_north_instruction_out) is
	begin
		if top_north_instruction_out.ENABLE = '1' then
			top_instruction <= top_north_instruction_out;
		elsif left_north_instruction_out.ENABLE = '1' then
			top_instruction <= left_north_instruction_out;
		elsif right_north_instruction_out.ENABLE = '1' then
			top_instruction <= right_north_instruction_out;
		elsif bottom_north_instruction_out.ENABLE = '1' then
			top_instruction <= bottom_north_instruction_out;
		else
			top_instruction <= IDLE_PAR_INST;
		end if;
	end process p_top_instruction;

	p_bottom_instruction : process(top_south_instruction_out, bottom_south_instruction_out, left_south_instruction_out, right_south_instruction_out) is
	begin
		if top_south_instruction_out.ENABLE = '1' then
			bottom_instruction <= top_south_instruction_out;
		elsif left_south_instruction_out.ENABLE = '1' then
			bottom_instruction <= left_south_instruction_out;
		elsif right_south_instruction_out.ENABLE = '1' then
			bottom_instruction <= right_south_instruction_out;
		elsif bottom_south_instruction_out.ENABLE = '1' then
			bottom_instruction <= bottom_south_instruction_out;
		else
			bottom_instruction <= IDLE_PAR_INST;
		end if;
	end process p_bottom_instruction;

	p_left_instruction : process(top_west_instruction_out, bottom_west_instruction_out, left_west_instruction_out, right_west_instruction_out) is
	begin
		if top_west_instruction_out.ENABLE = '1' then
			left_instruction <= top_west_instruction_out;
		elsif left_west_instruction_out.ENABLE = '1' then
			left_instruction <= left_west_instruction_out;
		elsif right_west_instruction_out.ENABLE = '1' then
			left_instruction <= right_west_instruction_out;
		elsif bottom_west_instruction_out.ENABLE = '1' then
			left_instruction <= bottom_west_instruction_out;
		else
			left_instruction <= IDLE_PAR_INST;
		end if;
	end process p_left_instruction;

	p_right_instruction : process(top_east_instruction_out, bottom_east_instruction_out, left_east_instruction_out, right_east_instruction_out) is
	begin
		if top_east_instruction_out.ENABLE = '1' then
			right_instruction <= top_east_instruction_out;
		elsif left_east_instruction_out.ENABLE = '1' then
			right_instruction <= left_east_instruction_out;
		elsif right_east_instruction_out.ENABLE = '1' then
			right_instruction <= right_east_instruction_out;
		elsif bottom_east_instruction_out.ENABLE = '1' then
			right_instruction <= bottom_east_instruction_out;
		else
			right_instruction <= IDLE_PAR_INST;
		end if;
	end process p_right_instruction;

	p_agu_r : process(top_agu_en_r, bottom_agu_en_r, left_agu_en_r, right_agu_en_r, top_SRAM_AGU_instruction_r, bottom_SRAM_AGU_instruction_r, left_SRAM_AGU_instruction_r, right_SRAM_AGU_instruction_r) is
	begin
		if bottom_agu_en_r = '1' then
			SRAM_AGU_instruction_r <= bottom_SRAM_AGU_instruction_r;
			agu_en_r               <= bottom_agu_en_r;
		elsif left_agu_en_r = '1' then
			SRAM_AGU_instruction_r <= left_SRAM_AGU_instruction_r;
			agu_en_r               <= left_agu_en_r;
		elsif right_agu_en_r = '1' then
			SRAM_AGU_instruction_r <= right_SRAM_AGU_instruction_r;
			agu_en_r               <= right_agu_en_r;
		elsif top_agu_en_r = '1' then
			SRAM_AGU_instruction_r <= top_SRAM_AGU_instruction_r;
			agu_en_r               <= top_agu_en_r;
		else
			SRAM_AGU_instruction_r <= sram_instr_zero;--(others => '0');
			agu_en_r               <= '0';
		end if;

	end process p_agu_r;
	p_agu_w : process(top_agu_en_w, bottom_agu_en_w, left_agu_en_w, right_agu_en_w, top_SRAM_AGU_instruction_w, bottom_SRAM_AGU_instruction_w, left_SRAM_AGU_instruction_w, right_SRAM_AGU_instruction_w) is
	begin
		if bottom_agu_en_w = '1' then
			SRAM_AGU_instruction_w <= bottom_SRAM_AGU_instruction_w;
			agu_en_w               <= bottom_agu_en_w;
		elsif left_agu_en_w = '1' then
			SRAM_AGU_instruction_w <= left_SRAM_AGU_instruction_w;
			agu_en_w               <= left_agu_en_w;
		elsif right_agu_en_w = '1' then
			SRAM_AGU_instruction_w <= right_SRAM_AGU_instruction_w;
			agu_en_w               <= right_agu_en_w;
		elsif top_agu_en_w = '1' then
			SRAM_AGU_instruction_w <= top_SRAM_AGU_instruction_w;
			agu_en_w               <= top_agu_en_w;
		else
			SRAM_AGU_instruction_w <= sram_instr_zero;--(others => '0');
			agu_en_w               <= '0';
		end if;

	end process p_agu_w;

	p_north_bus_out : process(top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_ver_sel, bottom_bus_ver_sel, left_bus_ver_sel, right_bus_ver_sel) is
	begin
		if left_bus_ver_sel = '1' then
			north_bus_out <= left_bus_out;
		elsif right_bus_ver_sel = '1' then
			north_bus_out <= right_bus_out;
		elsif bottom_bus_ver_sel = '1' then
			north_bus_out <= bottom_bus_out;
		elsif top_bus_ver_sel = '1' then
			north_bus_out <= top_bus_out;
		else
			north_bus_out <= IDLE_BUS;
		end if;
	end process p_north_bus_out;

	p_south_bus_out : process(top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_ver_sel, bottom_bus_ver_sel, left_bus_ver_sel, right_bus_ver_sel) is
	begin
		if left_bus_ver_sel = '1' then
			south_bus_out <= left_bus_out;
		elsif right_bus_ver_sel = '1' then
			south_bus_out <= right_bus_out;
		elsif bottom_bus_ver_sel = '1' then
			south_bus_out <= bottom_bus_out;
		elsif top_bus_ver_sel = '1' then
			south_bus_out <= top_bus_out;
		else
			south_bus_out <= IDLE_BUS;
		end if;
	end process p_south_bus_out;

	p_east_bus_out : process(top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_hor_sel, bottom_bus_hor_sel, left_bus_hor_sel, right_bus_hor_sel, bottom_turn_right) is
	begin
		if left_bus_hor_sel = '1' then
			east_bus_out <= left_bus_out;
		elsif right_bus_hor_sel = '1' then
			east_bus_out <= right_bus_out;
		elsif bottom_bus_hor_sel = '1' and bottom_turn_right = '1' then
			east_bus_out <= bottom_bus_out;
		elsif top_bus_hor_sel = '1' then
			east_bus_out <= top_bus_out;
		else
			east_bus_out <= IDLE_BUS;
		end if;
	end process p_east_bus_out;

	p_west_bus_out : process(top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_hor_sel, bottom_bus_hor_sel, left_bus_hor_sel, right_bus_hor_sel, bottom_turn_left) is
	begin
		if left_bus_hor_sel = '1' then
			west_bus_out <= left_bus_out;
		elsif right_bus_hor_sel = '1' then
			west_bus_out <= right_bus_out;
		elsif bottom_bus_hor_sel = '1' and bottom_turn_left = '1' then
			west_bus_out <= bottom_bus_out;
		elsif top_bus_hor_sel = '1' then
			west_bus_out <= top_bus_out;
		else
			west_bus_out <= IDLE_BUS;
		end if;
	end process p_west_bus_out;

end architecture RTL;
